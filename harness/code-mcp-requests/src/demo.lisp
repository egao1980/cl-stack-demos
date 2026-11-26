(in-package #:harness-demos-code-mcp-requests)

;;; cl-stack-code 0.16.0 (H3e-2b): what an MCP server may ask of its client
;;; (sampling, elicitation) and deferred tool loading behind tool_search.
;;; The -p scenes run `cl-stack-code -p --output-format stream-json` in
;;; process (CODE:RUN-CLI); the elicitation answer goes through the
;;; in-process SDK (CODE:MAKE-CLIENT). The built-in openai provider is
;;; pointed at a scripted OpenAI-compatible server on 127.0.0.1, so
;;; llm-protocol-openai's HTTP path and ai-agent-protocol 0.17.0's tool loop
;;; run end to end. The MCP server is a stub: a child SBCL serving
;;; mcp-protocol's DISPATCH-MCP-METHOD over rpc-backend-stdio, whose tools
;;; send the client sampling/createMessage and elicitation/create while they
;;; run. No real model, no real key: the key is the dummy +DEMO-KEY+, never
;;; printed. The model server records every request, so the output shows
;;; what the model saw.

(defparameter +demo-key+ "demo-key-3e2b51"
  "The dummy API key the provider sends (OPENAI_API_KEY).")

(defparameter +sample-system+ "You are the docs server's summarizer."
  "The systemPrompt of the stub's sampling request; the mock answers a
request with this system text as a sample.")

(defparameter +sample-reply+ "MCP sampling lets a server borrow the host's model, under the host's rules."
  "What the mock model answers a sample with.")

;;; Output.

(defvar *redactions* '() "(from . to) pairs applied to every printed line.")

(defun redact (text)
  (let ((text (princ-to-string text)))
    (loop for (from . to) in *redactions*
          do (loop for p = (search from text)
                   while p
                   do (setf text (concatenate 'string (subseq text 0 p) to
                                              (subseq text (+ p (length from)))))))
    text))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title "")
  (finish-output))

(defun note (fmt &rest args)
  (format t "~&~a~%" (redact (apply #'format nil fmt args)))
  (finish-output))

(defun note-block (prefix text)
  "TEXT line by line, each line after PREFIX."
  (dolist (line (uiop:split-string (string-right-trim '(#\Newline) (or text ""))
                                   :separator '(#\Newline)))
    (note "~a~a" prefix line)))

(defun elapsed (t0)
  (/ (- (get-internal-real-time) t0) internal-time-units-per-second))

(defun jget (object &rest keys)
  (dolist (k keys object)
    (setf object (and (hash-table-p object) (gethash k object)))))

(defun jlist (value)
  (cond ((stringp value) (list value))
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t '())))

(defun obj (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun args (&rest kv)
  "Tool-call arguments: a JSON object string."
  (json:encode (apply #'obj kv)))

(defun show-json (label value)
  (note "~a~a" label (if (or (hash-table-p value) (vectorp value)) (json:encode value) value)))

;;; The scripted model server. A request whose system text is
;;; +SAMPLE-SYSTEM+ is a sample: it gets +SAMPLE-REPLY+ and does not use the
;;; script. Every other request pops the next step (the last repeats):
;;;   (:text "answer")                      an answer
;;;   (:tools ("name" "{json args}") ...)   tool calls
;;; Streamed requests get SSE, the others one chat.completion object.

(defvar *requests* '() "Decoded request bodies, newest first.")
(defvar *requests-lock* (bt:make-lock "requests"))

(defun %read-line* (stream)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
          while b
          do (vector-push-extend b out)
             (when (and (>= (length out) 2) (= 13 (aref out (- (length out) 2)))
                        (= 10 (aref out (1- (length out)))))
               (return)))
    (and (plusp (length out))
         (string-right-trim '(#\Return #\Newline) (babel:octets-to-string out :errorp nil)))))

(defun %read-request (stream)
  "→ (values body request-line): the body (a string), or NIL when the client
hung up."
  (let ((len 0)
        (line (%read-line* stream)))
    (when line
      (loop for h = (%read-line* stream)
            while (and h (plusp (length h)))
            do (let ((c (position #\: h)))
                 (when (and c (string-equal "content-length" (subseq h 0 c)))
                   (setf len (or (parse-integer h :start (1+ c) :junk-allowed t) 0)))))
      (let ((body (make-array len :element-type '(unsigned-byte 8))))
        (read-sequence body stream)
        (values (babel:octets-to-string body :errorp nil) line)))))

(defun %out (stream string)
  (write-sequence (babel:string-to-octets string) stream)
  (force-output stream))

(defun %crlf (&rest lines)
  (format nil "~{~A~C~C~}" (loop for l in lines collect l collect #\Return collect #\Newline)))

(defun %chunk (stream text)
  (let ((o (babel:string-to-octets text)))
    (%out stream (format nil "~X~C~C" (length o) #\Return #\Newline))
    (write-sequence o stream)
    (%out stream (%crlf ""))))

(defvar *call-counter* 0)

(defun %usage (sample-p)
  (if sample-p
      (obj "prompt_tokens" 31 "completion_tokens" 14 "total_tokens" 45)
      (obj "prompt_tokens" 900 "completion_tokens" 40 "total_tokens" 940)))

(defun %tool-calls (calls)
  (coerce (loop for (name arguments) in calls
                for i from 0
                collect (obj "index" i
                             "id" (format nil "call_~d" (incf *call-counter*))
                             "type" "function"
                             "function" (obj "name" name "arguments" arguments)))
          'vector))

(defun %message (step)
  "→ (values message finish-reason) for STEP."
  (ecase (first step)
    (:text (values (obj "role" "assistant" "content" (second step)) "stop"))
    (:tools (values (obj "role" "assistant" "tool_calls" (%tool-calls (rest step))) "tool_calls"))))

(defun %answer (stream step streamed sample-p)
  (multiple-value-bind (message finish) (%message step)
    (if streamed
        (progn
          (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                              "Transfer-Encoding: chunked" "Connection: close" ""))
          (dolist (event (list (obj "model" "demo-model"
                                    "choices" (vector (obj "index" 0 "delta" message)))
                               (obj "model" "demo-model"
                                    "choices" (vector (obj "index" 0 "delta" (obj)
                                                           "finish_reason" finish)))
                               (obj "model" "demo-model" "choices" (vector)
                                    "usage" (%usage sample-p))))
            (%chunk stream (format nil "data: ~a~%~%" (json:encode event))))
          (%chunk stream (format nil "data: [DONE]~%~%"))
          (%out stream (%crlf "0" "")))
        (let ((body (babel:string-to-octets
                     (json:encode (obj "id" "chatcmpl-demo" "object" "chat.completion"
                                       "model" "demo-model"
                                       "choices" (vector (obj "index" 0 "message" message
                                                              "finish_reason" finish))
                                       "usage" (%usage sample-p))))))
          (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: application/json"
                              (format nil "Content-Length: ~d" (length body))
                              "Connection: close" ""))
          (write-sequence body stream)
          (force-output stream)))))

(defun content-string (content)
  "A message's content as text (a string or an array of text parts)."
  (cond ((stringp content) content)
        ((vectorp content)
         (format nil "~{~a~}" (map 'list (lambda (part) (or (jget part "text") "")) content)))
        ((null content) nil)
        (t (princ-to-string content))))

(defun request-messages (request) (jlist (jget request "messages")))

(defun system-text (request)
  (let ((m (find "system" (request-messages request)
                 :key (lambda (m) (jget m "role")) :test #'equal)))
    (and m (content-string (jget m "content")))))

(defun sample-request-p (request)
  (let ((s (system-text request)))
    (and s (search +sample-system+ s) t)))

(defvar *other-requests* '() "Request lines of the requests that were not a chat completion, newest first.")

(defun %serve-one (s steps-cell lock)
  "Answer one HTTP request on S: a chat completion from the script (or a
sample), anything else (a GET) a one-model list, logged apart."
  (multiple-value-bind (body line) (%read-request s)
    (when line
      (let ((request (and (plusp (length body)) (ignore-errors (json:decode body)))))
        (if (not (and (uiop:string-prefix-p "POST " line) (hash-table-p request)))
            (let ((list (babel:string-to-octets
                         (json:encode (obj "object" "list"
                                           "data" (vector (obj "id" "demo-model" "object" "model")))))))
              (bt:with-lock-held (*requests-lock*) (push line *other-requests*))
              (%out s (%crlf "HTTP/1.1 200 OK" "Content-Type: application/json"
                             (format nil "Content-Length: ~d" (length list)) "Connection: close" ""))
              (write-sequence list s)
              (force-output s))
            (let* ((sample-p (sample-request-p request))
                   (step (if sample-p
                             (list :text +sample-reply+)
                             (bt:with-lock-held (lock)
                               (let ((steps (car steps-cell)))
                                 (if (rest steps) (pop (car steps-cell)) (first steps))))))
                   (streamed (eq t (gethash "stream" request))))
              (bt:with-lock-held (*requests-lock*) (push request *requests*))
              (%answer s step streamed sample-p)))))))

(defun call-with-server (script fn)
  "Serve SCRIPT on 127.0.0.1 (a free port) while FN runs with the base URL."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t
                                                      :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (steps-cell (list (copy-list script)))
         (lock (bt:make-lock "script"))
         (thread
           (bt:make-thread
            (lambda ()
              (loop (handler-case
                        (let ((c (usocket:socket-accept server :element-type '(unsigned-byte 8))))
                          (bt:make-thread
                           (lambda ()
                             (unwind-protect
                                 (ignore-errors (%serve-one (usocket:socket-stream c) steps-cell lock))
                               (ignore-errors (usocket:socket-close c))))
                           :name "demo-model-request"))
                      (error () (return)))))
            :name "demo-model-server"))
         (*redactions* (list* (cons (format nil "127.0.0.1:~d" port) "127.0.0.1:<port>")
                              *redactions*)))
    (unwind-protect (funcall fn (format nil "http://127.0.0.1:~d/v1" port))
      (ignore-errors (usocket:socket-close server))
      (when (bt:thread-alive-p thread) (ignore-errors (bt:destroy-thread thread))))))

(defmacro with-scene ((base script) &body body)
  ;; SETF, not LET: the server's request threads see the global value.
  `(progn
     (bt:with-lock-held (*requests-lock*) (setf *requests* '() *other-requests* '()))
     (call-with-server ,script (lambda (,base) ,@body))))

(defun requests-seen () (bt:with-lock-held (*requests-lock*) (reverse *requests*)))

(defun request-tools (request)
  (mapcar (lambda (tool) (jget tool "function" "name")) (jlist (jget request "tools"))))

(defun last-tool-results (request)
  "The tool results at the end of REQUEST's messages, oldest first."
  (reverse (loop for m in (reverse (request-messages request))
                 while (equal "tool" (jget m "role"))
                 collect (content-string (jget m "content")))))

(defun %builtin-p (name)
  (not (or (uiop:string-prefix-p "mcp__" name) (equal name "tool_search"))))

(defun show-requests ()
  "Each model request: a sample's system prompt, messages and max tokens;
otherwise the tools offered (built-ins counted) and the tool results it
carries back. Then the requests that were not chat completions."
  (loop for r in (requests-seen)
        for i from 1
        do (if (sample-request-p r)
               (progn
                 (note "request ~d (a SAMPLE): model ~a, max_tokens ~a, tools ~a"
                       i (jget r "model")
                       (or (jget r "max_tokens") (jget r "max_completion_tokens") "none")
                       (length (request-tools r)))
                 (dolist (m (request-messages r))
                   (note "           ~a: ~a" (jget m "role") (content-string (jget m "content")))))
               (let ((tools (request-tools r)))
                 (note "request ~d: ~d built-in tools + ~:[(no MCP tools)~;~:*~{~a~^ ~}~]"
                       i (count-if #'%builtin-p tools) (remove-if #'%builtin-p tools))
                 (dolist (text (last-tool-results r))
                   (note-block "           tool result | " text)))))
  (let ((other (bt:with-lock-held (*requests-lock*) (reverse *other-requests*))))
    (when other
      (note "other requests (answered with a one-model list): ~{~a~^; ~}" other))))

;;; The stub MCP server: a child SBCL with this image's ASDF systems. It
;;; serves mcp-protocol's DISPATCH-MCP-METHOD on an rpc-backend-stdio
;;; transport in client mode, so a tool's handler can send the client a
;;; JSON-RPC request and wait for the answer (a 2025-11-25 server's
;;; sampling/createMessage and elicitation/create). STUB_MCP_TOOLS picks the
;;; tool set: "requests" (summarize, deploy) or "catalog" (eight plain tools).
;;; The forms are printed into the child's script file.

(defparameter +stub-forms+
  '((defvar *transport* nil)
    (defun env (name) (let ((v (uiop:getenv name))) (and v (plusp (length v)) v)))
    (defun obj (&rest kv) (apply #'mcp-protocol:json-object kv))
    (defun p (o k) (and (hash-table-p o) (mcp-protocol:param o k)))
    (defvar *server* (make-instance 'mcp-protocol:mcp-server :name "stub" :version "1.0.0"))
    (defun ask-client (method params)
      (handler-case (values (rpc-protocol:rpc-call method params :transport *transport* :timeout 120) nil)
        (rpc-protocol:rpc-error (e)
          (values nil (format nil "~a ~a" (rpc-protocol:rpc-error-code e)
                              (rpc-protocol:rpc-error-message e))))))
    (defun content-text (result)
      (let ((c (p result "content")))
        (cond ((hash-table-p c) (or (p c "text") ""))
              ((vectorp c) (format nil "~{~a~}" (map 'list (lambda (x) (or (p x "text") "")) c)))
              (t ""))))
    (defun kv-text (content)
      (if (hash-table-p content)
          (format nil "~{~a~^ ~}"
                  (sort (loop for k being the hash-keys of content using (hash-value v)
                              collect (format nil "~a=~a" k v))
                        #'string<))
          ""))
    (defun tool (name description handler)
      (mcp-protocol:register-tool
       *server*
       (mcp-protocol:make-mcp-tool name :description description
                                        :input-schema (obj "type" "object") :handler handler)))
    (if (equal (env "STUB_MCP_TOOLS") "catalog")
        (dolist (spec '(("invoice_create" "Create an invoice for a customer")
                        ("invoice_list" "List the open invoices")
                        ("invoice_send" "Send an invoice by e-mail")
                        ("weather_today" "Today's weather for a city")
                        ("calendar_next" "The next calendar event")
                        ("github_issues" "Open GitHub issues of a repository")
                        ("database_query" "Run a read-only SQL query")
                        ("echo" "Echo msg")))
          (let ((name (first spec)))
            (tool name (second spec)
                  (lambda (a) (format nil "~a ran (~a)" name (kv-text a))))))
        (progn
          (tool "summarize" "Summarize a topic with the client's model (MCP sampling)"
                (lambda (a)
                  (multiple-value-bind (result error)
                      (ask-client "sampling/createMessage"
                                  (obj "messages"
                                       (vector (obj "role" "user"
                                                    "content" (obj "type" "text"
                                                                   "text" (format nil "Summarize ~a in one line."
                                                                                  (or (p a "topic") "MCP")))))
                                       "systemPrompt" "You are the docs server's summarizer."
                                       "maxTokens" 10000
                                       "modelPreferences" (obj "hints" (vector (obj "name" "gpt-x")))
                                       "includeContext" "allServers"))
                    (if error
                        (format nil "sampling refused: ~a" error)
                        (format nil "summary: ~a (model ~a, stop ~a)" (content-text result)
                                (p result "model") (p result "stopReason"))))))
          (tool "deploy" "Deploy a service; asks the user where (MCP elicitation)"
                (lambda (a)
                  (multiple-value-bind (result error)
                      (ask-client "elicitation/create"
                                  (obj "mode" "form"
                                       "message" (format nil "Deploy ~a: which environment?"
                                                         (or (p a "service") "web"))
                                       "requestedSchema"
                                       (obj "type" "object"
                                            "properties"
                                            (obj "env" (obj "type" "string" "title" "Environment"
                                                            "enum" (vector "staging" "production"))
                                                 "replicas" (obj "type" "integer" "title" "Replicas"
                                                                 "minimum" 1 "maximum" 5))
                                            "required" (vector "env"))))
                    (if error
                        (format nil "elicitation error ~a" error)
                        (format nil "deploy: ~a ~a" (p result "action") (kv-text (p result "content")))))))))
    (defun handle (method params)
      (handler-case (mcp-protocol:dispatch-mcp-method *server* method params)
        (mcp-protocol:mcp-error (c)
          (error 'rpc-protocol:rpc-error :message (or (mcp-protocol:mcp-error-message c) "mcp error")
                                         :code (or (mcp-protocol:mcp-error-code c) -32603)
                                         :data (mcp-protocol:mcp-error-data c)))))
    (setf *transport* (rpc-backend-stdio:make-stdio-rpc-transport
                       :input *standard-input* :output *standard-output* :dispatch :concurrent
                       :on-request #'handle
                       :on-notification (lambda (m p) (ignore-errors (handle m p)))))
    (rpc-backend-stdio:stdio-rpc-transport-wait-closed *transport*)))

(defvar *scratch* nil)

(defun %native (p) (uiop:native-namestring p))

(defun %write-file (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text s)))

(defun %loaded-system-dirs ()
  (remove-duplicates
   (loop for name in (asdf:already-loaded-systems)
         for dir = (and (not (member name '("asdf" "uiop" "asdf-package-system") :test #'string=))
                        (not (uiop:string-prefix-p "sb-" name))
                        (ignore-errors (asdf:system-source-directory name)))
         when dir collect (%native dir))
   :test #'string=))

(defun %loaded-asd-table ()
  "(name . .asd) of every primary system this image loaded."
  (loop for name in (asdf:already-loaded-systems)
        for system = (asdf:registered-system name)
        for file = (and system (ignore-errors (asdf:system-source-file system)))
        when (and file (string= name (asdf:primary-system-name name)))
          collect (cons name (%native file))))

(defun %stub-setup-text ()
  "The child's ASDF setup: this image's source directories, output
translations and exact .asd files (so it loads these fasls), then
mcp-protocol and rpc-backend-stdio, with load output on stderr."
  (let ((dirs (%loaded-system-dirs)))
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            ;; the forms' own symbols (table, name, entry) print unqualified
            (*package* (find-package '#:harness-demos-code-mcp-requests)))
        (format nil "~s~%~s~%~s~%~s~%"
                `(asdf:initialize-source-registry
                  '(:source-registry ,@(mapcar (lambda (d) (list :directory d)) dirs)
                    :ignore-inherited-configuration))
                `(asdf:initialize-output-translations
                  '(:output-translations
                    ,@(mapcar (lambda (d)
                                (list d (%native (asdf:apply-output-translations
                                                  (uiop:parse-native-namestring d :ensure-directory t)))))
                              dirs)
                    :ignore-inherited-configuration))
                `(let ((table ',(%loaded-asd-table)))
                   (push (lambda (name)
                           (let ((entry (assoc (asdf:coerce-name name) table :test #'string=)))
                             (and entry (probe-file (uiop:parse-native-namestring (cdr entry))))))
                         asdf:*system-definition-search-functions*))
                '(let ((*standard-output* *error-output*))
                  (asdf:load-system "mcp-protocol")
                  (asdf:load-system "rpc-backend-stdio")))))))

(defun %stub-script-text ()
  (let ((*package* (find-package '#:harness-demos-code-mcp-requests))
        (*print-case* :downcase)
        (*print-readably* nil)
        (*print-pretty* t))
    (format nil "(in-package :cl-user)~%~{~s~%~}" +stub-forms+)))

(defun %asdf-load-form ()
  (let* ((home (ignore-errors (sb-int:sbcl-homedir-pathname)))
         (fasl (and home (probe-file (merge-pathnames "contrib/asdf.fasl" home)))))
    (if fasl (format nil "(load ~s)" (%native fasl)) "(require :asdf)")))

(defun stub-server (tools &rest keys)
  "An mcpServers entry (hash table) running the stub with STUB_MCP_TOOLS
TOOLS, plus the extra server KEYS (alternating names and values)."
  (let ((setup (merge-pathnames "stub-mcp-asdf.lisp" *scratch*))
        (script (merge-pathnames "stub-mcp-server.lisp" *scratch*)))
    (unless (probe-file setup) (%write-file setup (%stub-setup-text)))
    (unless (probe-file script) (%write-file script (%stub-script-text)))
    (let ((object (obj "command" (%native sb-ext:*runtime-pathname*)
                       "args" (vector "--core" (%native sb-ext:*core-pathname*)
                                      "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive"
                                      "--eval" (%asdf-load-form)
                                      "--eval" (format nil "(load ~s)" (%native setup))
                                      "--eval" (format nil "(load ~s)" (%native script)))
                       "env" (obj "STUB_MCP_TOOLS" tools))))
      (loop for (k v) on keys by #'cddr do (setf (gethash k object) v))
      object)))

(defun scene-dirs (name settings)
  "→ (values project config data): an empty project, a user config directory
whose settings.json is SETTINGS (a hash table), and a data directory."
  (let* ((root (merge-pathnames (format nil "~a/" name) *scratch*))
         (project (merge-pathnames "project/" root))
         (config (merge-pathnames "config/" root))
         (data (merge-pathnames "data/" root)))
    (%write-file (merge-pathnames "README.md" project) (format nil "# demo~%"))
    (%write-file (merge-pathnames "settings.json" config) (json:encode settings))
    (ensure-directories-exist data)
    (values project config data)))

(defun model-getenv (base)
  (lambda (name)
    (cond ((string= name "OPENAI_BASE_URL") base)
          ((string= name "OPENAI_API_KEY") +demo-key+)
          (t nil))))

(defun run-code (base argv &key project config data)
  "CODE:RUN-CLI on ARGV with the openai provider at BASE and the dummy key.
→ (values exit events stderr)."
  (let* ((argv (append (list "--cwd" (%native project) "--model" "openai:demo-model") argv))
         (err (make-string-output-stream))
         (exit nil)
         (out (with-output-to-string (o)
                (setf exit (code:run-cli argv :output o :error-output err
                                              :input (make-string-input-stream "")
                                              :user-config-dir config
                                              :data-dir data
                                              :getenv (model-getenv base))))))
    (values exit
            (loop for line in (uiop:split-string out :separator '(#\Newline))
                  when (plusp (length (string-trim " " line)))
                    collect (handler-case (json:decode line)
                              (error () line)))
            (get-output-stream-string err))))

(defun show-argv (argv)
  (note "$ cl-stack-code --model openai:demo-model ~{~a~^ ~}" argv))

(defun events-of (events type &optional subtype)
  (remove-if-not (lambda (e) (and (hash-table-p e) (equal type (jget e "type"))
                                  (or (null subtype) (equal subtype (jget e "subtype")))))
                 events))

(defun init-of (events) (first (events-of events "system" "init")))

(defun result-of (events) (first (events-of events "result")))

(defun server-entry (events name)
  (find name (jlist (jget (init-of events) "mcp_servers"))
        :key (lambda (s) (jget s "name")) :test #'equal))

(defun %bool (value) (if (and value (not (eq value :false))) "true" "false"))

(defun show-server-entry (events name)
  (let ((s (server-entry events name)))
    (note "init mcp_servers[~a]: status ~a, tools ~d, sampling ~a, elicitation ~a, deferred ~a, loaded ~a"
          name (jget s "status") (length (jlist (jget s "tools")))
          (%bool (jget s "sampling")) (%bool (jget s "elicitation")) (%bool (jget s "deferred"))
          (json:encode (or (jget s "loaded") (vector))))))

(defun show-tool-results (events)
  (dolist (e (events-of events "tool_result"))
    (note "tool_result ~a (~a): ~a" (jget e "name") (jget e "status") (jget e "content"))))

(defun show-result (events exit)
  (let ((r (result-of events)))
    (note "result: subtype ~a; permission_denials ~a" (jget r "subtype")
          (json:encode (or (jget r "permission_denials") (vector))))
    (note "exit code: ~a" exit)))

(defun show-stderr (stderr)
  (dolist (line (uiop:split-string stderr :separator '(#\Newline)))
    (when (plusp (length line)) (note "stderr: ~a" line))))

(defparameter +stream+ '("-p" "--output-format" "stream-json"))

(defun run-p (base argv project config data)
  "Run -p ARGV and print what it showed. → the events."
  (show-argv argv)
  (multiple-value-bind (exit events stderr)
      (run-code base argv :project project :config config :data data)
    (values events exit stderr)))

;;; Scene 1: sampling.

(defun scene-1-sampling ()
  (banner "1. MCP sampling: denied headless, then allowed by mcp_sampling(docs)")
  (note "user settings: mcpServers.docs = the stub (summarize, deploy) with \"sampling\": {\"maxTokens\": 256}")
  (note "the stub's summarize sends sampling/createMessage: maxTokens 10000, hint gpt-x, includeContext allServers")
  (multiple-value-bind (project config data)
      (scene-dirs "s1" (obj "mcpServers" (obj "docs" (stub-server "requests" "sampling" (obj "maxTokens" 256)))))
    (let ((script (list (list :tools (list "mcp__docs__summarize" (args "topic" "MCP sampling")))
                        (list :text "Done."))))
      (note "")
      (note "-- 1a. -p, no rule for sampling")
      (with-scene (base script)
        (multiple-value-bind (events exit stderr)
            (run-p base (append +stream+ '("--allowed-tools" "mcp__docs__summarize" "summarize MCP sampling"))
                   project config data)
          (show-server-entry events "docs")
          (dolist (e (events-of events "system" "mcp_sampling")) (show-json "event: " e))
          (show-tool-results events)
          (show-requests)
          (note "a sample reached the model: ~:[no~;YES~]" (some #'sample-request-p (requests-seen)))
          (show-result events exit)
          (show-stderr stderr)))
      (note "")
      (note "-- 1b. -p with --allowed-tools 'mcp_sampling(docs)'")
      (with-scene (base script)
        (multiple-value-bind (events exit stderr)
            (run-p base (append +stream+ '("--allowed-tools" "mcp__docs__summarize"
                                           "--allowed-tools" "mcp_sampling(docs)" "summarize MCP sampling"))
                   project config data)
          (dolist (e (events-of events "system" "mcp_sampling")) (show-json "event: " e))
          (show-tool-results events)
          (show-requests)
          (show-result events exit)
          (show-stderr stderr))))))

;;; Scene 2: elicitation.

(defun wait-for (fn &key (timeout 60))
  (let ((end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop for v = (funcall fn)
          when v return v
          when (> (get-internal-real-time) end) return nil
          do (sleep 0.05))))

(defun scene-2-elicitation ()
  (banner "2. MCP elicitation: declined headless, then accepted with content through the SDK")
  (note "user settings: mcpServers.docs = the stub; \"elicitation\" left at its default (true)")
  (note "the stub's deploy sends elicitation/create: env (enum staging|production, required), replicas (integer 1..5)")
  (multiple-value-bind (project config data)
      (scene-dirs "s2" (obj "mcpServers" (obj "docs" (stub-server "requests"))))
    (let ((script (list (list :tools (list "mcp__docs__deploy" (args "service" "web")))
                        (list :text "Deployed."))))
      (note "")
      (note "-- 2a. -p")
      (with-scene (base script)
        (multiple-value-bind (events exit stderr)
            (run-p base (append +stream+ '("--allowed-tools" "mcp__docs__deploy" "deploy web"))
                   project config data)
          (dolist (e (events-of events "system" "mcp_elicitation")) (show-json "event: " e))
          (show-tool-results events)
          (show-result events exit)
          (show-stderr stderr)))
      (note "")
      (note "-- 2b. the SDK: make-client, client-turn, client-pending-approvals, client-respond-approval")
      (with-scene (base script)
        (let ((seen '())
              (lock (bt:make-lock "events")))
          (code:with-client (client :cwd project
                                    :settings (code:make-settings
                                               :model "openai:demo-model"
                                               :allow (list (code:parse-permission-rule "mcp__docs__deploy" :allow)))
                                    :user-config-dir config
                                    :data-dir data
                                    :getenv (model-getenv base)
                                    :on-event (lambda (session event)
                                                (declare (ignore session))
                                                (bt:with-lock-held (lock) (push event seen))))
            (let* ((s (code:client-new-session client))
                   (turn (code:client-turn s "deploy web"))
                   (request (wait-for (lambda ()
                                        (find "mcp_elicitation" (code:client-pending-approvals s)
                                              :key (lambda (r) (code:event-value r "kind")) :test #'equal)))))
              (if (null request)
                  (note "!! no mcp_elicitation approval request within 60 s")
                  (let ((id (code:event-value request "id")))
                    (show-json "pending approval_request: " request)
                    (note "respond ~a :accept {\"env\": \"qa\"} (not in the enum)" id)
                    (handler-case (progn (code:client-respond-approval s id :accept :content (obj "env" "qa"))
                                         (note "  accepted?!"))
                      (error (c) (note "  ~(~a~): ~a" (type-of c) c)))
                    (note "still pending: ~:[no~;yes~]"
                          (find id (code:client-pending-approvals s)
                                :key (lambda (r) (code:event-value r "id")) :test #'equal))
                    (note "respond ~a :accept {\"env\": \"staging\", \"replicas\": 2}" id)
                    (note "  → ~s" (code:client-respond-approval s id :accept
                                                                 :content (obj "env" "staging" "replicas" 2)))))
              (let ((result (code:client-wait turn :timeout 60))
                    (events (bt:with-lock-held (lock) (reverse seen))))
                (dolist (e (events-of events "approval_resolved")) (show-json "event: " e))
                (dolist (e (events-of events "system" "mcp_elicitation")) (show-json "event: " e))
                (show-tool-results events)
                (show-requests)
                (note "result: subtype ~a, text ~s" (and result (code:event-value result "subtype"))
                      (and result (code:event-value result "result")))))))))))

;;; Scene 3: deferred tool loading.

(defun scene-3-tool-search ()
  (banner "3. Deferred tool loading: mcpToolSearch \"always\", tool_search, a deferred tool called by name")
  (note "user settings: mcpServers.catalog = the stub's 8 plain tools; \"mcpToolSearch\": \"always\"")
  (multiple-value-bind (project config data)
      (scene-dirs "s3" (obj "mcpServers" (obj "catalog" (stub-server "catalog"))
                            "mcpToolSearch" "always"))
    (with-scene (base (list (list :tools (list "tool_search" (args "query" "invoice")))
                            (list :tools (list "mcp__catalog__invoice_send" (args "invoice" "INV-7")))
                            (list :tools (list "mcp__catalog__weather_today" (args "city" "Oslo")))
                            (list :text "Sent INV-7 and looked up Oslo.")))
      (multiple-value-bind (events exit stderr)
          (run-p base (append +stream+ '("--allowed-tools" "mcp__catalog__invoice_send"
                                         "--allowed-tools" "mcp__catalog__weather_today"
                                         "send invoice INV-7, then the weather in Oslo"))
                 project config data)
        (let ((init (init-of events)))
          (show-json "init mcp_tool_search: " (jget init "mcp_tool_search"))
          (note "init tools (non-built-in): ~{~a~^ ~}" (remove-if #'%builtin-p (jlist (jget init "tools")))))
        (show-server-entry events "catalog")
        (let* ((first-request (first (requests-seen)))
               (search (find "tool_search" (jlist (jget first-request "tools"))
                             :key (lambda (tl) (jget tl "function" "name")) :test #'equal)))
          (note-block "request 1 tool_search description | " (jget search "function" "description"))
          (show-json "request 1 tool_search parameters: " (jget search "function" "parameters")))
        (show-requests)
        (show-tool-results events)
        (show-result events exit)
        (show-stderr stderr)))))

(defparameter *demos* '(scene-1-sampling scene-2-elicitation scene-3-tool-search))

(defun run-demos (&key (demos *demos*))
  (let* ((failed '())
         (t0 (get-internal-real-time))
         (*scratch* (let ((d (merge-pathnames (format nil "code-mcp-requests-demo-~36r/"
                                                      (random (expt 36 6) (make-random-state t)))
                                              (uiop:temporary-directory))))
                      (ensure-directories-exist d)
                      (truename d)))
         (*redactions* (append (list (cons (substitute #\- #\/ (%native *scratch*)) "<tmp-key>-")
                                     (cons (%native *scratch*) "<tmp>/")
                                     (cons +demo-key+ "<demo-key>")
                                     (cons (%native (user-homedir-pathname)) "~/"))
                               (let ((n (%native *scratch*)))
                                 (and (uiop:string-prefix-p "/private/" n)
                                      (list (cons (substitute #\- #\/ (subseq n 8)) "<tmp-key>-")
                                            (cons (subseq n 8) "<tmp>/")))))))
    (unwind-protect
         (progn
           (banner "Loaded systems")
           (dolist (name '("cl-stack-code" "ai-agent-protocol" "mcp-protocol" "mcp-backend-stdio"
                           "rpc-backend-stdio" "llm-protocol-openai" "http-backend-async"))
             (let ((s (asdf:find-system name nil)))
               (note "  ~22a ~a" name (and s (asdf:component-version s)))))
           (note "model: a scripted OpenAI-compatible server on 127.0.0.1 (no real model); key: a dummy")
           (note "MCP server: a stub child SBCL (mcp-protocol server, rpc-backend-stdio, stdio)")
           (dolist (d demos)
             (let ((t1 (get-internal-real-time)))
               (handler-case (funcall d)
                 (error (c) (push d failed)
                   (note "!! ~(~a~) FAILED: ~s~%!! ~a" d (type-of c) c)))
               (note "(~(~a~): ~,1f s)" d (elapsed t1)))))
      (uiop:delete-directory-tree *scratch* :validate t :if-does-not-exist :ignore))
    (banner "Summary")
    (note "scenes that raised an error: ~:[none~;~:*~{~(~a~)~^, ~}~]" (reverse failed))
    (format *error-output* "~&(total wall time ~,1f s)~%" (elapsed t0))
    (when failed (uiop:quit 1))))
