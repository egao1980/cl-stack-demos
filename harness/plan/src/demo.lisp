(in-package #:harness-demos-plan)

;;; cl-stack-code 0.15.0 (H5c-0): plan mode plans, headless. Every scene runs
;;; `cl-stack-code -p --output-format stream-json` in process (CODE:RUN-CLI)
;;; with the built-in openai provider pointed at a scripted OpenAI-compatible
;;; server on 127.0.0.1, so llm-protocol-openai's HTTP path, ai-agent-protocol
;;; 0.16.1's tool loop (a tool output ending the run), the plan-mode-tools
;;; policy, R4d-4b's sandbox and the plan documents all run end to end. No
;;; real model, no real key: the key is the dummy +DEMO-KEY+, never printed.
;;; The server records each request, so the demo shows what the model saw:
;;; the tools offered and the tool results.

(defparameter +demo-key+ "demo-key-5c0a17"
  "The dummy API key the provider sends (OPENAI_API_KEY).")

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
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t '())))

(defun %show (value)
  "A JSON value for the output: null / false / strings as JSON writes them."
  (cond ((eq value :null) "null")
        ((null value) "null")
        ((eq value t) "true")
        ((eq value :false) "false")
        ((stringp value) (format nil "~s" value))
        ((vectorp value) (format nil "[~{~a~^, ~}]" (map 'list #'%show value)))
        ((hash-table-p value) (json:encode value))
        (t (princ-to-string value))))

(defun obj (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun args (&rest kv)
  "Tool-call arguments: a JSON object string."
  (json:encode (apply #'obj kv)))

;;; The scripted server. Each request pops the next step of the script (the
;;; last step repeats). A step is
;;;   (:text "answer")                      an SSE stream with that answer
;;;   (:tools ("name" "{json args}") ...)   an SSE stream of tool calls

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
  "→ the request body (a string), or NIL when the client hung up."
  (let ((len 0))
    (when (%read-line* stream)
      (loop for h = (%read-line* stream)
            while (and h (plusp (length h)))
            do (let ((c (position #\: h)))
                 (when (and c (string-equal "content-length" (subseq h 0 c)))
                   (setf len (or (parse-integer h :start (1+ c) :junk-allowed t) 0)))))
      (let ((body (make-array len :element-type '(unsigned-byte 8))))
        (read-sequence body stream)
        (babel:octets-to-string body :errorp nil)))))

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

(defun %delta-event (delta &optional finish)
  (json:encode
   (obj "model" "demo-model"
        "choices" (vector (if finish
                              (obj "index" 0 "delta" delta "finish_reason" finish)
                              (obj "index" 0 "delta" delta))))))

(defun %sse (stream events)
  (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                      "Transfer-Encoding: chunked" "Connection: close" ""))
  (dolist (event (append events
                         (list "{\"model\":\"demo-model\",\"choices\":[],\"usage\":{\"prompt_tokens\":900,\"completion_tokens\":40,\"total_tokens\":940}}"
                               "[DONE]")))
    (%chunk stream (format nil "data: ~a~%~%" event)))
  (%out stream (%crlf "0" "")))

(defvar *call-counter* 0)

(defun %answer (stream step)
  (ecase (first step)
    (:text (%sse stream (list (%delta-event (obj "role" "assistant" "content" (second step)))
                              (%delta-event (obj) "stop"))))
    (:tools
     (%sse stream
           (append
            (loop for (name arguments) in (rest step)
                  for i from 0
                  collect (%delta-event
                           (obj "role" "assistant"
                                "tool_calls"
                                (vector (obj "index" i
                                             "id" (format nil "call_~d" (incf *call-counter*))
                                             "type" "function"
                                             "function" (obj "name" name "arguments" arguments))))))
            (list (%delta-event (obj) "tool_calls")))))))

(defun call-with-server (script fn)
  "Serve SCRIPT on 127.0.0.1 (a free port) while FN runs with the base URL."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t
                                                      :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (steps (copy-list script))
         (lock (bt:make-lock "script"))
         (thread
           (bt:make-thread
            (lambda ()
              (loop (handler-case
                        (let ((c (usocket:socket-accept server :element-type '(unsigned-byte 8))))
                          (bt:make-thread
                           (lambda ()
                             (unwind-protect
                                  (ignore-errors
                                   (let* ((s (usocket:socket-stream c))
                                          (body (%read-request s)))
                                     (when body
                                       (let ((step (bt:with-lock-held (lock)
                                                     (if (rest steps) (pop steps) (first steps)))))
                                         (bt:with-lock-held (*requests-lock*)
                                           (push (or (ignore-errors (json:decode body)) body)
                                                 *requests*))
                                         (%answer s step)))))
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
     (bt:with-lock-held (*requests-lock*) (setf *requests* '()))
     (call-with-server ,script (lambda (,base) ,@body))))

(defun requests-seen () (bt:with-lock-held (*requests-lock*) (reverse *requests*)))

(defun request-tools (request)
  (mapcar (lambda (tool) (jget tool "function" "name")) (jlist (jget request "tools"))))

(defun request-messages (request) (jlist (jget request "messages")))

(defun last-tool-results (request)
  "The tool results at the end of REQUEST's messages, oldest first."
  (reverse (loop for m in (reverse (request-messages request))
                 while (equal "tool" (jget m "role"))
                 collect (let ((c (jget m "content")))
                           (if (stringp c) c (%show c))))))

(defun last-user-text (request)
  (let ((user (find "user" (request-messages request)
                    :key (lambda (m) (jget m "role")) :test #'equal :from-end t)))
    (and user (let ((c (jget user "content"))) (if (stringp c) c (%show c))))))

(defun system-text (request)
  (let ((m (find "system" (request-messages request)
                 :key (lambda (m) (jget m "role")) :test #'equal)))
    (and m (jget m "content"))))

;;; Projects and running cl-stack-code.

(defvar *scratch* nil)

(defun %native (p) (uiop:native-namestring p))

(defun %write-file (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text s)))

(defun %read-file (path)
  (and (probe-file path) (uiop:read-file-string path)))

(defun %git (dir &rest args)
  (multiple-value-bind (out err code)
      (uiop:run-program (list* "git" "-C" (%native dir) args)
                        :output :string :error-output :string :ignore-error-status t)
    (unless (zerop code) (error "git ~{~a~^ ~} failed (~d): ~a" args code err))
    (string-trim '(#\Newline #\Space) out)))

(defun %porcelain (project)
  (let ((s (%git project "status" "--porcelain" "--untracked-files=all")))
    (if (string= s "") "(clean)" s)))

(defparameter *greet-sh*
  (format nil "#!/bin/sh~%# greet.sh: prints a greeting~%echo \"Hello\"~%"))

(defun scene-dirs (name &optional (settings "{}"))
  "→ (values project config data): a git project with greet.sh and README.md
committed, a user config directory whose settings.json is SETTINGS, and a
data directory."
  (let* ((root (merge-pathnames (format nil "~a/" name) *scratch*))
         (project (merge-pathnames "project/" root))
         (config (merge-pathnames "config/" root))
         (data (merge-pathnames "data/" root)))
    (ensure-directories-exist project)
    (%git project "init" "-q" "-b" "main")
    (%git project "config" "user.name" "Plan Demo")
    (%git project "config" "user.email" "plan-demo@invalid")
    (%git project "config" "commit.gpgsign" "false")
    (%write-file (merge-pathnames "greet.sh" project) *greet-sh*)
    (%write-file (merge-pathnames "README.md" project)
                 (format nil "# greet~%~%`sh greet.sh` prints a greeting.~%"))
    (%git project "add" "-A")
    (%git project "commit" "-q" "-m" "init")
    (%write-file (merge-pathnames "settings.json" config) settings)
    (ensure-directories-exist data)
    (values project config data)))

(defun run-code (base argv &key project config data (prompt ""))
  "CODE:RUN-CLI on ARGV with the openai provider at BASE and the dummy key.
→ (values exit events stderr)."
  (let* ((argv (append (list "--cwd" (%native project) "--model" "openai:demo-model") argv))
         (err (make-string-output-stream))
         (exit nil)
         (out (with-output-to-string (o)
                (setf exit (code:run-cli argv :output o :error-output err
                                              :input (make-string-input-stream prompt)
                                              :user-config-dir config
                                              :data-dir data
                                              :getenv (lambda (name)
                                                        (cond ((string= name "OPENAI_BASE_URL") base)
                                                              ((string= name "OPENAI_API_KEY") +demo-key+)
                                                              (t nil))))))))
    (values exit
            (loop for line in (uiop:split-string out :separator '(#\Newline))
                  when (plusp (length (string-trim " " line)))
                    collect (handler-case (json:decode line)
                              (error () line)))
            (get-output-stream-string err))))

(defun show-argv (argv &optional prompt)
  (note "$ ~@[echo ~s | ~]cl-stack-code --model openai:demo-model ~{~a~^ ~}" prompt argv))

(defun events-of (events type &optional subtype)
  (remove-if-not (lambda (e) (and (equal type (jget e "type"))
                                  (or (null subtype) (equal subtype (jget e "subtype")))))
                 events))

(defun result-of (events) (first (events-of events "result")))

(defun session-of (events) (jget (result-of events) "session_id"))

(defun show-stderr (stderr)
  (dolist (line (uiop:split-string stderr :separator '(#\Newline)))
    (when (plusp (length line)) (note "stderr: ~a" line))))

(defun show-result (events exit &key (plan t))
  (let ((r (result-of events)))
    (if (null r)
        (note "!! no result event")
        (progn
          (note "result: subtype ~a, is_error ~:[false~;true~]~@[, ~a~]"
                (%show (jget r "subtype")) (let ((e (jget r "is_error"))) (and e (not (eq e :false))))
                (and plan (multiple-value-bind (p present) (gethash "plan" r)
                            (cond ((not present) "no \"plan\" key")
                                  ((or (null p) (eq p :null)) "plan null")
                                  (t (format nil "plan {status ~a, revision ~a}"
                                             (%show (jget p "status"))
                                             (%show (jget p "revision"))))))))
          (when (stringp (jget r "result"))
            (note-block "        result | " (jget r "result")))))
    (note "exit code: ~a" exit)))

(defun show-plan-event (e)
  (let ((sub (jget e "subtype")))
    (cond
      ((member sub '("opened" "reopened") :test #'equal)
       (note "plan ~a: plan_id ~a…, revision ~a, path ~a" sub
             (subseq (jget e "plan_id") 0 8) (%show (jget e "revision")) (%show (jget e "path")))
       (let ((s (jget e "sandbox")))
         (note "        area ~a, sandbox {status ~a, backend ~a, network ~a}"
               (%show (jget e "area")) (%show (jget s "status")) (%show (jget s "backend"))
               (%show (jget s "network")))))
      ((equal sub "proposed")
       (note "plan proposed: revision ~a, title ~a" (%show (jget e "revision")) (%show (jget e "title")))
       (dolist (s (jlist (jget e "steps")))
         (note "        step ~a [~a] ~a" (jget s "index") (jget s "status") (jget s "text")))
       (note "        files ~a" (%show (jget e "files"))))
      ((equal sub "approved")
       (note "plan approved: revision ~a, mode ~a" (%show (jget e "revision")) (%show (jget e "mode"))))
      ((equal sub "step")
       (note "plan step: step ~a -> ~a; checklist ~{~a~^ ~}" (jget e "step") (%show (jget e "status"))
             (mapcar (lambda (s) (format nil "~a:~a" (jget s "index") (jget s "status")))
                     (jlist (jget e "steps")))))
      ((equal sub "executed")
       (note "plan executed: status ~a" (%show (jget e "status")))
       (dolist (row (jlist (jget e "rows")))
         (note "        row: ~a ~a, planned ~a" (%show (jget row "path")) (%show (jget row "change"))
               (%show (jget row "planned"))))
       (when (stringp (jget e "diff")) (note-block "        diff | " (jget e "diff"))))
      ((equal sub "rejected")
       (note "plan rejected: plan_id ~a…" (subseq (jget e "plan_id") 0 8)))
      (t (note "plan ~a: ~a" sub (json:encode e))))))

(defun show-plan-events (events)
  (mapc #'show-plan-event (events-of events "plan")))

(defun show-requests (&key (from 0))
  "Each model request from index FROM: the tools offered, then the tool
results it carries back."
  (loop for r in (nthcdr from (requests-seen))
        for i from (1+ from)
        do (note "request ~d: tools offered ~{~a~^ ~}" i (request-tools r))
           (dolist (text (last-tool-results r))
             (note-block "           tool result | " text))))

(defparameter +stream+ '("-p" "--output-format" "stream-json"))

(defun directory-files (dir)
  "Relative names of the files under DIR, sorted."
  (let ((dir (truename dir)))
    (sort (mapcar (lambda (p) (enough-namestring p dir))
                  (remove-if #'uiop:directory-pathname-p
                             (directory (merge-pathnames "**/*.*" dir))))
          #'string<)))

;;; The plan the mock proposes.

(defparameter *plan-r1*
  "# Greet by name

## Goal
greet.sh takes an optional name and prints \"Hello, NAME!\"; without one it keeps printing \"Hello\".

## Findings
- greet.sh is three lines and prints a fixed \"Hello\".
- A scratch version in .plan-scratch/greet-try.sh printed \"Hello, Ada!\" for `sh .plan-scratch/greet-try.sh Ada`.

## Steps
1. Read the name from the first argument in greet.sh.
2. Document the argument in README.md.

## Files to change
- `greet.sh`: the argument and the new greeting
- `README.md`: usage

## Risks
Callers that pass extra arguments now get them in the greeting.

## Test plan
`sh greet.sh` prints Hello; `sh greet.sh Ada` prints Hello, Ada!
")

(defparameter *greet-new*
  (format nil "#!/bin/sh~%# greet.sh: prints a greeting, by name when one is given~%if [ -n \"$1\" ]; then echo \"Hello, $1!\"; else echo \"Hello\"; fi~%"))

;;; Scenes.

(defvar *s1* nil "(project config data session plan-path) of scene 1, for scene 2.")

(defun scene-1-propose ()
  (banner "1. Plan mode: explore in a confined copy, propose_plan writes plan.md")
  (multiple-value-bind (project config data) (scene-dirs "s1")
    (note "project: a git repository with greet.sh and README.md committed")
    (with-scene (base (list (list :tools (list "bash" (args "command" "cat greet.sh; sh greet.sh; echo probe > probe.out; ls")))
                            (list :tools (list "write" (args "file_path" "greet.sh" "content" *greet-new*)))
                            (list :tools (list "write" (args "file_path" ".plan-scratch/greet-try.sh"
                                                             "content" *greet-new*))
                                  (list "bash" (args "command" "sh .plan-scratch/greet-try.sh Ada")))
                            (list :tools (list "propose_plan" (args "markdown" *plan-r1*)))
                            (list :text "UNREACHED: no model step follows propose_plan")))
      (let ((argv (append +stream+ '("--permission-mode" "plan"))))
        (show-argv argv "let greet.sh take a name")
        (multiple-value-bind (exit events stderr)
            (run-code base argv :project project :config config :data data
                                :prompt "let greet.sh take a name")
          (let ((init (first (events-of events "system" "init"))))
            (note "init: permission_mode ~a" (%show (jget init "permission_mode"))))
          (show-plan-events events)
          (show-requests)
          (let ((sys (system-text (first (requests-seen)))))
            (note "system prompt has a plan mode section: ~:[NO~;yes~]"
                  (and (stringp sys) (search "propose_plan" sys))))
          (note "requests the server saw: ~d (propose_plan ended the run: ~:[NO~;yes~])"
                (length (requests-seen)) (= 4 (length (requests-seen))))
          (show-result events exit)
          (show-stderr stderr)
          (let* ((proposed (first (events-of events "plan" "proposed")))
                 (path (and proposed (jget proposed "path")))
                 (dir (and path (uiop:pathname-directory-pathname path))))
            (when dir
              (note "plan directory files: ~{~a~^ ~}" (directory-files dir))
              (note "plan.md is outside the project: ~:[NO~;yes~]"
                    (not (uiop:subpathp (pathname path) project)))
              (note "plan.md = revisions/1.md: ~:[NO~;yes~]"
                    (equal (%read-file path) (%read-file (merge-pathnames "revisions/1.md" dir)))))
            (note "project after the plan run: git status ~a; greet.sh unchanged: ~:[NO~;yes~]"
                  (%porcelain project)
                  (equal *greet-sh* (%read-file (merge-pathnames "greet.sh" project))))
            (setf *s1* (list project config data (session-of events) path))))))
    (banner "1b. A second plan run on the same data directory: the migration notice is not repeated")
    (with-scene (base (list (list :text "greet.sh prints Hello; it takes no arguments.")))
      (let ((argv (append +stream+ '("--permission-mode" "plan"))))
        (show-argv argv "what does greet.sh print?")
        (multiple-value-bind (exit events stderr)
            (run-code base argv :project project :config config :data data
                                :prompt "what does greet.sh print?")
          (show-plan-events events)
          (show-result events exit)
          (show-stderr stderr)
          (note "migration notice on stderr: ~:[no~;YES~]" (search "now plans first" stderr)))))))

(defun scene-2-approve ()
  (banner "2. The user edits plan.md, then --resume ID --approve-plan runs it in the project")
  (destructuring-bind (project config data session path) *s1*
    (let ((edited (concatenate 'string *plan-r1*
                               (format nil "~%## Notes~%Keep the plain Hello for no argument (added by the user).~%"))))
      (%write-file path edited)
      (note "the user appends a \"Notes\" section to plan.md")
      (with-scene (base (list (list :tools (list "update_plan" (args "step" 1 "status" "in_progress")))
                              (list :tools (list "write" (args "file_path" "greet.sh" "content" *greet-new*)))
                              (list :tools (list "bash" (args "command" "sh greet.sh; sh greet.sh Ada"))
                                    (list "update_plan" (args "step" 1 "status" "done")))
                              (list :tools (list "update_plan" (args "step" 2 "status" "in_progress")))
                              (list :tools (list "edit" (args "file_path" "README.md"
                                                              "old_string" "prints a greeting."
                                                              "new_string" "prints a greeting; `sh greet.sh NAME` greets NAME.")))
                              (list :tools (list "update_plan" (args "step" 2 "status" "done")))
                              (list :text "Done: greet.sh takes a name and README.md says so.")))
        (let ((argv (append +stream+ (list "--resume" session "--approve-plan"))))
          (show-argv argv)
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data)
            (show-plan-events events)
            (let ((first-request (first (requests-seen))))
              (note "execution turn's user text (first 4 lines):")
              (note-block "        | " (format nil "~{~a~^~%~}"
                                               (subseq (uiop:split-string (or (last-user-text first-request) "")
                                                                          :separator '(#\Newline))
                                                       0 4)))
              (note "the user's Notes section reached the model: ~:[NO~;yes~]"
                    (search "added by the user" (or (last-user-text first-request) ""))))
            (show-requests)
            (show-result events exit)
            (show-stderr stderr)
            (let ((dir (uiop:pathname-directory-pathname path)))
              (note "revisions: ~{~a~^ ~}" (remove-if-not (lambda (f) (search "revisions/" f))
                                                          (directory-files dir))))
            (note "project after approval: git status ~a" (%porcelain project))
            (note-block "        greet.sh | " (%read-file (merge-pathnames "greet.sh" project)))))))
    (banner "2b. The executed plan cannot be approved again")
    (with-scene (base (list (list :text "unused")))
      (let ((argv (append +stream+ (list "--resume" (fourth *s1*) "--approve-plan"))))
        (show-argv argv)
        (multiple-value-bind (exit events stderr)
            (run-code base argv :project (first *s1*) :config (second *s1*) :data (third *s1*))
          (declare (ignore events))
          (show-stderr stderr)
          (note "exit code: ~a; requests the server saw: ~d" exit (length (requests-seen))))))))

(defun %replace-once (text from to)
  (let ((p (search from text)))
    (if p (concatenate 'string (subseq text 0 p) to (subseq text (+ p (length from)))) text)))

(defparameter *plan-fr*
  (%replace-once *plan-r1* "\"Hello, NAME!\"" "\"Bonjour, NAME !\""))

(defun scene-3-revise-and-reject ()
  (banner "3. Revise (revision 2, the prompt as feedback), then reject")
  (multiple-value-bind (project config data) (scene-dirs "s3")
    (let (session path)
      (with-scene (base (list (list :tools (list "propose_plan" (args "markdown" *plan-r1*)))))
        (let ((argv (append +stream+ '("--permission-mode" "plan"))))
          (show-argv argv "let greet.sh take a name")
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data
                                  :prompt "let greet.sh take a name")
            (declare (ignore stderr))
            (show-plan-events events)
            (show-result events exit)
            (setf session (session-of events)
                  path (jget (first (events-of events "plan" "proposed")) "path")))))
      (with-scene (base (list (list :tools (list "propose_plan" (args "markdown" *plan-fr*)))))
        (let ((argv (append +stream+ (list "--resume" session "--permission-mode" "plan"))))
          (show-argv argv "greet in French")
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data
                                  :prompt "greet in French")
            (show-plan-events events)
            (note "the revision turn's user text: ~a" (%show (last-user-text (first (requests-seen)))))
            (show-result events exit)
            (show-stderr stderr)
            (let ((dir (uiop:pathname-directory-pathname path)))
              (note "revisions: ~{~a~^ ~}" (remove-if-not (lambda (f) (search "revisions/" f))
                                                          (directory-files dir)))
              (note "plan.md is revision 2 (Bonjour): ~:[NO~;yes~]"
                    (search "Bonjour" (or (%read-file path) "")))))))
      (with-scene (base (list (list :text "unused")))
        (let ((argv (append +stream+ (list "--resume" session "--reject-plan"))))
          (show-argv argv)
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data)
            (show-plan-events events)
            (show-stderr stderr)
            (note "exit code: ~a; requests the server saw: ~d (reject is action only)"
                  exit (length (requests-seen))))))
      (with-scene (base (list (list :text "unused")))
        (let ((argv (append +stream+ (list "--resume" session "--approve-plan"))))
          (show-argv argv)
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data)
            (declare (ignore events))
            (show-stderr stderr)
            (note "exit code: ~a; requests the server saw: ~d" exit (length (requests-seen))))))
      (note "project after revise and reject: git status ~a" (%porcelain project)))))

(defun scene-4-preview ()
  (banner "4. Preview mode, the fifth permission mode (H5a renamed)")
  (multiple-value-bind (project config data) (scene-dirs "s4")
    (with-scene (base (list (list :text "unused")))
      (let ((argv (append +stream+ '("--permission-mode" "sandbox"))))
        (show-argv argv "hi")
        (multiple-value-bind (exit events stderr)
            (run-code base argv :project project :config config :data data :prompt "hi")
          (declare (ignore events))
          (show-stderr stderr)
          (note "exit code: ~a" exit))))
    (let (session)
      (with-scene (base (list (list :tools (list "write" (args "file_path" "greet.sh" "content" *greet-new*)))
                              (list :tools (list "bash" (args "command" "sh greet.sh Ada")))
                              (list :text "greet.sh now greets by name (in the preview).")))
        (let ((argv (append +stream+ '("--permission-mode" "preview"))))
          (show-argv argv "let greet.sh take a name")
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data
                                  :prompt "let greet.sh take a name")
            (dolist (e (events-of events "preview"))
              (note "preview ~a: preview_id ~a…~@[, sandbox ~a~]" (%show (jget e "subtype"))
                    (let ((id (jget e "preview_id"))) (if (stringp id) (subseq id 0 8) (%show id)))
                    (and (jget e "sandbox") (%show (jget e "sandbox" "status")))))
            (note "\"plan\" events: ~d" (length (events-of events "plan")))
            (show-requests)
            (let ((r (result-of events)))
              (note "result: subtype ~a; result.preview {status ~a}; \"plan\" key in the result: ~:[no~;YES~]"
                    (%show (jget r "subtype")) (%show (jget r "preview" "status"))
                    (nth-value 1 (gethash "plan" r)))
              (when (stringp (jget r "preview" "diff"))
                (note-block "        diff | " (jget r "preview" "diff"))))
            (show-stderr stderr)
            (note "exit code: ~a; migration notice: ~:[none~;YES~]" exit (search "now plans first" stderr))
            (note "project before --apply: git status ~a" (%porcelain project))
            (setf session (session-of events)))))
      (with-scene (base (list (list :text "unused")))
        (let ((argv (append +stream+ (list "--resume" session "--apply"))))
          (show-argv argv)
          (multiple-value-bind (exit events stderr)
              (run-code base argv :project project :config config :data data)
            (dolist (e (events-of events "preview"))
              (note "preview ~a" (%show (jget e "subtype"))))
            (show-stderr stderr)
            (note "exit code: ~a; requests the server saw: ~d" exit (length (requests-seen)))
            (note "project after --apply: git status ~a" (%porcelain project))))))))

(defparameter *demos* '(scene-1-propose scene-2-approve scene-3-revise-and-reject scene-4-preview))

(defun run-demos (&key (demos *demos*))
  (let* ((failed '())
         (t0 (get-internal-real-time))
         (*scratch* (let ((d (merge-pathnames (format nil "plan-demo-~36r/"
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
           (dolist (name '("cl-stack-code" "ai-agent-protocol" "policy-protocol" "llm-protocol"
                           "llm-protocol-openai" "http-backend-async"))
             (let ((s (asdf:find-system name nil)))
               (note "  ~22a ~a" name (and s (asdf:component-version s)))))
           (note "model: a scripted OpenAI-compatible server on 127.0.0.1 (no real model); key: a dummy")
           (dolist (d demos)
             (handler-case (funcall d)
               (error (c) (push d failed)
                 (note "!! ~(~a~) FAILED: ~s~%!! ~a" d (type-of c) c)))))
      (uiop:delete-directory-tree *scratch* :validate t :if-does-not-exist :ignore))
    (banner "Summary")
    (note "scenes that raised an error: ~:[none~;~:*~{~(~a~)~^, ~}~]" (reverse failed))
    (format *error-output* "~&(total wall time ~,1f s)~%" (elapsed t0))
    (when failed (uiop:quit 1))))
