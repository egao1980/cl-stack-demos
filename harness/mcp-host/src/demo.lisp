(in-package #:harness-demos-mcp-host)

;;; mcp-protocol 0.5.0, "host answers" (rules: the header of its src/host.lisp),
;;; against a stub MCP server. Client and server talk over rpc-backend-stdio's
;;; in-memory pipe pair; the server's RPC-SERVE loop runs in a thread of its own
;;; and records every request it gets and what it answered. The sampling "model"
;;; and the elicitation "user" are Lisp functions: no model, no network. Every
;;; tools/call round and tools/list has a timeout, and the server thread is
;;; joined with a bound.

(defparameter *timeout* 10
  "Seconds for each tools/call round (CALL-TOOL gives every round the full
timeout) and for tools/list.")

(defvar *checks* '()
  "(claim . ok) of every CHECK in this run, newest first.")

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun obj (&rest kvs) (apply #'mcp:json-object kvs))

(defun js (value)
  "VALUE as compact JSON, through json-protocol (jzon)."
  (json:encode value))

(defun keys (object)
  "The keys of the JSON object OBJECT in their order, NIL for anything else."
  (and (hash-table-p object)
       (loop for k being the hash-keys of object collect k)))

(defun same-keys-p (object expected)
  "True when the JSON object OBJECT has exactly the keys EXPECTED, in any order.
Key order does not survive the wire: json-backend-jzon writes the members of a
hash table sorted by key."
  (equal (sort (copy-list (keys object)) #'string<)
         (sort (copy-list expected) #'string<)))

(defun check (claim ok)
  "Print CLAIM as ok or FAIL and record it."
  (push (cons claim (and ok t)) *checks*)
  (format t "  ~:[FAIL~;ok~]  ~a~%" ok claim)
  ok)

(defun tool-text (result)
  "The first text item of a tools/call RESULT."
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (and (plusp (length content)) (gethash "text" (elt content 0)))))

;;; --- the host's handlers (client side, run on the thread that calls) ---------

(defparameter *summary* "call-tool now answers input requests until the tool is done")

(defun stub-model (params)
  "The sampling handler: a stub model. Prints the params it got and answers a
canned assistant message."
  (let* ((messages (gethash "messages" params))
         (last (and (plusp (length messages)) (elt messages (1- (length messages))))))
    (format t "  [model] sampling/createMessage: maxTokens ~a, systemPrompt ~s, ~d message(s), last ~s~%"
            (gethash "maxTokens" params) (gethash "systemPrompt" params) (length messages)
            (and last (gethash "text" (gethash "content" last))))
    (obj "role" "assistant"
         "content" (obj "type" "text" "text" *summary*)
         "model" "stub-model-1"
         "stopReason" "endTurn")))

(defun stub-user (params)
  "The elicitation handler: a stub user. Prints the form it got and accepts it
with content."
  (let ((schema (gethash "requestedSchema" params)))
    (format t "  [user]  elicitation/create (~a mode): ~s, fields ~{~a~^, ~}, required ~{~a~^, ~}~%"
            (gethash "mode" params) (gethash "message" params)
            (keys (gethash "properties" schema))
            (coerce (gethash "required" schema #()) 'list))
    (obj "action" "accept" "content" (obj "branch" "main" "force" nil))))

;;; --- the stub server (its tools run on the server thread) --------------------

(defun sampling-params (text)
  (obj "messages" (vector (obj "role" "user" "content" (obj "type" "text" "text" text)))
       "systemPrompt" "Summarize a diff in one line."
       "maxTokens" 64))

(defun push-form ()
  (obj "mode" "form"
       "message" "Push the summary commit?"
       "requestedSchema" (obj "type" "object"
                              "properties" (obj "branch" (obj "type" "string")
                                                "force" (obj "type" "boolean"))
                              "required" (vector "branch"))))

(defun push-text (answer)
  (if (equal (gethash "action" answer) "accept")
      (let ((content (gethash "content" answer)))
        (format nil "pushing ~a (force: ~:[no~;yes~])"
                (gethash "branch" content) (eq t (gethash "force" content))))
      (format nil "not pushed (~a)" (gethash "action" answer))))

(defun stub-server (note)
  "The stub MCP server. NOTE gets one line per tool handler run: what that run
found in *MCP-INPUT-RESPONSES* and *MCP-REQUEST-STATE* (a handler re-runs from
the top on every round)."
  (let ((server (make-instance 'mcp:mcp-server :name "stub" :version "0.1.0")))
    (flet ((tool (name handler)
             (mcp:register-tool
              server
              (mcp:make-mcp-tool
               name :input-schema (obj "type" "object")
                    :handler (lambda (args)
                               (declare (ignore args))
                               (funcall note (format nil "~a run: inputResponses ~:[none~;~:*~{~a~^ ~}~], requestState ~:[none~;~:*~s~]"
                                                     name (keys mcp:*mcp-input-responses*)
                                                     mcp:*mcp-request-state*))
                               (funcall handler))))))
      (tool "summarize_diff"
            (lambda ()
              (let ((r (mcp:request-sampling (sampling-params "Summarize: +12 -3 in src/host.lisp")
                                             :id "s1")))
                (format nil "summary: ~a" (gethash "text" (gethash "content" r))))))
      (tool "confirm_push"
            (lambda () (push-text (mcp:request-elicitation (push-form) :id "e1"))))
      (tool "summarize_and_push"
            (lambda ()
              (let* ((s (mcp:request-sampling (sampling-params "Summarize: +12 -3 in src/host.lisp")
                                              :id "s1" :request-state "step-1"))
                     (e (mcp:request-elicitation (push-form) :id "e1" :request-state "step-2")))
                (format nil "~a; ~a" (gethash "text" (gethash "content" s)) (push-text e)))))
      (tool "ask_forever"
            (lambda ()
              ;; A new question on every round: e0, e1, ...
              (let ((n (length (keys mcp:*mcp-input-responses*))))
                (mcp:request-elicitation
                 (obj "mode" "form" "message" (format nil "Again? (~d)" n)
                      "requestedSchema" (obj "type" "object" "properties" (obj)))
                 :id (format nil "e~d" n))
                "never reached"))))
    server))

;;; --- the pair ----------------------------------------------------------------

(defun serve-one (server method params)
  "DISPATCH-MCP-METHOD for the server loop; an MCP-ERROR is answered as an RPC
error with its code, message and data (SERVE-MCP's mapping)."
  (handler-case (mcp:dispatch-mcp-method server method params)
    (mcp:mcp-error (c)
      (error 'rpc:rpc-error :message (or (mcp:mcp-error-message c) "mcp error")
                            :code (or (mcp:mcp-error-code c) rpc:+internal-error+)
                            :data (mcp:mcp-error-data c)))))

(defun join-bounded (thread seconds)
  "Join THREAD if it ends within SECONDS; otherwise say so and leave it."
  (loop repeat (* 50 seconds) while (bt2:thread-alive-p thread) do (sleep 0.02))
  (if (bt2:thread-alive-p thread)
      (format t "  (the server thread still runs after ~d s)~%" seconds)
      (ignore-errors (bt2:join-thread thread))))

(defun call-with-pair (fn &key (era :unknown) sampling elicitation roots)
  "Call FN with a client (ERA, these handlers) wired to a fresh stub server over
rpc-backend-stdio's in-memory pair, a function → the requests the server got as
(method params result), oldest first (result NIL when it answered an error), and
a function → the tool runs' notes, oldest first. Then close the pair and join
the server thread (bounded)."
  (let* ((lock (bt2:make-lock :name "stub log"))
         (requests '())
         (notes '())
         (server (stub-server (lambda (line) (bt2:with-lock-held (lock) (push line notes)))))
         (client (make-instance 'mcp:mcp-client :name "demo-host" :version "0.1.0" :era era
                                                :sampling-handler sampling
                                                :elicitation-handler elicitation
                                                :roots roots)))
    (multiple-value-bind (on-notification on-request) (mcp:mcp-client-transport-handlers client)
      (multiple-value-bind (server-end client-end)
          (stdio:make-stdio-rpc-pair :on-notification on-notification :on-request on-request
                                     :dispatch :concurrent)
        (setf (mcp:mcp-client-transport client) client-end)
        (let ((thread (bt2:make-thread
                       (lambda ()
                         ;; bt2 starts a thread with standard IO syntax, where
                         ;; ~S prints a base string as #A((6) BASE-CHAR . "…").
                         (let ((*print-readably* nil))
                           (ignore-errors
                             (rpc:rpc-serve
                              (lambda (method params)
                                (let ((entry (list method params nil)))
                                  (bt2:with-lock-held (lock) (push entry requests))
                                  ;; Set before the answer is written, so the
                                  ;; caller sees it once its call returns.
                                  (setf (third entry) (serve-one server method params))))
                              :transport server-end))))
                       :name "stub MCP server")))
          (unwind-protect
              (funcall fn client
                       (lambda () (bt2:with-lock-held (lock) (reverse requests)))
                       (lambda () (bt2:with-lock-held (lock) (reverse notes))))
            (stdio:close-stdio-rpc-transport client-end :force t)
            (join-bounded thread 5)))))))

(defun tool-calls (requests)
  "The tools/call entries of REQUESTS: one per round."
  (remove-if-not (lambda (entry) (equal (first entry) "tools/call")) requests))

(defun request-params (requests method)
  "The params of the newest METHOD request in REQUESTS."
  (second (find method requests :key #'first :test #'equal :from-end t)))

(defun meta-capabilities (params)
  "The client capabilities in a 2026-07-28 request's _meta."
  (let ((meta (and (hash-table-p params) (gethash "_meta" params))))
    (and (hash-table-p meta) (gethash "io.modelcontextprotocol/clientCapabilities" meta))))

(defun outcome (result)
  "What the server answered a tools/call."
  (cond ((null result) "an error")
        ((equal (gethash "resultType" result) "input_required")
         (let ((asks (gethash "inputRequests" result)))
           (format nil "input_required, asks ~{~a~^, ~}"
                   (loop for id in (keys asks)
                         collect (format nil "~a (~a)" id (gethash "method" (gethash id asks)))))))
        (t (format nil "done: ~s" (tool-text result)))))

(defun print-rounds (requests &key answers)
  "One line per tools/call the server got: its round, the inputResponses ids and
requestState it carried, and what the server answered. With ANSWERS, also the
inputResponses as JSON."
  (loop for (nil params result) in (tool-calls requests)
        for round from 0
        for responses = (gethash "inputResponses" params)
        do (format t "  round ~d: tools/call ~a, inputResponses ~:[none~;~:*~{~a~^ ~}~], requestState ~:[none~;~:*~s~]~%           → ~a~%"
                   round (gethash "name" params) (keys responses) (gethash "requestState" params)
                   (outcome result))
           (when (and answers responses)
             (format t "           inputResponses ~a~%" (js responses)))))

(defun print-notes (notes)
  (dolist (line notes) (format t "  ~a~%" line)))

(defun call-tool-or-error (client name)
  "CALL-TOOL NAME → the result, or the MCP-ERROR it ended with."
  (handler-case (mcp:call-tool client name (obj) :timeout *timeout*)
    (mcp:mcp-error (c) c)))

;;; --- 1. capabilities ---------------------------------------------------------

(defun demo-capabilities ()
  (banner "1. Capabilities from the client's handlers")
  (let ((bare (make-instance 'mcp:mcp-client :name "bare" :version "0.1.0"))
        (full (make-instance 'mcp:mcp-client
                             :name "full" :version "0.1.0"
                             :sampling-handler #'stub-model :elicitation-handler #'stub-user
                             :roots (list (mcp:make-mcp-root "file:///work/demo" :name "demo")))))
    (format t "mcp-client-default-capabilities (as JSON; json-backend-jzon sorts members by key, here and on the wire):~%")
    (format t "  no handlers:                    ~a~%" (js (mcp:mcp-client-default-capabilities bare)))
    (format t "  sampling + elicitation + roots: ~a, keys in order ~{~a~^ ~}~%"
            (js (mcp:mcp-client-default-capabilities full))
            (keys (mcp:mcp-client-default-capabilities full)))
    (check "a bare client advertises {} (an object, never NIL)"
           (let ((caps (mcp:mcp-client-default-capabilities bare)))
             (and (hash-table-p caps) (null (keys caps)))))
    (check "the handlers make sampling, elicitation (form mode only) and roots, in that order"
           (let ((caps (mcp:mcp-client-default-capabilities full)))
             (and (equal (keys caps) '("sampling" "elicitation" "roots"))
                  (equal (keys (gethash "elicitation" caps)) '("form"))))))
  (call-with-pair
   (lambda (client requests notes)
     (declare (ignore notes))
     (mcp:mcp-initialize client)
     (mcp:list-tools client :timeout *timeout*)
     (let ((reqs (funcall requests)))
       (format t "~%A 2026-07-28 exchange (sampling + elicitation handlers, no :capabilities); the client is now ~(~a~) at ~a.~%"
               (mcp:mcp-client-era client) (mcp:mcp-client-protocol-version client))
       (format t "  server/discover _meta clientCapabilities: ~a~%"
               (js (meta-capabilities (request-params reqs "server/discover"))))
       (format t "  tools/list params, as the server got them: ~a~%"
               (js (request-params reqs "tools/list")))
       (check "every 2026-07-28 request's _meta carries the handlers' capabilities (0.4 sent {})"
              (every (lambda (method)
                       (same-keys-p (meta-capabilities (request-params reqs method))
                                    '("sampling" "elicitation")))
                     '("server/discover" "tools/list")))))
   :sampling #'stub-model :elicitation #'stub-user)
  (call-with-pair
   (lambda (client requests notes)
     (declare (ignore notes))
     (mcp:mcp-initialize client)
     (mcp:list-tools client :timeout *timeout*)
     (let ((reqs (funcall requests)))
       (format t "~%A 2025-11-25 exchange (:era :legacy, a sampling handler only):~%")
       (format t "  initialize params, as the server got them: ~a~%" (js (request-params reqs "initialize")))
       (format t "  tools/list params: ~a (no _meta in this era)~%" (js (request-params reqs "tools/list")))
       (check "a 2025-11-25 initialize carries them in \"capabilities\" (0.4 sent {}); no elicitation handler, no elicitation"
              (same-keys-p (gethash "capabilities" (request-params reqs "initialize")) '("sampling")))))
   :era :legacy :sampling #'stub-model)
  (call-with-pair
   (lambda (client requests notes)
     (declare (ignore notes))
     (mcp:mcp-initialize client :capabilities (obj "experimental" (obj "x.demo/trace" (obj))))
     (mcp:list-tools client :timeout *timeout*)
     (let ((caps (meta-capabilities (request-params (funcall requests) "tools/list"))))
       (format t "~%Explicit :capabilities {\"experimental\": …} on a client with a sampling handler:~%")
       (format t "  tools/list _meta clientCapabilities: ~a~%" (js caps))
       (check "explicit :capabilities replace the defaults whole (no merge)"
              (same-keys-p caps '("experimental")))))
   :sampling #'stub-model)
  (call-with-pair
   (lambda (client requests notes)
     (declare (ignore notes))
     (let ((caps (mcp:mcp-client-default-capabilities client)))
       (setf (gethash "experimental" caps) (obj "x.demo/trace" (obj)))
       (mcp:mcp-initialize client :capabilities caps))
     (mcp:list-tools client :timeout *timeout*)
     (let ((caps (meta-capabilities (request-params (funcall requests) "tools/list"))))
       (format t "Extended instead, starting from (mcp-client-default-capabilities client):~%")
       (format t "  tools/list _meta clientCapabilities: ~a~%" (js caps))
       (check "a host extends the defaults by starting from them"
              (same-keys-p caps '("sampling" "experimental")))))
   :sampling #'stub-model))

;;; --- 2. sampling ---------------------------------------------------------------

(defun demo-sampling ()
  (banner "2. Sampling over MRTR: the tool asks, the host's model answers, the retry has it")
  (let ((calls 0) (got nil))
    (call-with-pair
     (lambda (client requests notes)
       (mcp:mcp-initialize client)
       (format t "(call-tool client \"summarize_diff\" {} :timeout ~d)~%" *timeout*)
       (let* ((result (mcp:call-tool client "summarize_diff" (obj) :timeout *timeout*))
              (rounds (tool-calls (funcall requests))))
         (format t "The tools/call requests the server got:~%")
         (print-rounds (funcall requests) :answers t)
         (format t "The tool's handler, run once per round:~%")
         (print-notes (funcall notes))
         (format t "call-tool → ~s~%" (tool-text result))
         (check "two rounds: round 0 carried no answers; round 1 carried the model's answer as inputResponses s1"
                (and (= 2 (length rounds))
                     (null (gethash "inputResponses" (second (first rounds))))
                     (equal (keys (gethash "inputResponses" (second (second rounds)))) '("s1"))))
         (check "the model was asked once, with the server's params (maxTokens 64)"
                (and (= 1 calls) got (eql 64 (gethash "maxTokens" got))))
         (check "the tool's text is the model's answer"
                (equal (tool-text result) (format nil "summary: ~a" *summary*)))))
     :sampling (lambda (params) (incf calls) (setf got params) (stub-model params)))))

;;; --- 3. elicitation ------------------------------------------------------------

(defun demo-elicitation ()
  (banner "3. Elicitation over MRTR, then sampling and elicitation in one call")
  (call-with-pair
   (lambda (client requests notes)
     (mcp:mcp-initialize client)
     (format t "(call-tool client \"confirm_push\" {} :timeout ~d)~%" *timeout*)
     (let ((result (mcp:call-tool client "confirm_push" (obj) :timeout *timeout*)))
       (print-rounds (funcall requests) :answers t)
       (print-notes (funcall notes))
       (format t "call-tool → ~s~%" (tool-text result))
       (check "the user's accept and its content reached the tool on the retry"
              (equal (tool-text result) "pushing main (force: no)"))))
   :elicitation #'stub-user)
  (let ((asked '()))
    (call-with-pair
     (lambda (client requests notes)
       (mcp:mcp-initialize client)
       (format t "~%(call-tool client \"summarize_and_push\" {} :timeout ~d): sampling s1, then elicitation e1~%"
               *timeout*)
       (let* ((result (mcp:call-tool client "summarize_and_push" (obj) :timeout *timeout*))
              (rounds (tool-calls (funcall requests))))
         (print-rounds (funcall requests))
         (print-notes (funcall notes))
         (format t "call-tool → ~s~%" (tool-text result))
         (check "three rounds; the third carries both answers (s1 e1, merged by id) and the latest requestState"
                (and (= 3 (length rounds))
                     (same-keys-p (gethash "inputResponses" (second (third rounds))) '("s1" "e1"))
                     (equal (gethash "requestState" (second (third rounds))) "step-2")))
         (check "each handler was asked once: the model, then the user"
                (equal (reverse asked) '(:model :user)))
         (check "the tool finished with both answers"
                (equal (tool-text result) (format nil "~a; pushing main (force: no)" *summary*)))))
     :sampling (lambda (params) (push :model asked) (stub-model params))
     :elicitation (lambda (params) (push :user asked) (stub-user params)))))

;;; --- 4. refusal and limits -------------------------------------------------------

(defun demo-limits ()
  (banner "4. Refusal and limits")
  (let ((refusal (make-condition 'mcp:mcp-error :message "User rejected sampling request" :code -1
                                                :data (obj "server" "stub"))))
    (call-with-pair
     (lambda (client requests notes)
       (declare (ignore notes))
       (mcp:mcp-initialize client)
       (format t "A sampling handler that refuses: (error <mcp-error -1 \"User rejected sampling request\">)~%")
       (let ((caught (call-tool-or-error client "summarize_diff")))
         (print-rounds (funcall requests))
         (format t "  call-tool signalled ~a~%" caught)
         (format t "  as a host's tool error: ~a~%"
                 (js (mcp:tool-result (list (mcp:make-text-content (princ-to-string caught))) :is-error t)))
         (check "call-tool ended with the handler's own MCP-ERROR (the same object, code -1)"
                (eq caught refusal))
         (check "no further round was sent"
                (= 1 (length (tool-calls (funcall requests)))))))
     :sampling (lambda (params) (declare (ignore params)) (error refusal))))
  (let ((answers 0))
    (call-with-pair
     (lambda (client requests notes)
       (declare (ignore notes))
       (mcp:mcp-initialize client)
       (format t "~%A tool that asks again every round (ask_forever); +max-input-rounds+ is ~d:~%"
               mcp:+max-input-rounds+)
       (let ((caught (call-tool-or-error client "ask_forever"))
             (rounds (tool-calls (funcall requests))))
         (print-rounds (funcall requests))
         (format t "  call-tool signalled ~a~@[, data ~a~]~%"
                 caught (and (typep caught 'mcp:mcp-error) (js (mcp:mcp-error-data caught))))
         (check "after +max-input-rounds+ answered rounds: MCP-ERROR -32603 \"… still needs input after 10 rounds\""
                (and (typep caught 'mcp:mcp-error)
                     (eql -32603 (mcp:mcp-error-code caught))
                     (search "still needs input after 10 rounds" (mcp:mcp-error-message caught))))
         (check "11 tools/call (round 0 and 10 answered), the user asked 10 times, the last round carried all 10 answers"
                (and (= 11 (length rounds)) (= 10 answers)
                     (= 10 (length (keys (gethash "inputResponses" (second (car (last rounds)))))))))))
     :elicitation (lambda (params)
                    (declare (ignore params))
                    (incf answers)
                    (obj "action" "accept" "content" (obj)))))
  (call-with-pair
   (lambda (client requests notes)
     (declare (ignore notes))
     (mcp:mcp-initialize client)
     (format t "~%A client without a sampling handler (an elicitation handler only):~%")
     (let ((caught (call-tool-or-error client "summarize_diff")))
       (format t "  it advertised ~a~%" (js (meta-capabilities (request-params (funcall requests) "tools/call"))))
       (print-rounds (funcall requests))
       (format t "  call-tool signalled ~a~%" caught)
       (check "-32601 (method not found), as in 0.4"
              (and (typep caught 'mcp:mcp-error) (eql -32601 (mcp:mcp-error-code caught))))))
   :elicitation #'stub-user))

;;; --- 5. under the hood ---------------------------------------------------------

(defun demo-under-the-hood ()
  (banner "5. Under the hood")
  (format t "~{~a~%~}"
          '("Client and server run in one image: rpc-backend-stdio's MAKE-STDIO-RPC-PAIR (two in-memory"
            "pipes, the client's reader thread, :dispatch :concurrent) and the server's RPC-SERVE loop"
            "around DISPATCH-MCP-METHOD in a thread of its own, recording each request and its answer."
            "A 2026-07-28 server never calls the client: it answers input_required, and CALL-TOOL"
            "answers the inputRequests through the client's handlers (FULFILL-INPUT-REQUESTS) on the"
            "calling thread, then calls again. Outside a tools/call the server sees no answers:"))
  (multiple-value-bind (value present-p) (mcp:mcp-input-response "s1")
    (format t "  (mcp-input-response \"s1\") → ~s, ~s~%" value present-p)
    (check "outside a request MCP-INPUT-RESPONSE is (values NIL NIL)"
           (and (null value) (null present-p))))
  (format t "Loaded: ~{~a~^, ~}.~%"
          (mapcar (lambda (name)
                    (format nil "~a ~a" name (asdf:component-version (asdf:find-system name))))
                  '("mcp-protocol" "rpc-backend-stdio" "rpc-protocol" "rpc-protocol-json"
                    "json-protocol" "json-backend-jzon"))))

(defun run-demos ()
  (let ((*print-right-margin* 110)
        (*checks* '()))
    (demo-capabilities)
    (demo-sampling)
    (demo-elicitation)
    (demo-limits)
    (demo-under-the-hood)
    (let ((failed (count nil *checks* :key #'cdr)))
      (format t "~&~%~d checks, ~:[~d failed~;all passed~].~%" (length *checks*) (zerop failed) failed)
      (finish-output)
      (zerop failed))))
