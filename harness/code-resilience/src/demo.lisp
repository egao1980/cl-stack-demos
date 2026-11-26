(in-package #:harness-demos-code-resilience)

;;; cl-stack-code 0.14.0 (H3f): resilience in the coding agent, headless.
;;; Every scene runs `cl-stack-code -p --output-format stream-json` in process
;;; (CODE:RUN-CLI) with the built-in openai provider pointed at a scripted
;;; OpenAI-compatible server on 127.0.0.1, so llm-protocol-openai's real HTTP
;;; path, llm-protocol's typed errors, ai-agent-protocol's retry loop and
;;; cl-stack-code's classification and exit codes all run end to end. No real
;;; model, no real key: the key is the dummy +DEMO-KEY+, never printed.

(defparameter +demo-key+ "demo-key-0f3a9c"
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

(defun elapsed (t0)
  (/ (- (get-internal-real-time) t0) internal-time-units-per-second))

(defun jget (object &rest keys)
  (dolist (k keys object)
    (setf object (and (hash-table-p object) (gethash k object)))))

(defun %show (value)
  "A JSON value for the output: null / false / strings as JSON writes them."
  (cond ((eq value :null) "null")
        ((null value) "null")
        ((eq value t) "true")
        ((eq value :false) "false")
        ((stringp value) (format nil "~s" value))
        (t (princ-to-string value))))

;;; The scripted server. Each request pops the next step of the script (the
;;; last step repeats). A step is
;;;   (:text "answer")                  200, an SSE chat-completion stream
;;;   (:status N [:headers (...)] :message "...")   an error with a JSON body
;;;   (:silent SECONDS)                 reads the request, says nothing, hangs up

(defvar *requests* '() "(seconds-since-scene-start . body) of each request, newest first.")
(defvar *requests-lock* (bt:make-lock "requests"))
(defvar *scene-t0* 0)

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

(defun %sse-answer (stream text)
  (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                      "Transfer-Encoding: chunked" "Connection: close" ""))
  (dolist (event (list (format nil "{\"model\":\"demo-model\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":~s}}]}" text)
                       "{\"model\":\"demo-model\",\"choices\":[{\"index\":0,\"finish_reason\":\"stop\",\"delta\":{}}]}"
                       "{\"model\":\"demo-model\",\"choices\":[],\"usage\":{\"prompt_tokens\":1200,\"completion_tokens\":8,\"total_tokens\":1208}}"
                       "[DONE]"))
    (%chunk stream (format nil "data: ~a~%~%" event)))
  (%out stream (%crlf "0" "")))

(defun %error-answer (stream status headers message)
  (let ((o (babel:string-to-octets
            (format nil "{\"error\":{\"message\":~s,\"type\":\"server_error\"}}" message))))
    (%out stream (apply #'%crlf
                        (format nil "HTTP/1.1 ~d Error" status)
                        "Content-Type: application/json"
                        (format nil "Content-Length: ~d" (length o))
                        (append headers (list "Connection: close" ""))))
    (write-sequence o stream)
    (force-output stream)))

(defun %answer (stream step)
  (ecase (first step)
    (:text (%sse-answer stream (second step)))
    (:status (%error-answer stream (second step) (getf (cddr step) :headers)
                            (getf (cddr step) :message "error")))
    (:silent (sleep (second step)))))

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
                                           (push (cons (elapsed *scene-t0*) body) *requests*))
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

;;; Running cl-stack-code.

(defvar *scratch* nil)

(defun %native (p) (uiop:native-namestring p))

(defun %write-file (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text s)))

(defun scene-dirs (name settings-json)
  "→ (values project config data): a git-rooted project, a user config
directory whose settings.json is SETTINGS-JSON, and a data directory."
  (let* ((root (merge-pathnames (format nil "~a/" name) *scratch*))
         (project (merge-pathnames "project/" root))
         (config (merge-pathnames "config/" root))
         (data (merge-pathnames "data/" root)))
    (ensure-directories-exist (merge-pathnames ".git/" project))
    (%write-file (merge-pathnames "settings.json" config) settings-json)
    (ensure-directories-exist data)
    (values project config data)))

(defun run-code (base argv &key project config data (prompt ""))
  "CODE:RUN-CLI on ARGV with the openai provider at BASE and the dummy key.
→ (values exit events stderr seconds)."
  (let* ((argv (append (list "--cwd" (%native project) "--model" "openai:demo-model") argv))
         (err (make-string-output-stream))
         (exit nil)
         (t0 (get-internal-real-time))
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
            (get-output-stream-string err)
            (elapsed t0))))

(defun show-argv (argv &optional prompt)
  (note "$ ~@[echo ~s | ~]cl-stack-code --model openai:demo-model ~{~a~^ ~}" prompt argv))

(defun events-of (events subtype)
  (remove-if-not (lambda (e) (equal subtype (jget e "subtype"))) events))

(defun result-of (events)
  (find "result" events :key (lambda (e) (jget e "type")) :test #'equal))

(defun show-init (events)
  (let ((init (first (events-of events "init"))))
    (when init
      (note "init: retry ~a, request_timeout_ms ~a, turn_timeout_ms ~a, context_window ~a (~a)"
            (let ((r (jget init "retry")))
              (format nil "{max_attempts ~a, initial_delay_ms ~a, max_delay_ms ~a}"
                      (%show (jget r "max_attempts")) (%show (jget r "initial_delay_ms"))
                      (%show (jget r "max_delay_ms"))))
            (%show (jget init "request_timeout_ms")) (%show (jget init "turn_timeout_ms"))
            (%show (jget init "context_window")) (%show (jget init "context_window_source"))))))

(defun show-retry (event &key expected-ms jittered)
  "One retry event; EXPECTED-MS is the un-jittered delay: a jittered delay is
checked against (0.8 x EXPECTED, EXPECTED] (llm-protocol's jitter 0.2),
an exact one printed."
  (let ((delay (jget event "delay_ms")))
    (note "retry: reason ~a, attempt ~a of ~a, status ~a, retry_after_ms ~a, delay_ms ~a"
          (%show (jget event "reason")) (%show (jget event "attempt"))
          (%show (jget event "max_attempts")) (%show (jget event "status"))
          (%show (jget event "retry_after_ms"))
          (cond ((not jittered) (%show delay))
                ((and (realp delay) expected-ms
                      (> delay (* 0.8 expected-ms)) (<= delay expected-ms))
                 (format nil "in (~d, ~d]: yes (jittered)" (round (* 0.8 expected-ms)) expected-ms))
                (t (format nil "~a: NOT in (~d, ~d]" delay (round (* 0.8 expected-ms)) expected-ms))))
    (note "       error ~a" (%show (jget event "error")))))

(defun show-result (events exit)
  (let ((r (result-of events)))
    (if (null r)
        (note "!! no result event")
        (progn
          (note "result: subtype ~a, is_error ~:[false~;true~]~@[, error_class ~a~]~@[, retries ~a~]"
                (%show (jget r "subtype"))
                (let ((e (jget r "is_error"))) (and e (not (eq e :false))))
                (and (jget r "error_class") (%show (jget r "error_class")))
                (jget r "retries"))
          (if (equal (jget r "subtype") "success")
              (note "        result ~a" (%show (jget r "result")))
              (note "        error ~a" (%show (jget r "error"))))))
    (note "exit code: ~a" exit)))

(defun show-stderr (stderr)
  (dolist (line (uiop:split-string stderr :separator '(#\Newline)))
    (when (plusp (length line))
      (note "stderr: ~a" line))))

(defun requests-seen ()
  (reverse *requests*))

(defmacro with-scene ((base script) &body body)
  ;; SETF, not LET: the server's request threads see the global values.
  `(progn
     (bt:with-lock-held (*requests-lock*)
       (setf *requests* '() *scene-t0* (get-internal-real-time)))
     (call-with-server ,script (lambda (,base) ,@body))))

(defun %retry-settings (attempts &rest more)
  (format nil "{\"retry\": {\"maxAttempts\": ~d, \"initialDelayMs\": 50, \"maxDelayMs\": 2000}~{, ~a~}}"
          attempts more))

(defparameter +stream-argv+ '("-p" "--output-format" "stream-json"))

;;; Scenes.

(defun scene-1-retry-then-success ()
  (banner "1. 529 overloaded, then 429 with Retry-After: 1, then an answer")
  (multiple-value-bind (project config data)
      (scene-dirs "s1" (%retry-settings 4 "\"contextWindow\": 4096"))
    (note "settings.json: ~a" (%retry-settings 4 "\"contextWindow\": 4096"))
    (with-scene (base '((:status 529 :message "the server is overloaded")
                        (:status 429 :headers ("Retry-After: 1") :message "slow down")
                        (:text "Recovered after two retries.")))
      (show-argv +stream-argv+ "say something")
      (multiple-value-bind (exit events stderr seconds)
          (run-code base +stream-argv+ :project project :config config :data data
                                       :prompt "say something")
        (show-init events)
        (let ((retries (events-of events "retry")))
          (when (first retries) (show-retry (first retries) :expected-ms 100 :jittered t))
          (when (second retries) (show-retry (second retries))))
        (let ((assistant (find "assistant" events :key (lambda (e) (jget e "type")) :test #'equal)))
          (when assistant
            (note "assistant: context_tokens ~a, context_pct ~a (of contextWindow 4096)"
                  (%show (jget assistant "context_tokens")) (%show (jget assistant "context_pct")))))
        (show-result events exit)
        (show-stderr stderr)
        (let ((times (mapcar #'car (requests-seen))))
          (note "requests the server saw: ~d" (length times))
          (when (= 3 (length times))
            (note "gap before the third request >= 1.0 s (Retry-After honoured): ~:[NO~;yes~]"
                  (>= (- (third times) (second times)) 1.0))))
        (note "run took < 5 s: ~:[NO~;yes~]" (< seconds 5))))))

(defun scene-2-retries-exhausted ()
  (banner "2. Retries exhausted: the server keeps answering 503")
  (multiple-value-bind (project config data) (scene-dirs "s2" (%retry-settings 3))
    (note "settings.json: ~a" (%retry-settings 3))
    (with-scene (base '((:status 503 :message "Service Unavailable: the server is overloaded")))
      (show-argv +stream-argv+ "say something")
      (multiple-value-bind (exit events stderr)
          (run-code base +stream-argv+ :project project :config config :data data
                                       :prompt "say something")
        (let ((retries (events-of events "retry")))
          (loop for e in retries
                for expected in '(100 200)
                do (show-retry e :expected-ms expected :jittered t)))
        (show-result events exit)
        (show-stderr stderr)
        (note "requests the server saw: ~d (maxAttempts 3)" (length (requests-seen)))))
    (banner "2b. The same failure in text mode: the message goes to stderr")
    (with-scene (base '((:status 503 :message "Service Unavailable: the server is overloaded")))
      (show-argv '("-p" "--max-retries" "0") "say something")
      (multiple-value-bind (exit events stderr)
          (run-code base '("-p" "--max-retries" "0") :project project :config config :data data
                                                     :prompt "say something")
        (declare (ignore events))
        (show-stderr stderr)
        (note "exit code: ~a; requests the server saw: ~d (--max-retries 0)"
              exit (length (requests-seen)))))))

(defun scene-3-request-timeout ()
  ;; Two findings show here (see README.md): requestTimeoutMs does not bound
  ;; the streamed request -p makes (only the turn timeout ends it), and the
  ;; run returns only when the server hangs up, not when the turn timeout
  ;; fires.
  (banner "3. A silent server: requestTimeoutMs 1000 (turnTimeoutMs 4000 as a backstop)")
  (let ((settings (%retry-settings 1 "\"requestTimeoutMs\": 1000" "\"turnTimeoutMs\": 4000")))
    (multiple-value-bind (project config data) (scene-dirs "s3" settings)
      (note "settings.json: ~a" settings)
      (note "the server reads the request, says nothing for 6 s, then hangs up")
      (with-scene (base '((:silent 6)))
        (show-argv +stream-argv+ "say something")
        (multiple-value-bind (exit events stderr seconds)
            (run-code base +stream-argv+ :project project :config config :data data
                                         :prompt "say something")
          (show-init events)
          (show-result events exit)
          (show-stderr stderr)
          (note "requests the server saw: ~d" (length (requests-seen)))
          (note "ended by requestTimeoutMs (within 2 s): ~:[no, the turn timeout ended the turn~;yes~]"
                (< seconds 2))
          (note "run-cli returned after about ~d s (~:[before~;when~] the server hung up at 6 s)"
                (round seconds) (>= seconds 5.5)))))
    (banner "3b. The turn timeout alone: --turn-timeout 1.5")
    (multiple-value-bind (project config data) (scene-dirs "s3b" (%retry-settings 1))
      (with-scene (base '((:silent 6)))
        (let ((argv (append +stream-argv+ '("--turn-timeout" "1.5"))))
          (show-argv argv "say something")
          (multiple-value-bind (exit events stderr seconds)
              (run-code base argv :project project :config config :data data
                                  :prompt "say something")
            (show-result events exit)
            (show-stderr stderr)
            (note "run-cli returned after about ~d s (~:[before~;when~] the server hung up at 6 s)"
                  (round seconds) (>= seconds 5.5))))))))

(defun %last-user-text (body)
  (let* ((object (ignore-errors (json:decode body)))
         (messages (and object (coerce (or (jget object "messages") #()) 'list)))
         (user (find "user" messages :key (lambda (m) (jget m "role")) :test #'equal
                                     :from-end t)))
    (and user (jget user "content"))))

(defun scene-4-retry-flag ()
  (banner "4. --continue --retry re-sends a failed turn")
  (multiple-value-bind (project config data) (scene-dirs "s4" (%retry-settings 1))
    (note "settings.json: ~a" (%retry-settings 1))
    (with-scene (base '((:status 503 :message "the server is overloaded")))
      (show-argv +stream-argv+ "what is 2 + 2?")
      (multiple-value-bind (exit events)
          (run-code base +stream-argv+ :project project :config config :data data
                                       :prompt "what is 2 + 2?")
        (show-result events exit)))
    (with-scene (base '((:text "4")))
      (let ((argv (append +stream-argv+ '("--continue" "--retry"))))
        (show-argv argv)
        (multiple-value-bind (exit events stderr)
            (run-code base argv :project project :config config :data data)
          (show-result events exit)
          (show-stderr stderr)
          (let ((bodies (mapcar #'cdr (requests-seen))))
            (note "requests: ~d; the re-sent request's last user message: ~a"
                  (length bodies) (%show (and bodies (%last-user-text (first bodies)))))))))
    (with-scene (base '((:text "unused")))
      (let ((argv (append +stream-argv+ '("--continue" "--retry"))))
        (show-argv argv)
        (multiple-value-bind (exit events stderr)
            (run-code base argv :project project :config config :data data)
          (declare (ignore events))
          (show-stderr stderr)
          (note "exit code: ~a; requests the server saw: ~d" exit (length (requests-seen))))))))

(defun scene-5-auth ()
  (banner "5. 401 echoing part of the key: error_auth, the key redacted")
  (multiple-value-bind (project config data) (scene-dirs "s5" (%retry-settings 4))
    (with-scene (base (list (list :status 401
                                  :message (format nil "Incorrect API key provided: ~a. Check your key."
                                                   (subseq +demo-key+ 0 12)))))
      (show-argv +stream-argv+ "say something")
      (let ((*redactions* (list* (cons (subseq +demo-key+ 0 12) "<key-prefix>") *redactions*)))
        ;; The demo's own redaction of the prefix is a backstop: a "[redacted]"
        ;; in the output is cl-stack-code's, a "<key-prefix>" would be a leak.
        (multiple-value-bind (exit events stderr)
            (run-code base +stream-argv+ :project project :config config :data data
                                         :prompt "say something")
          (declare (ignore stderr))
          (show-result events exit)
          (let ((err (jget (result-of events) "error")))
            (note "the key or its 12-character prefix in the error: ~:[no~;YES~]"
                  (and (stringp err) (search (subseq +demo-key+ 0 12) err))))
          (note "requests the server saw: ~d (401 is not retried)" (length (requests-seen))))))))

(defparameter *demos* '(scene-1-retry-then-success scene-2-retries-exhausted
                        scene-3-request-timeout scene-4-retry-flag scene-5-auth))

(defun run-demos (&key (demos *demos*))
  (let* ((failed '())
         (t0 (get-internal-real-time))
         (*scratch* (let ((d (merge-pathnames (format nil "code-resilience-~36r/"
                                                      (random (expt 36 6) (make-random-state t)))
                                              (uiop:temporary-directory))))
                      (ensure-directories-exist d)
                      (truename d)))
         (*redactions* (append (list (cons (%native *scratch*) "<tmp>/")
                                     (cons +demo-key+ "<demo-key>")
                                     (cons (%native (user-homedir-pathname)) "~/"))
                               (let ((n (%native *scratch*)))
                                 (and (uiop:string-prefix-p "/private/" n)
                                      (list (cons (subseq n 8) "<tmp>/")))))))
    (unwind-protect
         (progn
           (banner "Loaded systems")
           (dolist (name '("cl-stack-code" "ai-agent-protocol" "llm-protocol" "llm-protocol-openai"
                           "http-protocol" "http-backend-async" "event-backend-libuv"))
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
    (note "total wall time under 30 s: ~:[NO~;yes~]" (< (elapsed t0) 30))
    (format *error-output* "~&(total wall time ~,1f s)~%" (elapsed t0))
    (when failed (uiop:quit 1))))
