(in-package #:harness-demos-isolation)

;;; Demos 4-5 support: printing temp paths as <tmp>, tool calls as a model sees
;;; them, and the live model (any OpenAI-compatible /v1 server) for the agent run.
;;; The live-model part is the supervisor set's (src/tool-support.lisp), trimmed.

(defun clip (string &optional (n 40))
  (if (> (length string) n)
      (format nil "~a…(~d chars)" (subseq string 0 n) (length string))
      string))

;;; ------------------------------------------------------------------
;;; Scrubbing: temp paths in printed tool text become <tmp>/…

(defvar *scrub-paths* '() "(native-namestring . replacement) pairs, longest first.")

(defun %replace-all (string old new)
  (with-output-to-string (o)
    (loop with start = 0
          for p = (search old string :start2 start)
          do (write-string string o :start start :end (or p (length string)))
             (if p (progn (write-string new o) (setf start (+ p (length old)))) (loop-finish)))))

(defun scrub (text)
  (reduce (lambda (s pair) (%replace-all s (car pair) (cdr pair)))
          *scrub-paths* :initial-value text))

(defmacro with-scrubbed-path ((dir replacement) &body body)
  "BODY with DIR's native namestring printed as REPLACEMENT."
  (let ((d (gensym)))
    `(let* ((,d ,dir)
            (*scrub-paths* (sort (remove-duplicates
                                  (append (list (cons (uiop:native-namestring ,d) ,replacement)
                                                (cons (string-right-trim "/\\" (uiop:native-namestring ,d))
                                                      (string-right-trim "/" ,replacement)))
                                          *scrub-paths*)
                                  :test #'equal :key #'car)
                                 #'> :key (lambda (p) (length (car p))))))
       ,@body)))

;;; ------------------------------------------------------------------
;;; Tool calls, printed: the call as a model sends it, then the exact text.

(defun json-args (&rest kv)
  "A JSON object string from alternating keys and values (what a model sends)."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    (json:encode h)))

(defun print-block (text &key (prefix "   | ") max-lines)
  (let* ((lines (uiop:split-string (scrub (string-right-trim '(#\Newline) text))
                                   :separator (string #\Newline)))
         (n (length lines)))
    (loop for l in lines
          for i from 1
          while (or (null max-lines) (<= i max-lines))
          do (format t "~&~a~a~%" prefix l))
    (when (and max-lines (> n max-lines))
      (format t "~&~a… (~d more lines)~%" prefix (- n max-lines)))
    (finish-output)))

(defun bash (tools command &rest more)
  "Call the bash tool of TOOLS with COMMAND (MORE: further key value arguments);
print the call and its exact result. → the result string. A condition is not
caught here."
  (let ((args (apply #'json-args "command" command more)))
    (format t "~&   > bash ~a~%" (scrub args))
    (let ((r (at:call-shell-tool tools "bash" args)))
      (print-block r)
      r)))

;;; ------------------------------------------------------------------
;;; Agent runs

(defmacro with-agent-loop (&body body)
  "Bind a libuv event backend and loop, as run-ai-agent needs."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (ev:make-event-loop eb)))
     (ev:with-event-backend (eb)
       (ev:with-event-loop-var (el)
         ,@body))))

(defun tool-results (run)
  (loop for turn in (agent:agent-run-turns run)
        append (remove-if-not #'llm:llm-tool-result-part-p (llm:llm-turn-parts turn))))

(defun print-run (run &key max-lines)
  "Each invocation of RUN: step number, tool, arguments, status, the exact result."
  (let ((results (tool-results run)))
    (loop for inv in (agent:agent-run-invocations run)
          for k from 1
          do (let ((part (find (agent:agent-invocation-id inv) results
                               :key #'llm:llm-tool-result-part-id :test #'equal)))
               (format t "~&~%   ~2d. ~a ~a~%" k (agent:agent-invocation-name inv)
                       (scrub (princ-to-string (agent:agent-invocation-arguments inv))))
               (format t "~&       status ~(~a~)~:[~; (error)~]~%" (agent:agent-invocation-status inv)
                       (and part (llm:llm-tool-result-part-error-p part)))
               (when part
                 (print-block (princ-to-string (llm:llm-tool-result-part-content part))
                              :prefix "       | " :max-lines max-lines))))
    (show "finish reason" (agent:agent-run-finish-reason run))
    (let ((text (ignore-errors (agent:agent-run-text run))))
      (when text (print-block text :prefix "   answer | ")))))

;;; ------------------------------------------------------------------
;;; The live model: any OpenAI-compatible /v1 server (HARNESS_DEMOS_BASE_URL,
;;; default LM Studio's), key HARNESS_DEMOS_API_KEY, else LM_API_TOKEN from the
;;; environment or the workspace .env. The key is never printed.

(defparameter *default-base-url* "http://127.0.0.1:1234/v1")

(defun %base-url ()
  (let ((u (uiop:getenv "HARNESS_DEMOS_BASE_URL")))
    (string-right-trim "/" (if (and u (plusp (length (string-trim " " u))))
                               (string-trim " " u)
                               *default-base-url*))))

(defun %url-host (url)
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position #\/ url :start start) (length url))))
    (subseq url start end)))

(defun %env-file-value (path key)
  (when (probe-file path)
    (with-open-file (s path :external-format :utf-8)
      (loop for line = (read-line s nil)
            while line
            do (let ((p (position #\= line)))
                 (when (and p (string= key (string-trim " " (subseq line 0 p))))
                   (return (string-trim " \"'" (subseq line (1+ p))))))))))

(defun %lm-token ()
  (string-trim " " (or (uiop:getenv "HARNESS_DEMOS_API_KEY")
                       (uiop:getenv "LM_API_TOKEN")
                       (%env-file-value (or (uiop:getenv "HARNESS_DEMOS_ENV_FILE")
                                            (merge-pathnames "Projects/cl-workspace/.env" (user-homedir-pathname)))
                                        "LM_API_TOKEN")
                       "")))

(defparameter *lm-request-timeout* 900
  "Seconds per real-model request (a reasoning model on a slow host can take
minutes per turn; llm-protocol-openai's own transport waits 180 s).")

(defvar *lm-url* nil)
(defvar *lm-model* nil "The live model id when a server answers, else NIL.")
(defvar *lm-usage* '() "(input output seconds) per real-model call, newest first.")

(defclass usage-tapping-backend (oa:openai-compat-backend) ()
  (:documentation "Records the usage and time of each generate / stream-generate."))

(defun %record-usage (start response)
  (let ((u (and (llm:llm-response-p response) (llm:llm-response-usage response))))
    (push (list (and u (llm:llm-usage-prompt-tokens u)) (and u (llm:llm-usage-output-tokens u))
                (/ (- (get-internal-real-time) start) internal-time-units-per-second))
          *lm-usage*)
    response))

(defmethod llm:generate :around ((b usage-tapping-backend) turns &key &allow-other-keys)
  (declare (ignore turns))
  (%record-usage (get-internal-real-time) (call-next-method)))

(defmethod llm:stream-generate :around ((b usage-tapping-backend) turns &key &allow-other-keys)
  (declare (ignore turns))
  (%record-usage (get-internal-real-time) (call-next-method)))

(defun %request-fn (timeout)
  (lambda (method url &key headers content want-stream)
    (let* ((res (apply #'http:request method url :headers headers :timeout timeout
                       :want-stream (and want-stream t)
                       (and content (list :content content))))
           (body (hp:response-body res))
           (text (cond (want-stream body)
                       ((stringp body) body)
                       ((vectorp body) (babel:octets-to-string body :encoding :utf-8))
                       (t ""))))
      (values (hp:response-status res) text))))

(defun lm-backend (&key (timeout *lm-request-timeout*))
  (change-class (oa:make-openai-compat-backend :base-url *lm-url* :api-key (%lm-token)
                                               :request-fn (%request-fn timeout))
                'usage-tapping-backend))

(defun detect-model ()
  "Set *LM-MODEL*: HARNESS_DEMOS_MODEL when set and the server answers, else
a coder model, else the first chat model; NIL (with the reason printed) when
no server answers."
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend
        *lm-url* (%base-url))
  (let* ((hp:*http-backend* (http-backend-async:make-async-backend))
         (ids (handler-case (mapcar #'llm:llm-model-info-id (llm:list-models (lm-backend :timeout 15)))
                (error (c)
                  (note "no model server at ~a: ~a" (%url-host *lm-url*) (clip (princ-to-string c) 160))
                  nil)))
         (override (let ((id (uiop:getenv "HARNESS_DEMOS_MODEL")))
                     (and id (plusp (length (string-trim " " id))) (string-trim " " id))))
         (chat (remove-if (lambda (id) (some (lambda (bad) (search bad id :test #'char-equal))
                                             '("embed" "ocr")))
                          ids)))
    (show "API key found (HARNESS_DEMOS_API_KEY or LM_API_TOKEN)" (and (plusp (length (%lm-token))) t))
    (setf *lm-model* (and ids (or override
                                  (find-if (lambda (id) (search "coder" id :test #'char-equal)) chat)
                                  (first chat))))
    (when *lm-model*
      (note "MODEL THAT RAN: ~a at ~a (~a)" *lm-model* (%url-host *lm-url*)
            (if override "from HARNESS_DEMOS_MODEL" "picked by the heuristic: a coder model, then the first chat model"))
      (note "~a" (sampling-description)))
    *lm-model*))

(defun print-lm-usage ()
  "The token usage of each real-model call since the last call; then forget them.
→ (values calls max-input total-output seconds)."
  (let ((calls (reverse *lm-usage*)))
    (setf *lm-usage* '())
    (note "token usage per model call (prompt tokens, cached included):")
    (loop for (in out secs) in calls
          for k from 1
          do (note "  call ~2d: input ~6@a  output ~6@a  ~6,1f s" k in out secs))
    (let ((max-in (reduce #'max calls :key (lambda (c) (if (integerp (first c)) (first c) 0)) :initial-value 0))
          (out (reduce #'+ calls :key (lambda (c) (if (integerp (second c)) (second c) 0))))
          (secs (reduce #'+ calls :key #'third)))
      (note "  ~d calls; max input ~d tokens; total input ~d; total output ~d tokens; ~,1f s in model calls"
            (length calls) max-in
            (reduce #'+ calls :key (lambda (c) (if (integerp (first c)) (first c) 0)))
            out secs)
      (values (length calls) max-in out secs))))

