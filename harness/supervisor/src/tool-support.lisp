(in-package #:harness-demos-supervisor)

;;; H8c support: the corpus fixtures, printing tool calls and results the way a
;;; model sees them, scripted mock and live OpenAI-compatible backends, and the
;;; agent loop (libuv) for ai-agent-protocol runs.

;;; ------------------------------------------------------------------
;;; Fixtures: three logs captured from real runs (supervisor-protocol's test
;;; corpus: make + cc, a Rove run, a Node dev server), local paths as /home/dev/….

(defun fixture-lines (name)
  "Lines of fixtures/NAME.log (UTF-8; a CR before LF dropped)."
  (with-open-file (s (asdf:system-relative-pathname "harness-demos-supervisor"
                                                    (format nil "fixtures/~a.log" name))
                     :external-format :utf-8)
    (loop for l = (read-line s nil) while l
          collect (string-right-trim '(#\Return) l))))

(defun devserver-stderr-p (line)
  "The dev server's request log (timestamped) went to stdout; traces and
warnings to stderr."
  (not (and (>= (length line) 5) (string= "2026-" line :end2 5))))

(defun printing-unit (sup lines &key labels stderr-p)
  "A thread unit (kind :shell-job) printing LINES (to *ERROR-OUTPUT* when
STDERR-P says so); waits until it finished and its log is closed."
  (let ((u (sp:start-unit sup :shell-job
                          :labels labels
                          :function (lambda (u)
                                      (declare (ignore u))
                                      (dolist (l lines)
                                        (if (and stderr-p (funcall stderr-p l))
                                            (progn (write-line l *error-output*) (finish-output *error-output*))
                                            (progn (write-line l) (finish-output))))
                                      :done))))
    (settle u)))

(defun settle (unit &key (timeout 30))
  "Wait until UNIT finished and its log has its end record → UNIT."
  (sp:wait-unit unit :timeout timeout)
  (wait-until (lambda () (ignore-errors (sp:log-closed-p unit))) :timeout 10)
  unit)

;;; ------------------------------------------------------------------
;;; Scrubbing: what varies run to run (wall-clock times, temp paths) is replaced
;;; in the printed text, so the scripted parts print the same every run.

(defvar *scrub-paths* '() "(native-namestring . replacement) pairs, longest first.")

(defun %timestamp-p (s)
  "RFC 3339 UTC as the unit tools print it: 2026-11-21T12:00:01Z or …01.250Z."
  (and (member (length s) '(20 24))
       (char= #\T (char s 10)) (char= #\Z (char s (1- (length s))))
       (digit-char-p (char s 0)) (char= #\- (char s 4))))

(defun %replace-all (string old new)
  (with-output-to-string (o)
    (loop with start = 0
          for p = (search old string :start2 start)
          do (write-string string o :start start :end (or p (length string)))
             (if p (progn (write-string new o) (setf start (+ p (length old)))) (loop-finish)))))

(defun scrub (text)
  "TEXT with the temp paths named in *SCRUB-PATHS* replaced, the time field of
unit_search hit headers and the started column of unit_list rows as <time>."
  (let ((text (reduce (lambda (s pair) (%replace-all s (car pair) (cdr pair)))
                      *scrub-paths* :initial-value text)))
    (format nil "~{~a~^~%~}"
            (mapcar (lambda (line)
                      (cond ((and (plusp (length line)) (char= #\[ (char line 0))
                                  (digit-char-p (char line (min 1 (1- (length line))))))
                             ;; [i] ref stream time kinds…
                             (let ((fields (uiop:split-string line :separator " ")))
                               (if (and (> (length fields) 3) (%timestamp-p (fourth fields)))
                                   (format nil "~{~a~^ ~}" (append (subseq fields 0 3) (list "<time>")
                                                                   (nthcdr 4 fields)))
                                   line)))
                            ((find #\Tab line)
                             (format nil "~{~a~^	~}"
                                     (mapcar (lambda (f) (if (and (= 20 (length f)) (%timestamp-p f)) "<time>" f))
                                             (uiop:split-string line :separator (string #\Tab)))))
                            (t line)))
                    (uiop:split-string text :separator (string #\Newline))))))

(defmacro with-scrubbed-path ((dir replacement) &body body)
  "BODY with DIR's native namestring (and its truename's) printed as REPLACEMENT."
  (let ((d (gensym)))
    `(let* ((,d ,dir)
            (*scrub-paths* (sort (remove-duplicates
                                  (append (list (cons (uiop:native-namestring ,d) ,replacement)
                                                (cons (string-right-trim "/\\" (uiop:native-namestring ,d)) ,replacement)
                                                (cons (namestring ,d) ,replacement))
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
  (let* ((lines (uiop:split-string (scrub text) :separator (string #\Newline)))
         (n (length lines)))
    (loop for l in lines
          for i from 1
          while (or (null max-lines) (<= i max-lines))
          do (format t "~&~a~a~%" prefix l))
    (when (and max-lines (> n max-lines))
      (format t "~&~a… (~d more lines)~%" prefix (- n max-lines)))
    (finish-output)))

(defun tool-call (source name &rest kv)
  "Call tool NAME of SOURCE (unit-tools or shell-tools) with KV as JSON; print
the call and its exact result (or the condition the agent loop would turn into
the tool result) → the result string or NIL."
  (let ((args (apply #'json-args kv)))
    (format t "~&~%   > ~a ~a~%" name (scrub args))
    (handler-case
        (let ((r (etypecase source
                   (at:unit-tools (at:call-unit-tool source name args))
                   (at:shell-tools (at:call-shell-tool source name args)))))
          (print-block r)
          r)
      (error (c)
        (format t "~&   ! ~s:~%" (type-of c))
        (print-block (princ-to-string c) :prefix "   ! ")
        nil))))

;;; ------------------------------------------------------------------
;;; Agent runs

(defmacro with-agent-loop (&body body)
  "Bind a libuv event backend and loop, as run-ai-agent needs."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (ev:make-event-loop eb)))
     (ev:with-event-backend (eb)
       (ev:with-event-loop-var (el)
         ,@body))))

(defun scripted-backend (steps &key (counter (list 0)))
  "Mock LLM backend answering STEPS in turn. A step is a string (final reply)
or a list of tool calls (name json-arguments). Tool call ids are unique in the
conversation (g<generate>-c<k>)."
  (llm:make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b turns))
              (let* ((i (car counter))
                     (step (nth (min i (1- (length steps))) steps)))
                (incf (car counter))
                (if (stringp step)
                    (llm:make-llm-response :parts (list (llm:make-llm-text-part :text step))
                                           :finish-reason :stop)
                    (llm:make-llm-response
                     :parts (loop for (name args) in step
                                  for k from 1
                                  collect (llm:make-llm-tool-call-part
                                           :id (format nil "g~d-c~d" (1+ i) k)
                                           :name name :arguments args))
                     :finish-reason :tool-use))))))

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
                 (print-block (string-right-trim '(#\Newline)
                                                 (princ-to-string (llm:llm-tool-result-part-content part)))
                              :prefix "       | " :max-lines max-lines))))
    (show "finish reason" (agent:agent-run-finish-reason run))
    (let ((text (ignore-errors (agent:agent-run-text run))))
      (when text (print-block text :prefix "   answer | ")))))

(defun tools-used (run)
  "Distinct tool names of RUN's invocations, in first-use order, with counts."
  (let ((counts '()))
    (dolist (inv (agent:agent-run-invocations run))
      (let* ((name (agent:agent-invocation-name inv))
             (cell (assoc name counts :test #'string=)))
        (if cell (incf (cdr cell)) (setf counts (append counts (list (cons name 1)))))))
    (format nil "~{~a~^, ~}" (mapcar (lambda (c) (format nil "~a ×~d" (car c) (cdr c))) counts))))

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
            (if override "from HARNESS_DEMOS_MODEL" "picked by the heuristic: a coder model, then the first chat model")))
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

(defun print-rule (rule)
  (note "rule (~s ~s ~s)~@[ feedback ~s~]"
        (agent:permission-rule-action rule) (agent:permission-rule-resource rule)
        (agent:permission-rule-effect rule) (ignore-errors (agent:permission-rule-feedback rule))))
