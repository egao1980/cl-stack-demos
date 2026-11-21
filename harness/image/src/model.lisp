(in-package #:harness-demos-image)

;;; An OpenAI-compatible server: LM Studio at http://127.0.0.1:1234/v1 unless
;;; HARNESS_DEMOS_BASE_URL names another; key HARNESS_DEMOS_API_KEY, else
;;; LM_API_TOKEN from the environment or the workspace .env (never printed).

(defparameter *default-base-url* "http://127.0.0.1:1234/v1")

(defun %base-url ()
  "HARNESS_DEMOS_BASE_URL (any OpenAI-compatible /v1 endpoint), else LM
Studio's default."
  (let ((u (uiop:getenv "HARNESS_DEMOS_BASE_URL")))
    (string-right-trim "/" (if (and u (plusp (length (string-trim " " u))))
                               (string-trim " " u)
                               *default-base-url*))))

(defun %url-host (url)
  "host[:port] of URL, for the output (the key is never printed)."
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position #\/ url :start start) (length url))))
    (subseq url start end)))

(defparameter *lm-studio-url* (%base-url)
  "The endpoint; DETECT-MODEL reads HARNESS_DEMOS_BASE_URL again.")

(defvar *lm-model* nil "The LM Studio model id when one is reachable.")

(defun %env-file-value (path key)
  (when (probe-file path)
    (with-open-file (s path :external-format :utf-8)
      (loop for line = (read-line s nil)
            while line
            do (let ((p (position #\= line)))
                 (when (and p (string= key (string-trim " " (subseq line 0 p))))
                   (return (string-trim " \"'" (subseq line (1+ p))))))))))

(defun %lm-token ()
  "HARNESS_DEMOS_API_KEY when set, else LM_API_TOKEN from the environment or
the workspace .env. \"\" (empty or absent) sends no Authorization header."
  (string-trim " " (or (uiop:getenv "HARNESS_DEMOS_API_KEY")
                       (uiop:getenv "LM_API_TOKEN")
                       (%env-file-value (or (uiop:getenv "HARNESS_DEMOS_ENV_FILE")
                                            (merge-pathnames "Projects/cl-workspace/.env" (user-homedir-pathname)))
                                        "LM_API_TOKEN")
                       "")))

(defparameter *lm-request-timeout* 900
  "Seconds per LM Studio request. llm-protocol-openai's own transport waits
180 s, too short for a 27B model on a long turn (image Demo 10), and has no
option for it (egao1980/llm-protocol-openai#13), so LM-BACKEND passes a
request-fn doing the same request with this timeout. DETECT-MODEL lists the
models with a 15 s timeout instead: on Linux a refused connection waits for
the whole timeout (egao1980/http-backend-async#59).")

(defun %request-fn (timeout)
  (lambda (method url &key headers content want-stream)
    (%request method url timeout headers content want-stream)))

(defvar *lm-usage* '()
  "(input-tokens output-tokens seconds) per real-model chat request since the
last PRINT-LM-USAGE, newest first.")

(defvar *lm-peak-input* nil
  "The largest prompt_tokens of any real-model request in this run.")

(defun %usage-numbers (text)
  "(input output) from the usage object of a chat/completions response TEXT,
or NIL when it has none."
  (let ((u (ignore-errors
            (let ((v (json:decode text)))
              (and (hash-table-p v) (gethash "usage" v))))))
    (and (hash-table-p u)
         (list (gethash "prompt_tokens" u) (gethash "completion_tokens" u)))))

(defun %request (method url timeout headers content want-stream)
  (let* ((start (get-internal-real-time))
         (res (apply #'http:request method url :headers headers :timeout timeout
                     :want-stream (and want-stream t)
                     (and content (list :content content))))
         (body (hp:response-body res))
         (text (cond (want-stream body)
                     ((stringp body) body)
                     ((vectorp body) (babel:octets-to-string body :encoding :utf-8))
                     (t ""))))
    (unless want-stream
      (let ((usage (and (search "/chat/completions" url) (%usage-numbers text))))
        (when usage
          (push (append usage (list (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)))
                *lm-usage*))))
    (values (hp:response-status res) text)))

(defun print-lm-usage (label)
  "Print the token usage of each real-model request since the last call
(input / output tokens, seconds) and the largest input; then forget them."
  (let ((calls (reverse *lm-usage*)))
    (setf *lm-usage* '())
    (when calls
      (note "  token usage of ~a, per model call (from the server's usage object):" label)
      (loop for (in out secs) in calls
            for k from 1
            do (note "    call ~2d: input ~6@a  output ~6@a  ~6,1f s" k in out secs))
      (let ((max-in (reduce #'max calls :key (lambda (c) (if (integerp (first c)) (first c) 0)))))
        (note "    ~d calls; max input ~d tokens; total output ~d tokens; ~,1f s in model calls"
              (length calls) max-in
              (reduce #'+ calls :key (lambda (c) (if (integerp (second c)) (second c) 0)))
              (reduce #'+ calls :key #'third))
        (setf *lm-peak-input* (max max-in (or *lm-peak-input* 0)))))))

(defun lm-backend (&key (timeout *lm-request-timeout*))
  (oa:make-openai-compat-backend :base-url *lm-studio-url* :api-key (%lm-token)
                                 :request-fn (%request-fn timeout)))

(defun %model-override ()
  "The exact model id in HARNESS_DEMOS_MODEL, or NIL when unset or empty."
  (let ((id (uiop:getenv "HARNESS_DEMOS_MODEL")))
    (and id (plusp (length (string-trim " " id))) (string-trim " " id))))

(defun detect-model ()
  "Set *LM-MODEL*: HARNESS_DEMOS_MODEL when set and LM Studio answers, else
a coder model, else the first chat model."
  (banner "Model")
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend
        *lm-studio-url* (%base-url))
  (let* ((hp:*http-backend* (http-backend-async:make-async-backend))
         (ids (handler-case (mapcar #'llm:llm-model-info-id (llm:list-models (lm-backend :timeout 15)))
                (error (c)
                  (note "LM Studio at ~a not reachable: ~a" *lm-studio-url* (clip c 160))
                  nil)))
         (override (%model-override))
         (chat (remove-if (lambda (id) (some (lambda (bad) (search bad id :test #'char-equal))
                                             '("embed" "ocr")))
                          ids)))
    (show "API key found (HARNESS_DEMOS_API_KEY or LM_API_TOKEN)" (and (plusp (length (%lm-token))) t))
    (when ids (show "GET /v1/models" ids))
    (setf *lm-model* (and ids
                          (or override
                              (find-if (lambda (id) (search "coder" id :test #'char-equal)) chat)
                              (first chat))))
    (when (and override ids (not (member override ids :test #'string=)))
      (note "HARNESS_DEMOS_MODEL ~a is not in GET /v1/models; using it anyway (LM Studio may load it on demand)" override))
    (note (if *lm-model*
              (format nil "MODEL THAT RAN: ~a at ~a for the real-model run (~a)" *lm-model*
                      (%url-host *lm-studio-url*) (if override "from HARNESS_DEMOS_MODEL" "picked by the heuristic: coder, then first chat model"))
              "no LM Studio model: the real-model run is skipped"))
    *lm-model*))

;;; A scripted mock: each step is (tool-name . json-arguments) or a string.

(defun scripted-backend (steps &optional log)
  "LOG, a cons, collects the turns each generate saw in its car."
  (let ((n 0))
    (llm:make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (when log (setf (car log) (append (car log) (list (copy-list turns)))))
                (let ((step (nth (min n (1- (length steps))) steps)))
                  (incf n)
                  (if (stringp step)
                      (llm:make-llm-response :parts (list (llm:make-llm-text-part :text step))
                                             :finish-reason :stop)
                      (llm:make-llm-response
                       :parts (list (llm:make-llm-tool-call-part
                                     :id (format nil "c~d" n) :name (car step) :arguments (cdr step)))
                       :finish-reason :tool-use)))))))
