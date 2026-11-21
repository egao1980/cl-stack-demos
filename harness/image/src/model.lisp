(in-package #:harness-demos-image)

;;; LM Studio (http://127.0.0.1:1234/v1, token LM_API_TOKEN from the
;;; environment or the workspace .env; never printed).

(defparameter *lm-studio-url* "http://127.0.0.1:1234/v1")

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
  (or (uiop:getenv "LM_API_TOKEN")
      (%env-file-value (or (uiop:getenv "HARNESS_DEMOS_ENV_FILE")
                           (merge-pathnames "Projects/cl-workspace/.env" (user-homedir-pathname)))
                       "LM_API_TOKEN")))

(defparameter *lm-request-timeout* 900
  "Seconds per LM Studio request. llm-protocol-openai's own transport waits
180 s, too short for a 27B model on a long turn (image Demo 10), and has no
option for it (egao1980/llm-protocol-openai#13), so LM-BACKEND passes a
request-fn doing the same request with this timeout.")

(defun %request-fn (method url &key headers content want-stream)
  (let* ((res (apply #'http:request method url :headers headers :timeout *lm-request-timeout*
                     :want-stream (and want-stream t)
                     (and content (list :content content))))
         (body (hp:response-body res)))
    (values (hp:response-status res)
            (cond (want-stream body)
                  ((stringp body) body)
                  ((vectorp body) (babel:octets-to-string body :encoding :utf-8))
                  (t "")))))

(defun lm-backend ()
  (oa:make-openai-compat-backend :base-url *lm-studio-url* :api-key (%lm-token)
                                 :request-fn #'%request-fn))

(defun %model-override ()
  "The exact model id in HARNESS_DEMOS_MODEL, or NIL when unset or empty."
  (let ((id (uiop:getenv "HARNESS_DEMOS_MODEL")))
    (and id (plusp (length (string-trim " " id))) (string-trim " " id))))

(defun detect-model ()
  "Set *LM-MODEL*: HARNESS_DEMOS_MODEL when set and LM Studio answers, else
a coder model, else the first chat model."
  (banner "Model")
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend)
  (let* ((hp:*http-backend* (http-backend-async:make-async-backend))
         (ids (handler-case (mapcar #'llm:llm-model-info-id (llm:list-models (lm-backend)))
                (error (c)
                  (note "LM Studio at ~a not reachable: ~a" *lm-studio-url* (clip c 160))
                  nil)))
         (override (%model-override))
         (chat (remove-if (lambda (id) (some (lambda (bad) (search bad id :test #'char-equal))
                                             '("embed" "ocr")))
                          ids)))
    (show "LM_API_TOKEN found" (and (%lm-token) (plusp (length (%lm-token))) t))
    (when ids (show "GET /v1/models" ids))
    (setf *lm-model* (and ids
                          (or override
                              (find-if (lambda (id) (search "coder" id :test #'char-equal)) chat)
                              (first chat))))
    (when (and override ids (not (member override ids :test #'string=)))
      (note "HARNESS_DEMOS_MODEL ~a is not in GET /v1/models; using it anyway (LM Studio may load it on demand)" override))
    (note (if *lm-model*
              (format nil "MODEL THAT RAN: LM Studio ~a for the real-model run (~a)" *lm-model*
                      (if override "from HARNESS_DEMOS_MODEL" "picked by the heuristic: coder, then first chat model"))
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
