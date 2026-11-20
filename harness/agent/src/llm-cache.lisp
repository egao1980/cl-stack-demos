(in-package #:harness-demos-agent)

;;; Demo 5: llm-protocol-openai 0.4.0 prompt cache, live against LM Studio.
;;; The token comes from LM_API_TOKEN, else from the workspace .env; it is
;;; never printed.

(defparameter *lm-studio-url* "http://127.0.0.1:1234/v1")

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

(defun %body-text (body)
  (etypecase body
    (string body)
    ((vector (unsigned-byte 8)) (babel:octets-to-string body :encoding :utf-8))
    (null "")))

(defun %tapping-request-fn (log)
  "A request-fn doing what the backend's own does (http-protocol:request on
the bound backend) and pushing (request-json . response-json) onto LOG's car."
  (lambda (method url &key headers content want-stream)
    (let* ((res (apply #'http:request method url :headers headers :timeout 180
                       :want-stream (and want-stream t)
                       (and content (list :content content))))
           (status (hp:response-status res))
           (body (if want-stream (hp:response-body res) (%body-text (hp:response-body res)))))
      (unless want-stream (push (cons content body) (car log)))
      (values status body))))

(defun %long-system-prompt ()
  (with-output-to-string (s)
    (write-line "You are coder, a careful Common Lisp assistant working in a cl-stack project." s)
    (write-line "Project rules (read them all; they never change during this session):" s)
    (loop for i from 1 to 60
          do (format s "Rule ~d: when you touch module ~r, keep its exported API stable, write a Rove test for each behaviour change, run the ASDF test-op before you report, and say which files you changed and why.~%"
                     i i))
    (write-line "Answer in one short sentence." s)))

(defun %print-usage (label usage)
  (if usage
      (note "  ~a llm-usage: input ~s  cache-read ~s  cache-write ~s  output ~s  total ~s  (prompt = ~s)"
            label (llm:llm-usage-input-tokens usage) (llm:llm-usage-cache-read-tokens usage)
            (llm:llm-usage-cache-write-tokens usage) (llm:llm-usage-output-tokens usage)
            (llm:llm-usage-total-tokens usage) (llm:llm-usage-prompt-tokens usage))
      (note "  ~a llm-usage: NIL" label)))

(defun %json-field (json &rest path)
  (let ((v (ignore-errors (json:decode json))))
    (dolist (k path v)
      (setf v (and (hash-table-p v) (gethash k v))))))

(defun %pick-model (models)
  (let ((ids (mapcar #'llm:llm-model-info-id models)))
    (flet ((chat-p (id) (notany (lambda (bad) (search bad id)) '("embed" "ocr"))))
      (or (find-if (lambda (id) (and (chat-p id) (search "2b" id))) ids)
          (find-if (lambda (id) (and (chat-p id) (search "4b" id))) ids)
          (find-if #'chat-p ids)))))

(defun demo-llm-cache ()
  (banner "Demo 5: llm-protocol-openai 0.4.0 prompt cache, live LM Studio")
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend)
  (let ((hp:*http-backend* (http-backend-async:make-async-backend))
        (token (%lm-token)))
    (show "LM_API_TOKEN found" (and token (plusp (length token)) t))
    ;; No LM Studio (CI runners): report the skip instead of failing the run.
    (handler-case (http:request :get (format nil "~a/models" *lm-studio-url*) :timeout 5)
      ((or hp:http-connection-error hp:http-timeout-error) (c)
        (note "skipped: no LM Studio reachable at ~a (~a)" *lm-studio-url* (type-of c))
        (return-from demo-llm-cache nil)))
    (let* ((log (list nil))
           (backend (oa:make-openai-compat-backend :base-url *lm-studio-url* :api-key token
                                                   :request-fn (%tapping-request-fn log))))
      (stage "5a. list models")
      (let* ((models (llm:list-models backend))
             (model (%pick-model models)))
        (show "models" (mapcar #'llm:llm-model-info-id models))
        (show "picked chat model" model)
        (unless model (error "no chat model in LM Studio"))

        (stage "5b. two requests, same long system prompt, prompt_cache_key \"harness-demo-run\" (reasoning_effort none)")
        (let ((system (%long-system-prompt)))
          (show "system prompt chars" (length system))
          (setf (car log) nil)
          (loop for (label question) in '(("request 1" "Which test framework do the rules ask for?")
                                          ("request 2" "Which ASDF operation must run before a report?"))
                do (let* ((response (llm:generate backend
                                                  (list (llm:system-turn system)
                                                        (llm:user-turn question))
                                                  :model model
                                                  :settings '(:temperature 0 :max-tokens 512
                                                              :extra (:prompt-cache-key
                                                                      "harness-demo-run"
                                                                      :reasoning-effort "none"))))
                          (sent (car (first (car log))))
                          (raw (cdr (first (car log)))))
                     (note "~a: ~a" label question)
                     (show "  prompt_cache_key sent" (%json-field sent "prompt_cache_key"))
                     (show "  reasoning_effort sent" (%json-field sent "reasoning_effort"))
                     (show "  reply" (llm:llm-response-text response))
                     (show "  thinking chars" (length (or (llm:llm-response-thinking response) "")))
                     (show "  finish reason" (llm:llm-response-finish-reason response))
                     (%print-usage label (llm:llm-response-usage response))
                     (show "  raw usage JSON from LM Studio"
                           (let ((u (%json-field raw "usage")))
                             (if u (json:encode u) "absent"))))))
        (let* ((usages (mapcar (lambda (e) (%json-field (cdr e) "usage")) (reverse (car log))))
               (cached (mapcar (lambda (u) (and (hash-table-p u)
                                                (let ((d (gethash "prompt_tokens_details" u)))
                                                  (and (hash-table-p d)
                                                       (gethash "cached_tokens" d)))))
                               usages)))
          (stage "5c. verdict")
          (if (some (lambda (n) (and (integerp n) (plusp n))) cached)
              (note "LM Studio reported cached prompt tokens: ~s" cached)
              (note "LM Studio did NOT report cached prompt tokens (prompt_tokens_details.cached_tokens per request: ~s); llm-usage-cache-read-tokens is therefore NIL/0."
                    cached)))))))
