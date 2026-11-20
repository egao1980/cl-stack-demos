(in-package #:harness-demos-code)

;;; Which model runs: a loaded LM Studio model when the server answers, else a
;;; scripted mock provider. The LM Studio token comes from LM_API_TOKEN, else
;;; from the workspace .env; it is never printed.

(defparameter *lm-studio-url* "http://127.0.0.1:1234/v1")

(defvar *model* nil
  "(:lm-studio id) or (:mock) once DETECT-MODEL ran.")

(defvar *mock-script* '()
  "Steps the mock provider answers with, one per generate: a string (final
text), a list of (tool-name json-arguments) calls, or a function of the
request turns returning either.")

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

(defun %capabilities (token)
  "id → list of LM Studio capabilities (\"tool_use\" …) from /api/v0/models,
as an alist; NIL when that endpoint does not answer."
  (ignore-errors
   (let* ((res (http:request :get "http://127.0.0.1:1234/api/v0/models"
                           :headers (and token `(("authorization" . ,(format nil "Bearer ~a" token))))
                           :timeout 10))
          (data (jget (json:decode (%body-text (hp:response-body res))) "data")))
     (loop for m across data
           collect (cons (jget m "id")
                         (coerce (or (let ((c (jget m "capabilities"))) (and (vectorp c) c))
                                     #())
                                 'list))))))

(defun %pick-model (ids capabilities)
  "Prefer a coder model, then one LM Studio says can use tools, then any chat
model (no embedding or OCR models)."
  (flet ((chat-p (id) (notany (lambda (bad) (search bad id :test #'char-equal))
                              '("embed" "ocr")))
         (tools-p (id) (member "tool_use" (cdr (assoc id capabilities :test #'string=))
                               :test #'string=)))
    (let ((chat (remove-if-not #'chat-p ids)))
      (or (find-if (lambda (id) (search "coder" id :test #'char-equal)) chat)
          (find-if #'tools-p chat)
          (first chat)))))

(defun detect-model ()
  "Set *MODEL*. → *MODEL*."
  (banner "Model")
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend)
  (let* ((token (%lm-token))
         (hp:*http-backend* (http-backend-async:make-async-backend))
         (ids (handler-case
                  (mapcar #'llm:llm-model-info-id
                          (llm:list-models (oa:make-openai-compat-backend
                                            :base-url *lm-studio-url* :api-key token)))
                (error (c)
                  (note "LM Studio at ~a not reachable: ~a" *lm-studio-url* (clip (princ-to-string c) 160))
                  nil)))
         (capabilities (and ids (%capabilities token)))
         (id (and ids (%pick-model ids capabilities))))
    (show "LM_API_TOKEN found" (and token (plusp (length token)) t))
    (when ids
      (show "GET /v1/models" ids)
      (show "capabilities (/api/v0/models)" capabilities))
    (setf *model* (if id (list :lm-studio id) (list :mock)))
    (if id
        (note "MODEL THAT RAN: LM Studio ~a (real model; its output varies run to run)" id)
        (note "MODEL THAT RAN: scripted mock provider \"mock\" (no LM Studio model available)"))
    *model*))

(defun real-model-p () (eq (first *model*) :lm-studio))

(defun model-designator ()
  (if (real-model-p)
      (format nil "openai:~a" (second *model*))
      "mock"))

(defun model-getenv ()
  "The environment RUN-CLI sees: only what the provider needs (the token is
read here and never printed)."
  (let ((token (%lm-token)))
    (lambda (name)
      (cond ((not (real-model-p)) nil)
            ((string= name "OPENAI_BASE_URL") *lm-studio-url*)
            ((string= name "LM_API_TOKEN") token)
            (t nil)))))

(defun %mock-response (step &optional (n 1))
  "N numbers the response, so call ids are unique in the conversation
(call-N-K): cl-stack-code reports one tool_result per id."
  (if (stringp step)
      (llm:make-llm-response :parts (list (llm:make-llm-text-part :text step))
                             :finish-reason :stop :model "mock")
      (llm:make-llm-response
       :parts (loop for (name args) in step
                    for k from 1
                    collect (llm:make-llm-tool-call-part
                             :id (format nil "call-~d-~d" n k) :name name :arguments args))
       :finish-reason :tool-use :model "mock")))

(defun %mock-backend (script)
  (let ((steps script) (n 0))
    (llm:make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (let ((step (if steps (pop steps) "(the mock script is out of steps)")))
                  (%mock-response (if (functionp step) (funcall step turns) step) (incf n)))))))

(defun model-providers ()
  "The mock provider when no real model runs (a fresh backend per run on
*MOCK-SCRIPT*); the built-in openai provider otherwise."
  (unless (real-model-p)
    (list (cons "mock" (lambda (model env)
                         (declare (ignore model env))
                         (%mock-backend *mock-script*))))))

(defun last-tool-result-text (turns)
  "The content of the last tool result in TURNS (for mock answers)."
  (let ((part (car (last (loop for turn in (llm:coerce-turns turns)
                               append (remove-if-not #'llm:llm-tool-result-part-p
                                                     (llm:llm-turn-parts turn)))))))
    (if part
        (let ((c (llm:llm-tool-result-part-content part)))
          (if (stringp c) c (princ-to-string c)))
        "")))

(defun json-args (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    (json:encode h)))

(defun call-with-mock-fallback (what thunk)
  "Call THUNK (→ true when the run showed WHAT). When the real model ran and
did not show it, say so and call THUNK again on the scripted mock."
  (unless (or (funcall thunk) (not (real-model-p)))
    (stage (format nil "the real model did not show ~a this run; the same argv on the scripted mock" what))
    (let ((*model* '(:mock)))
      (funcall thunk))))
