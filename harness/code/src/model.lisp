(in-package #:harness-demos-code)

;;; Which model runs: a model on an OpenAI-compatible server (LM Studio by
;;; default; HARNESS_DEMOS_BASE_URL for another) when it answers, else a
;;; scripted mock provider. The key comes from HARNESS_DEMOS_API_KEY, else
;;; LM_API_TOKEN, else the workspace .env; it is never printed.

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
  "HARNESS_DEMOS_API_KEY when set, else LM_API_TOKEN from the environment or
the workspace .env. \"\" (empty or absent) sends no Authorization header."
  (string-trim " " (or (uiop:getenv "HARNESS_DEMOS_API_KEY")
                       (uiop:getenv "LM_API_TOKEN")
                       (%env-file-value (or (uiop:getenv "HARNESS_DEMOS_ENV_FILE")
                                            (merge-pathnames "Projects/cl-workspace/.env" (user-homedir-pathname)))
                                        "LM_API_TOKEN")
                       "")))

(defun %body-text (body)
  (etypecase body
    (string body)
    ((vector (unsigned-byte 8)) (babel:octets-to-string body :encoding :utf-8))
    (null "")))

(defun %capabilities-url ()
  "LM Studio's /api/v0/models next to a base URL ending in /v1, else NIL."
  (let ((u *lm-studio-url*))
    (when (and (>= (length u) 3) (string= "/v1" u :start2 (- (length u) 3)))
      (concatenate 'string (subseq u 0 (- (length u) 3)) "/api/v0/models"))))

(defun %capabilities (token)
  "id → list of LM Studio capabilities (\"tool_use\" …) from /api/v0/models,
as an alist; NIL when that LM Studio-only endpoint is missing or fails (another
server): tool use is then unknown."
  (ignore-errors
   (let* ((url (or (%capabilities-url) (return-from %capabilities nil)))
          (res (http:request :get url
                             :headers (and (plusp (length token))
                                           `(("authorization" . ,(format nil "Bearer ~a" token))))
                             :timeout 10))
          (data (and (= 200 (hp:response-status res))
                     (jget (json:decode (%body-text (hp:response-body res))) "data"))))
     (unless (vectorp data) (return-from %capabilities nil))
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

(defun %model-override ()
  "The exact model id in HARNESS_DEMOS_MODEL, or NIL when unset or empty."
  (let ((id (uiop:getenv "HARNESS_DEMOS_MODEL")))
    (and id (plusp (length (string-trim " " id))) (string-trim " " id))))

(defun detect-model ()
  "Set *MODEL*: HARNESS_DEMOS_MODEL when set and LM Studio answers, else
%PICK-MODEL's heuristic. → *MODEL*."
  (banner "Model")
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend
        *lm-studio-url* (%base-url))
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
         (override (%model-override))
         (id (and ids (or override (%pick-model ids capabilities)))))
    (show "API key found (HARNESS_DEMOS_API_KEY or LM_API_TOKEN)" (and (plusp (length token)) t))
    (when ids
      (show "GET /v1/models" ids)
      (if capabilities
          (show "capabilities (/api/v0/models)" capabilities)
          (note "capabilities: /api/v0/models not available (not LM Studio?): tool use unknown")))
    (setf *model* (if id (list :lm-studio id) (list :mock)))
    (when (and override ids (not (member override ids :test #'string=)))
      (note "HARNESS_DEMOS_MODEL ~a is not in GET /v1/models; using it anyway (LM Studio may load it on demand)" override))
    (if id
        (progn
          (note "MODEL THAT RAN: ~a at ~a (~a; real model, its output varies run to run)"
                id (%url-host *lm-studio-url*) (if override "from HARNESS_DEMOS_MODEL" "picked by the heuristic: coder, then tool_use"))
          (note "~a; every real-model run gets it as --model-param flags (cl-stack-code 0.9.0 modelParams)"
                (sampling-description)))
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
