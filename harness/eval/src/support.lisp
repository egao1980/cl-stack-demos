(in-package #:harness-demos-eval)

;;; Printing, path scrubbing, the seeded local environment, and the live
;;; model's endpoint (HARNESS_DEMOS_BASE_URL / _API_KEY / _MODEL / _MODE).

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title "")
  (finish-output))

(defun note (control &rest args)
  (format t "~&~?~%" control args)
  (finish-output))

(defmacro show (form)
  "Print FORM's source, then evaluate it and print the value."
  `(progn (let ((*package* (find-package :harness-demos-eval)) (*print-right-margin* 100))
            (format t "~&> ~(~s~)~%" ',form))
          (let ((v ,form))
            (format t "~&  => ~a~%" (scrub (let ((*print-right-margin* 100)) (prin1-to-string v))))
            (finish-output)
            v)))

;;; ------------------------------------------------------------------
;;; Scrubbing: the demo directory, the run root, temp and home paths.

(defvar *scrub-paths* '() "(prefix . replacement) pairs, longest first.")

(defun %replace-all (string old new)
  (with-output-to-string (o)
    (loop with start = 0
          for p = (search old string :start2 start)
          do (write-string string o :start start :end (or p (length string)))
             (if p (progn (write-string new o) (setf start (+ p (length old)))) (loop-finish)))))

(defun add-scrub (dir replacement)
  "Print DIR's native namestring (and its truename) as REPLACEMENT."
  (let ((names (remove-duplicates
                (loop for d in (list dir (ignore-errors (truename dir)))
                      when d
                        append (let ((n (uiop:native-namestring d)))
                                 (list n (string-right-trim "/" n))))
                :test #'equal)))
    (dolist (n names)
      (push (cons n (if (char= #\/ (char n (1- (length n))))
                        replacement
                        (string-right-trim "/" replacement)))
            *scrub-paths*))
    (setf *scrub-paths* (sort (remove-duplicates *scrub-paths* :test #'equal :key #'car)
                              #'> :key (lambda (p) (length (car p)))))))

(defun scrub (text)
  (reduce (lambda (s pair) (%replace-all s (car pair) (cdr pair)))
          *scrub-paths* :initial-value text))

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

(defun write-text (pathname text)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (write-string text s))
  pathname)

(defun read-text (pathname)
  (uiop:read-file-string pathname :external-format :utf-8))

;;; ------------------------------------------------------------------
;;; The seeded local environment.
;;;
;;; A Terminal-Bench task's Dockerfile COPYs its starting files into the work
;;; dir (here: `COPY app/ /app/`). AGENT-EVAL's LOCAL-ENVIRONMENT builds no
;;; image, so its work dir starts empty. This subclass adds the COPY: after
;;; START-ENVIRONMENT it uploads the task's environment/app/ (when there is
;;; one) to the work dir, through the protocol's own UPLOAD-PATH.

(defclass seeded-local-environment (ae:local-environment) ()
  (:documentation "A LOCAL-ENVIRONMENT whose work dir starts with the task's
environment/app/ files (the Dockerfile's COPY app/ /app/)."))

(defmethod ae:start-environment :after ((env seeded-local-environment))
  (let* ((task (ae:environment-task env))
         (app (merge-pathnames "environment/app/"
                               (uiop:ensure-directory-pathname (ae:task-directory task)))))
    (when (uiop:directory-exists-p app)
      (ae:upload-path env app (ae:task-workdir task)))))

(defun seeded-environment (task directory)
  "An ENVIRONMENT-FACTORY for RUN-AGENT-EVAL / RUN-TRIAL."
  (change-class (ae:make-local-environment task :directory directory)
                'seeded-local-environment))

;;; ------------------------------------------------------------------
;;; The live endpoint: any OpenAI-compatible /v1 server. The key is passed to
;;; cl-stack-code as OPENAI_API_KEY in the adapter's :ENV (never in
;;; AGENT-CONFIG, so never in a saved run) and is never printed.

(defparameter *default-base-url* "http://127.0.0.1:1234/v1")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length (string-trim " " v))) (string-trim " " v))))

(defun base-url ()
  (string-right-trim "/" (or (%env "HARNESS_DEMOS_BASE_URL") *default-base-url*)))

(defun url-host (url)
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position #\/ url :start start) (length url))))
    (subseq url start end)))

(defun api-key ()
  "HARNESS_DEMOS_API_KEY, else LM_API_TOKEN, else \"\"."
  (or (%env "HARNESS_DEMOS_API_KEY") (%env "LM_API_TOKEN") ""))

(define-condition live-endpoint-unreachable (error)
  ((url :initarg :url :reader live-endpoint-url)
   (cause :initarg :cause :reader live-endpoint-cause))
  (:report (lambda (c s)
             (format s "the live model endpoint ~a is not reachable: ~a"
                     (url-host (live-endpoint-url c)) (live-endpoint-cause c)))))

(defun probe-endpoint ()
  "GET /v1/models on the endpoint (llm-protocol-openai over http-backend-async)
→ the model ids. Unreachable → LIVE-ENDPOINT-UNREACHABLE: no fallback."
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend)
  (let ((hp:*http-backend* (http-backend-async:make-async-backend))
        (url (base-url)))
    (handler-case
        (mapcar #'llm:llm-model-info-id
                (llm:list-models (oa:make-openai-compat-backend :base-url url :api-key (api-key))))
      (error (c)
        (error 'live-endpoint-unreachable :url url :cause (princ-to-string c))))))

(defun pick-model (ids)
  "HARNESS_DEMOS_MODEL when set, else the first id the server lists."
  (or (%env "HARNESS_DEMOS_MODEL") (first ids)))

;;; The sampling preset (HARNESS_DEMOS_MODE), the same fields as the other
;;; harness sets' src/sampling.lisp, as cl-stack-code --model-param pairs.

(defparameter +sampling-presets+
  '((:thinking . "{\"temperature\":1.0,\"top_p\":0.95,\"top_k\":20,\"min_p\":0.0,\"presence_penalty\":0.0,\"repeat_penalty\":1.0,\"chat_template_kwargs\":{\"enable_thinking\":true,\"preserve_thinking\":true}}")
    (:instruct . "{\"temperature\":0.7,\"top_p\":0.8,\"top_k\":20,\"min_p\":0.0,\"presence_penalty\":1.5,\"repeat_penalty\":1.0,\"chat_template_kwargs\":{\"enable_thinking\":false}}")))

(defun sampling-mode ()
  (let ((m (string-downcase (or (%env "HARNESS_DEMOS_MODE") ""))))
    (cond ((member m '("" "thinking") :test #'string=) :thinking)
          ((string= m "instruct") :instruct)
          (t (error "HARNESS_DEMOS_MODE must be thinking or instruct, not ~s" m)))))

(defun sampling-json () (cdr (assoc (sampling-mode) +sampling-presets+)))

(defun sampling-model-params ()
  "The preset as the adapter's MODEL-PARAMS alist (KEY . decoded value), key order."
  (let ((h (json:decode (sampling-json))) (out '()))
    (maphash (lambda (k v) (push (cons k v) out)) h)
    (sort out #'string< :key #'car)))

(defun source-registry-text ()
  "This image's source registry, as CL_SOURCE_REGISTRY text, so the
cl-stack-code command (bin/cl-stack-code, a fresh SBCL) finds the packages
cl-repo installed (`cl-repo run` configures the registry of the Lisp it
starts and exports no CL_SOURCE_REGISTRY)."
  (let ((param (symbol-value (find-symbol "*SOURCE-REGISTRY-PARAMETER*" "ASDF/SOURCE-REGISTRY"))))
    (unless (consp param)
      (error "this image has no explicit source registry: run the demo under `cl-repo run`"))
    (with-standard-io-syntax
      (let ((*print-readably* nil) (*print-pretty* nil))
        (prin1-to-string param)))))
