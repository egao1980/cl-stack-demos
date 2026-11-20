(in-package #:harness-demos-image)

;;; Output helpers and temporary projects (as in harness/code).

(defvar *redactions* '()
  "(from . to) pairs applied to everything printed: the temporary directory
becomes <tmp>/, the home directory ~/.")

(defun redact (text)
  (let ((text (princ-to-string text)))
    (loop for (from . to) in *redactions*
          do (loop for p = (search from text)
                   while p
                   do (setf text (concatenate 'string (subseq text 0 p) to
                                              (subseq text (+ p (length from)))))))
    text))

(defun banner (title)
  (format t "~&~%~a~%== ~a~%~a~%" (make-string 72 :initial-element #\=)
          title (make-string 72 :initial-element #\=))
  (finish-output))

(defun stage (title)
  (format t "~&~%--- ~a~%" title)
  (finish-output))

(defun note (control &rest args)
  (format t "~&~a~%" (redact (apply #'format nil control args)))
  (finish-output))

(defun show (label value)
  "LABEL: VALUE; a multi-line string goes on indented lines."
  (let ((text (if (stringp value) value (prin1-to-string value))))
    (if (find #\Newline text)
        (format t "~&~a:~%~{  | ~a~%~}" label
                (uiop:split-string (redact (string-right-trim '(#\Newline) text))
                                   :separator '(#\Newline)))
        (format t "~&~a: ~a~%" label (redact text))))
  (finish-output)
  value)

(defun clip (text &optional (n 240))
  (let ((text (princ-to-string text)))
    (if (> (length text) n)
        (format nil "~a… [~d chars]" (subseq text 0 n) (length text))
        text)))

(defun %native (pathname) (uiop:native-namestring pathname))

(defun write-file (root relative text)
  (let ((path (merge-pathnames relative root)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text s))
    path))

(defun read-file (root relative)
  (uiop:read-file-string (merge-pathnames relative root) :external-format :utf-8))

(defvar *scratch* nil)

(defun scratch-dir (name)
  (let ((dir (merge-pathnames (format nil "~a/" name) *scratch*)))
    (when (probe-file dir) (uiop:delete-directory-tree dir :validate t))
    (ensure-directories-exist dir)
    (truename dir)))

(defmacro with-scratch (() &body body)
  `(let* ((*scratch* (let ((d (merge-pathnames
                               (format nil "lisp-image-demo-~36r/"
                                       (random (expt 36 6) (make-random-state t)))
                               (uiop:temporary-directory))))
                       (ensure-directories-exist d)
                       (truename d)))
          (*redactions* (list* (cons (%native *scratch*) "<tmp>/")
                               (cons (%native (user-homedir-pathname)) "~/")
                               *redactions*)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree *scratch* :validate t :if-does-not-exist :ignore))))

(defun make-project (name files)
  (let ((root (scratch-dir name)))
    (loop for (relative text) in files do (write-file root relative text))
    root))

(defun json-args (&rest kv)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    (json:encode h)))

(defun %elide (text &key (head 6) (tail 3))
  "TEXT, or its first HEAD and last TAIL lines when it is longer (the demo's
printing only; the model gets the whole result)."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (if (<= (length lines) (+ head tail 2))
        text
        (format nil "~{~a~%~}  … [demo output: ~d of ~d lines not printed]~%~{~a~^~%~}"
                (subseq lines 0 head) (- (length lines) head tail) (length lines)
                (last lines tail)))))

(defun %token-after (prefix text)
  "The word after PREFIX in TEXT (\"job \" → \"j7\"), or NIL."
  (let ((p (and (stringp text) (search prefix text))))
    (when p
      (let* ((start (+ p (length prefix)))
             (end (or (position-if (lambda (c) (member c '(#\Space #\: #\, #\. #\]))) text :start start)
                      (length text))))
        (subseq text start end)))))

(defun tool (tools name &rest kv)
  "CALL-LISP-TOOL with a JSON object; prints the call and its result. An
error is printed as the condition the agent would turn into a tool result."
  (note "~&> ~a ~a" name (apply #'json-args kv))
  (handler-case
      (let ((r (lt:call-lisp-tool tools name (apply #'json-args kv))))
        (show "  =" (%elide (string-right-trim '(#\Newline) r)))
        r)
    (error (c)
      (show (format nil "  signalled ~(~s~)" (type-of c)) (princ-to-string c))
      c)))

(defmacro with-agent-loop (&body body)
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (ev:make-event-loop eb)))
     (ev:with-event-backend (eb)
       (ev:with-event-loop-var (el)
         ,@body))))
