(in-package #:harness-demos-isolation)

;;; Output helpers: one banner per demo, one line per step, values printed as is.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun stage (fmt &rest args)
  (format t "~&~%-- ~?~%" fmt args))

(defun show (label value)
  (format t "~&   ~a: ~s~%" label value))

(defun note (fmt &rest args)
  (format t "~&   ~?~%" fmt args))

(defun short (id &optional (n 12))
  "First N characters of an id string (for readable output)."
  (if (and (stringp id) (> (length id) n)) (subseq id 0 n) id))

;;; Temp directories (created and deleted by each demo). TRUENAME: on macOS the
;;; temp directory is under /var, a symlink to /private/var.

(defun make-temp-dir (prefix)
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~a-~36r" prefix (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defmacro with-temp-dir ((var prefix) &body body)
  `(let ((,var (make-temp-dir ,prefix)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun native (pathname) (uiop:native-namestring pathname))

(defun windows-p () (uiop:os-windows-p))

(defun write-file (pathname text)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (write-string text s))
  pathname)

(defun read-file (pathname)
  (uiop:read-file-string pathname :external-format :utf-8))

(defun lines (&rest strings)
  (format nil "~{~a~%~}" strings))

;;; Timing.

(defun ms-since (start)
  (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second))

