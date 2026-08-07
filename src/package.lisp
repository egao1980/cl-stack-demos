(defpackage #:cl-stack-demos
  (:use #:cl)
  (:export #:*results*
           #:register-app
           #:run-app
           #:run-all
           #:list-apps
           #:demo-root
           #:fixtures-dir
           #:*app-table*))

(in-package #:cl-stack-demos)

(defvar *app-table* (make-hash-table :test #'equal)
  "name → (list :title :upstream :fn)")

(defvar *results* nil)

(defun demo-root ()
  (asdf:system-source-directory "cl-stack-demos"))

(defun fixtures-dir ()
  (merge-pathnames "fixtures/" (demo-root)))

(defun register-app (name &key title upstream fn)
  (check-type name string)
  (check-type fn function)
  (setf (gethash name *app-table*)
        (list :title title :upstream upstream :fn fn))
  name)

(defun list-apps ()
  (sort (loop for k being the hash-keys of *app-table* collect k) #'string<))

(defun %ephemeral-port ()
  "Ask the OS for a free TCP port (http-server-protocol test pattern)."
  (let* ((sock (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t))
         (port (usocket:get-local-port sock)))
    (usocket:socket-close sock)
    port))

(defun run-app (name)
  (let ((entry (gethash name *app-table*)))
    (unless entry
      (error "unknown demo ~S; known: ~{~A~^, ~}" name (list-apps)))
    (format t "~&~%======== ~A ========~%" name)
    (format t "; upstream: ~A~%" (getf entry :upstream))
    (format t "; ~A~%" (getf entry :title))
    (finish-output)
    (let ((status
            (handler-case
                (progn
                  (funcall (getf entry :fn))
                  :ok)
              (error (e)
                (format t "~&FAIL ~A: ~A~%" name e)
                (finish-output)
                (list :fail e)))))
      (push (cons name status) *results*)
      status)))

(defun run-all (&optional names)
  (setf *results* nil)
  (dolist (name (or names (list-apps)))
    (run-app name))
  (format t "~&~%======== SUMMARY ========~%")
  (let ((ok 0) (fail 0))
    (dolist (r (reverse *results*))
      (destructuring-bind (name . status) r
        (if (eq status :ok)
            (progn (incf ok) (format t "  OK   ~A~%" name))
            (progn (incf fail)
                   (format t "  FAIL ~A  ~A~%" name (cadr status))))))
    (format t "~&; ok=~A fail=~A~%" ok fail)
    (values ok fail)))
