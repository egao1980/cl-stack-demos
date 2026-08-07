;;;; Bootstrap ASDF for workspace sibling checkouts + this project.
;;;;   ros -l scripts/bootstrap.lisp …

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(defparameter *workspace-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *demo-root*
  (uiop:pathname-directory-pathname *load-truename*))

;; scripts/ → demo root
(setf *demo-root*
      (uiop:pathname-parent-directory-pathname
       (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *sibling-systems*
  '("cl-stack-pathlib" "cl-stack-config" "cl-stack-http" "cl-stack-jwt" "cl-stack-oauth2"
    "cl-stack-ssl" "json-protocol" "serdes-protocol" "io-protocol" "cli-protocol"
    "log-protocol" "http-protocol" "http-server-protocol" "http-backend-async"
    "http-backend-dexador" "ws-protocol" "sql-protocol" "sql-query" "sql-query-sqlite3"
    "sql-query-csv" "sql-orm" "event-protocol" "event-backend-libuv" "event-backend-libev"
    "quri" "cl-mime" "cl-idna" "rove"))

(defun %register-tree (path)
  (when (probe-file path)
    (asdf:initialize-source-registry
     `(:source-registry
       (:tree ,(uiop:ensure-directory-pathname path))
       :inherit-configuration))))

(%register-tree *workspace-root*)
(%register-tree *demo-root*)

(asdf:load-asd (merge-pathnames "cl-stack-demos.asd" *demo-root*))

(format t "~&; bootstrap: workspace=~A demo=~A~%" *workspace-root* *demo-root*)
