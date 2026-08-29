;;;; Bootstrap ASDF for workspace sibling checkouts + this project.
;;;;   ros -l scripts/bootstrap.lisp …
;;;;
;;;; Prefers OCI cl-repository-client under ~/.local/share/cl-repository-client/
;;;; Workspace siblings take precedence over ~/.local/share/cl-repository/systems/.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(defparameter *demo-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *workspace-root*
  (uiop:pathname-parent-directory-pathname *demo-root*))

(defparameter *sibling-dirs*
  '("cl-stack-demos"
    "cl-stack-pathlib" "cl-stack-config" "cl-stack-http" "cl-stack-jwt" "cl-stack-oauth2"
    "cl-stack-ssl" "json-protocol" "serdes-protocol" "io-protocol" "cli-protocol"
    "log-protocol" "http-protocol" "http-server-protocol" "http-backend-async"
    "http-backend-dexador" "ws-protocol" "ws-backend-websocket-driver"
    "sql-protocol" "sql-query" "sql-query-sqlite3"
    "sql-query-csv" "sql-orm" "event-protocol" "event-backend-libuv" "event-backend-libev"
    "quri" "cl-mime" "cl-idna" "rove" "http-encoding-chipz" "http-encoding-brotli"
    "http-encoding-zstd" "cl-stack-brotli" "cl-stack-zstd"))

(defun %oci-client-dir ()
  (or (let ((e (uiop:getenv "CL_REPOSITORY_CLIENT_DIR")))
        (and e (probe-file e) (uiop:ensure-directory-pathname e)))
      (car (sort
            (directory
             (merge-pathnames
              "cl-oci-*/"
              (merge-pathnames ".local/share/cl-repository-client/"
                               (user-homedir-pathname))))
            #'string> :key #'namestring))))

(defun %repo-systems-dir ()
  (let ((p (merge-pathnames ".local/share/cl-repository/systems/"
                            (user-homedir-pathname))))
    (when (probe-file p)
      (uiop:ensure-directory-pathname p))))

(defun %sibling-trees ()
  (loop for name in *sibling-dirs*
        for path = (merge-pathnames (format nil "~A/" name) *workspace-root*)
        when (probe-file path)
        collect `(:tree ,(uiop:truenamize (uiop:ensure-directory-pathname path)))))

(let* ((oci (%oci-client-dir))
       (repo (%repo-systems-dir))
       (trees (append
               (%sibling-trees)
               (when oci (list `(:tree ,(uiop:truenamize oci))))
               (when repo (list `(:tree ,(uiop:truenamize repo)))))))
  (asdf:initialize-source-registry
   `(:source-registry ,@trees :inherit-configuration))
  (when oci
    (format t "~&; bootstrap: cl-repository-client from ~A~%" oci))
  (when repo
    (format t "~&; bootstrap: cl-repo systems from ~A~%" repo)))

(asdf:load-asd (merge-pathnames "cl-stack-demos.asd" *demo-root*))

(format t "~&; bootstrap: workspace=~A~%;          demo=~A~%"
        *workspace-root* *demo-root*)
