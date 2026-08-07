;;;; Pull OCI deps for local sibling development, then quit.
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(asdf:load-system "cl-repository-client")
(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(dolist (sys '(("tomlet" "0.1.0")
               ("cl-stack-config" "0.1.0")
               ("json-backend-jzon" "0.2.0")
               ("sexp-protocol" "0.2.0")
               ("cli-backend-clingon" "0.1.0")
               ("log-backend-log4cl" "0.1.1")
               ("http-server-backend-hunchentoot" "0.1.0")
               ("sql-backend-sqlite3" "0.1.0")
               ("ws-backend-websocket-driver" "0.2.2")
               ("cl-stack-jwt" "0.1.0")
               ("cl-stack-oauth2" "0.1.0")
               ("cl-stack-http" "0.1.8")
               ("sql-orm" "0.1.0")
               ("sql-query-csv" nil)
               ("event-backend-libuv" "0.1.1")
               ("jose" "0.1.0")))
  (destructuring-bind (name &optional ver) sys
    (format t "~&; load-system ~A ~A~%" name ver)
    (finish-output)
    (if ver
        (cl-repo:load-system name :version ver)
        (ignore-errors (cl-repo:load-system name)))))

(format t "~&; oci-load done~%")
(uiop:quit 0)
