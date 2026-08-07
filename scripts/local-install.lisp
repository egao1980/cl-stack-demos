;;;; Local dep install via cl-repository-client (SUT asd is local/unpublished).
;;;;   ros -l scripts/bootstrap.lisp -l scripts/local-install.lisp

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(defun call-with-ci-muffles (fn)
  #+sbcl
  (handler-bind ((sb-ext:defconstant-uneql
                  (lambda (c)
                    (declare (ignore c))
                    (let ((r (find-restart 'continue)))
                      (when r (invoke-restart r))))))
    (funcall fn))
  #-sbcl
  (funcall fn))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))
(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(let ((with '("event-backend-libuv"
              "cl-stack-ssl"
              "http-server-backend-hunchentoot"
              "cli-backend-clingon"
              "log-backend-log4cl"
              "json-backend-jzon"
              "sexp-protocol"
              "sql-backend-sqlite3"
              "ws-backend-websocket-driver"
              "http-backend-dexador"
              "http-backend-async")))
  (call-with-ci-muffles
   (lambda ()
     (cl-repo:ensure-system-dependencies "cl-stack-demos"
       :also-tests t
       :with with
       :sources '(("babel" :ql)
                  ("trivial-features" :ql)
                  ("cl-unicode" :ql)
                  ("usocket" :ql)
                  ("dbd-sqlite3" :ql)
                  ("cl-dbi" :ql)
                  ("jose" :ql)
                  ("ironclad" :ql)
                  ("websocket-driver" :ql)
                  ("clack" :ql)
                  ("hunchentoot" :ql)
                  ("tomlet" :ql)
                  ("clingon" :ql)
                  ("log4cl" :ql)
                  ("com.inuoe.jzon" :ql)
                  ("adopt" :ql)
                  ("alexandria" :ql)
                  ("bordeaux-threads" :ql)
                  ("blackbird" :ql)
                  ("quri" :ql)
                  ("dexador" :ql))))))

(format t "~&; local-install done~%")
(uiop:quit 0)
