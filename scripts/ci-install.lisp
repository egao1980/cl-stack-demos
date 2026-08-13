;;;; Phase 1: install SUT dependency closure via cl-repository-client.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(setf asdf:*compile-file-failure-behaviour* :warn)

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
                  ("usocket" :ql)
                  ("dbd-sqlite3" :ql)
                  ("cl-dbi" :ql)
                  ("jose" :ql)
                  ("ironclad" :ql)
                  ("websocket-driver" :ql)
                  ("clack" :ql)
                  ("hunchentoot" :ql))))))

(format t "~&; ci: install phase done~%")
(uiop:quit 0)
