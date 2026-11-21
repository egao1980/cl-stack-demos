(in-package #:harness-demos-supervisor)

(defparameter *demos*
  '(demo-scopes demo-queries demo-restart demo-caps demo-stop demo-sweep demo-logs))

(defun run-demos ()
  "Run every demo; an error ends that demo only (printed), the rest still run."
  (let ((*print-pretty* t)
        (*print-right-margin* 110)
        (failures (quote ()))
        (*outer-restarts* (compute-restarts))
        (start (get-internal-real-time)))
    (format t "~&supervisor-protocol ~a on ~a ~a (~a)~%"
            (asdf:component-version (asdf:find-system "supervisor-protocol"))
            (lisp-implementation-type) (lisp-implementation-version) (software-type))
    (dolist (d *demos*)
      (handler-case (funcall d)
        (error (c)
          (push d failures)
          (format t "~&~%!! ~a FAILED: ~a~%   (~s)~%" d c (type-of c)))))
    (shutdown-shared)
    (format t "~&~%~72,,,'=a~%Demos run: ~d, failed: ~:[none~;~:*~{~a~^, ~}~]; ~,1f s~%"
            "" (length *demos*) (reverse failures) (elapsed-since start))
    (finish-output)
    (null failures)))
