(in-package #:harness-demos-supervisor)

(defparameter *demos*
  '(demo-scopes demo-queries demo-restart demo-caps demo-stop demo-sweep demo-logs
    demo-search demo-unit-tools demo-agent-fix-mock demo-agent-fix-live))

(defun run-demos (&optional (demos *demos*))
  "Run every demo (or DEMOS); an error ends that demo only (printed), the rest still run."
  (let ((*print-pretty* t)
        (*print-right-margin* 110)
        (failures (quote ()))
        (*outer-restarts* (compute-restarts))
        (start (get-internal-real-time)))
    (format t "~&supervisor-protocol ~a, agent-tools ~a, llm-protocol-openai ~a on ~a ~a (~a)~%"
            (asdf:component-version (asdf:find-system "supervisor-protocol"))
            (asdf:component-version (asdf:find-system "agent-tools"))
            (asdf:component-version (asdf:find-system "llm-protocol-openai"))
            (lisp-implementation-type) (lisp-implementation-version) (software-type))
    (dolist (d demos)
      (let ((t0 (get-internal-real-time)))
        (handler-case (funcall d)
          (error (c)
            (push d failures)
            (format t "~&~%!! ~a FAILED: ~a~%   (~s)~%" d c (type-of c))))
        (format t "~&~%   [~(~a~): ~,1f s]~%" d (elapsed-since t0))))
    (shutdown-shared)
    (format t "~&~%~72,,,'=a~%Demos run: ~d, failed: ~:[none~;~:*~{~a~^, ~}~]; ~,1f s~%"
            "" (length demos) (reverse failures) (elapsed-since start))
    (finish-output)
    (null failures)))
