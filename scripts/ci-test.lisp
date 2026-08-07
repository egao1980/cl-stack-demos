;;;; Phase 2: load + run demos / tests.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(asdf:load-system "cl-stack-demos")
(in-package #:cl-stack-demos)

(multiple-value-bind (ok fail)
    (run-all)
  (declare (ignore ok))
  (uiop:quit (if (zerop fail) 0 1)))
