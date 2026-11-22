(in-package #:harness-demos-css)

(defparameter *demos*
  '(demo-serialise demo-scope demo-used-rules demo-conditions demo-theme demo-page))

(defun run-demos ()
  "Run every demo; an error ends that demo only (printed), the rest still run."
  (let ((*print-pretty* t)
        (*print-right-margin* 110)
        (failures (quote ())))
    (dolist (d *demos*)
      (handler-case (funcall d)
        (error (c)
          (push d failures)
          (format t "~&~%!! ~a FAILED: ~a~%   (~s)~%" d c (type-of c)))))
    (format t "~&~%~72,,,'=a~%Demos run: ~d, failed: ~:[none~;~:*~{~a~^, ~}~]~%"
            "" (length *demos*) (reverse failures))
    (finish-output)
    (null failures)))
