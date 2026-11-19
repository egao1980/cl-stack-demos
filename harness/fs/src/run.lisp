(in-package #:harness-demos-fs)

(defparameter *demos*
  '(demo-pathlib demo-process demo-git demo-jj))

(defun run-demos ()
  "Run every demo; an error ends that demo only (printed), the rest still run."
  (let ((*print-pretty* t)
        (*print-right-margin* 110)
        (failures (quote ()))
        (*outer-restarts* (compute-restarts)))
    (dolist (d *demos*)
      (handler-case (funcall d)
        (error (c)
          (push d failures)
          (format t "~&~%!! ~a FAILED: ~a~%   (~s)~%" d c (type-of c)))))
    (format t "~&~%~72,,,'=a~%Demos run: ~d, failed: ~:[none~;~:*~{~a~^, ~}~]~%"
            "" (length *demos*) (reverse failures))
    (finish-output)
    (null failures)))
