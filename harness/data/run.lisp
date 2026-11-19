(in-package #:harness-demos-data)

(defun run-demos ()
  "Run every demo in order. A demo that signals is reported and skipped."
  (let ((*print-right-margin* 200))
    (dolist (demo '(demo-diff demo-revision demo-orm demo-migrate))
      (handler-case (funcall demo)
        (error (e)
          (format t "~&!! DEMO ~A FAILED: ~A: ~A~%" demo (type-of e) e)))))
  (format t "~&~%== all demos finished~%")
  (finish-output)
  t)
