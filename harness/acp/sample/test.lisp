;;;; test.lisp — run with: sbcl --script test.lisp  (exit 0 = all pass)

(load (merge-pathnames "fib.lisp" *load-truename*))

(let ((expected '(0 1 1 2 3 5 8 13 21 34 55))
      (failures 0))
  (loop for n from 0
        for want in expected
        for got = (fib n)
        do (unless (eql got want)
             (incf failures)
             (format t "FAIL (fib ~D) = ~D, expected ~D~%" n got want)))
  (format t "~D of ~D tests passed~%" (- (length expected) failures) (length expected))
  (finish-output)
  (sb-ext:exit :code (if (zerop failures) 0 1)))
