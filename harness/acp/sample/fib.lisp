;;;; fib.lisp — the sample project of the ACP demo (it has a bug on purpose).

(defun fib (n)
  "The Nth Fibonacci number: (fib 0) = 0, (fib 1) = 1, (fib 10) = 55."
  (let ((a 0) (b 1))
    (loop repeat (1- n)
          do (psetf a b
                    b (+ a b)))
    a))
