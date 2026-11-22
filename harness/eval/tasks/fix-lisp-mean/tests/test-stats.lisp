;;;; Run with sbcl --script in the work dir: exit 0 when every check passes.

(defvar *failures* 0)

(defmacro check (form expected)
  `(let ((got (handler-case ,form (error (c) (list :error (princ-to-string c))))))
     (if (equal got ,expected)
         (format t "ok:   ~s => ~s~%" ',form got)
         (progn (incf *failures*)
                (format t "FAIL: ~s => ~s, want ~s~%" ',form got ,expected)))))

(handler-case (load "stats.lisp")
  (error (c) (format t "FAIL: loading stats.lisp: ~a~%" c) (sb-ext:exit :code 1)))

(check (mean '(1 2 3 4)) 5/2)
(check (mean '(10)) 10)
(check (mean '(2 4 6)) 4)
(check (spread '(3 9 1)) 8)

(sb-ext:exit :code (if (zerop *failures*) 0 1))
