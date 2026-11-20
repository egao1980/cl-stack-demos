(in-package #:harness-demos-image)

;;; Demo 1: LISP-MCP:FORM-RESOURCES, a pure reader walk (Eclector CST, no
;;; evaluation, no interning) from code to permission resources.

(defparameter *scan-table*
  '(("pure"
     "(+ 1 2)"
     "(let ((xs (list 3 1 2))) (sort xs #'<))"
     "(with-output-to-string (s) (format s \"~a\" 42))"
     "(loop for x in '(1 2 3) collect (* x x))"
     "(length *features*)")
    ("defines"
     "(defun greet (name) (format nil \"Hello, ~a!\" name))"
     "(defun (setf price) (v item) (setf (getf item :price) v))"
     "(defclass point () ((x :initarg :x)))")
    ("mutates"
     "(setf *print-base* 16)"
     "(push :demo *features*)"
     "(setf (gethash :k *cache*) 1)"
     "(in-package :shop)"
     "(nreverse *queue*)")
    ("package"
     "(defpackage :shop (:use :cl))"
     "(export 'cart-total :shop)")
    ("io"
     "(print 1)"
     "(with-open-file (s \"/etc/hosts\") (read-line s))"
     "(uiop:run-program \"ls\")"
     "(delete-file \"notes.txt\")")
    ("read-eval"
     "#.(+ 1 2)"
     "(read-from-string \"(a b)\")")
    ("calls"
     "(shop:cart-total items)"
     "(asdf:load-system \"shop\")"
     "(mapcar #'shop:line-total items)"
     "(|MixedCase| 1)")
    ("lisp:*"
     "(funcall (pick-function) 1)"
     "(eval '(+ 1 2))"
     "(setf (car (get-list)) 1)"
     "(+ 1"))
  "Rows: (kind code …).")

(defun demo-scanner ()
  (banner "Demo 1: form-resources, a table of forms (package \"shop\" for unqualified names)")
  (note "~&  ~10a ~52a ~a" "kind" "code" "resources → observe-safe-p")
  (let ((safe 0) (total 0))
    (loop for (kind . forms) in *scan-table*
          do (dolist (code forms)
               (let* ((r (lisp-mcp:form-resources code :package "shop"))
                      (ok (lt:observe-safe-resources-p r)))
                 (incf total)
                 (when ok (incf safe))
                 (note "  ~10a ~52a ~s → ~:[refused~;safe~]" kind code r ok))))
    (stage "one more: a definition's body is walked (the effect is reported at definition time)")
    (show "(defun reset () (clrhash *cache*) (print :reset))"
          (lisp-mcp:form-resources "(defun reset () (clrhash *cache*) (print :reset))" :package "shop"))
    (show "#+(or) (delete-file \"x\") (+ 1 2)"
          (lisp-mcp:form-resources "#+(or) (delete-file \"x\") (+ 1 2)"))
    (show "blank / comment only" (list (lisp-mcp:form-resources "") (lisp-mcp:form-resources "; nothing")))
    (show "observe-safe with :observe-allow (\"lisp:calls shop:*\")"
          (lt:observe-safe-resources-p (lisp-mcp:form-resources "(shop:cart-total items)" :package "shop")
                                       '("lisp:calls shop:*")))
    (note "  ~d of ~d forms observe-safe" safe total)
    t))
