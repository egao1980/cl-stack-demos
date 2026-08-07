;;;; ros -l scripts/bootstrap.lisp -l scripts/run-app.lisp <name>
(asdf:load-system "cl-stack-demos")
(in-package #:cl-stack-demos)

(let* ((argv (uiop:command-line-arguments))
       (name (find-if (lambda (a)
                        (and (not (uiop:string-prefix-p "-" a))
                             (not (search ".lisp" a))
                             (not (search "/" a))))
                      (reverse argv))))
  (unless name
    (format *error-output* "usage: run-app.lisp <demo-name>~%")
    (uiop:quit 2))
  (let ((status (run-app name)))
    (uiop:quit (if (eq status :ok) 0 1))))
