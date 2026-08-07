;;;; Load demos and run all (or names from argv after --).
(asdf:load-system "cl-stack-demos")
(in-package #:cl-stack-demos)

(let* ((argv (uiop:command-line-arguments))
       (names (member "--" argv :test #'string=))
       (names (and names (cdr names))))
  (multiple-value-bind (ok fail)
      (run-all names)
    (declare (ignore ok))
    (uiop:quit (if (zerop fail) 0 1))))
