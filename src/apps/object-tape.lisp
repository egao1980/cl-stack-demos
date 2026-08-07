;;;; Inspired by CPython pprint + ast.literal_eval — object dump/load stream.
(in-package #:cl-stack-demos)

(defun run-object-tape ()
  (let ((raw (with-output-to-string (o)
               (let ((out (stack-io:make-object-output-stream o)))
                 (stack-io:write-object out '(:user "ada" :id 1))
                 (stack-io:write-object out "hello")
                 (stack-io:write-object out 42)))))
    (with-input-from-string (in-raw raw)
      (let ((in (stack-io:make-object-input-stream in-raw)))
        (let ((a (stack-io:read-object in))
              (b (stack-io:read-object in))
              (c (stack-io:read-object in))
              (d (stack-io:read-object in)))
          (format t "~&; tape ~S ~S ~S eof=~S~%" a b c d)
          (assert (equal a '(:user "ada" :id 1)))
          (assert (string= b "hello"))
          (assert (= c 42))
          (assert (eq d :eof))
          t)))))

(register-app "object-tape"
              :title "Object input/output stream (pprint/read shape)"
              :upstream "https://github.com/python/cpython (pprint / ast.literal_eval)"
              :fn #'run-object-tape)
