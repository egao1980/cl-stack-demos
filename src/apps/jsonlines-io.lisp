;;;; Inspired by wbolster/jsonlines — one JSON value per line.
(in-package #:cl-stack-demos)

(defun run-jsonlines-io ()
  (asdf:load-system "json-backend-jzon")
  (let* ((rows (list
                (let ((ht (make-hash-table :test 'equal)))
                  (setf (gethash "name" ht) "ada" (gethash "n" ht) 1)
                  ht)
                (let ((ht (make-hash-table :test 'equal)))
                  (setf (gethash "name" ht) "grace" (gethash "n" ht) 2)
                  ht)))
         (raw (with-output-to-string (o)
                (let ((out (stack-serdes:make-output-stream o :format :json)))
                  (dolist (r rows)
                    (stack-serdes:stream-encode-value out r)))))
         (got nil))
    (stack-serdes:map-jsonl (lambda (v) (push v got)) raw :format :json)
    (setf got (nreverse got))
    (format t "~&; jsonl lines=~A first=~A~%"
            (length got) (gethash "name" (first got)))
    (assert (= 2 (length got)))
    (assert (string= (gethash "name" (first got)) "ada"))
    (assert (= 2 (gethash "n" (second got))))
    t))

(register-app "jsonlines-io"
              :title "JSONL writer/reader"
              :upstream "https://github.com/wbolster/jsonlines"
              :fn #'run-jsonlines-io)
