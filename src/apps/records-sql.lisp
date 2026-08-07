;;;; Inspired by kennethreitz/records — raw SQL for humans over SQLite.
(in-package #:cl-stack-demos)

(defun run-records-sql ()
  (asdf:load-system "sql-backend-sqlite3")
  (stack-sql:with-connection (c :driver :sqlite3 :database-name ":memory:")
    (stack-sql:execute c "CREATE TABLE active_users (
                            username TEXT, active INTEGER, name TEXT, user_email TEXT)")
    (stack-sql:execute c "INSERT INTO active_users VALUES (?, ?, ?, ?)"
                       '("model-t" 1 "Henry Ford" "model-t@gmail.com"))
    (stack-sql:execute c "INSERT INTO active_users VALUES (?, ?, ?, ?)"
                       '("ada" 1 "Ada Lovelace" "ada@x"))
    (let ((rows (stack-sql:fetch-all
                 (stack-sql:execute c "SELECT * FROM active_users WHERE active = ?" '(1)))))
      (format t "~&; rows=~A first.name=~A~%"
              (length rows) (getf (first rows) :name))
      (assert (= 2 (length rows)))
      (assert (string= (getf (first rows) :name) "Henry Ford"))
      t)))

(register-app "records-sql"
              :title "records-shaped raw SQL query"
              :upstream "https://github.com/kennethreitz/records"
              :fn #'run-records-sql)
