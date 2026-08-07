;;;; Inspired by wireservice/csvkit — filter/aggregate CSV report.
(in-package #:cl-stack-demos)

(defun run-csvkit-report ()
  (asdf:load-system "sql-query-csv")
  (let* ((csv (merge-pathnames "csv/users.csv" (fixtures-dir)))
         (d (sql-query-csv:csv-catalog :users csv))
         (rows (sql-query-csv:query-csv
                (sql-query:select
                 (sql-query:columns :city
                                    (sql-query:label (sql-query:count :*) :n)
                                    (sql-query:label (sql-query:sql-func :sum :score) :total))
                 (sql-query:from :users)
                 (sql-query:where (:= :active 1))
                 (sql-query:group-by :city)
                 (sql-query:order-by :city))
                :dialect d)))
    (format t "~&; csv report:~%")
    (dolist (r rows)
      (format t "  ~{~s ~s~^  ~}~%" r))
    (assert (>= (length rows) 2))
    t))

(register-app "csvkit-report"
              :title "csvkit-shaped CSV group-by report"
              :upstream "https://github.com/wireservice/csvkit"
              :fn #'run-csvkit-report)
