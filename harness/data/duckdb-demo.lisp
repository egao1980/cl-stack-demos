(in-package #:harness-demos-data)

;;; Demo 7 — DuckDB's own tutorial, "SQL Introduction"
;;; (https://duckdb.org/docs/2.0/sql/introduction.html), statement by
;;; statement, through the stack: the sql-query DSL compiles each statement
;;; with sql-query-duckdb (picked from the :duckdb connection), sql-protocol
;;; runs it on sql-backend-duckdb, cl-stack-duckdb carries DuckDB v2.0.1.
;;; Every result is checked against the output the tutorial prints.

(defun cell-text (v)
  (cond ((null v) "NULL")
        ((eq v t) "true")
        ((stringp v) v)
        ((floatp v) (format nil "~F" v))
        ((rationalp v) (if (integerp v) (princ-to-string v) (format nil "~,3F" v)))
        (t (prin1-to-string v))))

(defun print-table (rows)
  "ROWS are alists (column . value) as sql-protocol :alist fetch returns them."
  (if (null rows)
      (say "(0 rows)")
      (let* ((names (mapcar #'car (first rows)))
             (cells (mapcar (lambda (r) (mapcar (lambda (c) (cell-text (cdr c))) r)) rows))
             (widths (loop for i from 0 below (length names)
                           collect (reduce #'max (cons (length (nth i names))
                                                       (mapcar (lambda (r) (length (nth i r))) cells))))))
        (flet ((line (items) (say "│ ~{~A~^ │ ~} │"
                                  (loop for x in items for w in widths
                                        collect (format nil "~vA" w x)))))
          (line names)
          (say "├─~{~A~^─┼─~}─┤" (mapcar (lambda (w) (make-string w :initial-element #\─)) widths))
          (mapc #'line cells)))))

(defun duck (c stmt &key (show t))
  "Compile STMT with C's dialect, print the SQL, run it, print and return the rows (alists)."
  (let ((sql (values (q:compile-sql stmt :dialect (q:dialect-for-connection c)))))
    (say "SQL> ~A" sql)
    (let ((rows (sql:fetch-all (q:execute-query c stmt) :format :alist)))
      (when show (print-table rows))
      rows)))

(defun duck-sql (c sql)
  "Run raw SQL (the tutorial's text) through sql-protocol; print and return the rows."
  (say "SQL> ~A" sql)
  (let ((rows (sql:fetch-all (sql:execute c sql) :format :alist)))
    (print-table rows)
    rows))

(defun row-values (rows) (mapcar (lambda (r) (mapcar #'cdr r)) rows))

(defun same-rows (rows expected &key ordered)
  "ROWS (alists) hold EXPECTED (value lists): in order when ORDERED, else as a multiset."
  (let ((got (row-values rows)))
    (if ordered
        (equalp got expected)
        (flet ((key (r) (format nil "~S" r)))
          (equalp (sort (copy-list got) #'string< :key #'key)
                  (sort (copy-list expected) #'string< :key #'key))))))

;;; The tutorial's output tables, as values. FLOAT 0.25 arrives as a
;;; single-float, DATE as its ISO text, NULL as NIL, DECIMAL exactly.
(defparameter +weather-3+
  '(("San Francisco" 46 50 0.25 "1994-11-27")
    ("San Francisco" 43 57 0.0 "1994-11-29")
    ("Hayward" 37 54 nil "1994-11-29")))

(defun weather-csv ()
  (uiop:native-namestring
   (asdf:system-relative-pathname "harness-demos-data" "fixtures/duckdb/weather.csv")))

(defun arrow-type-text (type)
  "TYPE with struct fields shown as (name type)."
  (cond ((arrow:arrow-field-p type)
         (list (arrow:arrow-field-name type) (arrow-type-text (arrow:arrow-field-type type))))
        ((consp type) (mapcar #'arrow-type-text type))
        (t type)))

(defun arrow-col (table name)
  (coerce (arrow:arrow-array-values (arrow:table-column table name)) 'list))

(defun demo-duckdb ()
  (banner "Demo 7 — DuckDB SQL Introduction (duckdb.org tutorial) on sql-query-duckdb + sql-backend-duckdb")
  (say "cl-stack-duckdb ~A, DuckDB library ~A" "2.0.1.0" (ddb:library-version))
  (sql:with-connection (c :driver :duckdb)
    (say "connection driver ~S → dialect ~A" (sql:connection-driver c)
         (class-name (class-of (q:dialect-for-connection c))))

    (demo-step ("Creating a New Table")
      (duck c (q:create-table :weather
                (q:column :city :type '(:varchar 80))
                (q:column :temp_lo :type :integer)
                (q:column :temp_hi :type :integer)
                (q:column :prcp :type :float)
                (q:column :date :type :date))
            :show nil)
      (duck c (q:create-table :cities
                (q:column :name :type '(:varchar 80))
                (q:column :lat :type :decimal)
                (q:column :lon :type :decimal))
            :show nil)
      (check "tables" (equal '(("cities") ("weather"))
                             (row-values (duck c (q:select (q:columns :table_name)
                                                           (q:from (q:sql-raw "information_schema.tables"))
                                                           (q:order-by :table_name))
                                               :show nil)))))

    (demo-step ("Populating a Table with Rows")
      (duck c (q:insert-into :weather (q:sql-values "San Francisco" 46 50 0.25 (q:sql-cast "1994-11-27" :date))) :show nil)
      (duck c (q:insert-into :cities (q:sql-values "San Francisco" -194.0 53.0)) :show nil)
      (duck c (q:insert-into :weather (q:columns :city :temp_lo :temp_hi :prcp :date)
                             (q:sql-values "San Francisco" 43 57 0.0 (q:sql-cast "1994-11-29" :date)))
            :show nil)
      (duck c (q:insert-into :weather (q:columns :date :city :temp_hi :temp_lo)
                             (q:sql-values (q:sql-cast "1994-11-29" :date) "Hayward" 54 37))
            :show nil)
      ;; The tutorial's COPY, from its own weather.csv (fixtures/duckdb/), into a
      ;; second table so the queries below see the three rows it prints.
      (duck-sql c "CREATE TABLE weather_csv AS SELECT * FROM weather LIMIT 0")
      (duck c (qd:copy-from :weather_csv (weather-csv)) :show nil)
      (check "COPY weather FROM 'weather.csv' loads the same three rows"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather_csv)) :show nil) +weather-3+)))

    (demo-step ("Querying a Table")
      (check "SELECT * FROM weather"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather))) +weather-3+))
      (check "SELECT city, (temp_hi + temp_lo) / 2 AS temp_avg, date"
             (same-rows (duck c (q:select (q:columns :city
                                                     (q:label (:/ (:+ :temp_hi :temp_lo) 2) :temp_avg)
                                                     :date)
                                          (q:from :weather)))
                        '(("San Francisco" 48.0d0 "1994-11-27")
                          ("San Francisco" 50.0d0 "1994-11-29")
                          ("Hayward" 45.5d0 "1994-11-29"))))
      (check "WHERE city = 'San Francisco' AND prcp > 0.0"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather)
                                          (q:where (q:sql-and (:= :city "San Francisco") (:> :prcp 0.0)))))
                        (list (first +weather-3+))))
      (check "ORDER BY city, temp_lo"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather) (q:order-by :city :temp_lo)))
                        (list (third +weather-3+) (second +weather-3+) (first +weather-3+))
                        :ordered t))
      (check "SELECT DISTINCT city ORDER BY city"
             (same-rows (duck c (q:select (q:distinct) (q:columns :city) (q:from :weather) (q:order-by :city)))
                        '(("Hayward") ("San Francisco")) :ordered t)))

    (demo-step ("Joins between Tables")
      (check "the tutorial's implicit join, verbatim: SELECT * FROM weather, cities WHERE city = name"
             (same-rows (duck-sql c "SELECT * FROM weather, cities WHERE city = name")
                        '(("San Francisco" 46 50 0.25 "1994-11-27" "San Francisco" -194 53)
                          ("San Francisco" 43 57 0.0 "1994-11-29" "San Francisco" -194 53))))
      (check "qualified columns, INNER JOIN … ON weather.city = cities.name"
             (same-rows (duck c (q:select (q:columns (q:col :city :weather) (q:col :temp_lo :weather)
                                                     (q:col :temp_hi :weather) (q:col :prcp :weather)
                                                     (q:col :date :weather) (q:col :lon :cities)
                                                     (q:col :lat :cities))
                                          (q:from :weather)
                                          (q:inner-join :cities (q:on (:= (q:col :city :weather)
                                                                          (q:col :name :cities))))))
                        '(("San Francisco" 46 50 0.25 "1994-11-27" 53 -194)
                          ("San Francisco" 43 57 0.0 "1994-11-29" 53 -194))))
      (check "LEFT OUTER JOIN keeps Hayward with NULL city columns"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather)
                                          (q:left-join :cities (q:on (:= (q:col :city :weather)
                                                                         (q:col :name :cities))))))
                        '(("San Francisco" 46 50 0.25 "1994-11-27" "San Francisco" -194 53)
                          ("San Francisco" 43 57 0.0 "1994-11-29" "San Francisco" -194 53)
                          ("Hayward" 37 54 nil "1994-11-29" nil nil nil)))))

    (demo-step ("Aggregate Functions")
      (check "SELECT max(temp_lo) → 46"
             (same-rows (duck c (q:select (q:columns (q:sql-func :max :temp_lo)) (q:from :weather)))
                        '((46))))
      (say "the tutorial's deliberate mistake — an aggregate in WHERE:")
      (handler-case
          (progn (duck c (q:select (q:columns :city) (q:from :weather)
                                   (q:where (:= :temp_lo (q:sql-func :max :temp_lo)))))
                 (check "signalled" nil))
        (sql:sql-programming-error (e)
          (say "signalled ~A:" (type-of e))
          (say "  ~A" (sql:sql-error-message e))
          (check "the Binder Error the tutorial shows"
                 (and (search "WHERE clause cannot contain aggregates" (sql:sql-error-message e)) t))))
      (check "… and its fix, a subquery → San Francisco"
             (same-rows (duck c (q:select (q:columns :city) (q:from :weather)
                                          (q:where (:= :temp_lo (q:subquery (q:select (q:columns (q:sql-func :max :temp_lo))
                                                                                      (q:from :weather)))))))
                        '(("San Francisco"))))
      (check "GROUP BY city"
             (same-rows (duck c (q:select (q:columns :city (q:sql-func :max :temp_lo)) (q:from :weather)
                                          (q:group-by :city)))
                        '(("San Francisco" 46) ("Hayward" 37))))
      (check "… HAVING max(temp_lo) < 40"
             (same-rows (duck c (q:select (q:columns :city (q:sql-func :max :temp_lo)) (q:from :weather)
                                          (q:group-by :city) (q:having (:< (q:sql-func :max :temp_lo) 40))))
                        '(("Hayward" 37))))
      (check "… WHERE city LIKE 'S%' … HAVING max(temp_lo) < 40 → no rows"
             (null (duck c (q:select (q:columns :city (q:sql-func :max :temp_lo)) (q:from :weather)
                                     (q:where (q:sql-like :city "S%"))
                                     (q:group-by :city) (q:having (:< (q:sql-func :max :temp_lo) 40)))))))

    (demo-step ("Updates")
      (duck c (q:update :weather
                (q:sql-set (:= :temp_hi (:- :temp_hi 2)) (:= :temp_lo (:- :temp_lo 2)))
                (q:where (:> :date (q:sql-cast "1994-11-28" :date))))
            :show nil)
      (check "rows after UPDATE"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather)))
                        '(("San Francisco" 46 50 0.25 "1994-11-27")
                          ("San Francisco" 41 55 0.0 "1994-11-29")
                          ("Hayward" 35 52 nil "1994-11-29")))))

    (demo-step ("Deletions")
      (duck c (q:delete-from :weather (q:where (:= :city "Hayward"))) :show nil)
      (check "rows after DELETE"
             (same-rows (duck c (q:select (q:columns :*) (q:from :weather)))
                        '(("San Francisco" 46 50 0.25 "1994-11-27")
                          ("San Francisco" 41 55 0.0 "1994-11-29")))))

    (demo-step ("Beyond the tutorial: Parquet, QUALIFY, the columnar API")
      (let ((path (uiop:native-namestring (uiop:tmpize-pathname
                                           (merge-pathnames "duckdb-demo.parquet" (uiop:temporary-directory))))))
        (unwind-protect
             (progn
               (duck c (qd:copy-to :weather_csv path :format :parquet :compression :zstd) :show nil)
               (check "warmest day per city, from the Parquet file (QUALIFY, * EXCLUDE)"
                      (same-rows (duck c (q:select (q:columns (qd:star :exclude '(:prcp)))
                                                   (q:from (qd:read-parquet path))
                                                   (qd:qualify (:= (q:sql-raw "row_number() OVER (PARTITION BY city ORDER BY temp_hi DESC)") 1))
                                                   (q:order-by :city)))
                                 '(("Hayward" 37 54 "1994-11-29") ("San Francisco" 43 57 "1994-11-29"))
                                 :ordered t)))
          (uiop:delete-file-if-exists path)))
      ;; Under sql-protocol: the cl-stack-duckdb connection, its appender and data chunks.
      (let ((native (sql:raw-connection c)))
        (sql:execute c "CREATE TABLE readings (city VARCHAR, day INTEGER, temp DOUBLE)")
        (ddb:with-appender (a native "readings")
          (dotimes (i 100000)
            (ddb:append-row a (list (if (evenp i) "San Francisco" "Hayward") (mod i 365)
                                    (+ 40d0 (* 20d0 (sin (/ i 1000d0))))))))
        (let ((chunks 0) (rows 0))
          (ddb:with-result (r (ddb:query native "SELECT temp FROM readings"))
            (ddb:do-chunks (chunk r)
              (incf chunks)
              (incf rows (length (ddb:chunk-column-values chunk 0)))))
          (say "appended 100000 rows; read back column-wise in ~D chunks of ≤ 2048" chunks)
          (check "100000 rows" (= rows 100000)))
        (check "avg / min / max per city"
               (= 2 (length (duck c (q:select (q:columns :city (q:label (q:sql-func :round (q:sql-func :avg :temp) 2) :avg)
                                                        (q:label (q:sql-func :round (q:sql-func :min :temp) 2) :min)
                                                        (q:label (q:sql-func :round (q:sql-func :max :temp) 2) :max))
                                             (q:from :readings) (q:group-by :city) (q:order-by :city))))))))

    (demo-step ("Phase 5: Arrow tables (cl-stack-duckdb/arrow, arrow-protocol)")
      (let* ((native (sql:raw-connection c))
             (tb (dda:query-arrow native "SELECT city, temp_lo, temp_hi, prcp, date FROM weather_csv ORDER BY city, temp_lo")))
        (say "query-arrow → ~D rows in ~D record batch(es); schema:" (arrow:arrow-table-num-rows tb)
             (length (arrow:arrow-table-batches tb)))
        (dolist (f (arrow:arrow-schema-fields (arrow:arrow-table-schema tb)))
          (say "  ~A: ~S" (arrow:arrow-field-name f) (arrow-type-text (arrow:arrow-field-type f))))
        (check "date is date32 days since 1970 (1994-11-29 = 9098)" (equal '(9098 9098 9096) (arrow-col tb "date")))
        (let* ((ipc (arrow:encode tb :format :arrow))
               (back (arrow:decode ipc :format :arrow)))
          (say "Arrow IPC file: ~D bytes" (length ipc))
          (check "IPC round trip keeps every column" (every (lambda (n) (equalp (arrow-col tb n) (arrow-col back n)))
                                                         '("city" "temp_lo" "temp_hi" "prcp" "date")))
          (dda:append-arrow-table native "weather_arrow" back :create t)
          (check "append-arrow-table (appender) → the same rows back in DuckDB"
                 (same-rows (duck c (q:select (q:columns :*) (q:from :weather_arrow)) :show nil) +weather-3+))))
      (let* ((native (sql:raw-connection c))
             (nested (dda:query-arrow native "SELECT city, list(temp_hi ORDER BY date) AS highs, {'lo': min(temp_lo), 'hi': max(temp_hi)} AS range FROM weather_csv GROUP BY city ORDER BY city")))
        (say "nested columns: highs ~S, range ~S"
             (arrow-type-text (arrow:arrow-field-type (second (arrow:arrow-schema-fields (arrow:arrow-table-schema nested)))))
             (arrow-type-text (arrow:arrow-field-type (third (arrow:arrow-schema-fields (arrow:arrow-table-schema nested))))))
        (dda:append-arrow-table native "weather_nested" nested :create t)
        (check "nested table through arrow-protocol's Parquet writer, read by DuckDB"
               (same-rows (duck c (q:select (q:columns :*) (q:from :weather_nested) (q:order-by :city)))
                          '(("Hayward" (54) (("lo" . 37) ("hi" . 54)))
                            ("San Francisco" (50 57) (("lo" . 43) ("hi" . 57))))
                          :ordered t))))))
