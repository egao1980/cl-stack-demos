(defsystem "harness-demos-data"
  :version "0.3.1"
  :description "Live demos of the harness data layer: diff-protocol (and its engine API), diff-backend-lisp-source, revision-protocol, sql-orm schema diff, sql-migrate, DuckDB (sql-query-duckdb + sql-backend-duckdb)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "diff-protocol" "0.3.0")
               (:version "diff-backend-lisp-source" "0.1.0")
               (:version "datetime-protocol" "0.1.3")
               (:version "revision-protocol" "0.3.0")
               (:version "sql-orm" "0.4.0")
               (:version "sql-migrate" "0.2.1")
               (:version "sql-query-sqlite3" "0.2.0")
               (:version "sql-backend-sqlite3" "0.2.0")
               (:version "cl-stack-duckdb" "2.0.1.1")
               "cl-stack-duckdb/arrow"
               (:version "sql-backend-duckdb" "0.1.0")
               (:version "sql-query" "0.2.4")
               (:version "sql-query-duckdb" "0.2.0"))
  :serial t
  :components ((:file "package")
               (:file "diff-demo")
               (:file "revision-demo")
               (:file "orm-demo")
               (:file "migrate-demo")
               (:file "lisp-diff-demo")
               (:file "engine-api-demo")
               (:file "duckdb-demo")
               (:file "run")))
