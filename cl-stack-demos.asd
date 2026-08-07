(defsystem "cl-stack-demos"
  :version "0.1.0"
  :description "Thin ports of real GitHub Python apps onto cl-stack (showcase / bugs / gaps)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-stack-pathlib"
               "cl-stack-config"
               "json-protocol"
               "json-backend-jzon"
               "serdes-protocol"
               "sexp-protocol"
               "io-protocol"
               "cli-protocol"
               "cli-backend-clingon"
               "log-protocol"
               "log-backend-log4cl"
               "http-server-protocol"
               "http-server-backend-hunchentoot"
               "cl-stack-http"
               "http-backend-dexador"
               "http-backend-async"
               "ws-protocol"
               "ws-backend-websocket-driver"
               "sql-protocol"
               "sql-backend-sqlite3"
               "sql-query"
               "sql-query-sqlite3"
               "sql-query-csv"
               "sql-orm"
               "cl-stack-jwt"
               "cl-stack-oauth2"
               "event-protocol"
               "event-backend-libuv"
               "bordeaux-threads"
               "blackbird"
               "ironclad"
               "babel"
               "uiop"
               "usocket")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "harness")
               (:module "apps"
                :serial t
                :components ((:file "pathlib-organize")
                             (:file "settings-env")
                             (:file "requests-json")
                             (:file "jsonlines-io")
                             (:file "object-tape")
                             (:file "click-naval")
                             (:file "structlog-run")
                             (:file "flask-echo")
                             (:file "websockets-echo")
                             (:file "sqlalchemy-notes")
                             (:file "pyjwt-hs256")
                             (:file "authlib-pkce")
                             (:file "records-sql")
                             (:file "csvkit-report")
                             (:file "asyncio-sleep"))))
  :in-order-to ((test-op (test-op "cl-stack-demos/tests"))))

(defsystem "cl-stack-demos/tests"
  :depends-on ("cl-stack-demos" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "smoke"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "cl-stack-demos tests failed"))))
