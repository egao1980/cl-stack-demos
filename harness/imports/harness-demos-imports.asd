(defsystem "harness-demos-imports"
  :version "0.1.0"
  :description "Demo of the cl-stack-systems imports from cl-stack-systems#79 / #84 (issues #75 and #76, first tier): jose HS256 with cl-json and assoc-utils, lparallel, cl-csv, 3bmd with colorize, cl-pdf (egao1980 fork) + cl-typesetting + cl-pdf-parser, opticl, function-cache, lquery / clss, machine-state, check-it, parcom, and cl-smtp / cl-redis / cl-excel / legit / cl-progress-bar loaded, all installed from GHCR by cl-repo"
  :author "egao1980"
  :license "MIT"
  :depends-on ("jose" "cl-json" "assoc-utils" "ironclad" "lparallel" "cl-csv"
               "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables"
               "cl-pdf" "cl-pdf-parser" "cl-typesetting" "opticl" "function-cache"
               "plump" "clss" "lquery" "machine-state" "check-it" "parcom"
               "cl-smtp" "cl-redis" "cl-excel" "legit" "cl-progress-bar")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
