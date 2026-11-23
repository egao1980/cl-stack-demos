(defsystem "harness-demos-zed-sample"
  :version "0.1.0"
  :description "Rehearsal of the Zed demo: cl-stack-code --acp (through the wire-logging cl-stack-code-logged.sh) driven headless by harness/acp's scripted ACP client in a fresh cl-stack-sample clone at demo/zed-seeded-bug, against a live model; every permission request is approved, then an independent cl-repo test gives the verdict"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "acp-protocol" "0.1.0")
               (:version "acp-backend-stdio" "0.1.0")
               (:version "json-protocol" "0.2.3")
               (:version "process-protocol" "0.4.0")
               (:version "process-backend-uiop" "0.3.4")
               "bordeaux-threads" "trivial-gray-streams" "uiop")
  :serial t
  :components ((:module "acp-client"      ; harness/acp's scripted client, reused as is
                :pathname "../acp/src/"
                :components ((:file "package") (:file "wire-log") (:file "client")))
               (:module "src"
                :components ((:file "package") (:file "rehearsal")))))
