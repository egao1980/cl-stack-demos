(defsystem "harness-demos-code"
  :version "0.1.0"
  :description "Live demo of cl-stack-code -p (H3a): text, stream-json, headless denial, layered settings, AGENTS.md discovery; LM Studio or a scripted model"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-code" "0.1.1")
               (:version "ai-agent-protocol" "0.12.0")
               (:version "llm-protocol" "0.5.0")
               (:version "llm-protocol-openai" "0.4.0")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.3.10")
               (:version "http-backend-async" "0.2.20")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               "babel"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "model")
               (:file "demos")
               (:file "run")))
