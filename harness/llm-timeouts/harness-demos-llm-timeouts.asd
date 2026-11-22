(defsystem "harness-demos-llm-timeouts"
  :version "0.1.0"
  :description "Demo of configurable model-request timeouts: llm-protocol-openai 0.7.0 / llm-protocol-anthropic 0.5.0 (:request-timeout, :stream-idle-timeout, :connect-timeout and their env vars) on http-backend-async 0.2.25 (read = idle timeout, total only when asked): a slow steady stream outlives the old 180 s-style cap, a silent one fails, a live long answer on a slow local model"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "llm-protocol-openai" "0.7.0")
               (:version "llm-protocol-anthropic" "0.5.0")
               (:version "http-backend-async" "0.2.25")
               "event-backend-libuv"
               "usocket" "bordeaux-threads" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
