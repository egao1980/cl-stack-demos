(defsystem "harness-demos-llm-live-stream"
  :version "0.1.0"
  :description "Demo of live model streaming on http-backend-async 0.2.27 (#70): the blocking HTTP send returns at the headers with the live body, so llm-protocol-openai / -anthropic on-part callbacks fire as each SSE event arrives, not in one burst at the end; local paced origins for both dialects, early close, and a live stream from a local model with a timestamp per chunk"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "http-backend-async" "0.2.27")
               (:version "llm-protocol-openai" "0.7.0")
               (:version "llm-protocol-anthropic" "0.5.0")
               "event-backend-libuv"
               "usocket" "bordeaux-threads" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
