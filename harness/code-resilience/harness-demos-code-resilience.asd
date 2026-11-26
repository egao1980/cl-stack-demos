(defsystem "harness-demos-code-resilience"
  :version "0.1.0"
  :description "Demo of cl-stack-code 0.14.0's H3f resilience, headless (-p --output-format stream-json) against a scripted loopback OpenAI-compatible server: retry events for 529 and a 429 with Retry-After, retries exhausted as a typed error result with exit 3, a silent server under requestTimeoutMs / turnTimeoutMs, and --continue --retry re-sending a failed turn"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-code" "0.14.0")
               (:version "ai-agent-protocol" "0.16.0")
               (:version "llm-protocol" "0.7.1")
               (:version "llm-protocol-openai" "0.8.0")
               (:version "http-backend-async" "0.2.28")
               "json-protocol"
               "json-backend-jzon"
               "usocket" "bordeaux-threads" "babel" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
