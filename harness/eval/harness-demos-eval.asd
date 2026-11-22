(defsystem "harness-demos-eval"
  :version "0.1.0"
  :description "Demo of agent-eval 0.1.0 (E0): a three-task Terminal-Bench-format dataset run locally by the nop, oracle and cl-stack-code (-p, live model) agents; rewards, pass@1, tokens, cost and time; run records and reports; run comparisons; REWARD-MISSING resolved with SCORE-ZERO"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "agent-eval" "0.1.0")
               (:version "eval-protocol" "0.3.0")
               (:version "compute-protocol" "0.3.2")
               "json-protocol"
               ;; cl-stack-code runs as a command (bin/cl-stack-code); the
               ;; dependency makes cl-repo install it
               (:version "cl-stack-code" "0.10.0")
               ;; the endpoint probe before the live run
               (:version "llm-protocol" "0.6.0")
               (:version "llm-protocol-openai" "0.6.0")
               "http-protocol"
               "http-backend-async"
               "event-backend-libuv"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "demo")))
