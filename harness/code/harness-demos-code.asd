(defsystem "harness-demos-code"
  :version "0.2.0"
  :description "Live demo of cl-stack-code: -p (H3a: text, stream-json, headless denial, layered settings, AGENTS.md discovery) and the interactive mode (H3b) on the headless terminal; LM Studio or a scripted model"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-code" "0.2.0")
               ;; 0.2.1: its package carries the current cli-protocol.asd
               ;; (egao1980/cl-repository#117)
               (:version "cli-backend-clingon" "0.2.1")
               (:version "ai-agent-protocol" "0.12.0")
               (:version "llm-protocol" "0.5.0")
               (:version "llm-protocol-openai" "0.4.0")
               (:version "tui-protocol" "0.2.0")
               "tui-protocol/widgets"
               (:version "tui-backend-terminal" "0.1.0")
               (:version "terminal-protocol" "0.2.0")
               (:version "terminal-backend-lisp" "0.2.0")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.4.0")
               ;; 0.2.21 redials a keep-alive socket the server closed (egao1980/http-backend-async#55)
               (:version "http-backend-async" "0.2.21")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               "babel"
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "model")
               (:file "demos")
               (:file "interactive")
               (:file "run")))
