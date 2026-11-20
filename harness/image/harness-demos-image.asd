(defsystem "harness-demos-image"
  :version "0.1.0"
  :description "Live demo of lisp-mcp/agent (H4a): form-resources, a spawned Swank image through the native tool source, observe mode, agent runs with permission rules; LM Studio or a scripted model"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "lisp-mcp" "0.6.0")
               (:version "lisp-mcp/agent" "0.6.0")
               (:version "ai-agent-protocol" "0.12.0")
               (:version "llm-protocol" "0.5.0")
               (:version "llm-protocol-openai" "0.4.0")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.4.0")
               ;; 0.2.21 redials a keep-alive socket the server closed (egao1980/http-backend-async#55)
               (:version "http-backend-async" "0.2.21")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               "uiop")
  ;; repl-spawn images load swank with ASDF from where this image finds it:
  ;; an install root, not a dependency (as in lisp-mcp).
  :properties (:cl-repo (:ci (:with ("swank" "slynk"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "model")
               (:file "scanner")
               (:file "image")
               (:file "agent")
               (:file "run")))
