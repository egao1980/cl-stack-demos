(defsystem "harness-demos-image"
  :version "0.5.0"
  :description "Live demo of lisp-mcp/agent (H4a: form-resources, a spawned Swank image through the native tool source, observe mode, agent runs with permission rules; H4b: image events, a background debugger in the agent inbox, restarts under restart: rules, job events, observe; #+sbcl / #-sbcl top-level forms through the source and edit tools, lisp-source-protocol#9; comments and layout of edit code, lisp-source-protocol#11, #14, #15); LM Studio or a scripted model"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "lisp-mcp" "0.7.3")
               (:version "lisp-mcp/agent" "0.7.3")
               ;; 0.2.8 kills a spawned image's whole tree: on Windows ros.exe runs SBCL
               ;; as its child, which outlived the kill (egao1980/repl-protocol#11)
               (:version "repl-protocol" "0.2.8")
               (:version "ai-agent-protocol" "0.12.0")
               (:version "llm-protocol" "0.5.0")
               ;; 0.5.1 reads "tool_calls": null as no tool calls (egao1980/llm-protocol-openai#14)
               (:version "llm-protocol-openai" "0.5.1")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.4.0")
               ;; 0.2.21 redials a keep-alive socket the server closed (egao1980/http-backend-async#55)
               (:version "http-backend-async" "0.2.21")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               "babel"
               "uiop")
  ;; repl-spawn images load swank with ASDF from where this image finds it:
  ;; an install root, not a dependency (as in lisp-mcp).
  :properties (:cl-repo (:ci (:with ("swank" "slynk"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "sampling")       ; HARNESS_DEMOS_MODE: the sampling preset of live calls
               (:file "support")
               (:file "model")
               (:file "scanner")
               (:file "image")
               (:file "agent")
               (:file "events")
               (:file "conditionals")
               (:file "layout")
               (:file "run")))
