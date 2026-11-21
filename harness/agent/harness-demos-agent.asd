(defsystem "harness-demos-agent"
  :version "0.3.0"
  :description "Live demos of the coding-agent harness: permissions, file and shell tools, TUI widgets, markdown and diff rendering, tty backend, OpenAI prompt cache"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "ai-agent-protocol" "0.10.0")
               (:version "agent-tools" "0.2.0")
               (:version "tui-protocol" "0.2.0")
               "tui-protocol/widgets"
               (:version "tui-backend-terminal" "0.1.0")
               (:version "tui-backend-tty" "0.1.1")
               (:version "tui-markdown" "0.1.0")
               (:version "diff-protocol" "0.3.0")
               (:version "terminal-protocol" "0.2.0")
               (:version "terminal-backend-lisp" "0.2.0")
               (:version "process-protocol" "0.4.0")
               (:version "process-backend-uiop" "0.3.1")
               (:version "pty-protocol" "0.1.0")
               (:version "pty-backend-native" "0.2.1")
               (:version "llm-protocol" "0.5.0")
               ;; 0.5.1 reads "tool_calls": null as no tool calls (egao1980/llm-protocol-openai#14)
               (:version "llm-protocol-openai" "0.5.1")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.4.0")
               ;; 0.2.21 reveals http-protocol SECRET header values: llm-protocol-openai
               ;; 0.5.x sends the key as one (egao1980/http-backend-async#54)
               (:version "http-backend-async" "0.2.21")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               (:version "cl-stack-pathlib" "0.6.1")
               "babel"
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "sampling")       ; HARNESS_DEMOS_MODE: the sampling preset of live calls
               (:file "permissions")
               (:file "file-tools")
               (:file "tui-widgets")
               (:file "tty")
               (:static-file "tty-child.lisp")
               (:file "llm-cache")
               (:file "markdown")
               (:file "shell-tools")
               (:file "run")))
