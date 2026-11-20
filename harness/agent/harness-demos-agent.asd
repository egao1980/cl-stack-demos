(defsystem "harness-demos-agent"
  :version "0.2.0"
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
               (:version "llm-protocol-openai" "0.4.0")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.3.10")
               (:version "http-backend-async" "0.2.19")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               (:version "cl-stack-pathlib" "0.6.1")
               "babel"
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "permissions")
               (:file "file-tools")
               (:file "tui-widgets")
               (:file "tty")
               (:static-file "tty-child.lisp")
               (:file "llm-cache")
               (:file "markdown")
               (:file "shell-tools")
               (:file "run")))
