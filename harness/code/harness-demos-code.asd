(defsystem "harness-demos-code"
  :version "0.5.0"
  :description "Live demo of cl-stack-code: -p (H3a: text, stream-json, headless denial, layered settings, AGENTS.md discovery), the interactive mode (H3b) on the headless terminal, and plan mode (H5a: plan, project, approve, drift, reject; git and copy areas) and checkpoints with rewind (H5b: git private refs, jj @ commits, folder snapshots; files / both rewinds, drift restarts, plan checkpoints, retention, the rewind dialog), and the Lisp image (H4b: -p --lisp-image spawn with image_event lines, onDebugger turn and notice with the banner and status marker, observe against an attached image); LM Studio or a scripted model"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-code" "0.6.0")
               (:version "lisp-mcp" "0.7.0")
               (:version "lisp-mcp/agent" "0.7.0")
               ;; 0.2.8 kills a spawned image's whole tree: on Windows ros.exe runs SBCL
               ;; as its child, which outlived the kill (egao1980/repl-protocol#11)
               (:version "repl-protocol" "0.2.8")
               ;; 0.2.1: its package carries the current cli-protocol.asd
               ;; (egao1980/cl-repository#117)
               (:version "cli-backend-clingon" "0.2.1")
               (:version "ai-agent-protocol" "0.14.0")
               (:version "cl-stack-pathlib" "0.8.0")
               (:version "revision-backend-git" "0.5.1")
               (:version "revision-backend-jj" "0.3.1")
               (:version "diff-protocol" "0.3.0")
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
  ;; --lisp-image spawn starts ros +Q run images that load swank (or slynk)
  ;; with ASDF from where this image finds it: install roots, not deps.
  :properties (:cl-repo (:ci (:with ("swank" "slynk"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "model")
               (:file "demos")
               (:file "interactive")
               (:file "plan")
               (:file "rewind")
               (:file "image")
               (:file "run")))
