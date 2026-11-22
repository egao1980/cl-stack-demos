(defsystem "harness-demos-acp"
  :version "0.1.0"
  :description "ACP demo: a scripted Lisp coding agent served over stdio with acp-backend-stdio 0.1.0 (acp-protocol 0.1.0) — agent message chunks, a plan updated step by step, fs/read_text_file, a tool call with diff content, session/request_permission, fs/write_text_file, the project's tests in a terminal/* terminal, end_turn. Run headless against a scripted Lisp ACP client (spawn-acp-agent, wire log in wire-log.txt) and launched by Zed as a custom agent server (the build-core.sh executable, or acp-agent.sh)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "acp-protocol" "0.1.0")
               (:version "acp-backend-stdio" "0.1.0")
               (:version "json-protocol" "0.2.3")
               (:version "process-protocol" "0.4.0")
               (:version "process-backend-uiop" "0.3.4")
               "bordeaux-threads" "trivial-gray-streams" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "wire-log")    ; logging stdin / stdout wrappers, redaction
               (:file "agent")       ; the scripted demo agent (fix-fib turn)
               (:file "client")      ; the headless scripted ACP client
               (:file "run")))       ; run-demos: the headless wire test
