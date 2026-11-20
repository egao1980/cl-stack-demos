(defpackage #:harness-demos-code
  (:use #:cl)
  (:local-nicknames (#:code #:cl-stack-code)
                    (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:oa #:llm-protocol-openai)
                    (#:json #:json-protocol)
                    (#:hp #:http-protocol)
                    (#:bt #:bordeaux-threads)
                    (#:w #:tui-protocol/widgets)
                    (#:h #:tui-backend-terminal)
                    (#:tl #:terminal-backend-lisp))
  (:export #:run-demos
           #:demo-text
           #:demo-stream-json
           #:demo-headless-denial
           #:demo-settings
           #:demo-instructions
           #:demo-interactive
           #:demo-plan-headless
           #:demo-plan-interactive
           #:demo-plan-real-model
           #:demo-checkpoints
           #:demo-checkpoints-plan
           #:demo-checkpoints-retention
           #:demo-checkpoints-interactive))
