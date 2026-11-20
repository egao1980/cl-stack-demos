(defpackage #:harness-demos-code
  (:use #:cl)
  (:local-nicknames (#:code #:cl-stack-code)
                    (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:oa #:llm-protocol-openai)
                    (#:json #:json-protocol)
                    (#:hp #:http-protocol))
  (:export #:run-demos
           #:demo-text
           #:demo-stream-json
           #:demo-headless-denial
           #:demo-settings
           #:demo-instructions))
