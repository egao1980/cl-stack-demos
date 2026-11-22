(defpackage #:harness-demos-eval
  (:use #:cl)
  (:local-nicknames (#:ae #:agent-eval)
                    (#:ev #:eval-protocol)
                    (#:json #:json-protocol)
                    (#:llm #:llm-protocol)
                    (#:oa #:llm-protocol-openai)
                    (#:hp #:http-protocol))
  (:export #:run-demos))
