(defpackage #:harness-demos-llm-live-stream
  (:use #:cl)
  (:local-nicknames (#:llm #:llm-protocol)
                    (#:oa #:llm-protocol-openai)
                    (#:an #:llm-protocol-anthropic)
                    (#:hp #:http-protocol))
  (:export #:run-demos))
