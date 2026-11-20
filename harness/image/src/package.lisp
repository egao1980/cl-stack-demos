(defpackage #:harness-demos-image
  (:use #:cl)
  (:local-nicknames (#:lt #:lisp-mcp/agent)
                    (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:oa #:llm-protocol-openai)
                    (#:json #:json-protocol)
                    (#:hp #:http-protocol)
                    (#:ev #:event-protocol))
  (:export #:run-demos
           #:demo-scanner
           #:demo-image
           #:demo-observe
           #:demo-agent-rules
           #:demo-agent-real
           #:demo-image-events
           #:demo-image-events-denied
           #:demo-image-job-events
           #:demo-image-events-observe
           #:demo-image-events-real))
