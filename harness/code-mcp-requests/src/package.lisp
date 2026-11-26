(defpackage #:harness-demos-code-mcp-requests
  (:use #:cl)
  (:local-nicknames (#:code #:cl-stack-code)
                    (#:json #:json-protocol)
                    (#:bt #:bordeaux-threads))
  (:export #:run-demos))
