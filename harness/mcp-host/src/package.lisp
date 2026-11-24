(defpackage #:harness-demos-mcp-host
  (:use #:cl)
  (:local-nicknames (#:mcp #:mcp-protocol)
                    (#:rpc #:rpc-protocol)
                    (#:stdio #:rpc-backend-stdio)
                    (#:json #:json-protocol))
  (:export #:run-demos))
