(defpackage #:harness-demos-acp
  (:use #:cl)
  (:local-nicknames (#:acp #:acp-protocol)
                    (#:stdio #:acp-backend-stdio))
  (:export #:demo-agent #:serve-demo-agent #:agent-toplevel
           #:demo-client #:run-demos))
