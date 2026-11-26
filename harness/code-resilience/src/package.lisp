(defpackage #:harness-demos-code-resilience
  (:use #:cl)
  (:local-nicknames (#:code #:cl-stack-code)
                    (#:json #:json-protocol)
                    (#:bt #:bordeaux-threads))
  (:export #:run-demos))
