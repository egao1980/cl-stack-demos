(defpackage #:harness-demos-http-facade
  (:use #:cl)
  (:local-nicknames (#:http #:cl-stack-http)
                    (#:hp #:http-protocol)
                    (#:ev #:event-protocol)
                    (#:evp #:event-protocol/promises))
  (:export #:run-demos))
