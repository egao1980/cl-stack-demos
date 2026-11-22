(defpackage #:harness-demos-policy
  (:use #:cl)
  (:local-nicknames (#:pol #:policy-protocol)
                    (#:logic #:logic-protocol))
  (:export #:run-demos))
