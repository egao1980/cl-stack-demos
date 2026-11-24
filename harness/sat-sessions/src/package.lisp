(defpackage #:harness-demos-sat-sessions
  (:use #:cl)
  (:local-nicknames (#:sat #:sat-protocol)
                    (#:cdcl #:sat-backend-cdcl))
  (:export #:run-demos))
