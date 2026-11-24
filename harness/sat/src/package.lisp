(defpackage #:harness-demos-sat
  (:use #:cl)
  (:local-nicknames (#:sat #:sat-protocol)
                    (#:dpll #:sat-backend-dpll)
                    (#:cdcl #:sat-backend-cdcl))
  (:export #:run-demos))
