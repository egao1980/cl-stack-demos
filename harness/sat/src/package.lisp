(defpackage #:harness-demos-sat
  (:use #:cl)
  (:local-nicknames (#:sat #:sat-protocol)
                    (#:dpll #:sat-backend-dpll))
  (:export #:run-demos))
