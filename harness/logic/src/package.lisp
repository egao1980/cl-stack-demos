(defpackage #:harness-demos-logic
  (:use #:cl)
  (:local-nicknames (#:logic #:logic-protocol)
                    (#:datalog #:logic-backend-datalog)
                    (#:sat #:sat-protocol)
                    (#:dpll #:sat-backend-dpll))
  (:export #:run-demos))
