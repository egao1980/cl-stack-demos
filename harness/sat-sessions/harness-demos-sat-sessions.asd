(defsystem "harness-demos-sat-sessions"
  :version "0.1.0"
  :description "Demo of sat-protocol 0.3.0 incremental sessions (Track L, slice L2b-2) on sat-backend-cdcl 0.1.0, which keeps the protocol's default (stateless) session methods: a 50-segment chain added to WITH-SAT-SESSION one segment at a time, SESSION-SOLVE under changing assumptions, SESSION-MINIMIZE with a (:lex requests effort) objective under every strategy, SESSION-UNSAT-CORE on a contradictory assumption set, each answer next to a fresh SOLVE / MINIMIZE / UNSAT-CORE over SESSION-CNF and a dynamic-programming oracle, the session timed against fresh re-solves over 50 incremental steps, and SAT-BUDGET-REACHED with USE-BEST, CONTINUE and STORE-VALUE"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "sat-protocol" "0.3.0")
               (:version "sat-backend-cdcl" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
