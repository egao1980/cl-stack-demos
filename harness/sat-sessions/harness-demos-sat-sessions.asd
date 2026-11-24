(defsystem "harness-demos-sat-sessions"
  :version "0.2.0"
  :description "Demo of sat-protocol 0.3.0 incremental sessions (Track L, slice L2b-2) on sat-backend-cdcl 0.2.0 native sessions: a 50-segment chain added to WITH-SAT-SESSION one segment at a time, SESSION-SOLVE under changing assumptions, SESSION-MINIMIZE with a (:lex requests effort) objective under every strategy, SESSION-UNSAT-CORE on a contradictory assumption set, each answer checked against a fresh SOLVE / MINIMIZE / UNSAT-CORE over SESSION-CNF and a dynamic-programming oracle, SAT-BUDGET-REACHED with USE-BEST, CONTINUE and STORE-VALUE, learnt clauses surviving between two UNSAT calls (PHP(7,6) under a selector), and 50 incremental steps timed against fresh re-solves next to the recorded protocol-default sessions of sat-backend-cdcl 0.1.0"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "sat-protocol" "0.3.0")
               (:version "sat-backend-cdcl" "0.2.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
