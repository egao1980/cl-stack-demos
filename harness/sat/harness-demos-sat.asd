(defsystem "harness-demos-sat"
  :version "0.1.0"
  :description "Demo of sat-protocol 0.2.0 (Track L, slice L2b-1) on sat-backend-dpll 0.1.0: cardinality constraints in every encoding (clause and auxiliary counts, N-queens), a weighted sequence-labelling chain minimised with each strategy and checked against a dynamic-programming oracle, anytime budgets and timeouts (OPTIMAL-P NIL, SAT-BUDGET-EXCEEDED and CONTINUE), hints as a warm start, lexicographic objectives, and final-conflict cores"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "sat-protocol" "0.2.0")
               (:version "sat-backend-dpll" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
