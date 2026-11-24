(defsystem "harness-demos-sat"
  :version "0.2.0"
  :description "Demo of sat-protocol 0.2.0 (Track L, slices L2b-1 and L2b-3) on sat-backend-dpll 0.1.0 and sat-backend-cdcl 0.1.0: cardinality constraints in every encoding (clause and auxiliary counts, N-queens), a weighted sequence-labelling chain minimised with each strategy and checked against a dynamic-programming oracle, anytime budgets and timeouts (OPTIMAL-P NIL, SAT-BUDGET-EXCEEDED and CONTINUE), hints as a warm start, lexicographic objectives, final-conflict cores; then DPLL against CDCL on random 3-SAT, pigeonhole, a hidden unsatisfiable core and the chains, final-conflict cores from conflict analysis, the CDCL parameters and determinism"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "sat-protocol" "0.2.0")
               (:version "sat-backend-dpll" "0.1.0")
               (:version "sat-backend-cdcl" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")
               (:file "cdcl")))
