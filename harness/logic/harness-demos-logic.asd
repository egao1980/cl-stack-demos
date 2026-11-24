(defsystem "harness-demos-logic"
  :version "0.1.0"
  :description "Demo of logic-protocol 0.2.0 weak constraints on logic-backend-datalog 0.1.0, bridged to sat-protocol 0.2.0 on sat-backend-dpll 0.1.0: weak rules in the rule DSL (weights, levels, parse errors and restarts), WEAK-INSTANCES grounding and WEAK-COST on a small scheduling problem, the Datalog to weighted MaxSAT bridge (one-hot choices, a :LEX objective solved :CORE-GUIDED, checked by brute force), and EXPLAIN on the facts behind a penalty"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "logic-protocol" "0.2.0")
               (:version "logic-backend-datalog" "0.1.0")
               (:version "sat-protocol" "0.2.0")
               (:version "sat-backend-dpll" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
