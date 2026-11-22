(defsystem "harness-demos-policy"
  :version "0.1.0"
  :description "Demo of policy-protocol 0.1.0 (L1) on logic-backend-datalog 0.1.0: a user / project / session agent-permission policy loaded from text (permit / forbid / ask on tool actions, plan-mode conditions, :last-match and :forbid-overrides), a decision table with deciding rules, a proof and why-nots, a malformed rule skipped through SKIP-RULE, a request budget resumed with CONTINUE"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "policy-protocol" "0.1.0")
               (:version "logic-protocol" "0.1.0")
               (:version "logic-backend-datalog" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
