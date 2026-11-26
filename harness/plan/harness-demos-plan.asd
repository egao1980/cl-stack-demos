(defsystem "harness-demos-plan"
  :version "0.1.0"
  :description "Demo of cl-stack-code 0.15.0's H5c-0 plan mode, headless (-p --output-format stream-json) against a scripted loopback OpenAI-compatible server: exploration in a confined copy, edits refused by the plan-mode-tools policy, propose_plan writing plan.md outside the project and ending the run, a user edit recorded as a revision, --approve-plan executing it with an update_plan checklist and plan versus diff, a revision and a rejected plan, and preview mode (H5a renamed) as the fifth permission mode"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-code" "0.15.0")
               (:version "ai-agent-protocol" "0.16.1")
               (:version "llm-protocol-openai" "0.8.0")
               (:version "http-backend-async" "0.2.28")
               "json-protocol"
               "json-backend-jzon"
               "usocket" "bordeaux-threads" "babel" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
