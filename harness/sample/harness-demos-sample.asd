(defsystem "harness-demos-sample"
  :version "0.1.0"
  :description "The cl-stack-sample README, section 4, run as a newcomer would: a fresh clone of egao1980/cl-stack-sample, the cl-stack-code launcher downloaded from egao1980/lisp-workspace, a fresh executable build from GHCR, then the headless -p session that adds a w (week) unit with tests on a live model, checked by an independent cl-repo test"
  :author "egao1980"
  :license "MIT"
  :depends-on ("json-protocol"
               "json-backend-jzon"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
