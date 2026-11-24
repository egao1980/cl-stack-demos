(defsystem "harness-demos-cl-repo"
  :version "0.2.0"
  :description "Before and after: the published cl-repo client 0.27.35 against 0.27.34, each in fresh HOMEs against GHCR: a lock entry pinned by digest (#163), a missing minimum named as such (#165), cl-repo ros without Quicklisp (#166), resolution to the newest published version (#167), a qlfile-only project gets its closure (#170), and six parallel images sharing one fasl root (#162). And 0.27.38 against 0.27.37 with the registry unreachable (#179) and cl-repo --version (#189)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")
               (:file "offline")))
