(defsystem "harness-demos-fs"
  :version "0.1.0"
  :description "Live demos of cl-stack-pathlib 0.5.0 (H2a + R4a), process-protocol 0.3.0, revision-backend-git / -jj 0.1.0"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-pathlib" "0.5.0")
               (:version "process-protocol" "0.3.0")
               (:version "process-backend-uiop" "0.2.0")
               (:version "revision-protocol" "0.2.0")
               (:version "diff-protocol" "0.2.0")
               (:version "datetime-protocol" "0.1.3")
               (:version "revision-backend-git" "0.1.0")
               (:version "revision-backend-jj" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "pathlib-demo")
               (:file "process-demo")
               (:file "git-demo")
               (:file "jj-demo")
               (:file "run")))
