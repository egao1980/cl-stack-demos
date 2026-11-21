(defsystem "harness-demos-fs"
  :version "0.3.0"
  :description "Live demos of cl-stack-pathlib 0.6.1 (H2a, R4a, fnmatch globs, final-symlink probes) and 0.9.0 (R4b change journals on fs-backend-darwin / fs-backend-linux 0.1.0), process-protocol 0.4.0 (process groups, tree kill), revision-backend-git / -jj 0.2.0"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-pathlib" "0.9.0")
               "cl-stack-pathlib/journal-suite"
               ;; Both journal backends load and register on every OS; each is
               ;; available only on its own (FSEvents: macOS, inotify: Linux).
               (:version "fs-backend-darwin" "0.1.0")
               (:version "fs-backend-linux" "0.1.0")
               (:version "process-protocol" "0.4.0")
               (:version "process-backend-uiop" "0.3.1")
               (:version "revision-protocol" "0.3.0")
               (:version "diff-protocol" "0.3.0")
               (:version "datetime-protocol" "0.1.3")
               (:version "revision-backend-git" "0.2.0")
               (:version "revision-backend-jj" "0.2.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "pathlib-demo")
               (:file "process-demo")
               (:file "git-demo")
               (:file "jj-demo")
               (:file "pathlib-06-demo")
               (:file "tree-kill-demo")
               (:file "journal-demo")
               (:file "run")))
