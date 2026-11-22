(defsystem "harness-demos-isolation"
  :version "0.1.0"
  :description "Live demos of R4c (cl-stack-pathlib 0.10.0 fs-clone-tree + copy rung, fs-backend-darwin 0.2.0 :clonefile, fs-backend-linux 0.2.0 :ficlone) and R4d (compute-protocol 0.3.2 confinement + confinement suite, compute-backend-seatbelt 0.1.0 on macOS, compute-backend-landlock 0.1.0 on Linux)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-pathlib" "0.10.0")
               ;; Both clone backends load and register on every OS; each is
               ;; available only on its own (clonefile: macOS APFS, FICLONE: Linux).
               (:version "fs-backend-darwin" "0.2.0")
               (:version "fs-backend-linux" "0.2.0")
               (:version "compute-protocol" "0.3.2")
               (:version "compute-protocol/confinement-suite" "0.3.2")
               ;; Both confining backends load on every OS; seatbelt confines on
               ;; macOS only, landlock on Linux only.
               (:version "compute-backend-seatbelt" "0.1.0")
               (:version "compute-backend-landlock" "0.1.0")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.1")
               "sb-bsd-sockets"
               "sb-posix"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "clone-demo")         ; Demo 1: R4c COW clones vs the copy rung
               (:file "confine-demo")       ; Demos 2-3: R4d confinement, the suite
               (:file "run")))
