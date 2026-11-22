(defsystem "harness-demos-unkernel"
  :version "0.1.0"
  :description "Demo of compute-backend-unkernel 0.1.0 (R4h-4): commands confined in an unkernel guest (Linux compiled to wasm, under Node): availability, one-shot runs, isolation (mounts, network, TMPDIR), a warm session vs cold runs, a wall-clock deadline with ABORT-EXECUTION, SBCL inside the guest"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "compute-backend-unkernel" "0.1.0")
               (:version "compute-protocol" "0.3.2")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4")
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
