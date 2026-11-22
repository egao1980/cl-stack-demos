(defsystem "harness-demos-isolation"
  :version "0.3.0"
  :description "Live demos of R4c (cl-stack-pathlib 0.10.0 fs-clone-tree + copy rung, fs-backend-darwin 0.2.0 :clonefile, fs-backend-linux 0.2.0 :ficlone), R4d (compute-protocol 0.3.2 confinement + confinement suite, compute-backend-seatbelt 0.1.1 on macOS, compute-backend-landlock 0.1.0 on Linux) and R4d-4a (agent-tools 0.4.0 confined shell tools: sandbox-aware bash description and [sandbox: …] notes, refusals; a live model meeting the sandbox) and R4d-4b (cl-stack-code 0.10.0 plan mode confined: -p --permission-mode plan on Seatbelt, --allow-network, no sandbox, a live model fixing a test)"
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
               ;; macOS only, landlock on Linux only. Seatbelt 0.1.1: a confined
               ;; cat can write run-confined's captured stdout under :read :system
               ;; (egao1980/compute-backend-seatbelt#2).
               (:version "compute-backend-seatbelt" "0.1.1")
               (:version "compute-backend-landlock" "0.1.0")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4")
               ;; Demos 4-5: R4d-4a confined shell tools, the live model
               (:version "agent-tools" "0.4.0")
               (:version "ai-agent-protocol" "0.14.0")
               (:version "llm-protocol" "0.6.0")
               (:version "llm-protocol-openai" "0.6.0")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.4.0")
               (:version "http-backend-async" "0.2.23")
               ;; json-backend-jzon 0.2.3 needs json-protocol 0.2.3 but does not say so
               ;; (egao1980/json-protocol#27)
               (:version "json-protocol" "0.2.3")
               (:version "json-backend-jzon" "0.2.3")
               ;; Demos 6-7: R4d-4b plan mode confined
               (:version "cl-stack-code" "0.10.0")
               "babel"
               "bordeaux-threads"
               "sb-bsd-sockets"
               "sb-posix"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "sampling")           ; HARNESS_DEMOS_MODE: the sampling preset of live calls
               (:file "support")
               (:file "clone-demo")         ; Demo 1: R4c COW clones vs the copy rung
               (:file "confine-demo")       ; Demos 2-3: R4d confinement, the suite
               (:file "live")               ; Demos 4-5: tool calls, agent runs, the live model
               (:file "shell-demo")         ; Demos 4-5: R4d-4a confined shell tools
               (:file "plan-demo")          ; Demos 6-7: R4d-4b plan mode confined (cl-stack-code)
               (:file "run")))
