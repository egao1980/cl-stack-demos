(defsystem "harness-demos-supervisor"
  :version "0.1.0"
  :description "Live demo of supervisor-protocol 0.2.0: H8a (scopes, queries, restarts with backoff, the replayable event stream, caps + LRU eviction, stop escalation, the orphan sweep after a crash) and H8b (rotated, capped unit logs: drop markers, range reads, tail, follow, binary output, torn tails, the total cap)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "supervisor-protocol" "0.2.0")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.1")
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "units-demo")     ; Demos 1-5: scopes, queries, restarts, events, caps, stops
               (:file "sweep-demo")     ; Demo 6: crash recovery (orphan sweep)
               (:file "logs-demo")      ; Demo 7: H8b unit logs
               (:file "run")))
