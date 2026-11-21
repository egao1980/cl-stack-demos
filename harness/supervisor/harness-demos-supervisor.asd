(defsystem "harness-demos-supervisor"
  :version "0.3.0"
  :description "Live demo of supervisor-protocol 0.3.1: H8a (scopes, queries, restarts with backoff, the replayable event stream, caps + LRU eviction, stop escalation, the orphan sweep after a crash), H8b (rotated, capped unit logs: drop markers, range reads, tail, follow, binary output, torn tails, the total cap) and H8c (the log search index, agent-tools 0.3.1's unit tools, an agent finding and fixing a failure from process logs on a scripted mock and on a live model)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "supervisor-protocol" "0.3.1")
               (:version "supervisor-protocol/search" "0.3.1")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.1")
               (:version "agent-tools" "0.3.1")
               (:version "ai-agent-protocol" "0.14.0")
               (:version "llm-protocol" "0.6.0")
               (:version "llm-protocol-openai" "0.6.0")
               (:version "event-protocol" "0.2.2")
               (:version "event-backend-libuv" "0.2.2")
               (:version "http-protocol" "0.4.0")
               (:version "http-backend-async" "0.2.22")
               (:version "json-protocol" "0.2.1")
               (:version "json-backend-jzon" "0.2.1")
               "babel"
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "sampling")       ; HARNESS_DEMOS_MODE: the sampling preset of live calls
               (:file "support")
               (:file "units-demo")       ; Demos 1-5: scopes, queries, restarts, events, caps, stops
               (:file "sweep-demo")       ; Demo 6: crash recovery (orphan sweep)
               (:file "logs-demo")        ; Demo 7: H8b unit logs
               (:file "tool-support")     ; H8c: tool calls, agent runs, the live model
               (:file "search-demo")      ; Demo 8: the search engine directly
               (:file "unit-tools-demo")  ; Demo 9: the unit tools as an agent sees them
               (:file "agent-demo")       ; Demos 10-11: find and fix a failure (mock, live model)
               (:file "run")))
