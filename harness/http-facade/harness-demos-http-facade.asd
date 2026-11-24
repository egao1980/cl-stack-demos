(defsystem "harness-demos-http-facade"
  :version "0.1.0"
  :description "Demo of the cl-stack-http 0.2.0 facade and event-protocol 0.3.0 promises: http-protocol response readers and conditions re-exported from cl-stack-http (the QUICKSTART form reads), the two :auto orders (dexador for blocking calls, async/libuv for -async calls, :async with no event wiring), and asyncio.run / gather / await through run-promise, gather and await on a loopback keep-alive origin"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-http" "0.2.0")
               "event-protocol"
               "event-protocol/promises"
               "http-backend-dexador"
               "http-backend-async"
               "event-backend-libuv"
               "usocket" "bordeaux-threads" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "origin")
               (:file "demo")))
