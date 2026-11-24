(defsystem "harness-demos-http-facade"
  :version "0.2.0"
  :description "Demo of the cl-stack-http 0.2.0 facade and event-protocol 0.3.0 promises: http-protocol response readers and conditions re-exported from cl-stack-http (the QUICKSTART form reads), the two :auto orders (dexador for blocking calls, async/libuv for -async calls, :async with no event wiring), asyncio.run / gather / await through run-promise, gather and await on a loopback keep-alive origin, and https:// proxies from HTTPS_PROXY (http-protocol#53): TLS to a loopback proxy first, TLS inside TLS for https origins on http-backend-async, a refusal before any connection on http-backend-dexador"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-http" "0.2.0")
               "event-protocol"
               "event-protocol/promises"
               "http-backend-dexador"
               "http-backend-async"
               "event-backend-libuv"
               ;; the demo-4 proxy's server side of TLS; cl-stack-ssl loads
               ;; cl+ssl against its own OpenSSL
               "cl-stack-ssl"
               "trivial-gray-streams"
               "usocket" "bordeaux-threads" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "origin")
               (:file "tls-proxy")
               (:file "demo")))
