(defsystem "harness-demos-http-facade"
  :version "0.2.2"
  :description "Demo of the cl-stack-http 0.2.x facade and event-protocol 0.3.0 promises: http-protocol response readers and conditions re-exported from cl-stack-http (the QUICKSTART form reads), the two :auto orders (dexador for blocking calls, async/libuv for -async calls, :async with no event wiring), asyncio.run / gather / await through run-promise, gather and await on a loopback keep-alive origin, https:// proxies from HTTPS_PROXY (http-protocol#53): TLS to a loopback proxy first, TLS inside TLS for https origins on http-backend-async, a refusal before any connection on http-backend-dexador, a redirect through the session's :proxy that builds each hop for its own URL (http-backend-async#82), which proxy a session uses (a request :proxy, then the session's, then the environment under :trust-env, cl-stack-http#34 and #35), and redirect hops that keep HEAD and their kept-alive connection (http-backend-async#84, #85)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-http" "0.2.1")
               "event-protocol"
               "event-protocol/promises"
               (:version "http-backend-dexador" "0.2.4")
               (:version "http-backend-async" "0.2.31")
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
               (:file "redirect-proxy")
               (:file "demo")))
