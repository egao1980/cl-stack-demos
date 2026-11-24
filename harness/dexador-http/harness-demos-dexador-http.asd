(defsystem "harness-demos-dexador-http"
  :version "0.1.0"
  :description "Demo of http-backend-dexador 0.2.0 on dexador 0.9.16.1: protocol proxy configs resolved per URL (absolute-form to an HTTP proxy, CONNECT for HTTPS, per-request proxies, no_proxy, trust-env sessions, refused https:// / SOCKS4 proxies, no userinfo in Host), the TOTAL deadline and the read timeout on HTTPS (direct, after an http:// to https:// redirect, streamed bodies, fractional seconds), and pools keyed by :verify, all against loopback origins and proxies"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "http-backend-dexador" "0.2.0")
               "http-protocol"
               "cl+ssl"
               "usocket" "bordeaux-threads" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "fixtures")
               (:file "demo")))
