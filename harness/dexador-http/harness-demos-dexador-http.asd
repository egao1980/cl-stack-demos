(defsystem "harness-demos-dexador-http"
  :version "0.2.0"
  :description "Demo of http-backend-dexador 0.2.1 on dexador 0.9.16.1: protocol proxy configs resolved per URL (absolute-form to an HTTP proxy, CONNECT for HTTPS, per-request proxies, no_proxy, trust-env sessions, refused https:// / SOCKS4 proxies, no userinfo in Host), the TOTAL deadline and the read timeout on HTTPS (direct, after an http:// to https:// redirect, streamed bodies, fractional seconds), pools keyed by :verify, request bodies (form, multipart, coded) that reach the origin as encoded, and redirects followed by the backend with their response-history, all against loopback origins and proxies"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "http-backend-dexador" "0.2.1")
               "http-protocol"
               "cl+ssl"
               "usocket" "bordeaux-threads" "babel"
               ;; gzip for the coded form row of demo 4
               "http-encoding-chipz")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "fixtures")
               (:file "demo")))
