(defsystem "harness-demos-canvas"
  :version "0.2.0"
  :description "Canvas demo (H4h): canvases as plain functions in the live image (a sortable table, a bar chart, a line chart, an htmx threshold form, an SSE ticker), served on 127.0.0.1 by rpc-backend-htmx/canvas on Hunchentoot behind a one-time token and a strict CSP, driven by the in-process htmx client and by http-backend-async over real HTTP, a chart canvas redefined live, the dashboard frozen to dashboard.html styled by engels; canvas-protocol 0.2.0: a keyboard-driven grid editor (key chords, hx-sync, hx-swap-oob), engine-generated SVG embedded through the allowlist (unsafe-svg, strip-violations, frozen to diagram.html), and file uploads (file-input, multipart forms, coerce-param :file)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "canvas-protocol" "0.2.0")
               (:version "canvas-backend-svg" "0.1.0")
               (:version "rpc-backend-htmx" "0.1.0")
               (:version "rpc-backend-htmx/canvas" "0.1.0")
               (:version "rpc-protocol" "0.2.0")
               (:version "html-protocol" "0.1.2")
               (:version "mime-protocol" "0.2.0")
               (:version "engels" "0.2.1")
               (:version "encoding-protocol" "0.2.0")
               (:version "http-server-protocol" "0.1.0")
               "http-server-backend-hunchentoot"
               (:version "http-protocol" "0.4.0")
               (:version "http-backend-async" "0.2.21")
               (:version "event-backend-libuv" "0.2.2")
               (:version "crypto-protocol" "0.2.0")
               (:version "crypto-backend-ironclad" "0.2.1")
               "quri"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
