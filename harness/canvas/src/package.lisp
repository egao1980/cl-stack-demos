(defpackage #:harness-demos-canvas
  (:use #:cl)
  (:local-nicknames (#:rh #:rpc-backend-htmx)
                    (#:rhc #:rpc-backend-htmx/canvas)
                    (#:rpc #:rpc-protocol)
                    (#:srv #:http-server-protocol)
                    (#:hp #:http-protocol))
  (:export #:run-demos))
