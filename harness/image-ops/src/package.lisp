(defpackage #:harness-demos-image-ops
  (:use #:cl)
  (:local-nicknames (#:img #:image-protocol)
                    (#:stb #:image-backend-stb)
                    (#:proc #:process-protocol))
  (:export #:run-demos))
