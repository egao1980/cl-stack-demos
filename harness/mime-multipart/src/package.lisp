(defpackage #:harness-demos-mime-multipart
  (:use #:cl)
  (:local-nicknames (#:mime #:mime-protocol)
                    (#:proc #:process-protocol)
                    (#:hp #:http-protocol))
  (:export #:run-demos))
