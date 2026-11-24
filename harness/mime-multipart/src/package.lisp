(defpackage #:harness-demos-mime-multipart
  (:use #:cl)
  (:local-nicknames (#:mime #:mime-protocol)
                    (#:proc #:process-protocol))
  (:export #:run-demos))
