(defpackage #:harness-demos-supervisor
  (:use #:cl)
  (:local-nicknames (#:sp #:supervisor-protocol)
                    (#:pp #:process-protocol))
  (:export #:run-demos))
