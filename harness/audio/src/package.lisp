(defpackage #:harness-demos-audio
  (:use #:cl)
  (:local-nicknames (#:audio #:audio-protocol)
                    (#:midi #:midi-protocol)
                    (#:bin #:binary-protocol)
                    (#:crypto #:crypto-protocol))
  (:export #:run-demos))
