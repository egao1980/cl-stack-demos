(defpackage #:harness-demos-zed-sample
  (:use #:cl)
  (:local-nicknames (#:acp #:acp-protocol)
                    (#:stdio #:acp-backend-stdio)
                    (#:demo #:harness-demos-acp))
  (:export #:rehearse))
