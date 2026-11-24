(defpackage #:harness-demos-pdf-render
  (:use #:cl)
  (:local-nicknames (#:pdf #:doc-extract-backend-pdf)
                    (#:dx #:doc-extract-protocol)
                    (#:img #:image-protocol)
                    (#:crypto #:crypto-protocol))
  (:export #:run-demos))
