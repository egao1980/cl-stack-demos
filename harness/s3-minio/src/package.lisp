(defpackage #:harness-demos-s3-minio
  (:use #:cl)
  (:local-nicknames (#:s3 #:object-store-backend-s3)
                    (#:os #:object-store-protocol)
                    (#:cfg #:cl-stack-config)
                    (#:proc #:process-protocol))
  (:export #:run-demos))
