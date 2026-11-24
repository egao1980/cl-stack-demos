(defsystem "harness-demos-s3-minio"
  :version "0.1.0"
  :description "Demo of object-store-backend-s3 0.2.1 (0.2.0 + head-object size, #10) and http-backend-dexador 0.2.5 (#36) against a live MinIO container, with cl-stack-config 0.3.1's parse-ini (cl-stack-config#10): credentials from the environment, and from an ~/.aws-style credentials file whose secret holds # and ; and whose key names are upper-case, read as botocore reads it (cross-read with Python's configparser); the 0.3.0 reading's secret refused as SignatureDoesNotMatch; put / get / head / list, a presigned GET (#7), paged listings, a multipart upload of a 5 MiB part and a tail, MinIO's EntityTooSmall and NoSuchUpload as conditions, abort, and delete"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "object-store-backend-s3" "0.2.1")
               (:version "cl-stack-config" "0.3.1")
               (:version "cl-stack-http" "0.2.0")
               ;; blocking :auto requests go through dexador; 0.2.5 sends the
               ;; signed Content-Type of a bodyless POST (#36)
               (:version "http-backend-dexador" "0.2.5")
               "http-protocol"
               "encoding-protocol"
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
