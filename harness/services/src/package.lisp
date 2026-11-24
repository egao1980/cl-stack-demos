(defpackage #:harness-demos-services
  (:use #:cl)
  (:local-nicknames (#:cache #:cache-protocol)
                    (#:redis #:cache-backend-redis)
                    (#:mime #:mime-protocol)
                    (#:mq #:mq-protocol)
                    (#:amqp #:mq-backend-amqp)
                    (#:kafka #:mq-backend-kafka))
  (:export #:run-demos))
