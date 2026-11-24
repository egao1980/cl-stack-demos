(defsystem "harness-demos-services"
  :version "0.1.0"
  :description "Demos of the services wave: cache-protocol 0.2.0 (sub-second TTL, atomic incr / cas), cache-backend-redis 0.2.0 (connect by URL, AUTH, SELECT, TLS, reconnect, error replies, CAS create-if-absent), mq-backend-amqp 0.2.0 (handshake, heartbeats, Basic.Get, broker Close conditions), mq-backend-kafka 0.2.1 (librdkafka overlays, consumer groups, ack commits, polls without the session lock), mime-protocol 0.3.2 (header injection refused, RFC 2047), and the fixes from the adversarial review rounds. The mail section (src/mail.lisp) waits for mail-protocol 0.3.0 on GHCR."
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cache-protocol" "0.2.0")
               (:version "cache-backend-redis" "0.2.0")
               (:version "mime-protocol" "0.3.2")
               (:version "mq-protocol" "0.1.0")
               (:version "mq-backend-amqp" "0.2.0")
               (:version "mq-backend-kafka" "0.2.1")
               "bordeaux-threads"
               "usocket"
               "quri")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
