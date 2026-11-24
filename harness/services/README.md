# Services demo: cache, Redis, mail, AMQP, Kafka

`harness-demos-services` shows the services wave and the fixes from its adversarial review rounds that closed the gaps the [Python to Common Lisp](https://egao1980.github.io/python-to-cl/) pages linked: redis-py, cachetools, `email` + `smtplib`, pika and confluent_kafka jobs, done with cl-stack packages. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run.

| Section | Package | Shows |
|---|---|---|
| 1 | cache-protocol 0.2.0 | `:ttl 0.3` gone after 0.35 s (sub-second monotonic clock), a rate-limit counter whose `incr` keeps the TTL, 8 threads × 5000 `cache-incr` = 40000, 8 threads racing `cache-cas` with wins equal to the value, `cache-error` for `"1-2"` |
| 2 | cache-backend-redis 0.2.0 | `with-redis-cache :url "redis://:pw@host:port/3"` (HELLO 3 AUTH, SELECT), `:ttl 1.5` as PX, CAS create-if-absent and its mismatch, the server's error text for `INCR` of `"abc"`, `close`, a wrong password (`WRONGPASS`), a refused port (`cache-connection-error` with the usocket cause), `rediss://` against a TLS Valkey |
| 3 | mail-protocol 0.3.0 + mail-backend-smtp 0.3.0 | **Left out of this run.** 0.3.0 is merged ([mail-protocol#6](https://github.com/egao1980/mail-protocol/pull/6)) but not on GHCR yet, which still has 0.2.0 (no AUTH / STARTTLS, no `add-alternative`). The section returns when 0.3.0 is published: its code is in [`src/mail.lisp`](src/mail.lisp), outside the build, and the file's header lists the four steps that bring it back (pins, nicknames, two calls, Mailpit) |
| 4 | mq-backend-amqp 0.2.0 | `with-amqp-backend :url` against RabbitMQ 3.13.7 (negotiated heartbeat / frame-max / channel-max), a server-named queue, publish + `poll-messages` (Basic.Get) + `ack`, `nack :requeue t` → redelivered, a missing queue → `amqp-channel-closed` 404 and the channel reopening, a wrong password → `amqp-connection-closed` 403, refused → `amqp-transport-error`, `amqps://` |
| 5 | mq-backend-kafka 0.2.1 | librdkafka 2.16.0 from the native overlay, produce + `kafka-flush`, three members of one group: member 1 acks m1, member 2 resumes at m2 without acking, member 3 gets m2 again; `nack :requeue t` → redelivered |
| 6 | review fixes | what the adversarial review rounds broke, now passing: the default clock steps by 1 ns (`+monotonic-tick+`) and a 5 ms TTL is cut short in 0 of 200 puts; a NaN TTL is a `cache-error`; a Redis URL with an unescaped `/` in the password is refused with the password masked, and the escaped one parses; mime-protocol 0.3.2 refuses CR LF in a header value (`mime-header-error`, mime-protocol#12) and writes a non-ASCII Subject as an RFC 2047 encoded-word; an AMQP poll returns only its own queue, a handler-case that unwinds a poll on `amqp-consumer-cancelled` does not lose the message; Kafka acks out of order (a2, then a1) and the next member resumes at a3; a Kafka `publish` while another thread polls returns at once (0.2.1, mq-backend-kafka#5). The mail lines of this section (Message-IDs across a fork, CRLF-only DATA) wait with section 3 in `src/mail.lisp` |

## Run

```bash
cd harness/services
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-services")' \
  --eval '(harness-demos-services:run-demos)' > demo-output.txt 2>&1
```

The servers (each section says SKIP when its port is closed). `./make-certs.sh` writes `certs/`: a test CA (`ca.pem`), a certificate for 127.0.0.1 signed by it (`cert.pem`, `key.pem`), the Mailpit `auth.txt` and the TLS-only `rabbitmq.conf`. `DEMO_CA=certs/ca.pem` makes cl+ssl trust the CA.

```bash
docker run -d -p 127.0.0.1:16390:6379 valkey/valkey:8 valkey-server --save "" --requirepass s3cret
docker run -d -p 127.0.0.1:16391:6379 -v $PWD/certs:/certs:ro valkey/valkey:8 valkey-server --save "" \
  --requirepass s3cret --port 0 --tls-port 6379 --tls-cert-file /certs/cert.pem \
  --tls-key-file /certs/key.pem --tls-ca-cert-file /certs/ca.pem --tls-auth-clients no
docker run -d -p 127.0.0.1:15673:5672 rabbitmq:3.13.7-alpine
docker run -d -p 127.0.0.1:15674:5671 -v $PWD/certs:/certs:ro \
  -e RABBITMQ_CONFIG_FILE=/certs/rabbitmq.conf rabbitmq:3.13.7-alpine
docker run -d -p 127.0.0.1:19094:9092 -e KAFKA_NODE_ID=1 -e KAFKA_PROCESS_ROLES=broker,controller \
  -e KAFKA_LISTENERS=PLAINTEXT://:9092,CONTROLLER://:9093 \
  -e KAFKA_ADVERTISED_LISTENERS=PLAINTEXT://127.0.0.1:19094 \
  -e KAFKA_CONTROLLER_LISTENER_NAMES=CONTROLLER \
  -e KAFKA_LISTENER_SECURITY_PROTOCOL_MAP=CONTROLLER:PLAINTEXT,PLAINTEXT:PLAINTEXT \
  -e KAFKA_CONTROLLER_QUORUM_VOTERS=1@localhost:9093 -e KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR=1 \
  -e KAFKA_GROUP_INITIAL_REBALANCE_DELAY_MS=0 apache/kafka-native:3.9.0
```

Section 3, once it is back in the build, also needs Mailpit:

```bash
docker run -d -p 127.0.0.1:11026:1025 -v $PWD/certs:/certs:ro axllent/mailpit \
  --smtp-auth-file /certs/auth.txt --smtp-auth-allow-insecure
docker run -d -p 127.0.0.1:11027:1025 -v $PWD/certs:/certs:ro axllent/mailpit \
  --smtp-tls-cert /certs/cert.pem --smtp-tls-key /certs/key.pem --smtp-require-starttls
docker run -d -p 127.0.0.1:11028:1025 -v $PWD/certs:/certs:ro axllent/mailpit \
  --smtp-tls-cert /certs/cert.pem --smtp-tls-key /certs/key.pem --smtp-require-tls
```

Endpoints are overridable: `DEMO_REDIS_URL`, `DEMO_REDIS_TLS_URL`, `DEMO_AMQP_URL`, `DEMO_AMQPS_URL`, `DEMO_KAFKA_BOOTSTRAP`, and for section 3 `DEMO_SMTP_HOST`, `DEMO_SMTP_AUTH_PORT` / `_STARTTLS_PORT` / `_TLS_PORT`, `DEMO_SMTP_USER` / `_PASSWORD`.

The committed output ran on 2026-11-24 (SBCL 2.6.9, macOS arm64) with `cl-repo run` (client 0.27.39) against GHCR: cache-protocol 0.2.0, cache-backend-redis 0.2.0, mime-protocol 0.3.2, mq-protocol 0.1.0, mq-backend-amqp 0.2.0, mq-backend-kafka 0.2.1 (its darwin-arm64 librdkafka overlay), with encoding-protocol 0.3.0, binary-protocol 0.1.1 and quri 0.7.2 beneath them. The servers were Valkey 8 (plain and TLS), RabbitMQ 3.13.7 (plain and TLS) and apache/kafka-native 3.9.0, on other ports through the `DEMO_*` variables. Every section ran, none skipped.

On macOS a refused connect with a timeout reaches the backends as usocket's `invalid-argument-error`, not `connection-refused-error`. That is why the cause reads `INVALID-ARGUMENT-ERROR`. Each backend still reports it as its connection error.
