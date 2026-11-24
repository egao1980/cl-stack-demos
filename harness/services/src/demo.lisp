(in-package #:harness-demos-services)

;;; Five sections, one per package of the services wave. Each live step
;;; needs its server (Docker lines in README.md) and says SKIP without it.

(defun env (name default)
  (let ((v (uiop:getenv name)))
    (if (and v (plusp (length v))) v default)))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun reachable-p (host port)
  (let ((s (handler-case (usocket:socket-connect host port :timeout 2)
             (usocket:socket-error () nil))))
    (when s (usocket:socket-close s) t)))

(defun url-endpoint (url)
  "(values host port) of URL."
  (let ((uri (quri:uri url)))
    (values (quri:uri-host uri) (quri:uri-port uri))))

(defun text (octets)
  (if (stringp octets) octets (map 'string #'code-char octets)))

(defvar *ca-loaded* nil)

(defun trust-demo-ca ()
  "Add DEMO_CA (the test CA of the TLS servers) to cl+ssl's trust store."
  (let ((ca (uiop:getenv "DEMO_CA")))
    (when (and ca (probe-file ca))
      (unless *ca-loaded*
        (cl+ssl:ssl-load-global-verify-locations ca)
        (setf *ca-loaded* t))
      t)))

(defmacro live ((label host port) &body body)
  `(if (reachable-p ,host ,port)
       (progn ,@body)
       (format t "~&  SKIP ~a: nothing on ~a:~a~%" ,label ,host ,port)))

;;; --- 1. in-memory TTL cache ------------------------------------------------

(defun demo-memory-cache ()
  (banner "1. cache-protocol 0.2.0: in-memory TTL cache (sub-second, atomic)")
  (let ((c (cache:make-in-memory-ttl-cache)))
    (cache:cache-put c "session" "alice" :ttl 0.3)
    (format t "  put :ttl 0.3, read at once   -> ~s~%" (cache:cache-get c "session"))
    (sleep 0.35)
    (format t "  read 0.35 s later            -> ~s~%" (cache:cache-get c "session")))
  (let* ((now 1000)
         (c (cache:make-in-memory-ttl-cache :clock (lambda () now))))
    (cache:cache-put c "hits" 1 :ttl 60)
    (cache:cache-incr c "hits")
    (setf now 1059)
    (format t "  rate-limit counter: incr at t+0, t+59 -> ~a (TTL kept by incr)~%"
            (cache:cache-incr c "hits"))
    (setf now 1060)
    (format t "  at t+60 the window is over      -> ~s~%" (cache:cache-get c "hits")))
  (let* ((c (cache:make-in-memory-ttl-cache))
         (threads (loop repeat 8
                        collect (bt2:make-thread
                                 (lambda () (dotimes (i 5000) (cache:cache-incr c "n")))))))
    (mapc #'bt2:join-thread threads)
    (format t "  8 threads x 5000 cache-incr  -> ~a (none lost)~%" (cache:cache-get c "n")))
  (let* ((c (cache:make-in-memory-ttl-cache))
         (wins 0)
         (lock (bt2:make-lock)))
    (cache:cache-put c "k" 0)
    (mapc #'bt2:join-thread
          (loop repeat 8
                collect (bt2:make-thread
                         (lambda ()
                           (dotimes (i 2000)
                             (let ((seen (cache:cache-get c "k")))
                               (handler-case (progn (cache:cache-cas c "k" seen (1+ seen))
                                               (bt2:with-lock-held (lock) (incf wins)))
                                 (cache:cache-cas-mismatch () nil))))))))
    (format t "  8 threads racing cas N -> N+1: ~a wins, value ~a (equal: no double winner)~%"
            wins (cache:cache-get c "k")))
  (let ((c (cache:make-in-memory-ttl-cache)))
    (cache:cache-put c "x" "1-2")
    (format t "  cache-incr on \"1-2\" -> ~a~%"
            (handler-case (cache:cache-incr c "x")
              (cache:cache-error (e) (format nil "~(~a~): ~a" (type-of e) e))))))

;;; --- 2. Redis / Valkey -----------------------------------------------------

(defun demo-redis ()
  (banner "2. cache-backend-redis 0.2.0: connect by URL, AUTH, SELECT, TLS")
  (let ((url (env "DEMO_REDIS_URL" "redis://:s3cret@127.0.0.1:16390/3")))
    (multiple-value-bind (host port) (url-endpoint url)
      (live ("Valkey" host port)
        (redis:with-redis-cache (c :url url :client-name "harness-demo")
          (format t "  backend: ~a~%" c)
          (cache:cache-put c "demo:greeting" "hello" :ttl 1.5)
          (format t "  put :ttl 1.5 (sent as PX 1500), get -> ~s, connected ~a~%"
                  (cache:cache-get c "demo:greeting") (redis:redis-cache-backend-connected-p c))
          (cache:cache-delete c "demo:lock")
          (format t "  cas nil -> \"owner-1\" (create if absent) -> ~s~%"
                  (cache:cache-cas c "demo:lock" nil "owner-1" :ttl 30))
          (format t "  second create-if-absent -> ~a~%"
                  (handler-case (cache:cache-cas c "demo:lock" nil "owner-2")
                    (cache:cache-cas-mismatch (e)
                      (format nil "cache-cas-mismatch, actual ~s" (cache:cache-cas-mismatch-actual e)))))
          (cache:cache-put c "demo:text" "abc")
          (format t "  incr of \"abc\" -> ~a~%"
                  (handler-case (cache:cache-incr c "demo:text")
                    (cache:cache-error (e) (format nil "cache-error: ~a" (cache:cache-error-message e)))))
          (format t "  close -> ~a, connected ~a~%" (close c) (redis:redis-cache-backend-connected-p c)))
        (let ((e (handler-case (progn (cache:cache-get (redis:make-redis-cache-backend
                                                        :url (format nil "redis://:wrong@~a:~d" host port))
                                                       "k")
                                      nil)
                   (cache:cache-error (e) e))))
          (format t "  wrong password -> ~a: ~a~%" (type-of e) e)))))
  (let ((e (handler-case (progn (cache:cache-get (redis:make-redis-cache-backend :host "127.0.0.1" :port 1) "k") nil)
             (cache:cache-connection-error (e) e))))
    (format t "  refused 127.0.0.1:1 -> ~a, cause ~a~%"
            (type-of e) (type-of (cache:cache-connection-error-cause e))))
  (let ((url (env "DEMO_REDIS_TLS_URL" "rediss://:s3cret@127.0.0.1:16391/2")))
    (multiple-value-bind (host port) (url-endpoint url)
      (cond
        ((not (trust-demo-ca)) (format t "  SKIP rediss://: DEMO_CA not set~%"))
        (t (live ("Valkey TLS" host port)
             (redis:with-redis-cache (c :url url)
               (cache:cache-put c "demo:tls" "over TLS")
               (format t "  ~a: get -> ~s~%" url (cache:cache-get c "demo:tls")))))))))

;;; --- 3. mail ---------------------------------------------------------------

;;; --- 4. AMQP ---------------------------------------------------------------

(defun demo-amqp ()
  (banner "4. mq-backend-amqp 0.2.0: handshake, heartbeats, Basic.Get, broker Close")
  (let ((url (env "DEMO_AMQP_URL" "amqp://guest:guest@127.0.0.1:15673/%2f")))
    (multiple-value-bind (host port) (url-endpoint url)
      (live ("RabbitMQ" host port)
        (amqp:with-amqp-backend (b :url url :heartbeat 10)
          (let ((conn (amqp:amqp-backend-connection b)))
            (format t "  connected to ~a ~a: heartbeat ~ds, frame-max ~d, channel-max ~d~%"
                    (cdr (assoc "product" (amqp:amqp-connection-server-properties conn) :test #'equal))
                    (cdr (assoc "version" (amqp:amqp-connection-server-properties conn) :test #'equal))
                    (amqp:amqp-connection-heartbeat conn) (amqp:amqp-connection-frame-max conn)
                    (amqp:amqp-connection-channel-max conn)))
          (let ((q (amqp:amqp-declare-queue b "" :exclusive t :auto-delete t)))
            (format t "  queue.declare-ok: ~a~%" q)
            (dolist (p '("order-1" "order-2"))
              (mq:publish b q (mq:make-mq-message :key p :payload p)))
            (let ((msgs (mq:poll-messages b q :n 2 :timeout 2)))
              (format t "  poll-messages (Basic.Get) -> ~{~s~^, ~} (tags ~{~a~^, ~})~%"
                      (mapcar (lambda (m) (text (mq:mq-message-payload m))) msgs)
                      (mapcar #'mq:mq-message-delivery-tag msgs))
              (mq:ack b (first msgs))
              (mq:nack b (second msgs) :requeue t))
            (let ((again (first (mq:poll-messages b q :timeout 2))))
              (format t "  after nack :requeue t -> ~s redelivered ~a~%"
                      (text (mq:mq-message-payload again)) (mq:mq-message-redelivered-p again))
              (mq:ack b again)))
          (let ((e (handler-case (progn (mq:poll-messages b "no-such-queue") nil)
                     (amqp:amqp-channel-closed (e) e))))
            (format t "  Basic.Get on a missing queue -> ~a ~a ~s~%"
                    (type-of e) (amqp:amqp-reply-code e) (amqp:amqp-reply-text e)))
          (format t "  the channel reopens: declare -> ~a~%"
                  (amqp:amqp-declare-queue b "" :exclusive t :auto-delete t)))
        (let ((e (handler-case (progn (close (amqp:connect-amqp-backend :host host :port port
                                                                        :user "guest" :password "wrong"))
                                      nil)
                   (mq:mq-connection-error (e) e))))
          (format t "  wrong password -> ~a ~@[~a ~]~a~%" (type-of e)
                  (and (typep e 'amqp:amqp-connection-closed) (amqp:amqp-reply-code e)) e)))))
  (let ((e (handler-case (progn (close (amqp:connect-amqp-backend :port 1)) nil)
             (mq:mq-connection-error (e) e))))
    (format t "  refused 127.0.0.1:1 -> ~a, cause ~a~%"
            (type-of e) (type-of (amqp:amqp-transport-error-cause e))))
  (let ((url (env "DEMO_AMQPS_URL" "amqps://guest:guest@127.0.0.1:15674/%2f")))
    (multiple-value-bind (host port) (url-endpoint url)
      (cond
        ((not (trust-demo-ca)) (format t "  SKIP amqps://: DEMO_CA not set~%"))
        (t (live ("RabbitMQ TLS" host port)
             (amqp:with-amqp-backend (b :url url)
               (let ((q (amqp:amqp-declare-queue b "" :exclusive t :auto-delete t)))
                 (mq:publish b q (mq:make-mq-message :payload "over TLS"))
                 (format t "  ~a: round trip -> ~s~%" url
                         (text (mq:mq-message-payload (first (mq:poll-messages b q :timeout 2)))))))))))))

;;; --- 5. Kafka --------------------------------------------------------------

(defun demo-kafka ()
  (banner "5. mq-backend-kafka 0.2.1: librdkafka overlay, consumer groups, ack commits")
  (format t "  librdkafka ~a loaded at ASDF load~%" (kafka:librdkafka-version))
  (let* ((bootstrap (env "DEMO_KAFKA_BOOTSTRAP" "127.0.0.1:19094"))
         (colon (position #\: bootstrap :from-end t))
         (host (subseq bootstrap 0 colon))
         (port (parse-integer bootstrap :start (1+ colon))))
    (live ("Kafka" host port)
      (let ((topic (format nil "demo-orders-~d" (get-universal-time)))
            (group (format nil "demo-workers-~d" (get-universal-time))))
        (let ((p (kafka:make-kafka-backend :bootstrap-servers bootstrap)))
          (unwind-protect
               (progn
                 (dolist (s '("m1" "m2"))
                   (mq:publish p topic (mq:make-mq-message :key "customer-7" :payload s
                                                           :headers '(("source" . "demo")))))
                 (format t "  produced m1, m2 to ~a; flush -> ~a~%" topic (kafka:kafka-flush p)))
            (close p)))
        (flet ((member-run (label ack)
                 (let ((b (kafka:make-kafka-backend :bootstrap-servers bootstrap :group-id group)))
                   (unwind-protect
                        (let ((m (first (mq:poll-messages b topic :timeout 20))))
                          (format t "  ~a (group ~a) gets ~s key ~s headers ~s~@[ -> ack~*~]~%"
                                  label group (and m (text (mq:mq-message-payload m)))
                                  (and m (mq:mq-message-key m)) (and m (mq:mq-message-headers m))
                                  (and m ack))
                          (when (and m ack) (mq:ack b m)))
                     (close b)))))
          (member-run "member 1" t)
          (member-run "member 2" nil)
          (member-run "member 3" nil))
        (let ((b (kafka:make-kafka-backend :bootstrap-servers bootstrap)))
          (unwind-protect
               (let ((m (first (mq:poll-messages b topic :timeout 20))))
                 (mq:nack b m :requeue t)
                 (let ((again (first (mq:poll-messages b topic :timeout 20))))
                   (format t "  new group: ~s, nack :requeue t -> ~s redelivered ~a~%"
                           (text (mq:mq-message-payload m)) (text (mq:mq-message-payload again))
                           (mq:mq-message-redelivered-p again))))
            (close b)))))))

(defun ttl-cut-short-count (c ttl tries)
  "Puts of TTL seconds on C whose entry was gone before TTL had elapsed,
measured with the cache's own default clock read before the put."
  (loop repeat tries
        count (let ((t0 (cache:monotonic-seconds)))
                (cache:cache-put c "k" "v" :ttl ttl)
                (loop
                  (let ((present (cache:cache-get c "k"))
                        (elapsed (- (cache:monotonic-seconds) t0)))
                    (when (>= elapsed (* 98/100 ttl)) (return nil))
                    (unless present (return t)))))))

(defun demo-review-fixes ()
  (banner "6. Review fixes: the cases the adversarial rounds broke, now passing")
  ;; cache-protocol #1: the default clock was CLOCK_MONOTONIC_COARSE on Linux.
  (format t "  cache: +monotonic-tick+ ~a s; :ttl 1/200 cut short in ~d of 200 puts~%"
          cache:+monotonic-tick+
          (ttl-cut-short-count (cache:make-in-memory-ttl-cache) 1/200 200))
  (format t "  cache: :ttl NaN -> ~a~%"
          (handler-case (progn (cache:cache-put (cache:make-in-memory-ttl-cache) "k" "v"
                                                :ttl (sb-int:with-float-traps-masked (:invalid)
                                                       (- sb-ext:double-float-positive-infinity
                                                          sb-ext:double-float-positive-infinity)))
                          "stored")
            (cache:cache-error (e) (format nil "~(~a~): ~a" (type-of e) e))))
  ;; cache-backend-redis #7: an unescaped / in a password was printed in full.
  (format t "  redis: ~a~%"
          (handler-case (redis:parse-redis-url "redis://:s3cret/x+y==@cache.internal:6379/0")
            (cache:cache-error (e) (princ-to-string e))))
  (format t "  redis: escaped -> :password ~s~%"
          (getf (redis:parse-redis-url "redis://:s3cret%2Fx%2By%3D%3D@cache.internal:6379/0")
                :password))
  ;; mime-protocol #12 (0.3.2): CR LF in a header value would forge a field.
  ;; The mail lines of this section (Message-IDs across fork, DATA) are in
  ;; mail.lisp until mail-protocol 0.3.0 is published.
  (format t "  mime: Subject \"Hi\\r\\nBcc: x@evil\" -> ~a~%"
          (handler-case (progn (mime:set-header (mime:make-text-entity "hi") "Subject"
                                                (format nil "Hi~C~CBcc: x@evil" #\Return #\Newline))
                          "set")
            (mime:mime-header-error (c) (format nil "~(~a~): ~a" (type-of c) c))))
  (format t "  mime: RFC 2047 Subject ~s -> ~a~%"
          (format nil "Caf~C report" (code-char #xE9))
          (mime:encode-header-text (format nil "Caf~C report" (code-char #xE9))))
  ;; mq-backend-amqp #2: a handler that unwinds on consumer-cancelled; polls per queue.
  (let ((url (env "DEMO_AMQP_URL" "amqp://guest:guest@127.0.0.1:15673/%2f")))
    (multiple-value-bind (host port) (url-endpoint url)
      (live ("RabbitMQ" host port)
            (amqp:with-amqp-backend (b :url url)
              (amqp:with-amqp-backend (admin :url url)
                (let ((qa (format nil "demo-cancel-a-~d" (get-universal-time)))
                      (qb (amqp:amqp-declare-queue b "" :exclusive t :auto-delete t))
                      (qc (amqp:amqp-declare-queue b "" :exclusive t :auto-delete t)))
                  (amqp:amqp-declare-queue b qa)
                  (mq:subscribe b qc (lambda (m) (declare (ignore m))))
                  (mq:publish b qc (mq:make-mq-message :payload "for-c"))
                  (mq:publish b qb (mq:make-mq-message :payload "for-b"))
                  (sleep 0.3)
                  (format t "  amqp: consumer on C; poll B -> ~s~%"
                          (mapcar (lambda (m) (text (mq:mq-message-payload m)))
                                  (mq:poll-messages b qb :timeout 1)))
                  (mq:publish b qb (mq:make-mq-message :payload "for-b-2"))
                  (mq:subscribe b qa (lambda (m) (declare (ignore m))))
                  (amqp:amqp-rpc (amqp:amqp-backend-connection admin)
                                 :queue-delete '(:queue-delete-ok)
                                 :channel 1 :reserved-1 0 :queue qa
                                 :if-unused nil :if-empty nil :no-wait nil)
                  (sleep 0.3)
                  (format t "  amqp: A deleted; handler-case around poll B -> ~s~%"
                          (handler-case (progn (mq:poll-messages b qb :timeout 1) :returned)
                            (amqp:amqp-consumer-cancelled (c)
                              (format nil "~(~a~) ~a" (type-of c) (amqp:amqp-cancelled-consumer-tag c)))))
                  (format t "  amqp: next poll B -> ~s, poll C -> ~s~%"
                          (mapcar (lambda (m) (text (mq:mq-message-payload m)))
                                  (mq:poll-messages b qb :timeout 1))
                          (mapcar (lambda (m) (text (mq:mq-message-payload m)))
                                  (mq:poll-messages b qc :timeout 1)))))))))
  ;; mq-backend-kafka #1: acks out of order.
  (let* ((bootstrap (env "DEMO_KAFKA_BOOTSTRAP" "127.0.0.1:19094"))
         (colon (position #\: bootstrap :from-end t)))
    (live ("Kafka" (subseq bootstrap 0 colon) (parse-integer bootstrap :start (1+ colon)))
          (let ((topic (format nil "demo-ooo-~d" (get-universal-time)))
                (group (format nil "demo-ooo-workers-~d" (get-universal-time))))
            (let ((p (kafka:make-kafka-backend :bootstrap-servers bootstrap)))
              (unwind-protect
                  (dolist (s '("a1" "a2" "a3"))
                    (mq:publish p topic (mq:make-mq-message :payload s)))
                (close p)))
            (let ((b (kafka:make-kafka-backend :bootstrap-servers bootstrap :group-id group)))
              (unwind-protect
                  (let ((got (mq:poll-messages b topic :n 2 :timeout 20)))
                    (mq:ack b (second got))
                    (mq:ack b (first got))
                    (format t "  kafka: member 1 gets ~s, acks a2 then a1~%"
                            (mapcar (lambda (m) (text (mq:mq-message-payload m))) got)))
                (close b)))
            (let ((b (kafka:make-kafka-backend :bootstrap-servers bootstrap :group-id group)))
              (unwind-protect
                  (let ((got (mq:poll-messages b topic :n 3 :timeout 10)))
                    (format t "  kafka: member 2 resumes at ~s~%"
                            (mapcar (lambda (m) (text (mq:mq-message-payload m))) got))
                    (dolist (m got) (mq:ack b m)))
                (close b))))
          ;; mq-backend-kafka #5 (0.2.1): a poll held the session lock for its
          ;; whole timeout, so a publish on another thread waited for it.
          (let ((topic (format nil "demo-lock-~d" (get-universal-time)))
                (b (kafka:make-kafka-backend :bootstrap-servers bootstrap
                                             :group-id (format nil "demo-lock-workers-~d"
                                                               (get-universal-time)))))
            (unwind-protect
                (progn
                  (mq:publish b topic (mq:make-mq-message :payload "l1"))
                  (kafka:kafka-flush b)
                  (let ((poller (bt2:make-thread
                                 (lambda () (mq:poll-messages b topic :n 2 :timeout 20)))))
                    (sleep 1)
                    (let ((t0 (get-internal-real-time)))
                      (mq:publish b topic (mq:make-mq-message :payload "l2"))
                      (format t "  kafka: publish while another thread polls (:timeout 20) -> ~d ms~%"
                              (round (* 1000 (- (get-internal-real-time) t0))
                                     internal-time-units-per-second)))
                    (let ((got (bt2:join-thread poller)))
                      (format t "  kafka: that poll gets ~s~%"
                              (mapcar (lambda (m) (text (mq:mq-message-payload m))) got))
                      (dolist (m got) (mq:ack b m)))))
              (close b))))))

(defun run-demos ()
  (let ((start (get-internal-real-time))
        (*print-pretty* nil))
    (demo-memory-cache)
    (demo-redis)
    (format t "~&~%== 3. mail-protocol 0.3.0: left out until it is on GHCR (src/mail.lisp)~%")
    (demo-amqp)
    (demo-kafka)
    (demo-review-fixes)
    (format t "~&~%Done in ~,1f s.~%"
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
