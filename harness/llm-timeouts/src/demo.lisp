(in-package #:harness-demos-llm-timeouts)

;;; Model-request timeouts (llm-protocol-openai#13, cl-stack-code#19).
;;; Before: every request had a fixed :timeout 180, which http-backend-async
;;; enforced as a wall-clock cap, so a long answer from a slow local model
;;; died mid-stream ("sync await timed out"). Now: :request-timeout caps a
;;; non-streaming call, :stream-idle-timeout bounds the silence on a stream
;;; (no total cap), :connect-timeout bounds setup; env vars fill them in.
;;; Demos 1-5 use a local origin with sub-second pacing (seconds stand in
;;; for minutes); demo 6 asks a live slow model for a long answer.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title "")
  (finish-output))

(defun note (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))

(defun elapsed (t0)
  (/ (- (get-internal-real-time) t0) internal-time-units-per-second))

(defmacro with-http (&body body)
  "http-backend-async x libuv, the stack's HTTP client."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb))
          (http-backend-async:*event-backend-maker* (lambda () eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         (let ((hp:*http-backend* (http-backend-async:make-async-backend)))
           ,@body)))))

;;; A loopback origin: reads one request, then answers with RESPOND.

(defun %read-line* (stream)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
          while b
          do (vector-push-extend b out)
             (when (and (>= (length out) 2) (= 13 (aref out (- (length out) 2)))
                        (= 10 (aref out (1- (length out)))))
               (return)))
    (and (plusp (length out))
         (string-right-trim '(#\Return #\Newline) (babel:octets-to-string out :errorp nil)))))

(defun %read-request (stream)
  (let ((len 0))
    (when (%read-line* stream)
      (loop for h = (%read-line* stream)
            while (and h (plusp (length h)))
            do (let ((c (position #\: h)))
                 (when (and c (string-equal "content-length" (subseq h 0 c)))
                   (setf len (or (parse-integer h :start (1+ c) :junk-allowed t) 0)))))
      (when (plusp len)
        (read-sequence (make-array len :element-type '(unsigned-byte 8)) stream))
      t)))

(defun %out (stream string)
  (write-sequence (babel:string-to-octets string) stream)
  (force-output stream))

(defun %crlf (&rest lines)
  (format nil "~{~A~C~C~}" (loop for l in lines collect l collect #\Return collect #\Newline)))

(defun %sse (stream steps)
  "Chunked SSE; STEPS are (delay . text); a NIL text stalls DELAY s, then hangs up."
  (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                      "Transfer-Encoding: chunked" "Connection: close" ""))
  (dolist (s steps)
    (sleep (car s))
    (unless (cdr s) (return-from %sse))
    (let ((o (babel:string-to-octets (cdr s))))
      (%out stream (format nil "~X~C~C" (length o) #\Return #\Newline))
      (write-sequence o stream)
      (%out stream (%crlf ""))))
  (%out stream (%crlf "0" "")))

(defun %json (stream delay body)
  (sleep delay)
  (let ((o (babel:string-to-octets body)))
    (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: application/json"
                        (format nil "Content-Length: ~D" (length o)) "Connection: close" ""))
    (write-sequence o stream)
    (force-output stream)))

(defun call-with-origin (respond fn)
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t
                                                      :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (thread (bt:make-thread
                  (lambda ()
                    (loop (handler-case
                              (let ((c (usocket:socket-accept server :element-type '(unsigned-byte 8))))
                                (bt:make-thread
                                 (lambda ()
                                   (unwind-protect
                                        (ignore-errors
                                         (let ((s (usocket:socket-stream c)))
                                           (when (%read-request s) (funcall respond s))))
                                     (ignore-errors (usocket:socket-close c))))))
                            (error () (return)))))
                  :name "demo-origin")))
    (unwind-protect (funcall fn (format nil "http://127.0.0.1:~D/v1" port))
      (ignore-errors (usocket:socket-close server))
      (when (bt:thread-alive-p thread) (ignore-errors (bt:destroy-thread thread))))))

(defmacro with-origin ((base respond) &body body)
  `(call-with-origin ,respond (lambda (,base) (with-http ,@body))))

(defun %chat-chunk (&key content finish)
  (format nil "data: {\"model\":\"local\",\"choices\":[{\"index\":0,\"finish_reason\":~:[null~;~:*\"~a\"~],\"delta\":{~@[\"content\":\"~a\"~]}}]}~%~%"
          finish content))

(defun %tokens (n delay)
  (append (loop for i below n collect (cons delay (%chat-chunk :content (format nil "t~D " i))))
          (list (cons 0 (%chat-chunk :finish "stop")) (cons 0 (format nil "data: [DONE]~%~%")))))

(defun %backend (base &rest args)
  (apply #'oa:make-openai-compat-backend :base-url base :api-key :none
         :default-model "local" args))

(defun %settings (b)
  (format nil ":request-timeout ~a  :stream-idle-timeout ~a  :connect-timeout ~a"
          (oa:openai-request-timeout b) (oa:openai-stream-idle-timeout b)
          (oa:openai-connect-timeout b)))

(defun %env (name value)
  (setf (uiop:getenv name) value))

;;; Demos

(defun demo-1-settings ()
  (banner "1. Settings: defaults, initargs, env vars, and what goes on the wire")
  (let ((saved (mapcar (lambda (n) (cons n (or (uiop:getenv n) "")))
                       '("OPENAI_TIMEOUT" "OPENAI_STREAM_IDLE_TIMEOUT" "OPENAI_CONNECT_TIMEOUT"))))
    (unwind-protect
         (progn
           (dolist (s saved) (%env (car s) ""))
           (let ((b (oa:make-openai-compat-backend)))
             (note "defaults:            ~a" (%settings b))
             (note "  stream request  -> ~s" (oa::%request-timeout-spec b t))
             (note "  plain request   -> ~s" (oa::%request-timeout-spec b nil)))
           (note "initargs:            ~a"
                 (%settings (oa:make-openai-compat-backend :request-timeout 1800
                                                           :stream-idle-timeout nil)))
           (%env "OPENAI_TIMEOUT" "1200")
           (%env "OPENAI_STREAM_IDLE_TIMEOUT" "900")
           (%env "OPENAI_CONNECT_TIMEOUT" "none")
           (note "OPENAI_TIMEOUT=1200 OPENAI_STREAM_IDLE_TIMEOUT=900 OPENAI_CONNECT_TIMEOUT=none:")
           (note "                     ~a" (%settings (oa:make-openai-compat-backend)))
           (note "  an initarg wins:   ~a"
                 (%settings (oa:make-openai-compat-backend :request-timeout 60)))
           (%env "OPENAI_TIMEOUT" "soon")
           (handler-case (oa:make-openai-compat-backend)
             (llm:llm-error (c) (note "OPENAI_TIMEOUT=soon -> ~a: ~a" (type-of c) c)))
           (note "anthropic defaults:  :request-timeout ~a  :stream-idle-timeout ~a  :connect-timeout ~a"
                 (an:anthropic-request-timeout (an:make-anthropic-backend :api-key :none))
                 (an:anthropic-stream-idle-timeout (an:make-anthropic-backend :api-key :none))
                 (an:anthropic-connect-timeout (an:make-anthropic-backend :api-key :none))))
      (dolist (s saved) (%env (car s) (cdr s))))))

(defun demo-2-steady-stream ()
  (banner "2. A slow but steady stream outlives the cap (24 tokens, 0.5 s apart = 12 s)")
  (with-origin (base (lambda (s) (%sse s (%tokens 24 0.5))))
    (let ((b (%backend base :request-timeout 3 :stream-idle-timeout 2))
          (t0 (get-internal-real-time))
          (seen 0))
      (note "backend: ~a" (%settings b))
      (note "(the old fixed cap applied here would be 3 s; the stream needs 12 s)")
      (let ((r (llm:stream-generate b "hi" :model "local"
                                    :on-part (lambda (p)
                                               (when (llm:llm-text-part-p p)
                                                 (incf seen)
                                                 (when (zerop (mod seen 6))
                                                   (note "  ~5,1f s  ~d tokens" (elapsed t0) seen)))))))
        (note "done after ~,1f s: finish ~s, text ~s" (elapsed t0)
              (llm:llm-response-finish-reason r) (llm:llm-response-text r))))))

(defun demo-3-silent-stream ()
  (banner "3. A stream that goes silent fails on the idle timeout")
  (with-origin (base (lambda (s) (%sse s (list (cons 0.2 (%chat-chunk :content "Hel"))
                                               (cons 0.2 (%chat-chunk :content "lo"))
                                               (cons 30 nil)))))
    (let ((b (%backend base :stream-idle-timeout 2))
          (t0 (get-internal-real-time)))
      (handler-case (progn (llm:stream-generate b "hi" :model "local")
                           (note "unexpected: the stream finished"))
        (hp:http-timeout-error (c)
          (note "after ~,1f s: ~a~%  ~a" (elapsed t0) (type-of c) c))))))

(defun demo-4-non-streaming ()
  (banner "4. Non-streaming: :request-timeout caps the whole call (the answer takes 4 s)")
  (let ((answer "{\"id\":\"c1\",\"model\":\"local\",\"choices\":[{\"index\":0,\"finish_reason\":\"stop\",\"message\":{\"role\":\"assistant\",\"content\":\"late but whole\"}}]}"))
    (with-origin (base (lambda (s) (%json s 4 answer)))
      (dolist (rt '(2 10 nil))
        (let ((t0 (get-internal-real-time)))
          (handler-case
              (let ((r (llm:generate (%backend base :request-timeout rt) "hi" :model "local")))
                (note ":request-timeout ~3a -> ~s after ~,1f s" rt (llm:llm-response-text r) (elapsed t0)))
            (hp:http-timeout-error (c)
              (note ":request-timeout ~3a -> ~a after ~,1f s" rt c (elapsed t0)))))))))

(defun demo-5-no-retry ()
  (banner "5. A timed-out model call is not sent again")
  (let ((n 0))
    (with-origin (base (lambda (s) (incf n) (%sse s (list (cons 0.1 (%chat-chunk :content "a")) (cons 30 nil)))))
      ;; a client that retries up to 3 times, timeouts included
      (let ((hp:*http-client* (hp:make-http-client hp:*http-backend*
                                                   :retry (hp:make-http-retry :total 3 :backoff-factor 0))))
        (handler-case (llm:stream-generate (%backend base :stream-idle-timeout 1) "hi" :model "local")
          (hp:http-timeout-error (c) (note "~a" c))))
      (note "requests the origin saw: ~d (a POST; http-retry retries only idempotent methods)" n))))

;;; Live: a long answer from a slow local model (HARNESS_DEMOS_BASE_URL).

(defun %live-env (name &optional default)
  (let ((v (uiop:getenv name)))
    (if (and v (plusp (length (string-trim " " v)))) (string-trim " " v) default)))

(defun %url-host (url)
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position #\/ url :start start) (length url))))
    (subseq url start end)))

(defun demo-6-live-long-answer ()
  (banner "6. Live: a long streamed answer on a slow local model, default timeouts")
  (let ((base (%live-env "HARNESS_DEMOS_BASE_URL"))
        (model (%live-env "HARNESS_DEMOS_MODEL"))
        (key (%live-env "HARNESS_DEMOS_API_KEY" :none))
        (max-tokens (parse-integer (%live-env "HARNESS_DEMOS_MAX_TOKENS" "2800"))))
    (unless (and base model)
      (note "skipped: set HARNESS_DEMOS_BASE_URL and HARNESS_DEMOS_MODEL")
      (return-from demo-6-live-long-answer))
    (with-http
      (let* ((b (oa:make-openai-compat-backend :base-url base :api-key key :default-model model))
             (t0 (get-internal-real-time))
             (chars 0) (parts 0) (first-at nil) (next-mark 60))
        (note "MODEL THAT RAN: ~a at ~a" model (%url-host base))
        (note "backend: ~a (defaults)" (%settings b))
        (let ((r (llm:stream-generate
                  b (list (llm:user-turn "Write a detailed, step-by-step tutorial (at least 2000 words) on writing a Common Lisp ASDF system with Rove tests. No preamble."))
                  :model model
                  :settings (llm:make-llm-settings :max-tokens max-tokens :temperature 0.7
                                                   :extra '(:chat-template-kwargs (:enable-thinking nil)))
                  :on-part (lambda (p)
                             (when (or (llm:llm-text-part-p p) (llm:llm-thinking-part-p p))
                               (incf parts)
                               (incf chars (length (or (ignore-errors (llm:llm-text-part-text p))
                                                       (ignore-errors (llm:llm-thinking-part-text p))
                                                       "")))
                               (unless first-at (setf first-at (elapsed t0)))
                               (when (>= (elapsed t0) next-mark)
                                 (note "  ~5,0f s  ~d parts, ~d chars" (elapsed t0) parts chars)
                                 (incf next-mark 60)))))))
          (let ((u (llm:llm-response-usage r)) (secs (elapsed t0)))
            (note "first part after ~,1f s; done after ~,1f s (~:[within~;past~] the old 180 s cap)"
                  (or first-at 0) secs (> secs 180))
            (note "finish ~s, ~d chars of text~@[, ~d output tokens~]~@[ (~,1f tok/s)~]"
                  (llm:llm-response-finish-reason r) (length (llm:llm-response-text r))
                  (and u (llm:llm-usage-output-tokens u))
                  (and u (llm:llm-usage-output-tokens u) (plusp secs)
                       (/ (llm:llm-usage-output-tokens u) secs)))
            (note "text starts: ~s" (subseq (llm:llm-response-text r) 0
                                            (min 160 (length (llm:llm-response-text r)))))))))))

(defparameter *demos* '(demo-1-settings demo-2-steady-stream demo-3-silent-stream
                        demo-4-non-streaming demo-5-no-retry demo-6-live-long-answer))

(defun run-demos (&key (demos *demos*))
  (let ((failed '()) (t0 (get-internal-real-time)))
    (banner "Loaded systems")
    (dolist (name '("llm-protocol" "llm-protocol-openai" "llm-protocol-anthropic"
                    "http-protocol" "http-backend-async" "event-backend-libuv"))
      (let ((s (asdf:find-system name nil)))
        (note "  ~24a ~a" name (and s (asdf:component-version s)))))
    (dolist (d demos)
      (let ((t1 (get-internal-real-time)))
        (handler-case (funcall d)
          (error (c) (push d failed)
            (note "!! ~a FAILED: ~s~%!! ~a" d (type-of c) c)))
        (note "  (~(~a~): ~,1f s wall time)" d (elapsed t1))))
    (banner "Summary")
    (note "demos that raised an error: ~:[none~;~:*~{~(~a~)~^, ~}~]" (reverse failed))
    (note "total wall time ~,1f s" (elapsed t0))
    (when failed (uiop:quit 1))))
