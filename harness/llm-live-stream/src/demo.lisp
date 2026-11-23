(in-package #:harness-demos-llm-live-stream)

;;; Live model streaming (http-backend-async#70, 0.2.27). Before 0.2.26 the blocking
;;; HTTP send read a :want-stream body whole before it returned, so every
;;; llm-protocol on-part callback fired in one burst when the model call
;;; ended. Now send returns at the headers with the live body: a part
;;; reaches on-part as its SSE event arrives. Demos 1-3 use a loopback
;;; origin that logs when it sends each event; demo 4 streams from a live
;;; local model and prints a timestamp per chunk.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title "")
  (finish-output))

(defun note (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))

(defun elapsed (t0)
  (float (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))

(defmacro with-http (&body body)
  "http-backend-async x libuv, the stack's HTTP client."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb))
          (http-backend-async:*event-backend-maker* (lambda () eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         (let ((hp:*http-backend* (http-backend-async:make-async-backend)))
           ,@body)))))

;;; A loopback origin: reads one request, answers with chunked SSE, and logs
;;; the time it sent each event (and when the client hung up).

(defvar *t0* 0)
(defvar *sent* '() "(label . seconds) per event sent, newest first.")
(defvar *hung-up-at* nil)

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
  "Chunked SSE. STEPS are (delay label . text): wait DELAY, send TEXT, log LABEL."
  (%out stream (%crlf "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                      "Transfer-Encoding: chunked" "Connection: close" ""))
  (handler-case
      (progn
        (dolist (s steps)
          (sleep (first s))
          (let ((o (babel:string-to-octets (cddr s))))
            (%out stream (format nil "~X~C~C" (length o) #\Return #\Newline))
            (write-sequence o stream)
            (%out stream (%crlf "")))
          (when (second s) (push (cons (second s) (elapsed *t0*)) *sent*)))
        (%out stream (%crlf "0" "")))
    (error () (setf *hung-up-at* (elapsed *t0*)))))

(defun call-with-origin (steps fn)
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t
                                                      :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (thread (bt:make-thread
                  (lambda ()
                    (handler-case
                        (let ((c (usocket:socket-accept server :element-type '(unsigned-byte 8))))
                          (unwind-protect
                               (let ((s (usocket:socket-stream c)))
                                 (when (%read-request s) (%sse s steps)))
                            (ignore-errors (usocket:socket-close c))))
                      (error () nil)))
                  :name "demo-origin")))
    (setf *sent* '() *hung-up-at* nil *t0* (get-internal-real-time))
    (unwind-protect (with-http (funcall fn (format nil "http://127.0.0.1:~D/v1" port)))
      (loop repeat 100 while (bt:thread-alive-p thread) do (sleep 0.05))
      (ignore-errors (usocket:socket-close server))
      (when (bt:thread-alive-p thread) (ignore-errors (bt:destroy-thread thread))))))

(defmacro with-origin ((base steps) &body body)
  `(call-with-origin ,steps (lambda (,base) ,@body)))

;;; SSE bodies for both dialects

(defun %chat-chunk (&key content finish)
  (format nil "data: {\"model\":\"local\",\"choices\":[{\"index\":0,\"finish_reason\":~:[null~;~:*\"~a\"~],\"delta\":{~@[\"content\":\"~a\"~]}}]}~%~%"
          finish content))

(defun %openai-steps (n delay)
  (append (loop for i below n
                collect (list* delay (format nil "t~D" i) (%chat-chunk :content (format nil "t~D " i))))
          (list (list* 0 nil (%chat-chunk :finish "stop"))
                (list* 0 nil (format nil "data: [DONE]~%~%")))))

(defun %anthropic-event (type json)
  (format nil "event: ~a~%data: ~a~%~%" type json))

(defun %anthropic-steps (n delay)
  (append
   (list (list* 0 nil (%anthropic-event "message_start" "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"local\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}"))
         (list* 0 nil (%anthropic-event "content_block_start" "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}")))
   (loop for i below n
         collect (list* delay (format nil "t~D" i)
                        (%anthropic-event "content_block_delta"
                                          (format nil "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"t~D \"}}" i))))
   (list (list* 0 nil (%anthropic-event "content_block_stop" "{\"type\":\"content_block_stop\",\"index\":0}"))
         (list* 0 nil (%anthropic-event "message_delta" (format nil "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":~D}}" n)))
         (list* 0 nil (%anthropic-event "message_stop" "{\"type\":\"message_stop\"}")))))

(defun %part-text (p)
  (or (ignore-errors (llm:llm-text-part-text p)) ""))

(defun %stream-and-log (backend)
  "STREAM-GENERATE on BACKEND; log the time each text part arrives."
  (let ((seen '()))
    (let ((r (llm:stream-generate backend "hi" :model "local"
                                  :on-part (lambda (p)
                                             (when (llm:llm-text-part-p p)
                                               (push (cons (string-trim " " (%part-text p))
                                                           (elapsed *t0*))
                                                     seen))))))
      (values r (nreverse seen)))))

(defun %table (seen)
  (note "  ~6a ~10a ~12a ~a" "part" "sent at" "on-part at" "lag")
  (dolist (s seen)
    (let ((sent (cdr (assoc (car s) *sent* :test #'string=))))
      (note "  ~6a ~7,2f s  ~9,2f s  ~,2f s" (car s) (or sent 0) (cdr s)
            (- (cdr s) (or sent 0))))))

(defun %spread-verdict (seen)
  (let ((first (cdr (first seen))) (last (cdr (car (last seen)))))
    (note "first part at ~,2f s, last at ~,2f s: ~:[ONE BURST (buffered)~;incremental~]"
          first last (> (- last first) 1.0))))

;;; Demos

(defun demo-1-openai ()
  (banner "1. OpenAI-compatible stream: 8 tokens 0.5 s apart reach on-part as they are sent")
  (with-origin (base (%openai-steps 8 0.5))
    (let ((b (oa:make-openai-compat-backend :base-url base :api-key :none :default-model "local")))
      (multiple-value-bind (r seen) (%stream-and-log b)
        (%table seen)
        (%spread-verdict seen)
        (note "finish ~s, text ~s" (llm:llm-response-finish-reason r) (llm:llm-response-text r))))))

(defun demo-2-anthropic ()
  (banner "2. Anthropic Messages stream: the same, event by event")
  (with-origin (base (%anthropic-steps 8 0.5))
    (let ((b (an:make-anthropic-backend :base-url base :api-key "local" :default-model "local")))
      (multiple-value-bind (r seen) (%stream-and-log b)
        (%table seen)
        (%spread-verdict seen)
        (note "finish ~s, text ~s" (llm:llm-response-finish-reason r) (llm:llm-response-text r))))))

(defun %pump-threads ()
  (count-if (lambda (th) (and (bt:thread-alive-p th) (search "stream pump" (bt:thread-name th))))
            (bt:all-threads)))

(defun demo-3-stop-early ()
  (banner "3. The caller stops after 3 parts: the request ends, the origin sees it hang up")
  (with-origin (base (%openai-steps 40 0.25))
    (let ((b (oa:make-openai-compat-backend :base-url base :api-key :none :default-model "local"))
          (n 0) (stopped-at nil))
      (block stop
        (llm:stream-generate b "hi" :model "local"
                             :on-part (lambda (p)
                                        (when (and (llm:llm-text-part-p p) (= 3 (incf n)))
                                          (setf stopped-at (elapsed *t0*))
                                          (return-from stop)))))
      (loop repeat 60 until *hung-up-at* do (sleep 0.05))
      (note "caller stopped at ~,2f s after ~d parts; origin sent ~d of 40 events; it saw the hang-up at ~:[never~;~:*~,2f s~]"
            stopped-at n (length *sent*) *hung-up-at*)
      (loop repeat 40 until (zerop (%pump-threads)) do (sleep 0.05))
      (note "stream pump threads still running: ~d" (%pump-threads)))))

;;; Live: a local model (HARNESS_DEMOS_BASE_URL / _API_KEY / _MODEL).

(defun %live-env (name &optional default)
  (let ((v (uiop:getenv name)))
    (if (and v (plusp (length (string-trim " " v)))) (string-trim " " v) default)))

(defun %url-host (url)
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position #\/ url :start start) (length url))))
    (subseq url start end)))

(defun %visible (text)
  "TEXT quoted on one line: a newline shows as \\n."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across text
          do (if (char= c #\Newline) (write-string "\\n" o) (write-char c o)))
    (write-char #\" o)))

(defun demo-4-live ()
  (banner "4. Live: a local model's stream, one timestamp per chunk")
  (let ((base (%live-env "HARNESS_DEMOS_BASE_URL"))
        (model (%live-env "HARNESS_DEMOS_MODEL"))
        (key (%live-env "HARNESS_DEMOS_API_KEY" :none))
        (max-tokens (parse-integer (%live-env "HARNESS_DEMOS_MAX_TOKENS" "160"))))
    (unless (and base model)
      (note "skipped: set HARNESS_DEMOS_BASE_URL and HARNESS_DEMOS_MODEL")
      (return-from demo-4-live))
    (with-http
      (let* ((b (oa:make-openai-compat-backend :base-url base :api-key key :default-model model))
             (t0 (get-internal-real-time))
             (chunks '()))
        (note "MODEL THAT RAN: ~a at ~a" model (%url-host base))
        (let ((r (llm:stream-generate
                  b (list (llm:user-turn "Count from 1 to 40 in words, one number per line. No preamble."))
                  :model model
                  :settings (llm:make-llm-settings :max-tokens max-tokens :temperature 0.2
                                                   :extra '(:chat-template-kwargs (:enable-thinking nil)))
                  :on-part (lambda (p)
                             (when (llm:llm-text-part-p p)
                               (let ((at (elapsed t0)) (text (%part-text p)))
                                 (push (cons at text) chunks)
                                 (note "  +~6,2f s  ~a" at (%visible text))))))))
          (setf chunks (nreverse chunks))
          (let* ((times (mapcar #'car chunks))
                 (first (first times)) (last (car (last times)))
                 (distinct (length (remove-duplicates
                                    (mapcar (lambda (x) (round (* 10 x))) times)))))
            (note "~d chunks; first at ~,2f s, last at ~,2f s; ~d distinct 0.1 s stamps: ~:[ONE BURST (buffered)~;incremental~]"
                  (length chunks) (or first 0) (or last 0) distinct
                  (and first last (> (- last first) 1.0) (> distinct 5)))
            (note "finish ~s, ~d chars" (llm:llm-response-finish-reason r)
                  (length (llm:llm-response-text r)))))))))

(defparameter *demos* '(demo-1-openai demo-2-anthropic demo-3-stop-early demo-4-live))

(defun run-demos (&key (demos *demos*))
  (let ((failed '()) (t0 (get-internal-real-time)))
    (banner "Loaded systems")
    (dolist (name '("llm-protocol" "llm-protocol-openai" "llm-protocol-anthropic" "sse-protocol"
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
