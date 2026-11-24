(in-package #:harness-demos-dexador-http)

;;; Loopback listeners for the demos: origins (plain or TLS with the
;;; self-signed localhost certificate in certs/, demo use only) and a
;;; recording "proxy". A listener serves each connection on its own thread
;;; and calls its handler once per request head; every head is logged.

(defparameter *certs*
  (asdf:system-relative-pathname "harness-demos-dexador-http" "certs/"))

(defun seconds-since (t0)
  (float (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))

(defun crlf (&rest lines)
  (format nil "~{~A~C~C~}" (loop for l in lines collect l collect #\Return collect #\Newline)))

(defun send-text (stream text)
  (write-sequence (babel:string-to-octets text :encoding :latin-1) stream)
  (finish-output stream))

(defun respond (stream status body &key (reason "OK") headers close)
  (send-text stream
             (apply #'crlf
                    (format nil "HTTP/1.1 ~D ~A" status reason)
                    "Content-Type: text/plain"
                    (format nil "Content-Length: ~D" (length body))
                    (append headers
                            (when close (list "Connection: close"))
                            (list ""))))
  (send-text stream body))

(defun read-head (stream)
  "One request head as a list of lines, or NIL at EOF."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with tail = 0
          for b = (read-byte stream nil nil)
          unless b do (return-from read-head nil)
          do (vector-push-extend b bytes)
             (setf tail (logand #xffffffff (logior (ash tail 8) b)))
          until (= tail #x0d0a0d0a))
    (let ((text (babel:octets-to-string bytes :encoding :latin-1))
          (crlf (coerce '(#\Return #\Newline) 'string)))
      (loop with start = 0
            for end = (search crlf text :start2 start)
            while (and end (> end start))
            collect (subseq text start end)
            do (setf start (+ end 2))))))

(defun header (lines name)
  (loop for line in (rest lines)
        for colon = (position #\: line)
        when (and colon (string-equal name (subseq line 0 colon)))
          return (string-trim " " (subseq line (1+ colon)))))

(defstruct listener port log (lock (bt:make-lock "listener")) stop (connections 0))

(defun start-listener (handler &key tls)
  "HANDLER: (lambda (head-lines stream)) per request; it returns :CLOSE to
   end the connection. → a LISTENER."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :backlog 32
                                                      :element-type '(unsigned-byte 8)))
         (l (make-listener :port (usocket:get-local-port server)))
         (stopping (list nil)))
    (labels ((serve (client)
               (unwind-protect
                    (handler-case
                        (let ((stream (if tls
                                          (cl+ssl:make-ssl-server-stream
                                           (usocket:socket-stream client)
                                           :certificate (namestring (merge-pathnames "server.crt" *certs*))
                                           :key (namestring (merge-pathnames "server.key" *certs*)))
                                          (usocket:socket-stream client))))
                          (unwind-protect
                               (loop for lines = (read-head stream)
                                     while lines
                                     do (bt:with-lock-held ((listener-lock l))
                                          (push lines (listener-log l)))
                                     until (eq :close (funcall handler lines stream)))
                            (ignore-errors (close stream))))
                      (error () nil))
                 (ignore-errors (usocket:socket-close client))))
             (accept-loop ()
               (handler-case
                   (loop (let ((client (usocket:socket-accept server)))
                           (when (car stopping)
                             (ignore-errors (usocket:socket-close client))
                             (return))
                           (bt:with-lock-held ((listener-lock l))
                             (incf (listener-connections l)))
                           (bt:make-thread (lambda () (serve client)) :name "demo conn")))
                 (error () nil))))
      (let ((acceptor (bt:make-thread #'accept-loop :name "demo listener")))
        (setf (listener-stop l)
              (lambda ()
                (setf (car stopping) t)
                (ignore-errors
                  (usocket:socket-close (usocket:socket-connect "127.0.0.1" (listener-port l))))
                (ignore-errors (bt:join-thread acceptor))
                (ignore-errors (usocket:socket-close server))))
        l))))

(defun stop-listener (l)
  (funcall (listener-stop l)))

(defmacro with-listener ((var handler &rest keys) &body body)
  `(let ((,var (start-listener ,handler ,@keys)))
     (unwind-protect (progn ,@body)
       (stop-listener ,var))))

(defun listener-heads (l)
  (bt:with-lock-held ((listener-lock l)) (reverse (listener-log l))))

;;; Handlers

(defun recording-proxy (lines stream)
  "A proxy that only records: a CONNECT is refused with 502, anything else
   is answered 200 with the request line it got."
  (if (eql 0 (search "CONNECT " (first lines)))
      (progn (respond stream 502 "no tunnels here" :reason "Bad Gateway" :close t) :close)
      (respond stream 200 (format nil "proxy got: ~A" (first lines)))))

(defun answer-after (seconds &optional (body "hello"))
  (lambda (lines stream)
    (declare (ignore lines))
    (sleep seconds)
    (respond stream 200 body)))

(defun redirect-to (location)
  (lambda (lines stream)
    (declare (ignore lines))
    (send-text stream (crlf "HTTP/1.1 301 Moved Permanently"
                            (format nil "Location: ~A" location)
                            "Content-Length: 0" "Connection: close" ""))
    :close))

(defun headers-then-stall (stall &optional (body "late body"))
  "Status line and headers at once, then the body after STALL seconds."
  (lambda (lines stream)
    (declare (ignore lines))
    (send-text stream (crlf "HTTP/1.1 200 OK" "Content-Type: text/plain"
                            (format nil "Content-Length: ~D" (length body)) ""))
    (sleep stall)
    (handler-case (send-text stream body) (error () nil))
    :close))

(defun drip (count interval)
  "A body of COUNT octets, one every INTERVAL seconds."
  (lambda (lines stream)
    (declare (ignore lines))
    (send-text stream (crlf "HTTP/1.1 200 OK" "Content-Type: text/plain"
                            (format nil "Content-Length: ~D" count) ""))
    (handler-case
        (dotimes (i count)
          (sleep interval)
          (send-text stream "."))
      (error () nil))
    :close))
