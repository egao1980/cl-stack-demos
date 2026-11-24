(in-package #:harness-demos-http-facade)

;;; A loopback HTTP/1.1 keep-alive origin, one thread per connection. It logs
;;; every request (connection number, path, User-Agent, time) so the demos
;;; can show which backend sent it and over which connection.
;;;   /delay/S     answer after S seconds (a decimal)
;;;   /status/N    answer with status N
;;;   anything     200
;;; The body is "conn=C path=P" as text/plain.

(defvar *origin-t0* 0)
(defvar *origin-lock* (bt:make-lock "origin log"))
(defvar *origin-log* '() "(conn method path user-agent seconds), newest first.")
(defvar *origin-open* 0 "Connections the client still holds open.")
(defvar *origin-conns* 0 "Connections accepted.")

(defun %seconds-since (t0)
  (float (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))

(defun %read-head (stream)
  "One request head as a list of lines, or NIL at EOF."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (loop with tail = 0
          for b = (read-byte stream nil nil)
          unless b do (return-from %read-head nil)
          do (vector-push-extend b bytes)
             (setf tail (logand #xffffffff (logior (ash tail 8) b)))
          until (= tail #x0d0a0d0a))
    (let ((text (babel:octets-to-string bytes :encoding :latin-1)))
      (loop with start = 0
            for end = (search (coerce '(#\Return #\Newline) 'string) text :start2 start)
            while (and end (> end start))
            collect (subseq text start end)
            do (setf start (+ end 2))))))

(defun %header (lines name)
  (loop for line in (rest lines)
        for colon = (position #\: line)
        when (and colon (string-equal name (subseq line 0 colon)))
          return (string-trim " " (subseq line (1+ colon)))))

(defun %request-path (lines)
  (let* ((line (first lines))
         (sp1 (position #\Space line))
         (sp2 (and sp1 (position #\Space line :start (1+ sp1)))))
    (if sp1 (subseq line (1+ sp1) sp2) "/")))

(defun %prefixed-number (path prefix)
  (and (eql 0 (search prefix path))
       (let ((*read-default-float-format* 'double-float)
             (text (subseq path (length prefix)
                           (or (position #\? path) (length path)))))
         (ignore-errors
           (let ((n (with-standard-io-syntax
                      (let ((*read-eval* nil)) (read-from-string text)))))
             (and (realp n) n))))))

(defun %respond (stream status body &key location head)
  "Answer STATUS with BODY as text/plain, and a Location header when
   LOCATION. With HEAD the Content-Length is BODY's and no body follows, as
   a server answers HEAD."
  (let ((octets (babel:string-to-octets body))
        (location-line (if location
                           (format nil "Location: ~A~C~C" location #\Return #\Newline)
                           "")))
    (write-sequence
     (babel:string-to-octets
      (format nil "HTTP/1.1 ~D ~A~C~C~AContent-Type: text/plain~C~CContent-Length: ~D~C~C~C~C"
              status (if (< status 400) "OK" "Error")
              #\Return #\Newline location-line #\Return #\Newline
              (length octets) #\Return #\Newline #\Return #\Newline)
      :encoding :latin-1)
     stream)
    (unless head
      (write-sequence octets stream))
    (finish-output stream)))

(defun %request-method (lines)
  (let ((line (first lines)))
    (subseq line 0 (or (position #\Space line) (length line)))))

(defun %read-body (stream lines)
  "The request body the Content-Length in LINES announces, read off STREAM."
  (let* ((length (%header lines "content-length"))
         (body (make-array (or (and length (parse-integer length :junk-allowed t)) 0)
                           :element-type '(unsigned-byte 8))))
    (read-sequence body stream)
    body))

(defun %answer (stream conn lines)
  "Log and answer one request on connection CONN. /redirect/S answers S with
   Location /target; /delay/S and /status/N as above."
  (let* ((method (%request-method lines))
         (path (%request-path lines))
         (sent (%read-body stream lines))
         (delay (%prefixed-number path "/delay/"))
         (redirect (%prefixed-number path "/redirect/"))
         (status (or redirect (%prefixed-number path "/status/") 200)))
    (bt:with-lock-held (*origin-lock*)
      (push (list conn method path (or (%header lines "user-agent") "-")
                  (%seconds-since *origin-t0*) (length sent))
            *origin-log*))
    (when delay (sleep delay))
    (%respond stream status (format nil "conn=~D path=~A" conn path)
              :location (and redirect "/target")
              :head (string-equal method "HEAD"))))

(defun %serve (client conn)
  (unwind-protect
       (handler-case
           (let ((stream (usocket:socket-stream client)))
             (loop for lines = (%read-head stream)
                   while lines
                   do (%answer stream conn lines)))
         (error () nil))
    (ignore-errors (usocket:socket-close client))
    (bt:with-lock-held (*origin-lock*) (decf *origin-open*))))

(defun start-origin ()
  "→ (values port stop-fn)."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :backlog 64
                                                      :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (stopping (list nil)))
    (setf *origin-t0* (get-internal-real-time))
    (flet ((accept-loop ()
             (handler-case
                 (loop (let ((client (usocket:socket-accept server)))
                         (when (car stopping)
                           (ignore-errors (usocket:socket-close client))
                           (return))
                         (let ((conn (bt:with-lock-held (*origin-lock*)
                                       (incf *origin-open*)
                                       (incf *origin-conns*))))
                           (bt:make-thread (lambda () (%serve client conn))
                                           :name "demo origin conn"))))
               (error () nil))))
      (let ((acceptor (bt:make-thread #'accept-loop :name "demo origin")))
        (values port
                (lambda ()
                  (setf (car stopping) t)
                  (ignore-errors
                    (usocket:socket-close (usocket:socket-connect "127.0.0.1" port)))
                  (ignore-errors (bt:join-thread acceptor))
                  (ignore-errors (usocket:socket-close server))))))))

(defun origin-log-since (mark)
  "Log entries after the first MARK ones, oldest first."
  (bt:with-lock-held (*origin-lock*)
    (let ((all (reverse *origin-log*)))
      (nthcdr mark all))))

(defun origin-mark ()
  (bt:with-lock-held (*origin-lock*) (length *origin-log*)))

(defun origin-open-connections ()
  (bt:with-lock-held (*origin-lock*) *origin-open*))
