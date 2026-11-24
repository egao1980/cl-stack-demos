(in-package #:harness-demos-http-facade)

;;; A loopback https:// proxy for demo 4, one connection per START-TLS-PROXY.
;;; It records the raw octets it reads off the socket, takes the server side
;;; of TLS (certs/proxy.crt, CN 127.0.0.1), records the head it reads inside
;;; and answers CONNECT. Through the tunnel it is the origin too: TLS again
;;; with certs/origin.crt (origin.test), then the request, answered 200. A
;;; client that does not start with TLS gets its cleartext head recorded and
;;; a 502. certs/ holds a CA made for the http-backend-async tests (its key
;;; was not kept) and the two certificates it signed; demo use only.

(defparameter *certs*
  (asdf:system-relative-pathname "harness-demos-http-facade" "certs/"))

(defun %cert (name)
  (namestring (merge-pathnames name *certs*)))

(defun %octet-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %wire (&rest lines)
  (babel:string-to-octets
   (format nil "~{~A~C~C~}~C~C"
           (loop for line in lines
                 append (list line #\Return #\Newline))
           #\Return #\Newline)
   :encoding :latin-1))

(defclass recording-stream (trivial-gray-streams:fundamental-binary-input-stream
                            trivial-gray-streams:fundamental-binary-output-stream)
  ((inner :initarg :inner :reader recording-inner)
   (replay :initarg :replay :initform nil :accessor recording-replay)
   (log :initarg :log :reader recording-log))
  (:documentation "A binary stream over INNER that logs every octet it reads.
   cl+ssl's Lisp BIO reads through it, so under TLS the log is ciphertext."))

(defmethod stream-element-type ((stream recording-stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((stream recording-stream))
  (if (recording-replay stream)
      (pop (recording-replay stream))
      (let ((byte (read-byte (recording-inner stream) nil :eof)))
        (unless (eq byte :eof)
          (vector-push-extend byte (recording-log stream)))
        byte)))

(defmethod trivial-gray-streams:stream-listen ((stream recording-stream))
  (or (and (recording-replay stream) t)
      (listen (recording-inner stream))))

(defmethod trivial-gray-streams:stream-write-byte ((stream recording-stream) byte)
  (write-byte byte (recording-inner stream))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream recording-stream) sequence start end &key)
  (write-sequence sequence (recording-inner stream) :start start :end end)
  sequence)

(defmethod trivial-gray-streams:stream-finish-output ((stream recording-stream))
  (finish-output (recording-inner stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream recording-stream))
  (force-output (recording-inner stream)))

(defun %head-text (stream)
  "Octets through the first blank line, as a string (\"\" at EOF)."
  (let ((buf (%octet-buffer)))
    (loop with tail = 0
          for byte = (read-byte stream nil nil)
          while byte
          do (vector-push-extend byte buf)
             (setf tail (logand #xffffffff (logior (ash tail 8) byte)))
          until (or (= tail #x0d0a0d0a) (> (length buf) 8192)))
    (babel:octets-to-string buf :encoding :latin-1)))

(defstruct (proxy-record (:conc-name pr-))
  (connected nil)
  (raw (%octet-buffer))
  (cleartext nil)
  (inside nil)
  (tunnel (%octet-buffer))
  (origin nil)
  (stopping nil)
  (error nil)
  (done nil))

(defun %answer-ok (stream text)
  (let ((body (babel:string-to-octets text)))
    (write-sequence (%wire "HTTP/1.1 200 OK" "Content-Type: text/plain"
                           (format nil "Content-Length: ~D" (length body))
                           "Connection: close")
                    stream)
    (write-sequence body stream)
    (finish-output stream)))

(defun %serve-proxy (listener record)
  (let ((conn nil))
    (unwind-protect
         (handler-case
             (when (usocket:wait-for-input listener :timeout 15 :ready-only t)
               (setf conn (usocket:socket-accept listener :element-type '(unsigned-byte 8)))
               (unless (pr-stopping record)
                 (setf (pr-connected record) t)
                 (let* ((socket (usocket:socket-stream conn))
                        (raw (make-instance 'recording-stream :inner socket
                                                              :log (pr-raw record)))
                        (record-header (loop repeat 5 collect (read-byte raw))))
                   (setf (recording-replay raw) (copy-list record-header))
                   (if (/= #x16 (first record-header))
                       (progn
                         (setf (pr-cleartext record) (%head-text raw))
                         (write-sequence (%wire "HTTP/1.1 502 Bad Gateway" "Content-Length: 0"
                                                "Connection: close")
                                         socket)
                         (finish-output socket))
                       (let* ((tls (cl+ssl:make-ssl-server-stream
                                    raw :certificate (%cert "proxy.crt")
                                        :key (%cert "proxy.key")
                                        :unwrap-stream-p nil))
                              (head (%head-text tls)))
                         (setf (pr-inside record) head)
                         (if (eql 0 (search "CONNECT " head))
                             (progn
                               (write-sequence (%wire "HTTP/1.1 200 Connection established") tls)
                               (finish-output tls)
                               (let* ((tunnel (make-instance 'recording-stream
                                                             :inner tls :log (pr-tunnel record)))
                                      (origin (cl+ssl:make-ssl-server-stream
                                               tunnel :certificate (%cert "origin.crt")
                                                      :key (%cert "origin.key")
                                                      :unwrap-stream-p nil)))
                                 (setf (pr-origin record) (%head-text origin))
                                 (%answer-ok origin "origin.test, through the TLS tunnel")))
                             (%answer-ok tls "the proxy, over TLS")))))))
           (error (e) (setf (pr-error record) (princ-to-string e))))
      (when conn (ignore-errors (usocket:socket-close conn)))
      (ignore-errors (usocket:socket-close listener))
      (setf (pr-done record) t))))

(defun start-tls-proxy ()
  "→ (values port record stop-fn). STOP-FN wakes a never-used accept and
   waits for the fixture to finish."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (record (make-proxy-record))
         (thread (bt:make-thread (lambda () (%serve-proxy listener record))
                                 :name "demo https proxy")))
    (values port record
            (lambda ()
              (setf (pr-stopping record) t)
              (unless (pr-done record)
                (ignore-errors
                  (usocket:socket-close
                   (usocket:socket-connect "127.0.0.1" port
                                           :element-type '(unsigned-byte 8)))))
              (bt:join-thread thread)
              record))))
