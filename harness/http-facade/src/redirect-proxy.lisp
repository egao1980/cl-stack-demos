(in-package #:harness-demos-http-facade)

;;; A loopback proxy for demo 4's redirects (http-backend-async#82). It is
;;; plain (http://) or TLS (https://, certs/proxy.crt) and serves one
;;; connection at a time until STOP-REDIRECT-PROXY. It answers CONNECT, with
;;; TLS to the origin inside the tunnel (certs/origin.crt), and absolute-form
;;; requests. It plays origin.test itself from a route table, so nothing is
;;; resolved, and logs every hop: the head the proxy read, and the head read
;;; inside the tunnel. An origin-form request at the proxy names no origin:
;;; it gets 400, as from a real proxy.

(defstruct (redirect-proxy (:conc-name rp-))
  kind
  listener
  thread
  (lock (bt:make-lock "demo redirect proxy"))
  (hops '())
  (errors '())
  (stop nil))

(defstruct (proxy-hop (:conc-name hop-))
  (connect nil)
  (head "")
  (url nil)
  (at (get-internal-real-time)))

(defun %rp-push (proxy hop)
  (bt:with-lock-held ((rp-lock proxy))
    (push hop (rp-hops proxy))))

(defun %request-target (head)
  "The request-target of HEAD's request line, or the empty string."
  (let* ((sp1 (position #\Space head))
         (sp2 (and sp1 (position #\Space head :start (1+ sp1)))))
    (if sp2 (subseq head (1+ sp1) sp2) "")))

(defun %tunnel-origin (authority)
  "https://AUTHORITY without a default :443."
  (let ((colon (position #\: authority :from-end t)))
    (if (and colon (string= ":443" authority :start2 colon))
        (format nil "https://~A" (subseq authority 0 colon))
        (format nil "https://~A" authority))))

(defun %answer-route (stream routes url)
  "Answer the request for URL: the redirect ROUTES has for it as (URL status
   location), else 200 naming URL; 400 when URL is NIL. Connection: close."
  (let* ((route (assoc url routes :test #'equal))
         (status (cond (route (second route)) (url 200) (t 400)))
         (body (babel:string-to-octets
                (if (= status 200) (format nil "the origin, ~A" url) ""))))
    (write-sequence
     (apply #'%wire
            (format nil "HTTP/1.1 ~D ~A" status
                    (case status (200 "OK") (400 "Bad Request") (t "Redirect")))
            (append (when route
                      (list (format nil "Location: ~A" (third route))))
                    (list "Content-Type: text/plain"
                          (format nil "Content-Length: ~D" (length body))
                          "Connection: close")))
     stream)
    (write-sequence body stream)
    (finish-output stream)))

(defun %serve-redirect-hop (proxy socket routes)
  (let ((stream socket)
        (origin nil))
    (unwind-protect
         (progn
           (when (eq (rp-kind proxy) :https)
             (setf stream (cl+ssl:make-ssl-server-stream
                           socket :certificate (%cert "proxy.crt")
                                  :key (%cert "proxy.key")
                                  :unwrap-stream-p nil)))
           (let* ((head (%head-text stream))
                  (target (%request-target head)))
             (cond
               ((zerop (length head)))
               ((eql 0 (search "CONNECT " head))
                (write-sequence (%wire "HTTP/1.1 200 Connection established") stream)
                (finish-output stream)
                (setf origin (cl+ssl:make-ssl-server-stream
                              stream :certificate (%cert "origin.crt")
                                     :key (%cert "origin.key")
                                     :unwrap-stream-p nil))
                (let* ((inner (%head-text origin))
                       (inner-target (%request-target inner))
                       (url (if (eql 0 (search "/" inner-target))
                                (concatenate 'string (%tunnel-origin target) inner-target)
                                inner-target)))
                  (%rp-push proxy (make-proxy-hop :connect head :head inner :url url))
                  (%answer-route origin routes url)))
               (t
                (let ((url (and (eql 0 (search "http" target)) target)))
                  (%rp-push proxy (make-proxy-hop :head head :url url))
                  (%answer-route stream routes url))))))
      (when origin
        (ignore-errors (close origin)))
      (unless (eq stream socket)
        (ignore-errors (close stream))))))

(defun start-redirect-proxy (kind routes)
  "A loopback proxy of KIND (:http plain, :https TLS) answering ROUTES."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (proxy (make-redirect-proxy :kind kind :listener listener)))
    (setf (rp-thread proxy)
          (bt:make-thread
           (lambda ()
             (loop until (rp-stop proxy)
                   do (let ((conn nil))
                        (unwind-protect
                             (handler-case
                                 (when (usocket:wait-for-input listener :timeout 0.1
                                                                        :ready-only t)
                                   (setf conn (usocket:socket-accept
                                               listener :element-type '(unsigned-byte 8)))
                                   (ignore-errors
                                    (setf (usocket:socket-option conn :receive-timeout) 10))
                                   (%serve-redirect-hop proxy (usocket:socket-stream conn) routes))
                               (error (e)
                                 (bt:with-lock-held ((rp-lock proxy))
                                   (push (princ-to-string e) (rp-errors proxy)))))
                          (when conn
                            (ignore-errors (usocket:socket-close conn)))))))
           :name "demo redirect proxy"))
    proxy))

(defun redirect-proxy-url (proxy)
  (format nil "~(~A~)://user:secret@127.0.0.1:~D"
          (rp-kind proxy) (usocket:get-local-port (rp-listener proxy))))

(defun stop-redirect-proxy (proxy)
  "Stop PROXY. → its hops, oldest first."
  (setf (rp-stop proxy) t)
  (bt:join-thread (rp-thread proxy))
  (ignore-errors (usocket:socket-close (rp-listener proxy)))
  (reverse (rp-hops proxy)))
