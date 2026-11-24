(in-package #:harness-demos-dexador-http)

;;; http-backend-dexador 0.2.0 on dexador 0.9.16.1 (http-backend-dexador#23,
;;; #24), the gaps the "Python to Common Lisp" site found:
;;;   1. proxies: the protocol's HTTP-PROXY-CONFIG is resolved per URL and
;;;      reaches dexador as a URL (0.1.5 passed the object, which fell
;;;      through an ETYPECASE, and dropped the request's own :proxy);
;;;   2. timeouts: TOTAL is a deadline, and the read timeout holds on HTTPS
;;;      (0.1.5 waited out a silent HTTPS origin), streamed bodies included;
;;;   3. connection pools keyed by :verify.
;;; Every origin and proxy is a loopback listener (fixtures.lisp).

;;; http-backend-dexador 0.2.1 (#32, #33), found by the same site:
;;;   4. request bodies reach the origin as http-protocol encoded them
;;;      (0.2.0 failed every form post: dexador url-encoded the octets again);
;;;   5. redirects are followed by the backend, so RESPONSE-HISTORY lists them.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title "")
  (finish-output))

(defun note (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))

(defvar *backend* nil)

(defun client (&rest keys)
  (apply #'hp:make-http-client *backend*
         (append keys (list :proxy (hp:make-http-proxy-config :system nil
                                                              :use-system-proxy nil)))))

(defun send* (client &rest request-keys)
  (hp:send *backend* client (apply #'hp:make-http-request request-keys)))

(defmacro timed (&body body)
  "→ (values seconds result-or-condition)."
  (let ((t0 (gensym)))
    `(let ((,t0 (get-internal-real-time)))
       (let ((r (handler-case (progn ,@body) (error (e) e))))
         (values (seconds-since ,t0) r)))))

(defun short (thing &optional (max 120))
  (let ((s (typecase thing
             (condition (format nil "~A: ~A" (type-of thing) thing))
             (hp:http-response (format nil "HTTP-RESPONSE ~D" (hp:response-status thing)))
             (t (princ-to-string thing)))))
    (setf s (substitute #\Space #\Newline s))
    (if (> (length s) max) (concatenate 'string (subseq s 0 (- max 3)) "...") s)))

(defun body-string (response)
  (let ((b (hp:response-body response)))
    (typecase b
      (string b)
      ((vector (unsigned-byte 8)) (babel:octets-to-string b :encoding :latin-1))
      (t (princ-to-string b)))))

(defun local (port &optional (path "/") (scheme "http"))
  (format nil "~A://127.0.0.1:~D~A" scheme port path))

;;; ------------------------------------------------------------------
;;; Demo 1: proxies (#23)

(defun demo-1 ()
  (banner "Demo 1: proxy configs resolved per URL (#23)")
  (note "Proxy variables in this process: ~{~A~^, ~}"
        (loop for v in '("HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY" "NO_PROXY")
              collect (format nil "~A=~A" v (or (uiop:getenv v) "unset"))))
  (with-listener (proxy #'recording-proxy)
    (let* ((purl (format nil "http://demo:s3cret@127.0.0.1:~D" (listener-port proxy)))
           (c (hp:make-http-client *backend*
                                   :proxy (hp:make-http-proxy-config :proxy purl :system nil))))
      (note "~%Client proxy ~A (a loopback listener that records what it gets):"
            (format nil "http://demo:***@127.0.0.1:~D" (listener-port proxy)))
      (multiple-value-bind (secs res) (timed (send* c :url "http://origin.test/path?q=1" :timeout 5))
        (note "  GET http://origin.test/path?q=1 => ~A in ~,2F s, body ~S"
              (short res) secs (if (typep res 'hp:http-response) (body-string res) "-")))
      (let ((head (first (listener-heads proxy))))
        (note "    the proxy read: ~S" (first head))
        (note "                    Host: ~A, Proxy-Authorization: ~A"
              (header head "host") (or (header head "proxy-authorization") "none")))
      (note "    (0.1.5 passed the HTTP-PROXY-CONFIG object to dexador: ETYPECASE failure;")
      (note "     dexador 0.9.16 sent the origin-form \"GET /path?q=1\", which squid answers with 400)")
      (multiple-value-bind (secs res) (timed (send* c :url "https://origin.test/" :timeout 5))
        (note "  GET https://origin.test/ => ~A after ~,2F s" (short res 90) secs))
      (progn
        (note "    the proxy read: ~S (HTTPS goes through a tunnel; this proxy refuses it)"
              (first (second (listener-heads proxy))))
        (note "    (the 502 surfaces as a TLS error, not a proxy error: http-backend-dexador#30)"))))
  (note "~%A request's :proxy wins over the client's:")
  (with-listener (a #'recording-proxy)
    (with-listener (b #'recording-proxy)
      (let ((c (hp:make-http-client *backend*
                                    :proxy (hp:make-http-proxy-config
                                            :proxy (local (listener-port a) "") :system nil))))
        (multiple-value-bind (secs res)
            (timed (send* c :url "http://origin.test/per-request" :timeout 5
                            :proxy (hp:make-http-proxy-config
                                    :proxy (local (listener-port b) "") :system nil)))
          (declare (ignore secs))
          (note "  => ~A; client proxy saw ~D request~:P, request proxy saw ~D: ~S"
                (short res) (length (listener-heads a)) (length (listener-heads b))
                (first (first (listener-heads b))))))))
  (note "~%no_proxy: a host on the list goes direct:")
  (with-listener (proxy #'recording-proxy)
    (with-listener (origin (answer-after 0 "direct"))
      (let ((c (hp:make-http-client *backend*
                                    :proxy (hp:make-http-proxy-config
                                            :proxy (local (listener-port proxy) "")
                                            :no-proxy "localhost, 127.0.0.1"
                                            :system nil))))
        (multiple-value-bind (secs res) (timed (send* c :url (local (listener-port origin) "/np") :timeout 5))
          (declare (ignore secs))
          (note "  GET ~A with no_proxy \"localhost, 127.0.0.1\" => ~A ~S; the proxy saw ~D request~:P"
                (local (listener-port origin) "/np") (short res)
                (if (typep res 'hp:http-response) (body-string res) "-")
                (length (listener-heads proxy)))))))
  (note "~%The default proxy config (system / trust-env) with no proxy variables set:")
  (with-listener (origin (answer-after 0 "direct"))
    (let ((c (hp:make-http-client *backend*)))
      (multiple-value-bind (secs res) (timed (send* c :url (local (listener-port origin) "/env") :timeout 5))
        (declare (ignore secs))
        (note "  (make-http-client backend) => ~A (0.1.5: CASE-FAILURE, #<HTTP-PROXY-CONFIG> fell through ETYPECASE)"
              (short res)))))
  (note "~%Proxies dexador cannot honour are refused before any byte is sent:")
  (with-listener (tls-proxy #'recording-proxy)
    (dolist (p (list (format nil "https://demo:s3cret@127.0.0.1:~D" (listener-port tls-proxy))
                     "socks4://127.0.0.1:1080"
                     "socks5://demo:s3cret@127.0.0.1:1080"))
      (let ((c (hp:make-http-client *backend* :proxy (hp:make-http-proxy-config :proxy p :system nil))))
        (multiple-value-bind (secs res) (timed (send* c :url "http://origin.test/" :timeout 5))
          (declare (ignore secs))
          (note "  ~A~%    => ~A~%    message names the password: ~A"
                (substitute-string p "s3cret" "***")
                (short res 150)
                (if (search "s3cret" (princ-to-string res)) "YES" "no")))))
    (note "  connections the https:// proxy listener got: ~D (dexador would have sent the request and"
          (listener-connections tls-proxy))
    (note "  Proxy-Authorization to it over plain TCP)"))
  (note "~%Userinfo in the URL is not sent in Host (dexador 0.9.16.1):")
  (with-listener (origin (answer-after 0 "ok"))
    (let ((url (format nil "http://user:pw@127.0.0.1:~D/userinfo" (listener-port origin))))
      (multiple-value-bind (secs res) (timed (send* (client) :url url :timeout 5))
        (declare (ignore secs))
        (let ((head (first (listener-heads origin))))
          (note "  GET http://user:pw@127.0.0.1:P/userinfo => ~A; origin got Host: ~A, Authorization: ~A~%    (requests would send Basic auth from the URL: http-protocol#55)"
                (short res)
                (substitute-string (or (header head "host") "-")
                                   (princ-to-string (listener-port origin)) "P")
                (or (header head "authorization") "none")))))))

(defun substitute-string (string old new)
  (let ((pos (search old string)))
    (if pos
        (concatenate 'string (subseq string 0 pos) new
                     (substitute-string (subseq string (+ pos (length old))) old new))
        string)))

;;; ------------------------------------------------------------------
;;; Demo 2: timeouts (#24)

(defun timeout-row (label url timeout &rest keys)
  (multiple-value-bind (secs res)
      (timed (apply #'send* (client :verify nil) :url url :timeout timeout keys))
    (note "  ~52A => ~A after ~,2F s" label (short res 60) secs)))

(defun demo-2 ()
  (banner "Demo 2: the TOTAL deadline and the read timeout, HTTP and HTTPS (#24)")
  (note "Each origin stays silent after the request head, then answers.")
  (with-listener (http-slow (answer-after 5))
    (with-listener (https-slow (answer-after 5) :tls t)
      (with-listener (https-silent (answer-after 6) :tls t)
        (timeout-row "http, silent 5 s, :timeout 2.0" (local (listener-port http-slow)) 2.0)
        (timeout-row "https, silent 5 s, :timeout 2.0" (local (listener-port https-slow) "/" "https") 2.0)
        (timeout-row "https, silent 6 s, (:connect 5 :read 1)"
                     (local (listener-port https-silent) "/" "https")
                     (hp:make-http-timeout :connect 5 :read 1))
        (with-listener (redirect (redirect-to (local (listener-port https-silent) "/" "https")))
          (timeout-row "http -> 301 -> https, silent 6 s, (:connect 5 :read 1)"
                       (local (listener-port redirect))
                       (hp:make-http-timeout :connect 5 :read 1)))
        (timeout-row "https, silent 6 s, (:read 1.5)"
                     (local (listener-port https-silent) "/" "https")
                     (hp:make-http-timeout :read 1.5)))))
  (note "  (0.1.5: both https rows with :read returned the response after 6 s; the TLS reads")
  (note "   went through cl+ssl's socket BIO, which never sees the fd-stream timeout)")
  (note "~%The read timeout bounds silence, not the whole body:")
  (with-listener (dripping (drip 8 0.5) :tls t)
    (multiple-value-bind (secs res)
        (timed (send* (client :verify nil) :url (local (listener-port dripping) "/" "https")
                      :timeout (hp:make-http-timeout :read 1)))
      (note "  https, 8 octets 0.5 s apart, (:read 1) => ~A, body ~S, after ~,2F s"
            (short res 40) (if (typep res 'hp:http-response) (body-string res) "-") secs)))
  (note "~%A streamed HTTPS body (:want-stream t) keeps the read timeout after send returns:")
  (with-listener (stalled (headers-then-stall 6) :tls t)
    (multiple-value-bind (secs res)
        (timed (send* (client :verify nil) :url (local (listener-port stalled) "/" "https")
                      :timeout 2.0 :want-stream t))
      (note "  send => ~A after ~,2F s (headers only)" (short res 60) secs)
      (when (typep res 'hp:http-response)
        (let ((body (hp:response-body res)))
          (multiple-value-bind (secs2 r2) (timed (read-byte body nil :eof))
            (note "  first body read, the origin silent for 6 s => ~A after ~,2F s"
                  (short r2 90) secs2))
          (ignore-errors (close body)))))))

;;; ------------------------------------------------------------------
;;; Demo 3: pools keyed by :verify

(defun demo-3 ()
  (banner "Demo 3: an unverified connection never serves a client that verifies")
  (with-listener (origin (answer-after 0 "tls ok") :tls t)
    (let ((url (local (listener-port origin) "/" "https")))
      (multiple-value-bind (secs res) (timed (send* (client :verify nil) :url url :timeout 5))
        (declare (ignore secs))
        (note "  :verify nil client, self-signed origin  => ~A ~S" (short res)
              (if (typep res 'hp:http-response) (body-string res) "-")))
      (multiple-value-bind (secs res) (timed (send* (client :verify t) :url url :timeout 5))
        (declare (ignore secs))
        (note "  :verify t client, same origin, next     => ~A~%    (raw cl+ssl condition, not http-tls-error: http-backend-dexador#31)" (short res 110)))
      (note "  connections the origin accepted: ~D (the idle :verify nil one stayed in its own pool)"
            (listener-connections origin)))))

(defun escaped (octets &optional (max 110))
  "OCTETS as UTF-8 text in double quotes, CR and LF shown as \\r and \\n,
   cut at MAX."
  (let* ((text (babel:octets-to-string octets :encoding :utf-8 :errorp nil))
         (s (with-output-to-string (out)
              (loop for c across text
                    do (case c
                         (#\Return (write-string "\\r" out))
                         (#\Newline (write-string "\\n" out))
                         (t (write-char c out)))))))
    (format nil "\"~A\""
            (if (> (length s) max) (concatenate 'string (subseq s 0 (- max 3)) "...") s))))

(defun raw-header-line (lines name)
  "The header line for NAME in LINES exactly as the origin read it."
  (find-if (lambda (line)
             (let ((colon (position #\: line)))
               (and colon (string-equal name (subseq line 0 colon)))))
           (rest lines)))

(defparameter *multipart-body*
  (format nil "--XyZ~C~CContent-Disposition: form-data; name=\"k\"~C~C~C~Cv~C~C--XyZ--~C~C"
          #\Return #\Newline #\Return #\Newline #\Return #\Newline
          #\Return #\Newline #\Return #\Newline))

(defun octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun post-row (label &rest request-keys)
  "POST REQUEST-KEYS to a recording origin and show what it received."
  (let ((box (list nil)))
    (with-listener (origin (router box '()))
      (let ((res (handler-case
                     (apply #'send* (client) :method :post
                                             :url (local (listener-port origin) "/post") :timeout 5
                            request-keys)
                   (error (e) e))))
        (note "~%  ~A~%    => ~A" label (short res 110))
        (let ((seen (first (car box))))
          (if (null seen)
              (note "    the origin got no request")
              (destructuring-bind (lines . body) seen
                (note "    origin read ~S" (raw-header-line lines "content-type"))
                (if (header lines "content-encoding")
                    (note "                Content-Encoding: ~A, Content-Length: ~A, ~D octets that decode to ~A"
                          (header lines "content-encoding") (header lines "content-length")
                          (length body) (escaped (hp:decode-content-coding :gzip body)))
                    (note "                Content-Length: ~A, body ~A"
                          (header lines "content-length") (escaped body))))))))))

(defun demo-4 ()
  (banner "Demo 4: request bodies reach the origin as encoded (#32)")
  (note "Each POST goes to a loopback origin that records the head and body it reads.")
  (note "0.2.0 handed dexador http-protocol's encoded octets, and dexador encodes a")
  (note "body again when its Content-Type is application/x-www-form-urlencoded or multipart/*.")
  (post-row ":form-data ((\"key1\" . \"value1\"))  (0.2.0: QURI.ENCODE::PARAMS-ALIST is #(107 101 ...), not a LIST)"
            :form-data '(("key1" . "value1")))
  (post-row ":data ((\"q\" . \"hi there\"))  (an alist :data is urlencoded too; 0.2.0: the same error)"
            :data '(("q" . "hi there")))
  (post-row ":form-data \"city=Zürich\"  (a string goes as UTF-8)"
            :form-data "city=Zürich")
  (post-row ":form-data ((\"k\" . \"v\")) :files ((\"f\" . \"file body\"))  (0.2.0: refused as a stream body)"
            :form-data '(("k" . "v")) :files '(("f" . "file body")))
  (post-row "pre-encoded multipart :content  (0.2.0: TYPE-ERROR in dexador's MULTIPART-CONTENT-LENGTH)"
            :content (octets *multipart-body*)
            :headers '(("content-type" . "multipart/form-data; boundary=XyZ")))
  (if (hp:content-coding-supported-p :gzip)
      (post-row ":form-data ((\"key1\" . \"value1\")) :content-encoding :gzip  (0.2.0: refused as a stream body)"
                :form-data '(("key1" . "value1")) :content-encoding :gzip)
      (note "~%  (no gzip content coding in this image: the coded form row is skipped)"))
  (note "    (the leading space in the last three Content-Type values is optional whitespace,")
  (note "     RFC 9112 §5, which the origin strips; it keeps dexador from encoding the body again)")
  (post-row "JSON :content with Content-Type application/json  (worked in 0.2.0 too)"
            :content (octets "{\"a\":1}") :headers '(("content-type" . "application/json")))
  (note "~%The #32 report through http-protocol's facade, http:post :form-data:")
  (let ((box (list nil)))
    (with-listener (origin (router box '()))
      (let ((res (let ((hp:*http-backend* *backend*))
                   (handler-case (http:post (local (listener-port origin) "/post")
                                            :form-data '(("key1" . "value1")) :timeout 5)
                     (error (e) e)))))
        (note "  (http:post url :form-data '((\"key1\" . \"value1\"))) => ~A; the origin got ~A"
              (short res) (if (car box) (escaped (cdr (first (car box)))) "nothing"))))))

(defun port-free (url port)
  (substitute-string url (princ-to-string port) "P"))

(defun history-summary (response port)
  (mapcar (lambda (r) (format nil "~D ~A" (hp:response-status r)
                              (port-free (hp:response-url r) port)))
          (hp:response-history response)))

(defun demo-5 ()
  (banner "Demo 5: redirects and response-history (#33)")
  (note "0.2.0 let dexador follow redirects and kept only the last response, so")
  (note "response-history was always empty. 0.2.1 follows them in the backend.")
  (let ((box (list nil)))
    (with-listener (origin (router box '(("/r/2" 302 "Found" "Location: /r/1")
                                         ("/r/1" 301 "Moved Permanently" "Location: /r/0"))))
      (let* ((port (listener-port origin))
             (res (handler-case (send* (client) :url (local port "/r/2") :timeout 5)
                    (error (e) e))))
        (note "~%  GET /r/2 (302 → /r/1, 301 → /r/0) => ~A, url ~A" (short res)
              (if (typep res 'hp:http-response) (port-free (hp:response-url res) port) "-"))
        (when (typep res 'hp:http-response)
          (note "    response-history: ~{~A~^, ~}" (history-summary res port))))))
  (let ((box (list nil)))
    (with-listener (origin (router box '(("/form" 303 "See Other" "Location: /done"))))
      (let* ((port (listener-port origin))
             (res (handler-case
                      (send* (client) :method :post :url (local port "/form")
                                      :form-data '(("key1" . "value1")) :timeout 5)
                    (error (e) e))))
        (note "~%  POST /form :form-data, 303 → /done => ~A" (short res))
        (when (typep res 'hp:http-response)
          (note "    response-history: ~{~A~^, ~}" (history-summary res port)))
        (loop for (lines . body) in (reverse (car box))
              do (note "    origin: ~A, Content-Type ~A, ~D body octets"
                       (first lines) (or (header lines "content-type") "none") (length body)))
        (note "    (dexador's own redirect sent the GET with the POST body, its Content-Type")
        (note "     and Content-Length again, and Host without the port)"))))
  (let ((box (list nil)))
    (with-listener (origin (router box '(("/keep" 307 "Temporary Redirect" "Location: /kept"))))
      (let ((res (handler-case
                     (send* (client) :method :post :url (local (listener-port origin) "/keep")
                                     :content (octets "abc")
                                     :headers '(("content-type" . "text/plain"))
                                     :timeout 5)
                   (error (e) e))))
        (note "~%  POST /keep, 307 → /kept => ~A" (short res))
        (loop for (lines . body) in (reverse (car box))
              do (note "    origin: ~A, body ~A" (first lines) (escaped body))))))
  (let ((box (list nil)))
    (with-listener (origin (router box '(("/loop" 302 "Found" "Location: /loop"))))
      (let ((res (handler-case
                     (send* (client) :url (local (listener-port origin) "/loop")
                                     :max-redirects 3 :timeout 5)
                   (error (e) e))))
        (note "~%  GET /loop (302 → /loop), :max-redirects 3 => ~A"
              (port-free (short res 110) (listener-port origin)))
        (note "    the origin saw ~D requests (0.2.0 returned the 4th 302 as the response)"
              (length (car box))))))
  (let ((box-a (list nil)) (box-b (list nil)))
    (with-listener (b (router box-b '()))
      (with-listener (a (router box-a (list (list "/go" 302 "Found"
                                                  (format nil "Location: ~A"
                                                          (local (listener-port b) "/there"))))))
        (let ((res (handler-case
                       (send* (client) :url (local (listener-port a) "/go")
                                       :auth '(:basic "user" "pass") :timeout 5)
                     (error (e) e))))
          (note "~%  GET A/go with basic auth, 302 → B/there => ~A" (short res))
          (when (typep res 'hp:http-response)
            (note "    response-history: ~{~A~^, ~}" (history-summary res (listener-port a))))
          (let ((at-a (car (first (car box-a))))
                (at-b (car (first (car box-b)))))
            (note "    A got Authorization: ~A" (if (and at-a (header at-a "authorization")) "yes" "no"))
            (note "    B got Authorization: ~A, Host: ~A (B listens on port P)"
                  (if (and at-b (header at-b "authorization")) "yes" "no")
                  (if at-b (port-free (header at-b "host") (listener-port b)) "-"))
            (note "    (0.2.0 sent Host: 127.0.0.1 to B, without the port)")))))))

(defun run-demos ()
  (setf *backend* (dexb:make-dexador-backend))
  (note "http-backend-dexador ~A, dexador ~A, http-protocol ~A, cl+ssl ~A, ~A ~A"
        (asdf:component-version (asdf:find-system "http-backend-dexador"))
        (asdf:component-version (asdf:find-system "dexador"))
        (asdf:component-version (asdf:find-system "http-protocol"))
        (or (asdf:component-version (asdf:find-system "cl+ssl")) "(no .asd version)")
        (lisp-implementation-type) (lisp-implementation-version))
  (demo-1)
  (demo-2)
  (demo-3)
  (demo-4)
  (demo-5)
  (banner "done")
  t)
