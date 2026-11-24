(in-package #:harness-demos-http-facade)

;;; cl-stack-http 0.2.0 (cl-stack-http#26, #27) and event-protocol 0.3.0
;;; (event-protocol#26), the gaps the "Python to Common Lisp" site found:
;;;   1. the requests-shaped surface: response readers and conditions are
;;;      external in cl-stack-http, so the QUICKSTART form reads and runs;
;;;   2. :auto: blocking calls on dexador, -async calls on http-backend-async
;;;      over libuv, and :async with no event-loop wiring;
;;;   3. asyncio.run / gather / await: run-promise, gather, await on a
;;;      promise, await-timeout, a rejection re-signalled.
;;; Every request goes to a loopback keep-alive origin (origin.lisp) that
;;; logs the connection, the path and the User-Agent of each request.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title "")
  (finish-output))

(defun note (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))

(defvar *port* nil)

(defun url (path)
  (format nil "http://127.0.0.1:~D~A" *port* path))

(defmacro timed (&body body)
  "→ (values seconds result-or-condition)."
  (let ((t0 (gensym)) (r (gensym)))
    `(let* ((,t0 (get-internal-real-time))
            (,r (handler-case (progn ,@body) (error (e) e))))
       (values (%seconds-since ,t0) ,r))))

(defun short (thing)
  (let ((s (if (typep thing 'condition)
               (format nil "~A: ~A" (type-of thing) thing)
               (princ-to-string thing))))
    (if (> (length s) 110) (concatenate 'string (subseq s 0 107) "...") s)))

(defun body-text (response)
  (http:response-text response))

(defun show-log (mark)
  (dolist (e (origin-log-since mark))
    (destructuring-bind (conn path ua secs) e
      (note "    origin  conn ~D  ~6,2F s  ~22A  User-Agent: ~A" conn secs path ua))))

(defun libuv ()
  (event-backend-libuv:make-libuv-backend))

;;; ------------------------------------------------------------------
;;; Demo 1: the facade surface (cl-stack-http#26)

(defparameter *facade-names*
  '("RESPONSE-STATUS" "RESPONSE-HEADER" "RESPONSE-HEADERS" "RESPONSE-URL"
    "RESPONSE-HTTP-VERSION" "RESPONSE-HISTORY" "RESPONSE-COOKIES"
    "RESPONSE-ELAPSED" "RESPONSE-BODY" "HTTP-ERROR" "HTTP-STATUS-ERROR"
    "HTTP-STATUS-ERROR-STATUS" "HTTP-TIMEOUT-ERROR" "MAKE-HTTP-TIMEOUT"
    "MAKE-HTTP-RETRY" "MAKE-SECRET" "REVEAL" "*HTTP-BACKEND*"))

(defparameter *quickstart-form*
  "(let ((r (http:get \"https://httpbin.org/get\" :params '((\"q\" . \"1\")))))
  (format t \"~a ~a~%\" (http:response-status r) (http:response-http-version r))
  (princ (http:response-text r)))"
  "cl-stack docs/QUICKSTART.md section 3, as printed there.")

(defun demo-1 ()
  (banner "Demo 1: the requests-shaped surface is external in cl-stack-http (#26)")
  (note "In 0.1.12 these were inherited from http-protocol, not external, so")
  (note "`http:response-status` did not read (SIMPLE-READER-PACKAGE-ERROR).")
  (note "~&  ~28A ~10A ~A" "symbol" "status" "same symbol as http-protocol's")
  (dolist (name *facade-names*)
    (multiple-value-bind (sym status) (find-symbol name :cl-stack-http)
      (note "  ~28A ~10S ~A" name status
            (if (eq sym (find-symbol name :http-protocol)) "yes" "NO"))))
  (note "~%Reading the QUICKSTART section 3 form with http = cl-stack-http:")
  (let* ((*package* (find-package :harness-demos-http-facade))
         (form (handler-case (read-from-string *quickstart-form*)
                 (error (e) e))))
    (if (typep form 'condition)
        (note "  read failed: ~A" (short form))
        (progn
          (note "  read OK: ~S with ~D body forms" (first form) (length (cddr form)))
          (note "~%Running it against the loopback origin instead of httpbin.org:")
          (let ((local (subst (url "/get") "https://httpbin.org/get" form :test #'equal))
                (mark (origin-mark)))
            (handler-case (progn (format t "  ") (eval local) (terpri))
              (error (e) (note "  failed: ~A" (short e))))
            (show-log mark)))))
  (note "~%Conditions through the facade package:")
  (multiple-value-bind (secs res)
      (timed (http:raise-for-status (http:get (url "/status/404"))))
    (declare (ignore secs))
    (note "  raise-for-status on a 404: ~A, http-status-error-status => ~A"
          (type-of res)
          (if (typep res 'http:http-status-error) (http:http-status-error-status res) "-")))
  (multiple-value-bind (secs res)
      (timed (http:get (url "/delay/3") :timeout (http:make-http-timeout :read 0.5)))
    (note "  GET /delay/3 with (make-http-timeout :read 0.5): ~A after ~,1F s (typep http:http-timeout-error: ~A)"
          (type-of res) secs (typep res 'http:http-timeout-error))))

;;; ------------------------------------------------------------------
;;; Demo 2: the two :auto orders (cl-stack-http#27)

(defvar *maker-at-start* :unread)

(defun demo-2 ()
  (banner "Demo 2: :auto - dexador for blocking calls, async/libuv for -async calls (#27)")
  (note "http-backend-async:*event-backend-maker* before any call: ~S" *maker-at-start*)
  (note "event-protocol:*event-backend* before any call: ~S" ev:*event-backend*)
  (note "(ensure-http-backend :auto) => ~A backend"
        (hp:backend-name (http:ensure-http-backend :auto)))
  (let ((mark (origin-mark)))
    (multiple-value-bind (secs res) (timed (http:get (url "/blocking")))
      (note "~%Blocking (http:get ...) under :auto: ~A in ~,2F s"
            (if (typep res 'condition) (short res) (hp:response-status res)) secs))
    (multiple-value-bind (secs res)
        (timed (evp:run-promise (lambda () (http:get-async (url "/get-async")))
                                :backend (libuv) :timeout 10))
      (note "(http:get-async ...) under :auto, awaited with run-promise: ~A in ~,2F s"
            (if (typep res 'condition) (short res) (hp:response-status res)) secs))
    (show-log mark))
  (note "~%(with-backend (:async) (http:get ...)), no event-loop setup by the caller:")
  (let ((mark (origin-mark)))
    (multiple-value-bind (secs res)
        (timed (http:with-backend (:async) (http:get (url "/with-backend-async"))))
      (note "  => ~A in ~,2F s (0.1.12: HTTP-ERROR \"No event loop\")"
            (if (typep res 'condition) (short res) (hp:response-status res)) secs))
    (show-log mark))
  (note "  http-backend-async:*event-backend-maker* now: ~A"
        (if http-backend-async:*event-backend-maker* "set by cl-stack-http" "NIL"))
  (note "~%Blocking :auto calls from a pool of 4 threads, 5 GETs each:")
  (let* ((mark (origin-mark))
         (results (make-array 4 :initial-element nil))
         (threads (loop for i below 4
                        collect (let ((i i))
                                  (bt:make-thread
                                   (lambda ()
                                     (setf (aref results i)
                                           (loop repeat 5
                                                 collect (handler-case
                                                             (hp:response-status
                                                              (http:get (url (format nil "/pool/~D" i))))
                                                           (error (e) (type-of e))))))
                                   :name "demo pool")))))
    (mapc #'bt:join-thread threads)
    (note "  statuses per thread: ~S" (coerce results 'list))
    (let ((log (origin-log-since mark)))
      (note "  ~D requests over ~D connection~:P, User-Agents: ~{~A~^, ~}"
            (length log)
            (length (remove-duplicates (mapcar #'first log)))
            (remove-duplicates (mapcar #'third log) :test #'string=))))
  (note "~%A default session: blocking calls on dexador, -async calls on async:")
  (http:with-session (s :trust-env nil)
    (note "  (session-backend s) => ~A" (hp:backend-name (http:session-backend s)))
    (let ((mark (origin-mark)))
      (note "  session-get => ~A" (hp:response-status (http:session-get s (url "/session-get"))))
      (multiple-value-bind (secs res)
          (timed (evp:run-promise
                  (lambda ()
                    (evp:gather (http:session-get-async s (url "/session-get-async/1"))
                                (http:session-get-async s (url "/session-get-async/2"))))
                  :backend (libuv) :timeout 10))
        (note "  two session-get-async, gathered => ~A in ~,2F s"
              (if (typep res 'condition) (short res) (mapcar #'hp:response-status res)) secs))
      (show-log mark))))

;;; ------------------------------------------------------------------
;;; Demo 3: asyncio.run / gather / await (event-protocol#26)

(defun free-port ()
  (let* ((s (usocket:socket-listen "127.0.0.1" 0))
         (p (usocket:get-local-port s)))
    (usocket:socket-close s)
    p))

(defun demo-3 ()
  (banner "Demo 3: run-promise, gather and await (event-protocol 0.3.0, #26)")
  (note "asyncio.run(asyncio.gather(get(a), get(b), get(c))), each answer 1 s late:")
  (let ((mark (origin-mark)))
    (multiple-value-bind (secs res)
        (timed (evp:run-promise
                (lambda ()
                  (evp:gather (http:get-async (url "/delay/1?n=a"))
                              (http:get-async (url "/delay/1?n=b"))
                              (http:get-async (url "/delay/1?n=c"))))
                :backend (libuv) :timeout 10))
      (note "  => ~A after ~,2F s (one at a time would take 3 s)"
            (if (typep res 'condition) (short res) (mapcar #'hp:response-status res)) secs)
      (sleep 0.2)
      (note "  run-promise returned although the origin still has ~D idle keep-alive connection~:P"
            (origin-open-connections))
      (note "  open from the clients' pools. await stops the loop once the promise settles;")
      (note "  event-protocol 0.2.3's run waited 61.9 s for idle connections (http-backend-async#79)."))
    (show-log mark))
  (note "~%gather keeps argument order, whatever order the answers come in:")
  (multiple-value-bind (secs res)
      (timed (evp:run-promise
              (lambda ()
                (evp:gather (http:get-async (url "/delay/0.6"))
                            (http:get-async (url "/delay/0.1"))
                            (http:get-async (url "/delay/0.3"))))
              :backend (libuv) :timeout 10))
    (note "  => ~,2F s: ~{~A~^ | ~}" secs
          (if (typep res 'condition) (list (short res)) (mapcar #'body-text res))))
  (note "~%await on a promise, not only on a (lambda (resolve reject)) starter:")
  (multiple-value-bind (secs res)
      (timed (evp:run-promise
              (lambda () (evp:defer-promise ev:*event-loop* (lambda () 42)))
              :backend (libuv)))
    (note "  (run-promise (lambda () (defer-promise *event-loop* (lambda () 42)))) => ~A in ~,3F s"
          (short res) secs))
  (multiple-value-bind (secs res)
      (timed (evp:run-promise (lambda () :not-a-promise) :backend (libuv)))
    (note "  a thunk that returns a plain value => ~S in ~,3F s" res secs))
  (note "~%asyncio.wait_for: :timeout signals await-timeout:")
  (multiple-value-bind (secs res)
      (timed (evp:run-promise
              (lambda () (evp:sleep-promise ev:*event-loop* 5))
              :backend (libuv) :timeout 0.5))
    (note "  sleep-promise 5 s under :timeout 0.5 => ~A after ~,2F s, await-timeout-seconds ~A"
          (type-of res) secs
          (if (typep res 'evp:await-timeout) (evp:await-timeout-seconds res) "-")))
  (note "~%A rejected promise is re-signalled by run-promise:")
  (multiple-value-bind (secs res)
      (timed (evp:run-promise
              (lambda () (http:get-async (format nil "http://127.0.0.1:~D/" (free-port))
                                         :timeout 2))
              :backend (libuv) :timeout 10))
    (note "  get-async to a closed port => ~A after ~,2F s (typep http:http-error: ~A)"
          (type-of res) secs (typep res 'http:http-error))))

(defparameter +demo-proxy-credentials+ "dXNlcjpzZWNyZXQ="
  "user:secret in base64: the made-up proxy credentials of demo 4.")

(defun %hex-prefix (octets &optional (n 6))
  (format nil "~{~2,'0X~^ ~}"
          (coerce (subseq octets 0 (min n (length octets))) 'list)))

(defun %client-hello-p (octets)
  (and (>= (length octets) 6)
       (= #x16 (aref octets 0))
       (= 3 (aref octets 1))
       (= 1 (aref octets 5))))

(defun %head-lines (head)
  (loop with start = 0
        for end = (search (coerce '(#\Return #\Newline) 'string) head :start2 start)
        while (and end (> end start))
        collect (subseq head start end)
        do (setf start (+ end 2))))

(defun %proxy-auth-line (head)
  (find-if (lambda (line) (eql 0 (search "proxy-authorization:" (string-downcase line))))
           (%head-lines head)))

(defparameter *proxy-variables*
  '("https_proxy" "HTTPS_PROXY" "http_proxy" "HTTP_PROXY"
    "all_proxy" "ALL_PROXY" "no_proxy" "NO_PROXY"))

(defun %call-with-https-proxy-env (proxy thunk)
  "Call THUNK with https_proxy=PROXY the only proxy variable set."
  (let ((saved (mapcar (lambda (name) (cons name (uiop:getenv name))) *proxy-variables*)))
    (unwind-protect
        (progn
          (dolist (name *proxy-variables*)
            (setf (uiop:getenv name) ""))
          (setf (uiop:getenv "https_proxy") proxy)
          (funcall thunk))
      (dolist (name *proxy-variables*)
        (setf (uiop:getenv name) ""))
      (loop for (name . value) in saved
            when (plusp (length value))
              do (setf (uiop:getenv name) value)))))

(defun %via-https-proxy (backend url &key verify)
  "SESSION-GET of URL on BACKEND in a trust-env session, with a fresh
   loopback https:// proxy named by HTTPS_PROXY. → (values result record seconds)."
  (multiple-value-bind (port record stop) (start-tls-proxy)
    (multiple-value-bind (secs res)
        (%call-with-https-proxy-env
         (format nil "https://user:secret@127.0.0.1:~D" port)
         (lambda ()
           (timed (http:with-session (s :backend backend :verify verify :timeout 10)
                    (http:session-get s url)))))
      (funcall stop)
      (values res record secs))))

(defun %show-proxy-view (record)
  (cond
   ((not (pr-connected record))
    (note "      proxy: no connection, not one octet"))
   (t
    (note "      proxy, first octets off the socket: ~A  ~A"
          (%hex-prefix (pr-raw record))
          (if (%client-hello-p (pr-raw record))
              "(TLS handshake record: a ClientHello)"
              "(not TLS)"))
    (when (pr-cleartext record)
      (note "      in cleartext: ~{~A~^ | ~}" (%head-lines (pr-cleartext record))))
    (when (plusp (length (pr-inside record)))
      (note "      inside TLS: ~A" (first (%head-lines (pr-inside record))))
      (note "                  ~A" (or (%proxy-auth-line (pr-inside record))
                                       "(no Proxy-Authorization)")))
    (when (and (%client-hello-p (pr-raw record))
               (zerop (length (or (pr-inside record) ""))))
      (note "      inside TLS: nothing (the client ended the handshake)"))
    (when (plusp (length (pr-tunnel record)))
      (note "      tunnel, first octets: ~A  ~A"
            (%hex-prefix (pr-tunnel record))
            (if (%client-hello-p (pr-tunnel record))
                "(the origin's ClientHello: TLS inside TLS)"
                "(not TLS)")))
    (when (pr-origin record)
      (note "      origin, inside the inner TLS: ~A (Proxy-Authorization ~A)"
            (first (%head-lines (pr-origin record)))
            (if (%proxy-auth-line (pr-origin record)) "present" "absent")))
    (note "      credentials in the octets off the socket: ~A"
          (if (search +demo-proxy-credentials+
                      (babel:octets-to-string (pr-raw record) :encoding :latin-1))
              "YES, in cleartext"
              "no")))))

(defun %call-trusting-demo-ca (thunk)
  "THUNK with a TLS context that trusts certs/ca.crt (and only it)."
  (let ((ctx (cl+ssl:make-context :verify-location (%cert "ca.crt"))))
    (cl+ssl:with-global-context (ctx :auto-free-p t)
      (funcall thunk))))

(defun %result-text (res)
  (if (typep res 'condition)
      (short res)
      (format nil "~D ~S" (hp:response-status res) (body-text res))))

(defun demo-4 ()
  (banner "Demo 4: https:// proxies are reached over TLS (http-protocol#53)")
  (note "HTTPS_PROXY=https://user:secret@127.0.0.1:P (made-up credentials), read by a trust-env")
  (note "session. The proxy is a loopback TLS listener (tls-proxy.lisp). It logs the first octets")
  (note "off the socket and what it reads inside TLS; through CONNECT it is the origin as well.")
  (note "Before: http-protocol 0.4.4 classed the proxy :http and http-backend-async 0.2.28 sent")
  (note "the request line and Proxy-Authorization to it in plain TCP (http-backend-async#81).")
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    ;; async offers h2 over ALPN, so its first https request loads
    ;; http2/client. Load it now, quietly, to keep its compiler output out.
    (ignore-errors (asdf:load-system "http2/client")))
  (let ((refusal nil))
    (dolist (backend '(:async :dexador))
      (dolist (url '("http://origin.test/demo" "https://origin.test/demo"))
        (multiple-value-bind (res record secs) (%via-https-proxy backend url)
          (when (typep res 'hp:proxy-tls-unsupported)
            (setf refusal res))
          (note "~%  ~(~8A~) ~26A => ~A (~,2F s)" backend url (%result-text res) secs)
          (%show-proxy-view record))))
    (when refusal
      (note "~%dexador's refusal, in full: ~A" refusal)
      (note "  typep unsupported-operation: ~A, operation ~S"
            (typep refusal 'hp:unsupported-operation)
            (hp:unsupported-operation-operation refusal))
      ;; Once more, to look at the restart while the condition is live.
      (let ((report nil))
        (multiple-value-bind (port record stop) (start-tls-proxy)
          (declare (ignore record))
          (%call-with-https-proxy-env
           (format nil "https://user:secret@127.0.0.1:~D" port)
           (lambda ()
             (handler-case
                 (handler-bind ((hp:proxy-tls-unsupported
                                 (lambda (c)
                                   (let ((restart (find-restart 'use-value c)))
                                     (setf report (and restart (princ-to-string restart)))))))
                   (http:with-session (s :backend :dexador :timeout 10)
                     (http:session-get s "https://origin.test/demo")))
               (error () nil))))
          (funcall stop))
        (note "  its USE-VALUE restart: ~A" (or report "none")))))
  (note "~%The proxy hop uses the session's TLS settings (async, :verify t):")
  (multiple-value-bind (res record secs)
      (%call-trusting-demo-ca
       (lambda () (%via-https-proxy :async "https://origin.test/demo" :verify t)))
    (note "~%  test CA trusted    https://origin.test/demo => ~A (~,2F s)" (%result-text res) secs)
    (note "      (the proxy's certificate checked against 127.0.0.1, the origin's against origin.test)")
    (%show-proxy-view record))
  (multiple-value-bind (res record secs)
      (%via-https-proxy :async "https://origin.test/demo" :verify t)
    (note "~%  CA not trusted     https://origin.test/demo => ~A (~,2F s)" (%result-text res) secs)
    (%show-proxy-view record)))

(defun run-demos ()
  (setf *maker-at-start* http-backend-async:*event-backend-maker*)
  (note "cl-stack-http ~A, http-protocol ~A, event-protocol ~A, http-backend-dexador ~A, http-backend-async ~A, event-backend-libuv ~A, dexador ~A"
        (asdf:component-version (asdf:find-system "cl-stack-http"))
        (asdf:component-version (asdf:find-system "http-protocol"))
        (asdf:component-version (asdf:find-system "event-protocol"))
        (asdf:component-version (asdf:find-system "http-backend-dexador"))
        (asdf:component-version (asdf:find-system "http-backend-async"))
        (asdf:component-version (asdf:find-system "event-backend-libuv"))
        (asdf:component-version (asdf:find-system "dexador")))
  (multiple-value-bind (port stop) (start-origin)
    (setf *port* port)
    (unwind-protect
        (progn
          ;; The first facade request loads the optional content-encoding
          ;; codecs (brotli, zstd, snappy); keep their compiler output out.
          (let ((*standard-output* (make-broadcast-stream))
                (*error-output* (make-broadcast-stream)))
            (ignore-errors
              (http:with-backend (:dexador) (http:get (url "/warm-up")))))
          (note "(a warm-up request on dexador loaded the optional content-encoding codecs)")
          (demo-1) (demo-2) (demo-3) (demo-4))
      (funcall stop)))
  (banner "done")
  t)
