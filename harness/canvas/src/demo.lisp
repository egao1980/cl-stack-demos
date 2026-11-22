(in-package #:harness-demos-canvas)

;;; The first canvas demo (H4h). A canvas is a plain function in this image:
;;; keyword strings in, a canvas-protocol tree out. Nothing is written to disk
;;; except the frozen dashboard.html at the end.

;;; ---- Image state -----------------------------------------------------------

(defparameter *tests*                   ; assertions per system, this session's runs
  '((:system "policy-protocol" :tests 445) (:system "rpc-backend-htmx" :tests 258)
    (:system "canvas-backend-svg" :tests 495) (:system "compute-backend-unkernel" :tests 394)
    (:system "agent-eval" :tests 487)))

(defparameter *timings*                 ; harness/unkernel section 6, ms: (guest . host)
  '(("generic loop" 2052 . 262) ("fixnum loop" 163 . 47) ("cons/sort/GC" 5471 . 591)))

;;; engels as the canvas theme, plus the --chart-1..8 series rules. canvas-protocol
;;; 0.1.1 has only :placeholder (the :engels method is the H4h-0b minor).
(defmethod canvas:theme-stylesheet ((theme (eql :engels)))
  (stack-encoding:encode (concatenate 'string (engels:css :minify t) (string #\Newline)
                                             (canvas:chart-css))
                                :encoding :utf-8))

;;; ---- Canvases: plain functions ---------------------------------------------

(defun test-table (&key sort)
  (canvas:table *tests*
                (list (canvas:column "System" :system :name "system" :sortable t)
                      (canvas:column "Assertions" :tests :name "tests" :sortable t :align :end))
                :id "tests" :link (canvas:canvas-link 'test-table) :sort sort
                :caption "Assertions per system (click a header to sort)"))

(defun test-chart (&key min)
  "Bar chart of the systems with at least MIN assertions."
  (multiple-value-bind (form errors)
      (canvas:coerce-form (list :min min) '((:min :integer :default 0 :min 0)))
    (let ((rows (remove-if (lambda (r) (< (getf r :tests) (getf form :min))) *tests*)))
      (canvas:el :div '(:id "chart")
                 (mapcar #'canvas:error-fragment errors)
                 (if rows
                     (canvas:make-chart :bar :title (format nil "Assertions (≥ ~d)" (getf form :min))
                       :series (list (canvas:make-series "assertions"
                                       (loop for r in rows collect (cons (getf r :system) (getf r :tests))))))
                     (canvas:el :p nil "No system has that many assertions."))))))

(defun threshold-form (&key (min "0"))
  "Posts to TEST-CHART; the response replaces only #chart."
  (canvas:form (list (canvas:number-input "min" :label "Minimum assertions" :value min :min 0)
                     (canvas:submit-button "Filter"))
               :link (canvas:canvas-link 'test-chart) :target "#chart" :swap :outer-html))

(defun timing-chart (&key)
  (canvas:make-chart :line :title "unkernel: SBCL workloads, guest vs host"
    :x-axis (canvas:make-axis :label "workload" :ticks '(1 2 3))
    :y-axis (canvas:make-axis :label "ms" :scale :log)
    :series (list (canvas:make-series "guest (wasm)" (loop for (nil g) in *timings* for i from 1 collect (cons i g)))
                  (canvas:make-series "host" (loop for (nil nil . h) in *timings* for i from 1 collect (cons i h))))))

(defun ticker (stream &key (n "3"))
  "SSE canvas: one <p> per event \"row\"."
  (dotimes (i (canvas:coerce-param n :integer :default 3 :min 1 :max 10))
    (rpc:rpc-send stream (canvas:fragment-event "row" (canvas:el :p nil (format nil "tick ~d: ~a ok" (1+ i) (getf (nth (mod i (length *tests*)) *tests*) :system)))))
    (sleep 0.1)))

(defun dashboard (&key)              ; &key: handlers are called with :allow-other-keys t
  (canvas:el :main nil (canvas:stack
   (list (canvas:el :h1 nil "Canvas demo: this session's test runs")
         (canvas:grid (list (canvas:card (test-table) :title "Test counts")
                            (canvas:card (list (threshold-form) (test-chart)) :title "Filter"))
                      :columns 2)
         (canvas:card (timing-chart) :title "Timings")
         (canvas:card (canvas:live-region (canvas:canvas-link 'ticker :n "3") "row" :id "ticks" :swap :before-end
                                         :content (canvas:el :p nil "Ticks stream in here over SSE (the frozen file has none)."))
                      :title "Live (SSE)")))))

;;; ---- Server: allowlist, token, CSP ------------------------------------------

(defparameter +csp+ "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'")
(defparameter *handlers*
  (rh:make-htmx-handlers 'dashboard 'test-table 'test-chart 'threshold-form '(ticker :mode :call-stream)))
(defvar *transport* (rhc:make-canvas-htmx-transport :page-title "canvas demo"))

(defun with-csp (response)              ; a (status headers body) list or a streaming function
  (flet ((add (r) (list* (first r) (list* :content-security-policy +csp+ (second r)) (cddr r))))
    (if (functionp response)
        (lambda (responder) (funcall response (lambda (r) (funcall responder (add r)))))
        (add response))))

(defun guarded-app (token)
  "The canvas app behind a one-time ?token= URL that sets an HttpOnly cookie, and the CSP."
  (let ((app (rh:htmx-app *transport* *handlers*)) (url-token token))
    (lambda (env)
      (let* ((headers (getf env :headers))
             (cookie (or (gethash "cookie" headers) ""))
             (query (quri:url-decode-params (or (getf env :query-string) ""))))
        (with-csp
            (cond ((search (format nil "canvas-token=~a" token) cookie) (funcall app env))
                  ((and url-token (equal url-token (cdr (assoc "token" query :test #'string=))))
                   (setf url-token nil)        ; one-time
                   (list 303 (list :location (getf env :path-info)
                                   :set-cookie (format nil "canvas-token=~a; HttpOnly; SameSite=Strict; Path=/" token))
                         '("")))
                  (t (list 403 '(:content-type "text/plain") '("forbidden: no canvas token")))))))))

;;; ---- Driving it -------------------------------------------------------------

(defun banner (title) (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))
(defun clip (s &optional (n 700)) (if (> (length s) n) (format nil "~a … [~d chars]" (subseq s 0 n) (length s)) s))
(defun redact (s token)
  (let* ((s (format nil "~{~a~^, ~}" (if (listp s) s (list s)))) (i (search token s)))
    (if i (concatenate (quote string) (subseq s 0 i) "<token>" (subseq s (+ i (length token)))) s)))
(defun text (body) (if (stringp body) body (stack-encoding:decode-octets body :encoding :utf-8)))
(defun url (port name &optional (query "")) (format nil "http://127.0.0.1:~d/canvas/~a~a" port (rh:htmx-method-name name) query))

(defun get* (url &rest headers)
  (let ((r (http:request :get url :headers headers :timeout 10 :max-redirects 0)))
    (values (hp:response-status r) (text (hp:response-body r)) r)))

(defun demo-in-process (token)
  (banner "1. In-process htmx client (no socket): fragments, the form, SSE, the allowlist")
  (let ((c (rh:make-htmx-client (guarded-app token) :codec (rh:htmx-transport-codec *transport*)))
        (cookie (format nil "canvas-token=~a" token)))
    (flet ((call (name params &rest keys)
             (let ((r (apply #'rh:htmx-request c (rh:htmx-method-name name) params :headers `(("cookie" . ,cookie)) keys)))
               (format t "~&~a ~s → ~d~%  ~a~%" name params (rh:htmx-reply-status r) (clip (text (rh:htmx-reply-body r)) 500))
               r)))
      (call 'test-table '(:sort "-tests"))
      (call 'threshold-form '(:min "400"))
      (call 'test-chart '(:min "400") :http-method :post)
      (call 'test-chart '(:min "lots") :http-method :post)
      (format t "~&ticker (Accept: text/event-stream) → events ~s~%"
              (rh:htmx-reply-events (call 'ticker '(:n "3") :accept "text/event-stream")))
      (format t "~&not in the allowlist: CL-USER::SECRET → ~d~%"
              (rh:htmx-reply-status (rh:htmx-request c "COMMON-LISP-USER::SECRET" nil :headers `(("cookie" . ,cookie))))))))

(defun demo-http (port token)
  (banner "2. Real HTTP (http-backend-async on libuv) against Hunchentoot on 127.0.0.1")
  (format t "no token        → ~d~%" (get* (url port 'dashboard)))
  (multiple-value-bind (status body r) (get* (url port 'dashboard (format nil "?token=~a" token)))
    (declare (ignore body))
    (format t "one-time URL    → ~d, Location ~a~%                  Set-Cookie ~a~%" status
            (hp:response-header r "location") (redact (hp:response-header r "set-cookie") token)))
  (let ((hp:*http-client* (hp:make-http-client hp:*http-backend*))) ; a client without the cookie
    (format t "same URL again, another client → ~d (the URL token is spent)~%"
            (get* (url port 'dashboard (format nil "?token=~a" token)))))
  (multiple-value-bind (status body r) (get* (url port 'dashboard))
    (format t "~&GET dashboard (no HX-Request: full page) → ~d ~a~%Content-Security-Policy: ~a~%~a~%"
            status (hp:response-header r "content-type") (hp:response-header r "content-security-policy") (clip body 900))
    (let ((i (search "<div id=\"ticks\"" body))) (format t "~&the live region in that page: ~a~%" (subseq body i (+ 6 (search "</div>" body :start2 i)))))
    (format t "~&csp-violations of the served dashboard tree: ~s~%" (let ((canvas:*canvas-resolver* (rhc:make-htmx-resolver *transport* *handlers*))) (canvas:csp-violations (dashboard)))))
  (let ((r (http:request :post (url port 'test-chart) :form-data '(("min" . "450")) :headers '(("hx-request" . "true")) :timeout 10)))
    (format t "~&form submit: POST min=450 to TEST-CHART (HX-Request) → ~d~%  ~a~%" (hp:response-status r) (clip (text (hp:response-body r)) 400)))
  (multiple-value-bind (status body) (get* (url port 'ticker) '("accept" . "text/event-stream"))
    (format t "~&SSE ticker → ~d~%~a" status body))
  (format t "~&asset htmx.min.js → ~d (served from 'self', htmx ~a)~%" (get* (format nil "http://127.0.0.1:~d/canvas-assets/htmx.min.js" port)) canvas:+htmx-version+))

(defun demo-redefine (port)
  (banner "4. Redefine the chart canvas live: no restart, the next request shows it")
  (let ((before (nth-value 1 (get* (url port 'test-chart) '("hx-request" . "true"))))
        (form '(defun test-chart (&key min)
                (declare (ignore min))
                (canvas:el :div '(:id "chart")
                  (canvas:make-chart :scatter :title "Assertions (redefined: scatter, vermillion)"
                    :series (list (canvas:make-series "assertions" (loop for r in *tests* for i from 1 collect (cons i (getf r :tests)))
                                                      :color "#D55E00")))))))
    (let ((*package* (find-package :harness-demos-canvas))) (let ((*print-case* :downcase)) (format t "~s~%" form)) (eval form))
    (let ((after (nth-value 1 (get* (url port 'test-chart) '("hx-request" . "true")))))
      (format t "before: ~a~%after:  ~a~%" (subseq before 0 60) (subseq after 0 60))
      (format t "bar before: ~s, scatter after: ~s, color=\"#D55E00\" after: ~s~%"
              (and (search "data-chart=\"bar\"" before) t) (and (search "data-chart=\"scatter\"" after) t) (and (search "color=\"#D55E00\"" after) t)))))

(defun demo-freeze (dir)
  (banner "3. freeze: the dashboard as one self-contained static file")
  (let* ((html (canvas:freeze 'dashboard :title "Canvas demo"))
         (path (merge-pathnames "dashboard.html" dir)))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8) (write-string html o))
    (format t "wrote dashboard.html: ~d chars, deterministic: ~s~%" (length html) (string= html (canvas:freeze 'dashboard :title "Canvas demo")))
    (dolist (needle '("http:" "https:" "//" "url(" "@import" "<script" "<link" "hx-" "sse-"))
      (format t "  ~10a ~d occurrence~:p~%" needle (loop for i = (search needle html) then (search needle html :start2 (1+ i)) while i count t)))
    path))

(defun run-demos (&key (port 18467) (dir (uiop:getcwd)))
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend
        canvas:*canvas-theme* :engels)
  (let* ((token (format nil "~(~{~2,'0x~}~)" (coerce (stack-crypto:generate-key :nbytes 16) 'list)))
         (hp:*http-backend* (http-backend-async:make-async-backend))
         (hp:*http-client* (hp:make-http-client hp:*http-backend*)))
    (format t "canvas-protocol ~a, theme ~s, chart backend ~s, htmx ~a + sse ~a~%token: ~d hex chars (printed as <token>)~%"
            (asdf:component-version (asdf:find-system "canvas-protocol")) canvas:*canvas-theme*
            (type-of canvas:*chart-backend*) canvas:+htmx-version+ canvas:+htmx-sse-version+ (length token))
    (demo-in-process token)
    (srv:with-server (s (guarded-app token) :port port)
      (format t "~&serving on http://127.0.0.1:~d/canvas/ (Hunchentoot)~%" port)
      (demo-http port token)
      (demo-freeze dir)                 ; before the redefinition: the bar chart
      (demo-redefine port))
    (format t "~&~%server stopped~%")
    t))
