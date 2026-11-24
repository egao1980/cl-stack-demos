;;;; A tour of doc-extract-backend-pdf 0.2.0 RENDER-PAGE: PDF pages rasterized by
;;;; libpdfium (the package's native overlay, from GHCR) into image-protocol
;;;; images. The input is fixtures/pages.pdf, three pages written byte by byte for
;;;; this demo (fixtures/PROVENANCE.md). Every number printed is computed by this
;;;; run. The self-checks compare the sizes with ones worked out by hand, probe
;;;; pixels with the fixture's colors, every format with image-protocol's own
;;;; conversion, and the PNG with its decoded pixels. The summary reports the
;;;; mismatches.

(in-package #:harness-demos-pdf-render)

;;; Bookkeeping -----------------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args))

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (actual expected label &key (test #'equal))
  "Count one self-check; print it only when it fails. Returns ACTUAL."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected))
  actual)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

(defun ms-since (start)
  (/ (* 1000d0 (- (get-internal-real-time) start)) internal-time-units-per-second))

(defmacro timed (&body body)
  "(values milliseconds . BODY's values)."
  (let ((start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (let ((values (multiple-value-list (progn ,@body))))
         (values-list (cons (ms-since ,start) values))))))

(defun signalled (thunk)
  "The error THUNK signals, or NIL when it returns."
  (handler-case (progn (funcall thunk) nil)
    (error (c) c)))

(defun new-restarts (condition outer)
  "Restarts visible for CONDITION that were not in OUTER, innermost first: the
ones established between the caller and the signal."
  (remove-if (lambda (r) (member r outer)) (compute-restarts condition)))

(defun find-restart-named (name condition)
  "The restart for CONDITION whose name is NAME (a string), in any package."
  (find name (compute-restarts condition)
        :key (lambda (r) (and (restart-name r) (symbol-name (restart-name r))))
        :test #'equal))

(defun tidy (namestring)
  "NAMESTRING with the home directory written as ~/."
  (let ((home (namestring (user-homedir-pathname))))
    (if (and (>= (length namestring) (length home))
             (string= home namestring :end2 (length home)))
        (concatenate 'string "~/" (subseq namestring (length home)))
        namestring)))

(defun version-of (system)
  (asdf:component-version (asdf:find-system system)))

(defun sha256-hex (octets)
  "SHA-256 of OCTETS through crypto-protocol, as lower-case Base16 through
encoding-protocol."
  (string-downcase (stack-encoding:encode (crypto:digest octets :algorithm :sha256)
                                          :encoding :base16)))

;;; Inputs ------------------------------------------------------------------------

(defun here (relative)
  (asdf:system-relative-pathname "harness-demos-pdf-render" relative))

(defvar *pdf-path* nil "fixtures/pages.pdf as a pathname.")
(defvar *pdf* nil "The same file as an octet vector, the source most calls use.")

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defparameter *page-notes*
  '((0 "300 x 200 pt: red, green, blue rectangles, a gray bar, Helvetica")
    (1 "A4 portrait, 595.2756 x 841.8898 pt: a black 2 x 2 inch square")
    (2 "300 x 200 pt CropBox in a 400 x 400 MediaBox, /Rotate 90"))
  "What each page of the fixture holds.")

(defparameter *expected-sizes*
  ;; (page dpi width height), worked out by hand from the point sizes:
  ;; points x dpi / 72 to the nearest pixel, halves up. Page 2 shows its
  ;; 300 x 200 pt CropBox turned by /Rotate 90, so it is 200 x 300 on screen.
  '((0  72  300  200) (0 150  625  417) (0 300 1250  833)
    (1  72  595  842) (1 150 1240 1754) (1 300 2480 3508)
    (2  72  200  300) (2 150  417  625) (2 300  833 1250)))

(defparameter *probes*
  ;; (page x-pt y-pt rgba label): points from the top-left corner of the page
  ;; as shown, at the middle of a shape, so every dpi lands inside it.
  '((0  60  50 (255 0 0 255) "red")
    (0 150  50 (0 255 0 255) "green")
    (0 240  50 (0 0 255 255) "blue")
    (0   5   5 (255 255 255 255) "white")
    (1 144 144 (0 0 0 255) "black square")
    (1 500 800 (255 255 255 255) "white")
    (2 100  75 (255 0 0 255) "red top half")
    (2 100 225 (255 255 255 255) "white bottom half")))

;;; Pixels ------------------------------------------------------------------------

(defun pixel (image x y)
  "The channel values of pixel (X, Y) of IMAGE, top-left origin, as a list."
  (let* ((n (img:channels (img:image-format image)))
         (i (* n (+ x (* y (img:image-width image))))))
    (coerce (subseq (img:image-pixels image) i (+ i n)) 'list)))

(defun at (image x-pt y-pt dpi)
  "PIXEL of IMAGE, rendered at DPI, under the point (X-PT, Y-PT) of the page."
  (pixel image (floor (* x-pt dpi) 72) (floor (* y-pt dpi) 72)))

(defun count-rgba (image predicate)
  "Pixels of the :RGBA IMAGE for which (PREDICATE r g b a) is true."
  (let ((p (img:image-pixels image)))
    (declare (type (simple-array (unsigned-byte 8) (*)) p)
             (type function predicate))
    (loop for i of-type fixnum from 0 below (length p) by 4
          count (funcall predicate (aref p i) (aref p (+ i 1)) (aref p (+ i 2)) (aref p (+ i 3))))))

(defun luma (r g b)
  "BT.601 luma of 8-bit R G B, 0.299 R + 0.587 G + 0.114 B, rounded half up."
  (floor (+ (* 299/1000 r) (* 587/1000 g) (* 114/1000 b) 1/2)))

;;; 1. The input ------------------------------------------------------------------

(defun demo-input ()
  (out "fixtures/pages.pdf: ~:d bytes, sha256 ~a~%" (length *pdf*) (sha256-hex *pdf*))
  (out "Three pages written byte by byte by fixtures/make-pages-pdf.py (no third-party~%~
        content; Helvetica is named, not embedded, so pdfium substitutes a font for it):~%")
  (dolist (note *page-notes*)
    (out "  page ~d  ~a~%" (first note) (second note)))
  (let ((meta (dx:extract-metadata (pdf:make-pdf-doc-extract-backend) *pdf* :format :pdf)))
    (let ((*print-pretty* nil))
      (out "~%dx:extract-metadata on the same octets (the 0.1 backend API):~%  ~s~%" meta))
    (check (getf meta :page-count) 3 "page count")
    (check (getf meta :title) "Render demo pages" "title")))

;;; 2. Sizes and timing -----------------------------------------------------------

(defun probe-page (page image dpi)
  "Check the probes of PAGE in IMAGE (rendered at DPI); the labels that matched."
  (loop for (p x y rgba label) in *probes*
        when (= p page)
          collect (progn (check (at image x y dpi) rgba
                                (format nil "page ~d at ~d dpi, ~a" page dpi label))
                         label)))

(defun demo-sizes ()
  (out "(render-page document index :dpi d) returns an :rgba image (the default format)~%~
        on a white background. Each side is points x dpi / 72 pixels, to the nearest~%~
        pixel, halves up. The expected sizes were worked out by hand.~%~%")
  (out "  page  dpi   width x height      pixels       ms  probes (top-left points)~%")
  (dolist (row *expected-sizes*)
    (destructuring-bind (page dpi width height) row
      (multiple-value-bind (ms image) (timed (pdf:render-page *pdf* page :dpi dpi))
        (let ((w (img:image-width image)) (h (img:image-height image)))
          (check (list w h) (list width height) (format nil "page ~d at ~d dpi" page dpi))
          (check (img:image-format image) :rgba "the default format")
          (check (count-rgba image (lambda (r g b a) (declare (ignore r g b)) (/= a 255))) 0
                 (format nil "page ~d at ~d dpi: every pixel opaque" page dpi))
          (out "  ~4d ~4d  ~6d x ~6d ~11:d ~8,1f  ~{~a~^, ~}~%"
               page dpi w h (* w h) ms (probe-page page image dpi))
          (case page
            (0 (check (plusp (count-rgba image (lambda (r g b a) (declare (ignore a))
                                                  (and (< r 100) (< g 100) (< b 100)))))
                      t (format nil "page 0 at ~d dpi: Helvetica drawn" dpi)))
            (2 (check (count-rgba image (lambda (r g b a) (declare (ignore a))
                                          (and (> b 200) (< r 60) (< g 60))))
                      0 (format nil "page 2 at ~d dpi: no blue" dpi))))))))
  (out "~%Every render is opaque (alpha 255 after the white fill). Page 0 has dark text~%~
        pixels: pdfium found a font for the non-embedded Helvetica. Page 2 has no blue~%~
        pixel: its blue strip lies outside the CropBox, and the red left half of the box~%~
        is the top half on screen after /Rotate 90. The first render also pays pdfium's~%~
        one-time start-up.~%")
  (let ((from-octets (pdf:render-page *pdf* 0))
        (from-path (pdf:render-page *pdf-path* 0)))
    (out "~%The source can be the octets (FPDF_LoadMemDocument) or the pathname~%~
          (FPDF_LoadDocument): page 0 at 72 dpi, same pixels: ~s~%"
         (equalp (img:image-pixels from-octets) (img:image-pixels from-path)))
    (check (equalp (img:image-pixels from-octets) (img:image-pixels from-path)) t
           "pathname and octets give the same pixels")))

;;; 3. Pixel formats --------------------------------------------------------------

(defparameter *format-probes*
  '((60 50 "red") (150 50 "green") (240 50 "blue") (150 170 "gray bar") (5 5 "white"))
  "Points of page 0 sampled in every format.")

(defun demo-formats ()
  (let* ((dpi 150)
         (images (loop for format in '(:rgba :rgb :gray :gray-alpha)
                       collect (cons format (pdf:render-page *pdf* 0 :dpi dpi :format format))))
         (rgba (cdr (assoc :rgba images)))
         (gray (cdr (assoc :gray images))))
    (out ":format picks the pixel layout. pdfium renders BGRA; render-page copies it to~%~
          R G B A and converts with image-protocol's CONVERT-IMAGE. Page 0 at ~d dpi:~%~%" dpi)
    (out "  format       bytes/px      bytes  ~{~16a~}~a~%"
         (butlast (mapcar #'third *format-probes*)) (third (car (last *format-probes*))))
    (dolist (entry images)
      (destructuring-bind (format . image) entry
        (let ((n (img:channels format)))
          (let ((cells (loop for (x y) in *format-probes*
                             collect (format nil "~{~d~^ ~}" (at image x y dpi)))))
            (out "  ~12s ~8d ~10:d  ~{~16a~}~a~%" format n (length (img:image-pixels image))
                 (butlast cells) (car (last cells))))
          (check (length (img:image-pixels image)) (* 625 417 n) (format nil "~s byte count" format))
          (check (list (img:image-depth image) (img:image-color-space image)) '(8 :srgb)
                 (format nil "~s depth and color space" format))
          (check (equalp (img:image-pixels image)
                         (img:image-pixels (img:convert-image rgba format)))
                 t (format nil "~s = (convert-image rgba ~s)" format format)))))
    (loop for (x y label) in *format-probes*
          for (r g b) = (at rgba x y dpi)
          do (check (at gray x y dpi) (list (luma r g b)) (format nil ":gray ~a is BT.601 luma" label)))
    (destructuring-bind (r g b a) (at rgba 150 170 dpi)
      (declare (ignore a))
      (check (and (= r g b) (<= 127 r 128)) t "the 0.5 g bar is mid gray"))
    (out "~%:gray is BT.601 luma of the :rgba pixel, 0.299 R + 0.587 G + 0.114 B rounded half~%~
          up: red 76, green 150, blue 29, checked at every probe. Each format equals~%~
          (convert-image (render-page ... :format :rgba) format) byte for byte. The PDF's~%~
          0.5 g bar is ~d in every channel.~%"
         (first (at rgba 150 170 dpi)))))

;;; 4. Limits ---------------------------------------------------------------------

(defun render-fitting (index dpi max-pixels)
  "RENDER-PAGE of page INDEX under MAX-PIXELS inside a RENDER-SMALLER restart, which
scales the dpi by a factor and tries again. Returns the image and its dpi."
  (loop
    (restart-case
        (return (values (pdf:render-page *pdf* index :dpi dpi :max-pixels max-pixels) dpi))
      (render-smaller (factor)
        :report "Render the page again at a lower dpi"
        (setf dpi (floor (* dpi factor)))
        (out "  RENDER-SMALLER: again at ~d dpi~%" dpi)))))

(defun show-limit (c label)
  (out "  → ~s: ~a~%    limit ~s, allowed ~:d, actual ~:d, format ~s~%"
       (type-of c) c (img:image-limit-exceeded-limit c) (img:image-limit-exceeded-allowed c)
       (img:image-limit-exceeded-actual c) (img:image-error-format c))
  (check (type-of c) 'img:image-limit-exceeded label))

(defun demo-limits ()
  (out "Page 1 (A4) at 300 dpi is 2480 x 3508 = 8,699,840 pixels. Both limits are checked~%~
        after the page size is known and before pdfium allocates a bitmap.~%~%")
  (let (c ms)
    (setf ms (timed (setf c (signalled (lambda ()
                                         (pdf:render-page *pdf* 1 :dpi 300 :max-pixels 4000000))))))
    (out "(render-page pdf 1 :dpi 300 :max-pixels 4000000), refused after ~,1f ms~%" ms)
    (show-limit c ":max-pixels")
    (check (list (img:image-limit-exceeded-limit c) (img:image-limit-exceeded-allowed c)
                 (img:image-limit-exceeded-actual c) (img:image-error-format c))
           '(:pixels 4000000 8699840 :pdf) ":max-pixels slots"))
  (out "~%RENDER-PAGE offers no restart for a refused size (its file header says so). A~%~
        caller can set one up. RENDER-SMALLER, around the call, scales the dpi by~%~
        sqrt(allowed / actual), rounded down; the handler reads both from the condition:~%")
  (let ((outer (compute-restarts)) (seen '()))
    (multiple-value-bind (image dpi)
        (handler-bind ((img:image-limit-exceeded
                         (lambda (c)
                           (let ((names (mapcar #'restart-name (new-restarts c outer))))
                             (push names seen)
                             (out "  handler: ~a~%  restarts set up between the handler and the signal: ~s~%"
                                  c names))
                           (invoke-restart 'render-smaller
                                           (sqrt (/ (img:image-limit-exceeded-allowed c)
                                                    (img:image-limit-exceeded-actual c)))))))
          (render-fitting 1 300 4000000))
      (let ((pixels (* (img:image-width image) (img:image-height image))))
        (out "  rendered at ~d dpi: ~d x ~d = ~:d pixels (allowed 4,000,000)~%"
             dpi (img:image-width image) (img:image-height image) pixels)
        (check (list dpi (img:image-width image) (img:image-height image)) '(203 1678 2374)
               "the retry at 203 dpi")
        (check (<= pixels 4000000) t "the retry fits")
        (check seen '((render-smaller)) "one refusal, only the caller's restart"))))
  (out "~%:limits takes an image-protocol IMAGE-LIMITS (default *IMAGE-LIMITS*, which allows~%~
        16384 a side and 2^28 pixels). It counts the BGRA bitmap pdfium renders into, 4 bytes~%~
        a pixel, whatever :format is asked for:~%")
  (let ((c (signalled (lambda ()
                        (pdf:render-page *pdf* 1 :dpi 300
                                                 :limits (img:make-image-limits :max-width 2000))))))
    (out "(render-page pdf 1 :dpi 300 :limits (make-image-limits :max-width 2000))~%")
    (show-limit c ":limits :max-width")
    (check (list (img:image-limit-exceeded-limit c) (img:image-limit-exceeded-actual c))
           '(:width 2480) ":max-width slots"))
  (let ((c (signalled (lambda ()
                        (pdf:render-page *pdf* 1 :dpi 300 :format :gray
                                                 :limits (img:make-image-limits
                                                          :max-bytes (* 16 1024 1024)))))))
    (out "(render-page pdf 1 :dpi 300 :format :gray :limits (make-image-limits :max-bytes 16 MiB))~%")
    (show-limit c ":limits :max-bytes")
    (check (list (img:image-limit-exceeded-limit c) (img:image-limit-exceeded-actual c))
           (list :bytes (* 2480 3508 4)) ":max-bytes counts 4 bytes a pixel")
    (out "    The :gray result would be 8,699,840 bytes; the bitmap behind it is 34,799,360.~%")))

;;; 5. Errors and restarts --------------------------------------------------------

(defun demo-page-not-found ()
  (let ((c (signalled (lambda () (pdf:render-page *pdf* 3)))))
    (out "(render-page pdf 3) on a 3-page PDF (indexes count from 0)~%  → ~s: ~a~%"
         (type-of c) c)
    (out "    index ~d, page-count ~d, format ~s, a dx:doc-extract-error: ~s~%"
         (pdf:pdf-page-not-found-index c) (pdf:pdf-page-not-found-page-count c)
         (dx:doc-extract-error-format c) (typep c 'dx:doc-extract-error))
    (check (list (type-of c) (pdf:pdf-page-not-found-index c)
                 (pdf:pdf-page-not-found-page-count c) (dx:doc-extract-error-format c)
                 (typep c 'dx:doc-extract-error))
           '(pdf:pdf-page-not-found 3 3 :pdf t) "pdf-page-not-found")))

(defun demo-argument-check ()
  (let* ((outer (compute-restarts)) (names nil) (report nil)
         (image (handler-bind ((type-error (lambda (c)
                                             (setf names (mapcar #'restart-name (new-restarts c outer))
                                                   report (princ-to-string c))
                                             (store-value 0 c))))
                  (pdf:render-page *pdf* -1))))
    (out "~%(render-page pdf -1): the arguments are checked first, with CHECK-TYPE~%  → TYPE-ERROR: ~a~%    restarts: ~s~%    STORE-VALUE 0 from the handler → page 0, ~d x ~d~%"
         (substitute #\Space #\Newline report) names (img:image-width image) (img:image-height image))
    (check (first names) 'store-value "check-type offers STORE-VALUE")
    (check (list (img:image-width image) (img:image-height image)) '(300 200) "store-value 0 renders page 0")))

(defun demo-pdfium-not-loaded ()
  (out "~%This machine has libpdfium: the overlay loaded with the system. To show a machine~%~
        without it, the demo binds the backend's internal *PDFIUM-LOADED* and~%~
        *PDFIUM-SEARCH-ENABLED* to NIL, as the library's own missing-library tests do.~%")
  (let ((reference (pdf:render-page *pdf* 0))
        (pdf::*pdfium-loaded* nil)
        (pdf::*pdfium-search-enabled* nil))
    (out "  pdfium-available-p → ~s~%" (pdf:pdfium-available-p))
    (check (pdf:pdfium-available-p) nil "pdfium hidden")
    ;; The condition and the restarts RENDER-PAGE offers with it.
    (let* ((outer (compute-restarts)) (names nil)
           (c (block probe
                (handler-bind ((pdf:pdfium-not-loaded
                                 (lambda (c)
                                   (setf names (mapcar #'restart-name (new-restarts c outer)))
                                   (return-from probe c))))
                  (pdf:render-page *pdf* 0)))))
      (out "(render-page pdf 0)~%  → ~s: ~a~%    (the report repeats its message: doc-extract-backend-pdf#16)~%    ~
            a dx:doc-extract-unsupported: ~s~%    restarts: ~s~%"
           (type-of c) c (typep c 'dx:doc-extract-unsupported) names)
      (check (typep c 'dx:doc-extract-unsupported) t "pdfium-not-loaded is doc-extract-unsupported")
      (check (mapcar #'symbol-name names) '("USE-VALUE" "RETRY") "use-value and retry offered")
      (out "    RETRY is ~(~a~) in ~a, so a caller finds it by name~%    ~
            (doc-extract-protocol#9: (invoke-restart 'retry) is a CONTROL-ERROR)~%"
           (nth-value 1 (find-symbol "RETRY" (symbol-package (second names))))
           (package-name (symbol-package (second names)))))
    ;; USE-VALUE, from a handler on the protocol's parent condition.
    (let* ((blank (img:make-image :width 300 :height 200 :format :rgba
                                  :pixels (make-array (* 4 300 200) :element-type '(unsigned-byte 8)
                                                                    :initial-element 255)))
           (got (handler-bind ((dx:doc-extract-unsupported (lambda (c) (use-value blank c))))
                  (pdf:render-page *pdf* 0))))
      (out "~%A handler on dx:doc-extract-unsupported calls (use-value blank c), BLANK a white~%~
            300 x 200 image made by the caller → render-page returns BLANK itself: ~s~%"
           (eq got blank))
      (check (eq got blank) t "use-value returns the supplied image"))
    ;; RETRY, after the handler loads libpdfium.
    (let* ((attempts 0)
           (got (handler-bind ((pdf:pdfium-not-loaded
                                 (lambda (c)
                                   (incf attempts)
                                   (let ((pdf::*pdfium-search-enabled* t))
                                     (out "~%A handler calls (load-pdfium) → ~s, then invokes RETRY~%"
                                          (pdf:load-pdfium)))
                                   (invoke-restart (find-restart-named "RETRY" c)))))
                  (pdf:render-page *pdf* 0))))
      (out "  → ~d x ~d, the same pixels as the normal render: ~s (handler ran ~d time~:p)~%"
           (img:image-width got) (img:image-height got)
           (equalp (img:image-pixels got) (img:image-pixels reference)) attempts)
      (check (equalp (img:image-pixels got) (img:image-pixels reference)) t "retry renders the page")
      (check attempts 1 "one pdfium-not-loaded before the retry"))))

;;; 6. PNG ------------------------------------------------------------------------

(defun demo-png (dir)
  (let* ((dpi 150)
         (image (pdf:render-page *pdf* 0 :dpi dpi :format :rgb))
         (path (merge-pathnames "page-1-150dpi.png" dir)))
    (multiple-value-bind (ms png) (timed (img:encode-image image :png))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence png o))
      (out "Page 0 at ~d dpi as :rgb (~d x ~d), (encode-image image :png) on ~(~a~): ~,1f ms~%"
           dpi (img:image-width image) (img:image-height image)
           (type-of (first (img:image-backends))) ms)
      (out "wrote output/~a: ~:d bytes, sha256 ~a~%" (file-namestring path) (length png) (sha256-hex png))
      (check (coerce (subseq png 0 8) 'list) '(137 80 78 71 13 10 26 10) "PNG signature")
      (check (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))
             (length png) "file size")
      (let ((back (img:decode-image path)))
        (out "decode-image of the file → ~d x ~d ~s, the same pixels: ~s~%"
             (img:image-width back) (img:image-height back) (img:image-format back)
             (equalp (img:image-pixels back) (img:image-pixels image)))
        (check (list (img:image-width back) (img:image-height back) (img:image-format back))
               '(625 417 :rgb) "decoded size and format")
        (check (equalp (img:image-pixels back) (img:image-pixels image)) t "lossless round trip")))))

;;; Entry point -------------------------------------------------------------------

(defun run-demos (&key (dir (here "output/")))
  "Print the tour and write DIR/page-1-150dpi.png. Returns T when every check
matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 100)
        (crypto:*crypto-backend* (crypto-backend-ironclad:make-ironclad-crypto-backend))
        (*pdf-path* (here "fixtures/pages.pdf")))
    (ensure-directories-exist dir)
    (let ((*pdf* (read-octets *pdf-path*))
          (library (uiop:symbol-call :asdf-native :library-loaded-path
                                     "doc-extract-backend-pdf" "pdfium")))
      (format t "~&doc-extract-backend-pdf ~a, image-protocol ~a, image-backend-stb ~a~%~
                 (~a ~a, ~a ~a)~%libpdfium loaded: ~s~@[ (~a)~]~%"
              (version-of "doc-extract-backend-pdf") (version-of "image-protocol")
              (version-of "image-backend-stb") (software-type) (machine-type)
              (lisp-implementation-type) (lisp-implementation-version)
              (pdf:pdfium-available-p) (and library (tidy (namestring library))))
      (section "1. The input: three pages written for this demo" (demo-input))
      (section "2. RENDER-PAGE at 72, 150 and 300 dpi" (demo-sizes))
      (section "3. Pixel formats: :rgba, :rgb, :gray, :gray-alpha" (demo-formats))
      (section "4. :max-pixels and :limits refuse before any bitmap exists" (demo-limits))
      (section "5. PDF-PAGE-NOT-FOUND, argument checks, PDFIUM-NOT-LOADED"
        (demo-page-not-found)
        (demo-argument-check)
        (demo-pdfium-not-loaded))
      (section "6. One page as PNG through image-backend-stb" (demo-png dir))
      (banner "Summary")
      (format t "~d results checked against their expected values: ~d mismatched.~%~
                 ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
              *checks* *mismatches* (reverse *failed-sections*))
      (finish-output)
      (and (zerop *mismatches*) (null *failed-sections*)))))
