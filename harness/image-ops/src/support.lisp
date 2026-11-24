(in-package #:harness-demos-image-ops)

;;;; Output, checks, the generated test card, sample differences (S8), timing,
;;;; and the Pillow cross-read through uv.

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")
(defvar *skipped* '() "Cross-reads skipped because uv is not on PATH.")

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun out (control &rest args)
  "FORMAT to *STANDARD-OUTPUT* with the padding at the end of each line removed."
  (let ((text (apply #'format nil control args)))
    (with-input-from-string (in text)
      (loop for (line missing-newline-p) = (multiple-value-list (read-line in nil))
            while line
            do (write-string (string-right-trim " " line))
               (unless missing-newline-p (terpri))))))

(defun para (&rest lines)
  "Print LINES of prose, then a blank line."
  (out "~&~{~a~%~}~%" lines))

(defun check (actual expected label &key (test #'equalp))
  "Count a check of ACTUAL against EXPECTED; print a line only when they differ."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (out "~&  !! ~a: got ~s, expected ~s~%" label actual expected))
  actual)

(defmacro section (title &body body)
  "Print a banner, run BODY, and record the section as failed if it signals."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (e)
         (push ,title *failed-sections*)
         (out "~&!! unexpected ~s, the rest of this section is skipped:~%   ~a~%"
              (type-of e) e)))))

(defun code-text (object)
  "OBJECT printed readably in lower case, on one line."
  (let ((*package* (find-package '#:harness-demos-image-ops))
        (*print-case* :downcase)
        (*print-pretty* nil))
    (prin1-to-string object)))

(defun one-line (string)
  "STRING with each run of whitespace made one space."
  (format nil "~{~a~^ ~}"
          (remove "" (uiop:split-string string :separator '(#\Space #\Tab #\Newline))
                  :test #'string=)))

(defun condition-of (thunk)
  "The condition THUNK signals as an error, or NIL when it returns."
  (handler-case (progn (funcall thunk) nil)
    (error (c) c)))

;;; Images --------------------------------------------------------------------

(defun gray-of (r g b)
  "The test card's gray value: ITU-R 601 weights, truncated."
  (floor (+ (* r 299) (* g 587) (* b 114)) 1000))

(defun test-card (format width height)
  "A test card made in code, so no outside asset is involved: red grows to the
right, green downwards, blue along the diagonal; the right half carries a
black-and-white checkerboard of squares 1/16 of the width; in an alpha
format the top half's alpha grows from 0 at the left edge to 255 at the
right, and the bottom half is opaque."
  (let* ((c (img:channels format))
         (side (max 2 (floor width 16)))
         (pixels (make-array (* width height c) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (dotimes (x width)
        (multiple-value-bind (r g b)
            (if (and (>= x (floor width 2)) (evenp (+ (floor x side) (floor y side))))
                (values 255 255 255)
                (if (>= x (floor width 2))
                    (values 0 0 0)
                    (values (floor (* 255 x) (max 1 (1- width)))
                            (floor (* 255 y) (max 1 (1- height)))
                            (floor (* 255 (+ x y)) (max 1 (+ width height -2))))))
          (let ((a (if (< y (floor height 2)) (floor (* 255 x) (max 1 (1- width))) 255))
                (i (* c (+ x (* y width)))))
            (flet ((put (&rest samples)
                     (loop for s in samples for k from i do (setf (aref pixels k) s))))
              (ecase format
                (:gray (put (gray-of r g b)))
                (:gray-alpha (put (gray-of r g b) a))
                (:rgb (put r g b))
                (:rgba (put r g b a))))))))
    (img:make-image :width width :height height :format format :pixels pixels)))

(defun flat-image (format width height sample)
  "WIDTH × HEIGHT in FORMAT, every sample SAMPLE."
  (img:make-image :width width :height height :format format
                  :pixels (make-array (* width height (img:channels format))
                                      :element-type '(unsigned-byte 8)
                                      :initial-element sample)))

(defun size-of (image)
  (list (img:image-width image) (img:image-height image)))

(defun size-text (image)
  (format nil "~d × ~d" (img:image-width image) (img:image-height image)))

(defun pixel (image x y)
  "IMAGE's samples at X, Y, as a list."
  (let* ((n (img:channels (img:image-format image)))
         (i (* n (+ x (* y (img:image-width image))))))
    (coerce (subseq (img:image-pixels image) i (+ i n)) 'list)))

(defun same-pixels-p (a b)
  "True when the images A and B have the same size, format and samples."
  (and (equal (size-of a) (size-of b))
       (eq (img:image-format a) (img:image-format b))
       (equalp (img:image-pixels a) (img:image-pixels b))))

;;; Sample differences, as image-backend-stb's S8 measures them -----------------

(defparameter +s8-max+ 43
  "image-backend-stb S8: the largest sample difference allowed against the
protocol's own resampler (the worst case measured against Pillow 11.3, plus a
quarter).")

(defparameter +s8-mean+ 16/5
  "image-backend-stb S8: the mean sample difference allowed (3.2).")

(defun difference (a b format)
  "(values max mean) of the sample differences of the octet vectors A and B in
FORMAT, exactly (rationals). As S8 does it, the colour samples of an alpha
format are compared premultiplied (c · a / 255) and alpha and opaque samples
as they are."
  (let ((c (img:channels format))
        (alpha (member format '(:gray-alpha :rgba)))
        (worst 0) (total 0) (n 0))
    (flet ((add (d) (setf worst (max worst d)) (incf total d) (incf n)))
      (loop for p from 0 below (length a) by c
            do (if alpha
                   (let ((aa (aref a (+ p c -1))) (ab (aref b (+ p c -1))))
                     (dotimes (i (1- c))
                       (add (/ (abs (- (* (aref a (+ p i)) aa) (* (aref b (+ p i)) ab))) 255)))
                     (add (abs (- aa ab))))
                   (dotimes (i c)
                     (add (abs (- (aref a (+ p i)) (aref b (+ p i)))))))))
    (values worst (/ total (max n 1)))))

(defun diff-text (max mean)
  "MAX and MEAN as a short table cell."
  (format nil "~6,2f ~5,3f" max mean))

;;; Timing --------------------------------------------------------------------

(defun seconds-of (thunk)
  "Wall-clock seconds THUNK takes, from GET-INTERNAL-REAL-TIME."
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (/ (- (get-internal-real-time) start) internal-time-units-per-second)))

(defun median-seconds (thunk runs)
  "The median of RUNS timed calls of THUNK, after one untimed warm-up call."
  (funcall thunk)
  (let ((times (sort (loop repeat runs collect (seconds-of thunk)) #'<)))
    (if (oddp runs)
        (nth (floor runs 2) times)
        (/ (+ (nth (1- (floor runs 2)) times) (nth (floor runs 2) times)) 2))))

;;; Files and the Pillow cross-read -------------------------------------------------

(defun here (relative)
  (asdf:system-relative-pathname "harness-demos-image-ops" relative))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun write-octets (octets path)
  (with-open-file (o path :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence octets o))
  path)

(defvar *uv* :unknown)

(defun uv-available-p ()
  (when (eq *uv* :unknown)
    (setf *uv* (handler-case (zerop (proc:run '("uv" "--version") :output nil
                                                                   :error-output nil :timeout 60))
                 (error () nil))))
  *uv*)

(defun pillow-mode (format)
  (ecase format (:gray "L") (:gray-alpha "LA") (:rgb "RGB") (:rgba "RGBA")))

(defun pillow-resize (cases)
  "Pillow 11.3.0's resize of each case of CASES, a list of
(image width height filter gap), through crosscheck/pillow_resize.py under
`uv run --with pillow==11.3.0 python -I`. → (values pixel-vectors version),
the vectors in CASES' order; NIL, with a note, when uv is not on PATH."
  (unless (uv-available-p)
    (push "pillow_resize.py" *skipped*)
    (out "  (uv is not on PATH: the Pillow cross-read is skipped)~%")
    (return-from pillow-resize nil))
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames "harness-demos-image-ops/" (uiop:temporary-directory))))
         (manifest (merge-pathnames "manifest.tsv" dir))
         (outputs '()))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (m manifest :direction :output :if-exists :supersede
                                       :external-format :utf-8)
             (loop for (image width height filter gap) in cases
                   for k from 0
                   for in = (merge-pathnames (format nil "in-~d.raw" k) dir)
                   for to = (merge-pathnames (format nil "out-~d.raw" k) dir)
                   do (write-octets (img:image-pixels image) in)
                      (push to outputs)
                      (format m "~a~c~a~c~d~c~d~c~d~c~d~c~(~a~)~c~a~c~a~%"
                              (namestring in) #\Tab (pillow-mode (img:image-format image)) #\Tab
                              (img:image-width image) #\Tab (img:image-height image) #\Tab
                              width #\Tab height #\Tab filter #\Tab (if gap gap "-") #\Tab
                              (namestring to))))
           (multiple-value-bind (code stdout stderr)
               (proc:run (list "uv" "run" "--no-project" "--with" "pillow==11.3.0"
                               "python" "-I" (namestring (here "crosscheck/pillow_resize.py"))
                               (namestring manifest))
                         :output :string :error-output :string :timeout 900)
             (check code 0 "uv run pillow_resize.py exit code")
             (cond ((zerop code)
                    (values (mapcar #'read-octets (reverse outputs))
                            (string-trim '(#\Space #\Newline #\Return) stdout)))
                   (t (out "~a~%" stderr) nil))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))
