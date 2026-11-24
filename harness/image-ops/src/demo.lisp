(in-package #:harness-demos-image-ops)

;;;; image-protocol 0.3.0's BACKEND-RESIZE hook and image-backend-stb 0.3.0's
;;;; native resize on stb_image_resize2. Section 1 points at the 0.2.0
;;;; operations, which harness/documents tours against Pillow. Sections 2-5 show
;;;; the hook (rules R1-R8, image-protocol src/ops.lisp, "Backend fast path"),
;;;; and sections 6-8 the stb fast path (rules S1-S8, image-backend-stb
;;;; src/resize.lisp). Every result with a known answer is checked, and
;;;; RUN-DEMOS returns NIL when one differs or a section stops on an error.
;;;; Timings are printed and not checked.

;;; Demo backends -------------------------------------------------------------

(defclass demo-backend (img:image-backend)
  ((name :initarg :name :initform "backend" :reader backend-name))
  (:documentation "A resize-only backend for the demo, printed by its NAME so the
output has no addresses. It decodes nothing, and the demo never registers it."))

(defmethod img:backend-formats ((backend demo-backend)) '())

(defmethod print-object ((backend demo-backend) stream)
  (print-unreadable-object (backend stream)
    (format stream "~(~a~) ~a" (class-name (class-of backend)) (backend-name backend))))

(defclass recording-backend (demo-backend)
  ((calls :initform '() :accessor calls)
   (answer :initarg :answer :initform nil :reader answer))
  (:documentation "Records each BACKEND-RESIZE call as (image width height . keys),
newest first. With ANSWER NIL it declines; otherwise ANSWER is called with
(image width height) and its value returned."))

(defmethod img:backend-resize ((backend recording-backend) image width height
                               &rest keys &key filter reducing-gap linear)
  (declare (ignore filter reducing-gap linear))
  (push (list* image width height keys) (calls backend))
  (let ((answer (answer backend)))
    (and answer (funcall answer image width height))))

(defclass plain-backend (demo-backend) ()
  (:documentation "No BACKEND-RESIZE method of its own: the method on IMAGE-BACKEND
declines."))

(defclass failing-backend (demo-backend) ()
  (:documentation "Signals IMAGE-NATIVE-UNAVAILABLE from BACKEND-RESIZE, as a
native backend does when its library call fails (stb's S7)."))

(defmethod img:backend-resize ((backend failing-backend) image width height
                               &key &allow-other-keys)
  (declare (ignore image width height))
  (error 'img:image-native-unavailable :backend backend :cause "the native call failed"))

(defclass counting-stb (stb:stb-backend)
  ((calls :initform 0 :accessor stb-calls)
   (answers :initform 0 :accessor stb-answers))
  (:documentation "The stb backend, counting its BACKEND-RESIZE calls and the ones
it answered. Not registered."))

(defmethod img:backend-resize :around ((backend counting-stb) image width height
                                       &key &allow-other-keys)
  (declare (ignore image width height))
  (incf (stb-calls backend))
  (let ((answer (call-next-method)))
    (when answer (incf (stb-answers backend)))
    answer))

(defun recorder (name &optional answer)
  (make-instance 'recording-backend :name name :answer answer))

;;; Printing forms ------------------------------------------------------------

(defun form-text (form)
  "FORM as it reads in the output: symbols without their package, in lower case."
  (typecase form
    (null "nil")
    (keyword (format nil ":~(~a~)" (symbol-name form)))
    (symbol (string-downcase (symbol-name form)))
    (cons (cond ((and (eq (first form) 'quote) (consp (rest form)))
                 (format nil "'~a" (form-text (second form))))
                ((and (eq (first form) 'function) (consp (rest form)))
                 (format nil "#'~a" (form-text (second form))))
                (t (format nil "(~{~a~^ ~})" (mapcar #'form-text form)))))
    (t (prin1-to-string form))))

(defun value-text (value)
  (typecase value
    (img:image (format nil "an image, ~a ~(~s~)" (size-text value) (img:image-format value)))
    (t (code-text value))))

(defmacro show (form &optional (expected nil expected-p) (test '#'equalp))
  "Print FORM => its value; check the value against EXPECTED with TEST when one
is given."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (out "  ~a => ~a~%" (form-text ',form) (value-text ,value))
       ,@(when expected-p `((check ,value ,expected (form-text ',form) :test ,test)))
       ,value)))

;;; 1. The 0.2.0 operations ----------------------------------------------------

(defun demo-operations ()
  (para "image-protocol 0.2.0's editing operations (crop-image, rotate-image, flip-image,"
        "resize-image with five filters, thumbnail, fit-image, composite-image) are toured in"
        "harness/documents, section 5, where all 16 results have the same pixels as Pillow"
        "11.3.0. This demo starts where 0.3.0 does. Its input is a test card made in code"
        "(no outside asset): a colour gradient on the left half and a black-and-white"
        "checkerboard of 64-pixel squares on the right half.")
  (let ((card (test-card :rgb 1024 768)))
    (show (size-of card) '(1024 768))
    (show (pixel card 0 0) '(0 0 0))
    (show (pixel card 511 0) '(127 0 72))
    (show (pixel card 0 767) '(0 255 109))
    (show (pixel card 511 767) '(127 255 182))
    (show (pixel card 512 0) '(255 255 255))
    (show (pixel card 576 0) '(0 0 0))
    (para ""
          "One 0.2.0 operation, the one the timing table in section 8 runs: thumbnail fits the"
          "card into 256 × 256, keeping 4:3, with :lanczos3 and Pillow's reducing gap 2. No"
          "backend is named, so this is the pure-Lisp resampler, as in 0.2.0.")
    (let ((thumb (img:thumbnail card 256 256)))
      (show (size-of thumb) '(256 192))
      (show (pixel thumb 0 0))
      (show (pixel thumb 127 191))
      (show (pixel thumb 200 100)))))

;;; 2. What reaches BACKEND-RESIZE (R3, R4) ---------------------------------------

(defun call-text (call image name)
  "One recorded BACKEND-RESIZE call as text; NAME stands for IMAGE when the call
got IMAGE itself."
  (destructuring-bind (im width height &rest keys) call
    (format nil "(backend-resize rec ~a ~d ~d~{ ~a~})"
            (if (eq im image) name "<another image>") width height (mapcar #'form-text keys))))

(defun report-calls (form value rec image name expected)
  "Print FORM, VALUE and each call REC recorded, oldest first; check the calls
against EXPECTED, a list of (width height filter gap linear), with EQUAL, so
2 and 2.0 differ."
  (let ((calls (reverse (calls rec))))
    (out "  ~a~%    => ~a~%" (form-text form) (value-text value))
    (if calls
        (dolist (call calls) (out "    called ~a~%" (call-text call image name)))
        (out "    no BACKEND-RESIZE call~%"))
    (check (mapcar (lambda (call)
                     (destructuring-bind (im width height &key filter reducing-gap linear) call
                       (list (eq im image) width height filter reducing-gap linear)))
                   calls)
           (mapcar (lambda (e) (cons t e)) expected)
           (format nil "calls of ~a" (form-text form))
           :test #'equal)))

(defmacro show-calls ((rec image name) form &rest expected)
  "Run FORM with REC's record cleared, then print the calls it made (REPORT-CALLS).
→ FORM's value."
  (let ((value (gensym "VALUE")))
    `(progn
       (setf (calls ,rec) '())
       (let ((,value ,form))
         (report-calls ',form ,value ,rec ,image ,name ',expected)
         ,value))))

(defun demo-arguments ()
  (para "A backend with a native resampler specialises BACKEND-RESIZE. REC below records each"
        "call and declines (answers NIL), so the pure-Lisp resampler still does the work. The"
        "caller's image itself arrives (EQ, not a copy), and the three keys are always passed"
        "(R4): the filter (:lanczos3 when none is given), the reducing gap the pure-Lisp"
        "resampler would apply, and LINEAR as T or NIL. card is a 96 × 72 :rgb test card,"
        "card-rgba the same in :rgba.")
  (let ((card (test-card :rgb 96 72))
        (card-rgba (test-card :rgba 96 72))
        (rec (recorder "rec")))
    (show-calls (rec card "card") (img:resize-image card 24 18 :backend rec)
                (24 18 :lanczos3 nil nil))
    (show-calls (rec card "card")
                (img:resize-image card 24 18 :filter :bicubic :reducing-gap 2 :linear :yes :backend rec)
                (24 18 :bicubic 2 t))
    (show-calls (rec card "card") (img:resize-image card 24 18 :reducing-gap 1.7 :backend rec)
                (24 18 :lanczos3 1.7 nil))
    (out "  The gap goes to the backend as the caller gave it (EQL: 2 stays 2, 1.7 stays a~%  ~
          single float). An image with alpha gets no gap, as in Pillow, and neither does~%  ~
          :nearest:~%")
    (show-calls (rec card-rgba "card-rgba")
                (img:resize-image card-rgba 24 18 :filter :bicubic :reducing-gap 2 :backend rec)
                (24 18 :bicubic nil nil))
    (show-calls (rec card "card") (img:resize-image card 24 18 :filter :nearest :reducing-gap 3 :backend rec)
                (24 18 :nearest nil nil))
    (out "  thumbnail and fit-image pass :backend on to resize-image, with their own default~%  ~
          gap of 2. A backend listed twice is consulted twice:~%")
    (show-calls (rec card "card") (img:thumbnail card 32 32 :backend rec)
                (32 24 :lanczos3 2 nil))
    (show-calls (rec card "card") (img:fit-image card :max-width 192 :enlarge t :backend rec)
                (192 144 :lanczos3 2 nil))
    (show-calls (rec card "card") (img:resize-image card 24 18 :filter :box :backend (list rec rec))
                (24 18 :box nil nil) (24 18 :box nil nil))
    (out "~%  Nothing to do, no call (R3): the input itself comes back.~%")
    (check (show-calls (rec card "card") (img:resize-image card 96 72 :backend rec)) card
           "resize to the same size returns the input" :test #'eq)
    (check (show-calls (rec card "card") (img:thumbnail card 200 200 :backend rec)) card
           "thumbnail of an image that fits returns the input" :test #'eq)
    (check (show-calls (rec card "card") (img:fit-image card :max-width 96 :backend rec)) card
           "fit-image at scale 1 returns the input" :test #'eq)
    (para ""
          "When a backend answers, its pixels are the result (R5, R6). The result is a fresh"
          "COPY-IMAGE of the caller's image with the answer's pixel vector, so the caller's"
          "metadata stays and the answer object itself is not returned. marker answers with"
          "every sample 128; photo is card with orientation 6 and gamma 0.45455, as a decoder"
          "would return it.")
    (let* ((answers '())
           (marker (recorder "marker" (lambda (image width height)
                                        (let ((a (flat-image (img:image-format image) width height 128)))
                                          (push a answers)
                                          a))))
           (photo (let ((p (img:copy-image card)))
                    (setf (img:image-orientation p) 6 (img:image-gamma p) 0.45455)
                    p))
           (before (copy-seq (img:image-pixels photo)))
           (result (show (img:resize-image photo 24 18 :backend marker)))
           (answer (first answers)))
      (show (every (lambda (v) (= v 128)) (img:image-pixels result)) t)
      (show (list (img:image-orientation result) (img:image-gamma result)) '(6 0.45455))
      (show (list (img:image-orientation answer) (img:image-gamma answer)) '(nil nil))
      (show (eq result answer) nil)
      (show (eq (img:image-pixels result) (img:image-pixels answer)) t)
      (show (equalp (img:image-pixels photo) before) t))))

;;; 3. Declining backends (R5) ------------------------------------------------------

(defparameter +formats+ '(:gray :gray-alpha :rgb :rgba))

(defun demo-declining ()
  (para "A backend declines by answering NIL, and the method on IMAGE-BACKEND always does."
        "plain has no BACKEND-RESIZE method of its own; rec is the recorder of section 2."
        "With :backend (list plain rec) both are consulted, in order, both decline, and the"
        "pure-Lisp resampler runs: the result is 0.2.0's, the same samples as :backend nil."
        "Each cell: a 64 × 48 test card resized to 23 × 17 with :reducing-gap 2.")
  (let ((plain (make-instance 'plain-backend :name "plain"))
        (rec (recorder "rec"))
        (same 0))
    (out "  ~10a~{ ~11a~}~%" "filter" (mapcar (lambda (f) (format nil "~(~s~)" f)) +formats+))
    (dolist (filter img:+resize-filters+)
      (out "  ~10a~{ ~11a~}~%" (format nil "~(~s~)" filter)
           (loop for format in +formats+
                 for card = (test-card format 64 48)
                 for via = (img:resize-image card 23 17 :filter filter :reducing-gap 2
                                                        :backend (list plain rec))
                 for pure = (img:resize-image card 23 17 :filter filter :reducing-gap 2 :backend nil)
                 for ok = (same-pixels-p via pure)
                 do (when ok (incf same))
                 collect (if ok "same" "DIFFERENT"))))
    (out "~%")
    (show same 20)
    (show (length (calls rec)) 20)))

;;; 4. Bad answers and errors (R5, R7) -----------------------------------------------

(defun demo-bad-answers ()
  (para "Any answer other than NIL must be a fresh image of the requested size, in the"
        "input's format, depth 8, with the right number of octets and a pixel vector that is"
        "not the input's own (R5). Otherwise RESIZE-IMAGE signals IMAGE-CORRUPT, with the"
        "backend and a cause naming the broken rule. card is the 96 × 72 :rgb test card.")
  (let ((card (test-card :rgb 96 72)))
    (flet ((try (name answer width height cause)
             (let* ((broken (recorder name answer))
                    (c (condition-of (lambda () (img:resize-image card width height :backend broken)))))
               (out "  ~a, resize to ~d × ~d~%    => ~a~%    cause: ~a~%    message: ~a~%"
                    name width height (type-of c) (and c (img:image-error-cause c))
                    (and c (one-line (princ-to-string c))))
               (check (typep c 'img:image-corrupt) t (format nil "~a: image-corrupt" name))
               (check (and (typep c 'img:image-error) (eq (img:image-error-backend c) broken)) t
                      (format nil "~a: the backend slot" name))
               (check (and (typep c 'img:image-decode-error)
                           (search cause (or (img:image-error-cause c) "")) t)
                      t (format nil "~a: the cause names ~s" name cause)))))
      (try "a vector, not an image"
           (lambda (image width height)
             (make-array (* width height (img:channels (img:image-format image)))
                         :element-type '(unsigned-byte 8)))
           24 18 "not NIL or an IMAGE")
      (try "10 × 10 instead of 24 × 18"
           (lambda (image width height) (declare (ignore width height))
             (flat-image (img:image-format image) 10 10 0))
           24 18 "width 10, expected 24")
      (try ":gray for an :rgb input"
           (lambda (image width height) (declare (ignore image)) (flat-image :gray width height 0))
           24 18 "format :GRAY, expected :RGB")
      (try "depth 16"
           (lambda (image width height)
             (img:make-image :width width :height height :format (img:image-format image) :depth 16
                             :pixels (make-array (* 2 width height 3) :element-type '(unsigned-byte 8))))
           24 18 "depth 16, expected 8")
      (try "the input's own pixels as 72 × 96 (the same length as 96 × 72)"
           (lambda (image width height)
             (img:make-image :width width :height height :format (img:image-format image)
                             :pixels (img:image-pixels image)))
           72 96 "IMAGE's own pixel vector"))
    (para ""
          "The message reads \"Could not decode nil with ...\": IMAGE-CORRUPT keeps the report"
          "of IMAGE-DECODE-ERROR, and the answer check gives no :format. That is open as"
          "image-protocol#8; the condition type, the backend and the cause are right.")
    (para
          "RESIZE-IMAGE establishes the restart TRY-NEXT-BACKEND around each backend's call and"
          "the check of its answer (R7). It treats that backend as having declined, so the next"
          "one runs, or the pure-Lisp resampler after the last. Below, gray answers :gray for"
          ":rgb, and a handler-bind takes the restart:")
    (let* ((gray (recorder "gray" (lambda (image width height) (declare (ignore image))
                                    (flat-image :gray width height 0))))
           (stb (make-instance 'stb:stb-backend))
           (taken '())
           (via-stb (handler-bind ((img:image-corrupt
                                     (lambda (c)
                                       (let ((r (find-restart 'img:try-next-backend c)))
                                         (push (format nil "~a" r) taken)
                                         (invoke-restart r)))))
                      (img:resize-image card 24 18 :filter :bicubic :backend (list gray stb))))
           (via-lisp (handler-bind ((img:image-corrupt
                                      (lambda (c) (declare (ignore c))
                                        (invoke-restart 'img:try-next-backend))))
                       (img:resize-image card 24 18 :filter :bicubic :backend gray))))
      (out "  restart report: ~a~%" (one-line (first taken)))
      (out "  :backend (list gray stb), restart taken => the same pixels as :backend stb: ~a~%"
           (same-pixels-p via-stb (img:resize-image card 24 18 :filter :bicubic :backend stb)))
      (out "  :backend gray, restart taken            => the same pixels as :backend nil: ~a~%"
           (same-pixels-p via-lisp (img:resize-image card 24 18 :filter :bicubic :backend nil)))
      (check (length taken) 1 "the restart was offered once")
      (check (same-pixels-p via-stb (img:resize-image card 24 18 :filter :bicubic :backend stb)) t
             "after try-next-backend, stb answers")
      (check (same-pixels-p via-lisp (img:resize-image card 24 18 :filter :bicubic :backend nil)) t
             "after try-next-backend, the pure-Lisp resampler runs"))
    (para ""
          "A condition the backend signals itself reaches the caller unchanged: the protocol"
          "handles nothing (R7). failing signals IMAGE-NATIVE-UNAVAILABLE, as stb does when"
          "stbir_resize fails (S7); the same restart recovers from it.")
    (let* ((failing (make-instance 'failing-backend :name "failing"))
           (stb (make-instance 'stb:stb-backend))
           (c (condition-of (lambda () (img:resize-image card 24 18 :filter :bicubic
                                                                      :backend (list failing stb)))))
           (recovered (handler-bind ((img:image-native-unavailable
                                       (lambda (c) (declare (ignore c))
                                         (invoke-restart 'img:try-next-backend))))
                        (img:resize-image card 24 18 :filter :bicubic :backend (list failing stb)))))
      (out "  unhandled => ~a: ~a~%" (type-of c) (and c (one-line (princ-to-string c))))
      (out "  with try-next-backend => the same pixels as :backend stb: ~a~%"
           (same-pixels-p recovered (img:resize-image card 24 18 :filter :bicubic :backend stb)))
      (check (type-of c) 'img:image-native-unavailable "failing's own condition, unchanged")
      (check (and c (img:image-error-backend c)) failing "its backend slot" :test #'eq)
      (check (same-pixels-p recovered (img:resize-image card 24 18 :filter :bicubic :backend stb)) t
             "failing skipped, stb answers"))))

;;; 5. *RESIZE-BACKENDS* against :BACKEND (R1, R2, R8) --------------------------------

(defun report-refusal (form c operation argument value)
  (out "  ~a~%    => ~a: ~a~%" (form-text form) (type-of c) (and c (one-line (princ-to-string c))))
  (check (and (typep c 'img:image-invalid-argument)
              (list (img:image-invalid-argument-operation c)
                    (symbol-name (img:image-invalid-argument-argument c))))
         (list operation argument)
         (format nil "~a: operation and argument" (form-text form)))
  (when (typep c 'img:image-invalid-argument)
    (check (img:image-invalid-argument-value c) value
           (format nil "~a: the value is the designator itself" (form-text form)) :test #'eql)))

(defmacro show-refusal (form &key operation (argument "BACKEND") value)
  `(report-refusal ',form (condition-of (lambda () ,form)) ,operation ,argument ,value))

(defun demo-designators ()
  (para "A backend resizes only when the caller names it, through :backend or by binding"
        "*resize-backends*, the default of :backend in resize-image, thumbnail and fit-image."
        "Registering a backend never changes the samples resize-image returns. Loading"
        "image-backend-stb registered an stb backend for decoding and encoding:")
  (let* ((card (test-card :rgb 96 72))
         (stb (make-instance 'stb:stb-backend))
         (rec (recorder "rec"))
         (other (recorder "other")))
    (show img:*resize-backends* nil)
    (show (mapcar #'type-of (img:image-backends)) '(stb:stb-backend) #'equal)
    (let ((default (img:resize-image card 24 18 :filter :bicubic)))
      (show (same-pixels-p default (img:resize-image card 24 18 :filter :bicubic :backend nil)) t)
      (show (same-pixels-p default (img:resize-image card 24 18 :filter :bicubic :backend stb)) nil))
    (para ""
          "Binding *resize-backends* opts in; an explicit :backend wins over the binding, and"
          "nil there means the pure-Lisp resampler (counts are calls to rec and other):")
    (macrolet ((counted (form expected)
                 `(progn
                    (setf (calls rec) '() (calls other) '())
                    ,form
                    (let ((counts (list (length (calls rec)) (length (calls other)))))
                      (out "    ~62a rec ~d, other ~d~%" (form-text ',form)
                           (first counts) (second counts))
                      (check counts ',expected (form-text ',form))))))
      (out "  (let ((*resize-backends* rec)) ...)~%")
      (let ((img:*resize-backends* rec))
        (counted (img:resize-image card 24 18) (1 0))
        (counted (img:resize-image card 24 18 :backend nil) (0 0))
        (counted (img:resize-image card 24 18 :backend other) (0 1))
        (counted (img:fit-image card :max-width 48) (1 0))
        (counted (img:thumbnail card 32 32 :backend (list other rec)) (1 1))
        (show (eq img:*resize-backends* rec) t))
      (out "  (let ((*resize-backends* (list rec other))) ...)~%")
      (let ((img:*resize-backends* (list rec other)))
        (counted (img:thumbnail card 32 32) (1 1))
        (counted (img:thumbnail card 200 200) (0 0))))
    (out "  The operations change no special variable and register nothing (R8):~%")
    (show img:*resize-backends* nil)
    (show (length (img:image-backends)) 1)
    (para ""
          "A designator is NIL, one IMAGE-BACKEND or a proper list of them (R1). Anything else"
          "is IMAGE-INVALID-ARGUMENT with argument BACKEND and the designator itself as the"
          "value. It is checked after the 0.2.0 arguments and before anything else, so it is"
          "reported even when nothing would be resized (R2). listed is (list rec 42), dotted"
          "is (cons rec rec).")
    (let ((listed (list rec 42))
          (dotted (cons rec rec)))
      (show-refusal (img:resize-image card 24 18 :backend :stb) :operation :resize :value :stb)
      (show-refusal (img:resize-image card 24 18 :backend 'stb:stb-backend)
                    :operation :resize :value 'stb:stb-backend)
      (show-refusal (img:resize-image card 24 18 :backend listed) :operation :resize :value listed)
      (show-refusal (img:resize-image card 24 18 :backend dotted) :operation :resize :value dotted)
      (show-refusal (img:resize-image card 96 72 :backend :protocol) :operation :resize :value :protocol)
      (show-refusal (img:thumbnail card 200 200 :backend 7) :operation :thumbnail :value 7)
      (show-refusal (let ((img:*resize-backends* :fast)) (img:fit-image card :max-width 48))
                    :operation :fit :value :fast)
      (out "  The 0.2.0 checks come first: a bad width is reported before a bad designator.~%")
      (show-refusal (img:resize-image card 0 18 :backend :stb)
                    :operation :resize :argument "WIDTH" :value 0))))

;;; 6. stb: which filters it serves (S1, S2) ----------------------------------------------

(defun demo-stb-filters ()
  (para "image-backend-stb 0.3.0 adds a BACKEND-RESIZE method on stb_image_resize2. It serves"
        ":bilinear (stb's triangle filter) and :bicubic (Catmull-Rom: Keys' cubic with"
        "a = -0.5, Pillow's BICUBIC) without :linear, in every pixel format, and declines the"
        "rest (S2): :lanczos3 (stb has no Lanczos-3 kernel), :box (stb's box is a trapezoid),"
        ":nearest (no gain) and :linear t (no Pillow reference). A declined call falls back to"
        "the pure-Lisp resampler, byte for byte. Below, a counting stb backend resizes a"
        "320 × 240 :rgb test card to 80 × 60; the difference is against :backend nil, in"
        "sample values.")
  (let ((card (test-card :rgb 320 240))
        (cstb (make-instance 'counting-stb)))
    (out "  ~10a ~7a ~11a ~22a ~a~%" "filter" "linear" "stb" "same as pure Lisp" "max / mean difference")
    (dolist (row '((:nearest nil) (:box nil) (:bilinear nil) (:bicubic nil) (:lanczos3 nil)
                   (:bilinear t) (:bicubic t)))
      (destructuring-bind (filter linear) row
        (setf (stb-calls cstb) 0 (stb-answers cstb) 0)
        (let* ((native (img:resize-image card 80 60 :filter filter :linear linear :backend cstb))
               (pure (img:resize-image card 80 60 :filter filter :linear linear :backend nil))
               (served (and (member filter '(:bilinear :bicubic)) (not linear) t))
               (answered (= 1 (stb-answers cstb))))
          (multiple-value-bind (max mean)
              (difference (img:image-pixels native) (img:image-pixels pure) :rgb)
            (out "  ~10a ~7a ~11a ~22a ~a~%" (form-text filter) (form-text linear)
                 (if answered "answered" "declined")
                 (if (same-pixels-p native pure) "same" "close, not equal")
                 (diff-text max mean))
            (check (list (stb-calls cstb) answered) (list 1 served)
                   (format nil "stb ~(~s~) linear ~a: consulted once, answered ~a" filter linear served))
            (unless served
              (check (same-pixels-p native pure) t
                     (format nil "~(~s~) declined: the pure-Lisp result" filter)))))))
    (out "~%  :bicubic in every format, 64 × 48 → 23 × 17 (S2, S6):~%")
    (dolist (format +formats+)
      (let* ((card (test-card format 64 48))
             (before (copy-seq (img:image-pixels card)))
             (stb (make-instance 'counting-stb))
             (r (img:resize-image card 23 17 :filter :bicubic :backend stb)))
        (out "  ~12a stb ~a, result ~a ~(~s~) depth ~d, input unchanged: ~a~%"
             (form-text format) (if (= 1 (stb-answers stb)) "answered" "declined")
             (size-text r) (img:image-format r) (img:image-depth r)
             (equalp before (img:image-pixels card)))
        (check (list (stb-answers stb) (size-of r) (img:image-format r) (img:image-depth r)
                     (equalp before (img:image-pixels card)))
               (list 1 '(23 17) format 8 t)
               (format nil "stb :bicubic on ~(~s~)" format))))))

;;; 7. Closeness: pure Lisp, stb and Pillow (S8) ---------------------------------------

(defparameter *closeness-cases*
  ;; (format width height out-width out-height filter gap)
  '((:rgb 1024 768 256 192 :bicubic nil)
    (:rgb 1024 768 256 192 :bilinear nil)
    (:rgb 1024 768 333 250 :bicubic nil)
    (:rgb 512 384 128 96 :bicubic 2)
    (:gray 200 150 640 480 :bilinear nil)
    (:gray-alpha 160 120 53 40 :bicubic nil)
    (:rgba 320 240 80 60 :bicubic nil)
    (:rgba 64 48 160 120 :bilinear nil)))

(defun case-text (format width height out-width out-height filter gap)
  (format nil "~(~s~) ~d×~d → ~d×~d ~(~s~)~@[ gap ~d~]"
          format width height out-width out-height filter gap))

(defun demo-closeness ()
  (para "stb resamples in floats with no rounding between its passes and clamps the kernel"
        "at the edges where Pillow renormalises it, so its samples are close to Pillow's, not"
        "equal (S8). Each row compares three results of the same test card: the pure-Lisp"
        "resampler (:backend nil), stb (:backend stb) and Pillow 11.3.0, run through"
        "`uv run --with pillow==11.3.0 python -I crosscheck/pillow_resize.py`. Differences are"
        "in sample values, the colour of alpha formats premultiplied, as S8 measures them."
        (format nil "S8's bounds: max ~d, mean ~,1f. gap is :reducing-gap, which stb accepts and does"
                +s8-max+ +s8-mean+)
        "not apply.")
  (let* ((stb (make-instance 'stb:stb-backend))
         (rows (loop for (format w h ow oh filter gap) in *closeness-cases*
                     for card = (test-card format w h)
                     collect (list (case-text format w h ow oh filter gap) format card ow oh filter gap
                                   (img:resize-image card ow oh :filter filter :reducing-gap gap :backend nil)
                                   (img:resize-image card ow oh :filter filter :reducing-gap gap :backend stb)))))
    (multiple-value-bind (pillow version)
        (pillow-resize (loop for (nil nil card ow oh filter gap) in rows
                             collect (list card ow oh filter gap)))
      (out "  ~42a ~14a ~14a ~14a~%" "" "Lisp – Pillow" "stb – Lisp" "stb – Pillow")
      (out "  ~42a ~14a ~14a ~14a~%" "case" "max   mean" "max   mean" "max   mean")
      (loop for (label format nil nil nil nil nil pure native) in rows
            for k from 0
            for theirs = (and pillow (nth k pillow))
            do (flet ((cell (a b)
                        (if (and a b)
                            (multiple-value-bind (max mean) (difference a b format)
                              (values (diff-text max mean) max mean))
                            (values "     —" nil nil))))
                 (multiple-value-bind (lp lp-max) (cell (img:image-pixels pure) theirs)
                   (multiple-value-bind (sl sl-max sl-mean) (cell (img:image-pixels native) (img:image-pixels pure))
                     (multiple-value-bind (sp sp-max sp-mean) (cell (img:image-pixels native) theirs)
                       (out "  ~42a ~14a ~14a ~14a~%" label lp sl sp)
                       (check (and (<= sl-max +s8-max+) (<= sl-mean +s8-mean+)) t
                              (format nil "~a: stb within S8 of pure Lisp" label))
                       (when theirs
                         (check lp-max 0 (format nil "~a: pure Lisp equals Pillow" label))
                         (check (and (<= sp-max +s8-max+) (<= sp-mean +s8-mean+)) t
                                (format nil "~a: stb within S8 of Pillow" label))))))))
      (out "~%  ~a~%"
           (if pillow
               (format nil "Pillow ~a. Lisp – Pillow is 0 in every row: the pure-Lisp resampler gives Pillow's samples." version)
               "Pillow was not run (uv is not on PATH); only stb – Lisp is checked."))
      (when pillow (check version "11.3.0" "Pillow version" :test #'equal)))))

;;; 8. Timing ------------------------------------------------------------------

(defparameter *timing-cases*
  ;; (format width height out-width out-height filter gap)
  '((:rgb 1024 768 256 192 :bicubic nil)
    (:rgb 1024 768 256 192 :bilinear nil)
    (:rgb 2048 2048 512 512 :bicubic nil)
    (:rgb 2048 2048 512 512 :bicubic 2)
    (:gray 2048 2048 512 512 :bicubic nil)
    (:gray 2048 2048 512 512 :bicubic 2)
    (:rgba 2048 2048 512 512 :bicubic nil)
    (:rgb 4096 4096 256 256 :bicubic nil)
    (:rgb 4096 4096 256 256 :bicubic 2)
    (:rgb 512 384 2048 1536 :bicubic nil)))

(defparameter *timing-runs* 11)

(defun demo-timing ()
  (para (format nil "Wall-clock milliseconds per resize, the median of ~d runs after one warm-up run,"
                *timing-runs*)
        "pure Lisp (:backend nil) against stb (:backend stb). These numbers depend on the"
        "machine and are not checked. With gap 2 the pure-Lisp resampler first box-reduces"
        "by an integer factor (2 for 2048² → 512², 8 for 4096² → 256²), as Pillow's"
        "reducing_gap does, and keeps Pillow's samples. Since image-protocol 0.3.1 that"
        "makes these reductions faster; in 0.3.0 the box reduce cost more than it saved"
        "(image-protocol#9). stb ignores the gap."
        (format nil "This run: ~a ~a, ~a ~a."
                (software-type) (machine-type)
                (lisp-implementation-type) (lisp-implementation-version)))
  (let ((stb (make-instance 'stb:stb-backend)))
    (out "  ~42a ~12a ~10a ~a~%" "case" "pure Lisp ms" "stb ms" "pure / stb")
    (loop for (format w h ow oh filter gap) in *timing-cases*
          for card = (test-card format w h)
          do (flet ((ms (backend)
                      (* 1000 (median-seconds
                               (lambda () (img:resize-image card ow oh :filter filter :reducing-gap gap
                                                                       :backend backend))
                               *timing-runs*))))
               (let ((pure (ms nil)) (native (ms stb)))
                 (out "  ~42a ~12,1f ~10,1f ~,1f×~%" (case-text format w h ow oh filter gap)
                      pure native (if (plusp native) (/ pure native) 0)))))))

;;; Run -----------------------------------------------------------------------

(defun version-of (system)
  (asdf:component-version (asdf:find-system system)))

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()) (*skipped* '()))
    (out "~&image-protocol ~a: the BACKEND-RESIZE hook (rules R1-R8, sections 2-5);~%~
          image-backend-stb ~a: native resize on stb_image_resize2 (rules S1-S8, sections 6-8)~%"
         (version-of "image-protocol") (version-of "image-backend-stb"))
    (section "1. The 0.2.0 operations, and the test card" (demo-operations))
    (section "2. What reaches BACKEND-RESIZE (R3, R4)" (demo-arguments))
    (section "3. Declining backends: the pure-Lisp result, byte for byte (R5)" (demo-declining))
    (section "4. Bad answers and errors: IMAGE-CORRUPT and TRY-NEXT-BACKEND (R5, R7)" (demo-bad-answers))
    (section "5. *RESIZE-BACKENDS* against :BACKEND (R1, R2, R8)" (demo-designators))
    (section "6. stb: :bilinear and :bicubic served, the rest declined (S1, S2)" (demo-stb-filters))
    (section "7. Closeness: pure Lisp, stb and Pillow 11.3.0 (S8)" (demo-closeness))
    (section "8. Timing: pure Lisp against stb" (demo-timing))
    (banner "Summary")
    (out "~d results checked against expected values: ~d mismatched.~%~
          ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%~
          ~@[Skipped without uv: ~{~a~^, ~}.~%~]"
         *checks* *mismatches* (reverse *failed-sections*) *skipped*)
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
