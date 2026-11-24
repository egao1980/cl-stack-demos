;;;; 5. Image editing operations (image-protocol#2).

(in-package #:harness-demos-documents)

(defun make-rgba (width height pixel)
  "A WIDTH x HEIGHT :RGBA image; (PIXEL x y) returns (values r g b a)."
  (let ((pixels (make-array (* width height 4) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (dotimes (x width)
        (multiple-value-bind (r g b a) (funcall pixel x y)
          (let ((i (* 4 (+ x (* y width)))))
            (setf (aref pixels i) r (aref pixels (+ i 1)) g
                  (aref pixels (+ i 2)) b (aref pixels (+ i 3)) a)))))
    (img:make-image :width width :height height :format :rgba :pixels pixels)))

(defun demo-input-image ()
  "64 x 48: red grows to the right, green downwards, blue is x*y; the left
half is opaque, the right half's alpha grows downwards."
  (make-rgba 64 48 (lambda (x y)
                     (values (mod (* x 4) 256) (mod (* y 5) 256) (mod (* x y) 256)
                             (if (< x 32) 255 (min 255 (* y 6)))))))

(defun demo-logo ()
  "20 x 12: a translucent orange ellipse on a transparent ground."
  (make-rgba 20 12 (lambda (x y)
                     (if (<= (+ (* 36 (expt (- (* 2 x) 19) 2)) (* 100 (expt (- (* 2 y) 11) 2)))
                             (* 100 36 4))
                         (values 255 40 0 160)
                         (values 0 0 0 0)))))

(defun image-operations (src logo)
  "(label . result) for every operation the section shows, in order."
  (list
   (cons "input" src)
   (cons "logo" logo)
   (cons "crop 10 8 40 24" (img:crop-image src 10 8 40 24))
   (cons "rotate 90" (img:rotate-image src 90))
   (cons "rotate 180" (img:rotate-image src 180))
   (cons "rotate 270" (img:rotate-image src 270))
   (cons "rotate -90" (img:rotate-image src -90))
   (cons "flip :horizontal" (img:flip-image src :horizontal))
   (cons "flip :vertical" (img:flip-image src :vertical))
   (cons "resize 37x23 :nearest" (img:resize-image src 37 23 :filter :nearest))
   (cons "resize 37x23 :box" (img:resize-image src 37 23 :filter :box))
   (cons "resize 37x23 :bilinear" (img:resize-image src 37 23 :filter :bilinear))
   (cons "resize 37x23 :bicubic" (img:resize-image src 37 23 :filter :bicubic))
   (cons "resize 37x23 :lanczos3" (img:resize-image src 37 23 :filter :lanczos3))
   (cons "resize 160x120 :bicubic" (img:resize-image src 160 120 :filter :bicubic))
   (cons "rgb resize 21x16 :lanczos3 :reducing-gap 2"
         (img:resize-image (img:convert-image src :rgb) 21 16 :filter :lanczos3 :reducing-gap 2))
   (cons "thumbnail 32x32 :bicubic" (img:thumbnail src 32 32 :filter :bicubic))
   (cons "composite logo at 12,6" (img:composite-image src logo :x 12 :y 6))))

(defun image-size (image)
  (list (img:image-width image) (img:image-height image)))

(defun pixel (image x y)
  "IMAGE's samples at X, Y as a list."
  (let* ((n (img:channels (img:image-format image)))
         (i (* n (+ x (* y (img:image-width image))))))
    (coerce (subseq (img:image-pixels image) i (+ i n)) 'list)))

(defun demo-image (dir)
  (let* ((src (demo-input-image))
         (logo (demo-logo))
         (ops (image-operations src logo))
         (input-path (merge-pathnames "input.png" dir))
         (logo-path (merge-pathnames "logo.png" dir)))
    (write-octets (img:encode-image src :png) input-path)
    (write-octets (img:encode-image logo :png) logo-path)
    (out "Inputs, made in code and written as PNG by image-backend-stb (lossless):~%~
          output/input.png 64 x 48 :rgba (left half opaque, right half's alpha grows downwards),~%~
          output/logo.png 20 x 12 :rgba (a translucent orange ellipse).~%~
          Every operation returns a new image and leaves its input as it was.~%~%")
    (check (img:image-pixels (img:decode-image input-path)) (img:image-pixels src)
           "input.png decodes to the same pixels" :test #'equalp)
    ;; Values worked out by hand from the input formula.
    (check (pixel (img:crop-image src 10 8 40 24) 0 0) '(40 40 80 255) "crop: (0,0) is source (10,8)")
    (check (image-size (img:rotate-image src 90)) '(48 64) "rotate 90: sides swap")
    (check (pixel (img:rotate-image src 90) 0 0) (pixel src 63 0)
           "rotate 90 is counter-clockwise: top-left is the old top-right")
    (check (pixel (img:flip-image src :horizontal) 0 5) (pixel src 63 5) "flip :horizontal")
    (check (image-size (img:thumbnail src 32 32)) '(32 24) "thumbnail keeps 4:3")
    (check (img:thumbnail src 100 100) src "thumbnail never enlarges: the input comes back" :test #'eq)
    (check (image-size (img:fit-image src :max-width 50)) '(50 38) "fit-image :max-width 50")
    (check (image-size (img:fit-image src :max-height 96 :enlarge t)) '(128 96)
           "fit-image :max-height 96 :enlarge t")
    (let ((c (signalled (lambda () (img:crop-image src 60 40 10 10)))))
      (out "crop-image past the edge => ~a: ~a~%" (type-of c) (one-line (princ-to-string c)))
      (check (typep c 'img:image-invalid-argument) t "crop out of bounds"))
    (let ((c (signalled (lambda () (img:rotate-image src 45)))))
      (out "rotate-image 45 => ~a: ~a~%~%" (type-of c) (one-line (princ-to-string c)))
      (check (typep c 'img:image-invalid-argument) t "rotate by 45"))
    (let ((clipped (img:composite-image src logo :x -5 :y 40)))
      (check (image-size clipped) '(64 48) "a composite partly outside keeps the destination's size")
      (check (pixel clipped 63 0) (pixel src 63 0) "outside the logo nothing changes"))
    (dolist (name '("resize-160x120-bicubic" "thumbnail-32x32" "composite"))
      (let ((image (cdr (assoc (cond ((search "resize" name) "resize 160x120 :bicubic")
                                     ((search "thumb" name) "thumbnail 32x32 :bicubic")
                                     (t "composite logo at 12,6"))
                               ops :test #'string=))))
        (write-octets (img:encode-image image :png) (merge-pathnames (format nil "~a.png" name) dir))))
    (out "Wrote output/resize-160x120-bicubic.png, thumbnail-32x32.png and composite.png.~%~%")
    (let* ((py (python "pillow_ops.py" '("pillow==11.3.0") input-path logo-path))
           (theirs (and py (jget py "ops"))))
      (out "~44a ~9a ~10a ~16a ~a~%" "operation" "size" "format" "sha256 of pixels"
           (if py (format nil "Pillow ~a" (jget py "version")) ""))
      (loop for (label . image) in ops
            for ours = (sha256-hex (img:image-pixels image))
            for pillow = (and theirs (gethash label theirs))
            do (out "~44a ~9a ~10s ~16a ~a~%" label
                    (format nil "~{~d x ~d~}" (image-size image)) (img:image-format image)
                    (subseq ours 0 16)
                    (cond ((null pillow) "")
                          ((string= ours (jget pillow "sha256")) "same pixels")
                          (t (format nil "DIFFERENT (~a)" (subseq (jget pillow "sha256") 0 16)))))
               (when pillow
                 (check ours (jget pillow "sha256") (format nil "~a: Pillow's pixels" label))
                 (check (image-size image) (coerce (jget pillow "size") 'list)
                        (format nil "~a: Pillow's size" label)))))))
