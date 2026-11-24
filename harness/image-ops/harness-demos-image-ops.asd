(defsystem "harness-demos-image-ops"
  :version "0.1.1"
  :description "Demo of image-protocol 0.3.0's BACKEND-RESIZE hook and image-backend-stb 0.3.0's native resize on stb_image_resize2: a recording backend showing the arguments RESIZE-IMAGE, THUMBNAIL and FIT-IMAGE pass (R4) and when no backend is consulted (R3), declining backends falling back to the pure-Lisp resampler byte for byte (R5), bad answers as IMAGE-CORRUPT and a backend's own error recovered with TRY-NEXT-BACKEND (R7), *RESIZE-BACKENDS* against :BACKEND and bad designators as IMAGE-INVALID-ARGUMENT (R1, R2, R8); the stb backend serving :BILINEAR and :BICUBIC and declining :NEAREST, :BOX, :LANCZOS3 and :LINEAR T (S2), a closeness table of pure Lisp, stb and Pillow 11.3.0 (through uv) against the S8 bounds, and a timing table of pure Lisp against native, with and without the reducing gap that image-protocol 0.3.1 made faster (#9)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "image-protocol" "0.3.1")
               (:version "image-backend-stb" "0.3.0")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "demo")))
