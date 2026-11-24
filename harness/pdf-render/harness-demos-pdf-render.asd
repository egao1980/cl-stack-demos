(defsystem "harness-demos-pdf-render"
  :version "0.1.0"
  :description "Demo of doc-extract-backend-pdf 0.2.0 RENDER-PAGE on libpdfium (the package's native overlay): a three-page PDF fixture rasterized at 72, 150 and 300 dpi (sizes, timing, probe pixels, /Rotate and CropBox), the :rgba, :rgb, :gray and :gray-alpha formats against image-protocol's CONVERT-IMAGE, :max-pixels and :limits refusing a render before any bitmap exists, PDF-PAGE-NOT-FOUND, PDFIUM-NOT-LOADED with USE-VALUE and RETRY, and one page written as PNG through image-backend-stb with its SHA-256 from crypto-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "doc-extract-backend-pdf" "0.2.0")
               (:version "doc-extract-protocol" "0.3.1")
               (:version "image-protocol" "0.1.0")
               (:version "encoding-protocol" "0.3.0")
               (:version "image-backend-stb" "0.2.1")
               (:version "crypto-protocol" "0.2.0")
               (:version "crypto-backend-ironclad" "0.2.2"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
