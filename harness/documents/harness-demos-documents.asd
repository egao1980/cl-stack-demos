(defsystem "harness-demos-documents"
  :version "0.1.0"
  :description "Demo of the documents and data formats fixes: PDF text with no password (doc-extract-backend-pdf 0.2.0), DOCX and XLSX extraction (doc-extract-protocol 0.4.0 on compression-protocol 0.5.1), YAML block output without trailing blanks (yaml-protocol 0.2.2), .env files and typed settings (cl-stack-config 0.3.0), image editing operations (image-protocol 0.2.0), each cross-read by the Python library it replaces (pypdf, python-docx, openpyxl, PyYAML, python-dotenv, Pillow) when uv is on PATH"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "doc-extract-backend-pdf" "0.2.0")
               (:version "doc-extract-protocol" "0.4.0")
               (:version "compression-protocol" "0.5.1")
               (:version "yaml-protocol" "0.2.2")
               (:version "cl-stack-config" "0.3.0")
               (:version "schema-protocol" "0.2.2")
               (:version "image-protocol" "0.2.0")
               (:version "image-backend-stb" "0.2.1")
               (:version "encoding-protocol" "0.3.0")
               (:version "crypto-protocol" "0.2.0")
               (:version "crypto-backend-ironclad" "0.2.2")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "pdf")
               (:file "office")
               (:file "yaml")
               (:file "dotenv")
               (:file "image")
               (:file "run")))
