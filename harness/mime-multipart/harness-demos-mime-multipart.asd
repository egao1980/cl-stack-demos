(defsystem "harness-demos-mime-multipart"
  :version "0.2.0"
  :description "Demo of mime-protocol 0.3.1. The fix for mime-protocol#9: the mail layout multipart/mixed(multipart/alternative(text, html), pdf) printed, parsed back and printed again to the same text, part content ending in CRLF kept, a nested boundary that has the outer one as a prefix, an LF-only message, preamble and epilogue, canonical header names on print (MIME-Version, Content-Transfer-Encoding, Message-ID) with case-insensitive lookups, and a cross-read with Python's email package in both directions when uv is on PATH. The fix for mime-protocol#14: a browser's multipart/form-data part headers (UTF-8 name and filename, a literal backslash, %22 %0D %0A), the ISO-8859-1 fallback, our own encoders parsed back, and the same form read by python-multipart and werkzeug"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "mime-protocol" "0.3.1")
               "encoding-protocol"
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
