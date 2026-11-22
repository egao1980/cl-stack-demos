(defsystem "harness-demos-css"
  :version "0.2.0"
  :description "Demos of css-protocol 0.1.0 (H4h-0b-0), css-backend-lass 0.1.0 (H4h-0b-1) and engels 0.2.0 (H4h-0b-2): a classless page built with html-protocol, its stylesheet built with css-protocol's model (light / dark custom-property theme, @media), pretty vs minified output, scope-stylesheet (:prefix, :scope-rule), used-rules pruning against the page, invalid-input conditions and restarts; then the same page styled by engels plus page rules written as LASS files (unit arithmetic, colour functions, :let*, :include, calc pass-through), pruned; writes page.html and page-lass.html"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "css-protocol" "0.1.0")
               (:version "css-backend-lass" "0.1.0")
               (:version "engels" "0.2.0")
               (:version "html-protocol" "0.1.1")
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "page")        ; the classless page as an html-protocol tree
               (:file "styles")      ; the stylesheet: theme, typography, layout, forms, unused rules
               (:file "demos")       ; Demos 1-6
               (:file "lass")        ; Demos 7-11: lass/*.lass on engels, page-lass.html
               (:file "run")))
