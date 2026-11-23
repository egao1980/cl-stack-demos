(defsystem "harness-demos-css"
  :version "0.3.0"
  :description "Demos of css-protocol 0.1.0 (H4h-0b-0), css-backend-lass 0.1.0 (H4h-0b-1) and engels 0.2.1 (H4h-0b-2; sanitize.css base layer): a classless page built with html-protocol, its stylesheet built with css-protocol's model (light / dark custom-property theme, @media), pretty vs minified output, scope-stylesheet (:prefix, :scope-rule), used-rules pruning against the page, invalid-input conditions and restarts; then the same page styled by engels plus page rules written as LASS files (unit arithmetic, colour functions, :let*, :include, calc pass-through), pruned; then markdown-protocol 0.2.0 images, org-markup 0.1.0 (the same note in Org gives the same AST) and html-markdown 0.1.0 (the AST as HTML, a URL function, unknown nodes) on an engels page; writes page.html, page-lass.html and page-markup.html"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "css-protocol" "0.1.0")
               (:version "css-backend-lass" "0.1.0")
               (:version "engels" "0.2.1")
               (:version "html-protocol" "0.1.1")
               (:version "markdown-protocol" "0.2.0")
               (:version "org-markup" "0.1.0")
               (:version "html-markdown" "0.1.0")
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "page")        ; the classless page as an html-protocol tree
               (:file "styles")      ; the stylesheet: theme, typography, layout, forms, unused rules
               (:file "demos")       ; Demos 1-6
               (:file "lass")        ; Demos 7-11: lass/*.lass on engels, page-lass.html
               (:file "markup")      ; Demos 12-14: markdown images, org-markup, html-markdown, page-markup.html
               (:file "run")))
