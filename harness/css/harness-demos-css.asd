(defsystem "harness-demos-css"
  :version "0.1.0"
  :description "Demos of css-protocol 0.1.0 (H4h-0b-0): a classless page built with html-protocol, its stylesheet built with css-protocol's model (light / dark custom-property theme, @media), pretty vs minified output, scope-stylesheet (:prefix, :scope-rule), used-rules pruning against the page, invalid-input conditions and restarts; writes page.html with the pruned stylesheet inlined"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "css-protocol" "0.1.0")
               (:version "html-protocol" "0.1.0")
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "support")
               (:file "page")        ; the classless page as an html-protocol tree
               (:file "styles")      ; the stylesheet: theme, typography, layout, forms, unused rules
               (:file "demos")       ; Demos 1-5
               (:file "run")))
