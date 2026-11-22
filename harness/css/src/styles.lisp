(in-package #:harness-demos-css)

;;; The stylesheet, built with css-protocol's constructors: a custom-property
;;; theme (light + dark, data-theme toggle), typography and layout, tables,
;;; forms, a small-screen @media rule, and a few rules this page never uses.

(defun r (selectors &rest properties-and-values)
  "A style rule: SELECTORS (text) and alternating property / value strings."
  (css:make-rule selectors
                 (loop for (property value) on properties-and-values by #'cddr
                       collect (css:make-declaration property value))))

(defun v (name &optional fallback)
  "var(--NAME) through css-var."
  (css:css-var name fallback))

;;; Contrast (WCAG 2.x, against --bg): fg ~15:1 / ~15:1, muted ~6.2:1 / ~7.2:1,
;;; accent ~6:1 / ~7.9:1 (light / dark); button text on --accent ~6.2:1 / ~8:1.

(defparameter *light*
  '(("bg" . "#fdfdfc") ("fg" . "#1f2328") ("muted" . "#59636e")
    ("surface" . "#f4f5f7") ("border" . "#d8dee4") ("border-strong" . "#8c959f")
    ("accent" . "#0b5cd5") ("accent-hover" . "#0847a6") ("on-accent" . "#ffffff")))

(defparameter *dark*
  '(("bg" . "#14161a") ("fg" . "#e4e6e9") ("muted" . "#9aa4b1")
    ("surface" . "#1d2026") ("border" . "#30353d") ("border-strong" . "#5b636e")
    ("accent" . "#7ab0ff") ("accent-hover" . "#a3c8ff") ("on-accent" . "#0b1220")))

(defun page-theme (&key (light *light*) (dark *dark*) (selector ":root"))
  (css:make-css-theme :light light :dark dark :selector selector
                      :toggle-attribute "data-theme"))

(defparameter +sans+
  "system-ui, -apple-system, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif")

(defparameter +mono+
  "ui-monospace, SFMono-Regular, Menlo, Consolas, \"Liberation Mono\", monospace")

(defun base-rules ()
  "Everything but the theme: typography, layout, tables, forms, figures."
  (list
   (r "*, *::before, *::after" "box-sizing" "border-box")
   (r "html" "font-size" "100%" "-webkit-text-size-adjust" "100%")
   (r "body"
      "margin" 0
      "background" (v "bg")
      "color" (v "fg")
      "font-family" +sans+
      "line-height" 1.6
      "text-rendering" "optimizeLegibility")
   ;; The centred column: about 70 characters per line.
   (r "header, main, footer"
      "max-width" "44rem"
      "margin-inline" "auto"
      "padding-inline" "1.25rem")
   (r "header" "padding-block" "2.5rem 1.25rem" "border-bottom" (format nil "1px solid ~a" (v "border")))
   (r "header h1" "margin" 0 "font-size" "2rem" "letter-spacing" "-0.015em")
   (r "header p" "margin" "0.35rem 0 0" "color" (v "muted"))
   (r "nav ul"
      "display" "flex" "flex-wrap" "wrap" "gap" "0.25rem 1.25rem"
      "margin" "1.25rem 0 0" "padding" 0 "list-style" "none")
   (r "nav a" "font-weight" 500 "text-decoration" "none")
   (r "nav a:hover" "text-decoration" "underline")
   (r "a" "color" (v "accent") "text-underline-offset" "0.2em")
   (r "a:hover" "text-decoration-thickness" "2px")
   (r "a:focus-visible, button:focus-visible, input:focus-visible, select:focus-visible, summary:focus-visible"
      "outline" (format nil "2px solid ~a" (v "accent"))
      "outline-offset" "2px")
   (r "main" "padding-block" "0.5rem 1rem")
   (r "h1, h2, h3" "line-height" 1.25 "text-wrap" "balance")
   (r "h2" "margin" "2.75rem 0 0.75rem" "font-size" "1.45rem" "letter-spacing" "-0.01em")
   (r "h3" "margin" "1.75rem 0 0.5rem" "font-size" "1.1rem")
   (r "p, ul, ol" "margin" "0 0 1rem")
   (r "main li + li" "margin-top" "0.25rem")
   (r "code, pre" "font-family" +mono+ "font-size" "0.9em")
   (r ":not(pre) > code"
      "padding" "0.1em 0.35em"
      "border-radius" "4px"
      "background" (v "surface"))
   (r "pre"
      "margin" "1.25rem 0"
      "padding" "1rem 1.1rem"
      "overflow-x" "auto"
      "line-height" 1.5
      "background" (v "surface")
      "border" (format nil "1px solid ~a" (v "border"))
      "border-radius" "8px")
   ;; Tables
   (r "table"
      "width" "100%"
      "margin" "1.25rem 0"
      "border-collapse" "collapse"
      "font-size" "0.95rem")
   (r "caption" "padding-bottom" "0.5rem" "color" (v "muted") "text-align" "left")
   (r "th, td"
      "padding" "0.55rem 0.75rem"
      "border-bottom" (format nil "1px solid ~a" (v "border"))
      "text-align" "left"
      "vertical-align" "top")
   (r "thead th" "border-bottom" (format nil "2px solid ~a" (v "border-strong")) "font-weight" 600)
   (r "tbody tr:nth-child(even)" "background" (v "surface"))
   (r "tbody th" "font-weight" 500)
   (r "th[data-num], td[data-num]" "text-align" "right" "font-variant-numeric" "tabular-nums")
   ;; Forms
   (r "form"
      "display" "grid"
      "gap" "1rem"
      "margin" "1.25rem 0"
      "padding" "1.25rem"
      "background" (v "surface")
      "border" (format nil "1px solid ~a" (v "border"))
      "border-radius" "10px")
   (r "label" "display" "grid" "gap" "0.35rem" "font-weight" 500)
   (r "label:has(input[type=\"checkbox\"])"
      "display" "flex" "align-items" "center" "gap" "0.5rem" "font-weight" 400)
   (r "input, select, button" "font" "inherit" "color" "inherit")
   (r "input:not([type=\"checkbox\"]), select"
      "width" "100%"
      "padding" "0.5rem 0.65rem"
      "background" (v "bg")
      "border" (format nil "1px solid ~a" (v "border-strong"))
      "border-radius" "6px")
   (r "input::placeholder" "color" (v "muted") "opacity" 1)
   (r "input[type=\"checkbox\"]" "width" "1.1rem" "height" "1.1rem" "margin" 0
      "accent-color" (v "accent"))
   (r "button"
      "justify-self" "start"
      "padding" "0.55rem 1.15rem"
      "color" (v "on-accent")
      "background" (v "accent")
      "border" 0
      "border-radius" "6px"
      "font-weight" 600
      "cursor" "pointer")
   (r "button:hover" "background" (v "accent-hover"))
   ;; Figures, details, footer
   (r "figure" "margin" "1.75rem 0")
   (r "figure svg" "display" "block" "width" "100%" "height" "auto")
   (r "svg rect" "fill" (v "accent"))
   (r "svg text" "fill" (v "fg") "font-family" +sans+ "font-size" "14px")
   (r "figcaption" "margin-top" "0.5rem" "color" (v "muted") "font-size" "0.9rem")
   (r "details"
      "margin" "1.25rem 0"
      "padding" "0.75rem 1rem"
      "border" (format nil "1px solid ~a" (v "border"))
      "border-radius" "8px")
   (r "summary" "font-weight" 600 "cursor" "pointer")
   (r "details[open] > summary" "margin-bottom" "0.75rem")
   (r "details > :last-child" "margin-bottom" 0)
   (r "footer"
      "margin-top" "3rem"
      "padding-block" "1.5rem 3rem"
      "border-top" (format nil "1px solid ~a" (v "border"))
      "color" (v "muted")
      "font-size" "0.9rem")
   ;; Small screens
   (css:make-media-rule
    "(max-width: 40rem)"
    (list (r "html" "font-size" "93.75%")
          (r "header, main, footer" "padding-inline" "1rem")
          (r "header" "padding-block" "1.75rem 1rem")
          (r "header h1" "font-size" "1.6rem")
          (r "th, td" "padding" "0.45rem 0.5rem")
          (r "form" "padding" "1rem")
          (r "button" "justify-self" "stretch")
          (r "svg text" "font-size" "20px")))))

(defun unused-rules ()
  "Rules nothing on the page uses: used-rules drops the style rules and the
@media / @supports blocks whose children all go; @keyframes is always kept."
  (list
   (r ".unused" "display" "none")
   (r "aside"
      "float" "right"
      "width" "14rem"
      "margin" "0 0 1rem 1.5rem"
      "padding" "1rem"
      "border-left" (format nil "3px solid ~a" (v "accent")))
   (r "#sidebar" "position" "sticky" "top" "1rem")
   (r "blockquote" "margin" "1.5rem 0" "padding-left" "1rem"
      "border-left" (format nil "3px solid ~a" (v "border-strong")) "color" (v "muted"))
   (css:make-keyframes-rule
    "pulse"
    (list (css:make-keyframe "from" (list (css:make-declaration "opacity" 1)))
          (css:make-keyframe "to" (list (css:make-declaration "opacity" 0.4)))))
   (css:make-media-rule "print" (list (r "aside, nav" "display" "none")
                                      (r ".no-print" "display" "none")))
   (css:make-supports-rule "(display: grid)" (list (r ".grid-only" "display" "grid")))))

(defun page-stylesheet (&key (theme (page-theme)))
  "The whole stylesheet: theme, base rules, unused rules."
  (css:merge-stylesheets (css:css-theme-stylesheet theme)
                         (css:make-stylesheet (base-rules))
                         (css:make-stylesheet (unused-rules))))
