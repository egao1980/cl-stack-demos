(in-package #:harness-demos-css)

;;; The page: classless, semantic HTML as an html-protocol tree. No class
;;; attributes; the only data-* attribute is data-num on numeric table cells
;;; (the canvas decision: semantic elements first, data-* where a hook is needed).

(defparameter *block-elements*
  '("html" "head" "body" "header" "nav" "ul" "ol" "main" "article" "table"
    "thead" "tbody" "tr" "form" "figure" "details" "footer" "select" "svg")
  "Elements whose children go on their own lines in page.html (whitespace
text nodes only; PRE and inline elements are left alone).")

(defun text (string)
  (make-instance 'html:html-text :data string))

(defun el (name attributes &rest children)
  "Element NAME with ATTRIBUTES (a plist of strings, in order) and CHILDREN
(strings become text nodes, NILs are dropped, nested lists are spliced)."
  (let* ((kids (loop for child in (splice-children children)
                     when (stringp child) collect (text child)
                     else when child collect child))
         (kids (if (member name *block-elements* :test #'string=)
                   (append (loop for k in kids collect (text (string #\Newline)) collect k)
                           (when kids (list (text (string #\Newline)))))
                   kids)))
    ;; html-protocol 0.1.1 writes attributes in the order given.
    (html:make-html-element
     name
     :attributes (loop for (k v) on attributes by #'cddr collect (cons k v))
     :children kids)))

(defun splice-children (list)
  "LIST with nested lists of children spliced in (one level is all the page uses)."
  (loop for item in list
        if (and (listp item) item) append (splice-children item)
        else collect item))

(defun size-bars (bars)
  "An inline SVG bar chart of BARS, a list of (label value). Colours come from
the stylesheet (svg rect / svg text), so it follows the theme."
  (let* ((max (max 1 (reduce #'max bars :key #'second)))
         (row 34)
         (height (+ 8 (* row (length bars)))))
    (el "svg" (list "viewBox" (format nil "0 0 640 ~d" height)
                    "role" "img"
                    "aria-labelledby" "chart-title")
        (el "title" '("id" "chart-title")
            (format nil "Stylesheet sizes in bytes: ~{~a ~d~^, ~}"
                    (loop for (label value) in bars collect label collect value)))
        (loop for (label value) in bars
              for i from 0
              for y = (+ 6 (* i row))
              for w = (max 2 (round (* 380 value) max))
              collect (el "text" (list "x" "0" "y" (princ-to-string (+ y 18))) label)
              collect (el "rect" (list "x" "190" "y" (princ-to-string y)
                                       "width" (princ-to-string w) "height" "24" "rx" "3"))
              collect (el "text" (list "x" (princ-to-string (+ 198 w)) "y" (princ-to-string (+ y 18)))
                          (format nil "~:d" value))))))

(defparameter *theme-snippet*
  "(css:make-css-theme
  :light '((\"bg\" . \"#fdfdfc\") (\"fg\" . \"#1f2328\") (\"accent\" . \"#0b5cd5\"))
  :dark  '((\"bg\" . \"#14161a\") (\"fg\" . \"#e4e6e9\") (\"accent\" . \"#7ab0ff\"))
  :toggle-attribute \"data-theme\")")

(defparameter *model-text*
  (list "Every rule on this page was built with "
        (el "code" nil "make-rule") " and " (el "code" nil "make-declaration")
        ", not written as text. Selectors are parsed into compounds, declaration values are
validated and normalised, and the serializer writes either a readable or a minified form.")
  "Children of the first paragraph (strings and elements).")

(defparameter *theme-text*
  (list "Colours live in one theme. The rules only say "
        (el "code" nil "var(--fg)") "; the theme maps it to a light or a dark value through "
        (el "code" nil "prefers-color-scheme") ".")
  "Children of the theme paragraph.")

(defun page-tree (&key (style "")
                    (title "Stylesheets as data — css-protocol 0.1.0")
                    (heading "Stylesheets as data")
                    (lede "A classless page whose stylesheet is a CLOS model, pruned to the rules it uses.")
                    (snippet *theme-snippet*)
                    (model-text *model-text*)
                    (theme-text *theme-text*)
                    (footer "Built with html-protocol 0.1.0 and css-protocol 0.1.0 · cl-stack-demos, harness/css")
                    (sizes '(:full-rules 0 :full-pretty 0 :full-mini 0
                                            :used-rules 0 :used-pretty 0 :used-mini 0)))
  "The demo page. STYLE is the stylesheet text inlined in <style>; SIZES feeds
the table and the chart (the element structure does not depend on either)."
  (destructuring-bind (&key full-rules full-pretty full-mini used-rules used-pretty used-mini)
      sizes
    (flet ((num (n) (el "td" '("data-num" "") (format nil "~:d" n))))
      (html:make-html-document
       :children
       (list
        (make-instance 'html:html-doctype :name "html")
        (text (string #\Newline))
        (el "html" '("lang" "en")
            (el "head" nil
                (el "meta" '("charset" "utf-8"))
                (el "meta" '("name" "viewport" "content" "width=device-width, initial-scale=1"))
                (el "meta" '("name" "color-scheme" "content" "light dark"))
                (el "title" nil title)
                (el "style" nil (format nil "~%~a" style)))
            (el "body" nil
                (el "header" nil
                    (el "h1" nil heading)
                    (el "p" nil lede)
                    (el "nav" '("aria-label" "Sections")
                        (el "ul" nil
                            (el "li" nil (el "a" '("href" "#model") "The model"))
                            (el "li" nil (el "a" '("href" "#sizes") "Sizes"))
                            (el "li" nil (el "a" '("href" "#try") "Try the form"))
                            (el "li" nil (el "a" '("href" "#pruning") "Pruning")))))
                (el "main" nil
                    (el "article" nil
                        (el "h2" '("id" "model") "The model is the stylesheet")
                        (el "p" nil model-text)
                        (el "h3" nil "Validated on construction")
                        (el "p" nil
                            "A value such as " (el "code" nil "red;}body{x:y") " is refused with a "
                            (el "code" nil "css-invalid-value") " condition. A handler can supply a replacement with "
                            (el "code" nil "use-value") " or drop the item with "
                            (el "code" nil "skip-invalid") ".")
                        (el "h3" nil "Light and dark from custom properties")
                        (el "p" nil theme-text)
                        (el "pre" nil (el "code" nil snippet))
                        (el "h2" '("id" "sizes") "What it weighs")
                        (el "table" nil
                            (el "caption" nil "The stylesheet before and after pruning against this page, in bytes")
                            (el "thead" nil
                                (el "tr" nil
                                    (el "th" '("scope" "col") "Stylesheet")
                                    (el "th" '("scope" "col" "data-num" "") "Style rules")
                                    (el "th" '("scope" "col" "data-num" "") "Pretty")
                                    (el "th" '("scope" "col" "data-num" "") "Minified")))
                            (el "tbody" nil
                                (el "tr" nil (el "th" '("scope" "row") "Everything written")
                                    (num full-rules) (num full-pretty) (num full-mini))
                                (el "tr" nil (el "th" '("scope" "row") "Used by this page")
                                    (num used-rules) (num used-pretty) (num used-mini))))
                        (el "figure" nil
                            (size-bars (list (list "Full, pretty" full-pretty)
                                             (list "Full, minified" full-mini)
                                             (list "Used, pretty" used-pretty)
                                             (list "Used, minified" used-mini)))
                            (el "figcaption" nil
                                "Figure 1. The same stylesheet four ways. This page inlines the used, pretty form."))
                        (el "h2" '("id" "try") "Try the form")
                        (el "p" nil "The form is styled from element selectors alone. Nothing is sent anywhere.")
                        (el "form" '("action" "#try" "method" "get")
                            (el "label" '("for" "name") "Name"
                                (el "input" '("id" "name" "name" "name" "type" "text"
                                              "autocomplete" "name" "placeholder" "Ada Lovelace")))
                            (el "label" '("for" "scheme") "Colour scheme"
                                (el "select" '("id" "scheme" "name" "scheme")
                                    (el "option" '("value" "system" "selected" "") "Follow the system")
                                    (el "option" '("value" "light") "Light")
                                    (el "option" '("value" "dark") "Dark")))
                            (el "label" nil
                                (el "input" '("type" "checkbox" "name" "notes" "checked" ""))
                                "Send me the release notes")
                            (el "button" '("type" "submit") "Save preferences"))
                        (el "h2" '("id" "pruning") "How pruning decides")
                        (el "details" nil
                            (el "summary" nil "Which rules does used-rules keep?")
                            (el "p" nil
                                "A selector may match when each of its compounds is satisfied by some single element:
its type, its ids and its classes. Combinators, attribute selectors and pseudo-classes count as satisfied,
so anything it cannot decide is kept.")
                            (el "ul" nil
                                (el "li" nil (el "code" nil "table th") " is kept: both elements are on the page.")
                                (el "li" nil (el "code" nil "aside") " and " (el "code" nil ".unused")
                                    " are dropped: nothing here matches.")
                                (el "li" nil (el "code" nil "@keyframes") " and " (el "code" nil "@font-face")
                                    " are always kept.")))))
                (el "footer" nil
                    (el "p" nil footer)))))))))

(defun page-elements (tree)
  "Every html-element in TREE, document order."
  (let ((out '()))
    (labels ((walk (node)
               (typecase node
                 (html:html-document (mapc #'walk (html:html-document-children node)))
                 (html:html-element
                  (push node out)
                  (mapc #'walk (html:html-element-children node))))))
      (walk tree))
    (nreverse out)))
