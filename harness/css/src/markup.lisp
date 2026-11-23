(in-package #:harness-demos-css)

;;; Demos 12-14: markup to pages. markdown-protocol 0.2.0 (images), org-markup
;;; 0.1.0 (the same AST from Org), html-markdown 0.1.0 (the AST as
;;; html-protocol nodes), then one note rendered onto an engels page:
;;; writes page-markup.html. The cl-blog generator (egao1980/cl-blog) is this
;;; pipeline plus templates and feeds.

(defparameter *note-markdown*
  "# Notes on markup

Some *emphasis*, `code`, a [link](https://example.com) and ~~strike~~.

![](images/diagram.svg)

- one
- two

| name | count |
|:-----|------:|
| ada  | 2     |

```lisp
(+ 1 2)
```
")

(defparameter *note-org*
  "#+TITLE: Notes on markup
#+DATE: <2026-11-23 Mon>
#+FILETAGS: :lisp:demo:

* Notes on markup
:PROPERTIES:
:ID: dropped-with-its-drawer
:END:
Some /emphasis/, ~code~, a [[https://example.com][link]] and +strike+.

[[images/diagram.svg]]

- one
- two

| <l>  | <r>   |
| name | count |
|------+-------|
| ada  | 2     |

#+BEGIN_SRC lisp
(+ 1 2)
#+END_SRC

# a comment line, dropped
* Draft notes                                                     :noexport:
Not exported.
")

(defun show-ast (label ast)
  (note "~a:" label)
  (let ((*print-right-margin* 100))
    (block-text (with-output-to-string (s) (pprint ast s)) :indent 5)))

(defun demo-markdown-images ()
  (banner "Demo 12: markdown-protocol 0.2.0 — images")
  (dolist (case '(("an image" "![logo](logo.png \"title dropped\")")
                  ("alt text keeps its inlines" "![a *b* c](i.png)")
                  ("a badge: an image inside a link" "[![build](badge.svg)](https://ci.example)")
                  ("a link inside alt text" "![see [docs](d.html)](i.png)")
                  ("escaped !: a plain link" "\\![x](u)")
                  ("no destination: text" "![x]")))
    (destructuring-bind (label source) case
      (stage "~a: ~s" label source)
      (show "AST" (markdown-protocol:parse-markdown source)))))

(defun demo-org-markup ()
  (banner "Demo 13: org-markup 0.1.0 — the same note in Org, the same AST")
  (stage "the note in Org")
  (block-text *note-org*)
  (multiple-value-bind (org-blocks keywords) (org-markup:parse-org *note-org*)
    (let ((md-blocks (markdown-protocol:parse-markdown *note-markdown*)))
      (stage "keywords (post metadata)")
      (show "keywords" keywords)
      (stage "blocks (drawer, comment and the :noexport: subtree dropped)")
      (show-ast "org-markup" org-blocks)
      (stage "the same note in markdown")
      (block-text *note-markdown*)
      (show "(equal org-blocks markdown-blocks)" (equal org-blocks md-blocks))
      (stage "org specifics")
      (dolist (source '("*bold* /italic/ _underline_ =verbatim= ~code~ +strike+"
                        "snake_case a/b/c 2*3*4 stay text"
                        "[[https://example.com][see [[thumb.png]]]]"
                        "plain https://example.com/x, then text"
                        "- term :: its description"))
        (show source (org-markup:parse-org source))))))

(defun %markup-url (url kind)
  "Images under images/ come from a CDN; anything else as SAFE-URL says."
  (let ((safe (html-markdown:safe-url url kind)))
    (if (and safe (eq kind :image) (eql 0 (search "images/" safe)))
        (concatenate 'string "https://cdn.example/" safe)
        safe)))

(defun markup-page (article-nodes style)
  (html:make-html-document
   :children
   (list (make-instance 'html:html-doctype)
         (el "html" '("lang" "en")
             (el "head" nil
                 (el "meta" '("charset" "utf-8"))
                 (el "meta" '("name" "viewport" "content" "width=device-width, initial-scale=1"))
                 (el "title" nil "Notes on markup")
                 (el "style" nil style))
             (el "body" nil
                 (el "main" nil
                     (html:make-html-element "article" :children article-nodes)))))))

(defun demo-markup-page ()
  (banner "Demo 14: html-markdown 0.1.0 — the AST as HTML, on an engels page — page-markup.html")
  (let ((blocks (org-markup:parse-org *note-org*)))
    (stage "(markdown-html blocks :heading-ids t)")
    (block-text (html-markdown:markdown-html blocks :heading-ids t))
    (stage "a URL function: images to a CDN, unsafe URLs dropped (SAFE-URL)")
    (block-text (html-markdown:markdown-html
                 "![d](images/d.svg) [ok](https://x.y) [bad](javascript:alert(1)) ![x](data:text/html,hi)"
                 :url #'%markup-url))
    (stage "a node the renderer does not know")
    (show "signals"
          (handler-case (html-markdown:render-markdown '((:paragraph ("a")) (:math "x^2")))
            (html-markdown:unknown-markdown-node (c) (type-of c))))
    (show "CONTINUE"
          (handler-bind ((html-markdown:unknown-markdown-node #'continue))
            (html-markdown:markdown-html '((:paragraph ("a")) (:math "x^2")))))
    (show "USE-VALUE"
          (handler-bind ((html-markdown:unknown-markdown-node
                           (lambda (c)
                             (use-value (el "p" nil (format nil "[~a]"
                                                                  (second (html-markdown:unknown-markdown-node-node c))))))))
            (html-markdown:markdown-html '((:math "x^2")))))
    (stage "the page: engels, pruned to the page, inlined")
    (let* ((nodes (html-markdown:render-markdown blocks :heading-ids t :url #'%markup-url))
           (draft (markup-page nodes ""))
           (sheet (css:used-rules (engels:stylesheet) draft))
           (page (markup-page nodes (mini sheet)))
           (out (format nil "~a~%" (html:serialize page)))
           (path (asdf:system-relative-pathname "harness-demos-css" "page-markup.html")))
      (show "engels rules, all / used" (list (count-style-rules (engels:stylesheet))
                                            (count-style-rules sheet)))
      (show "class attributes in the page"
            (count-if (lambda (e) (html:element-attr e "class")) (page-elements page)))
      (show "bytes" (utf-8-length out))
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string out s))
      (note "wrote ~a" (file-namestring path)))))
