(in-package #:harness-demos-css)

;;; State shared by the demos: the full stylesheet, the page tree it is pruned
;;; against, the pruned stylesheet and the size numbers.

(defvar *sheet* nil)
(defvar *tree* nil)
(defvar *used* nil)
(defvar *sizes* nil)

(defun ensure-state ()
  (unless *sheet*
    (setf *sheet* (page-stylesheet)
          ;; The page's element structure does not depend on the numbers in it,
          ;; so prune against a first build and fill the numbers in afterwards.
          *tree* (page-tree)
          *used* (css:used-rules *sheet* *tree*)
          *sizes* (list :full-rules (count-style-rules *sheet*)
                        :full-pretty (utf-8-length (pretty *sheet*))
                        :full-mini (utf-8-length (mini *sheet*))
                        :used-rules (count-style-rules *used*)
                        :used-pretty (utf-8-length (pretty *used*))
                        :used-mini (utf-8-length (mini *used*))))))

(defun percent (part whole)
  (if (zerop whole) 0 (round (* 100 part) whole)))

;;; ---------------------------------------------------------------------------
;;; Demo 1: the page and the stylesheet; pretty vs minified.

(defun demo-serialise ()
  (banner "Demo 1: css-protocol 0.1.0 model + serializer — the page, the stylesheet, pretty vs minified")
  (ensure-state)
  (let* ((elements (page-elements *tree*))
         (names (remove-duplicates (mapcar #'html:html-element-name elements)
                                   :test #'string= :from-end t)))
    (stage "the page (html-protocol 0.1.0 tree)")
    (show "elements" (length elements))
    (show "element types" (length names))
    (note "~{~a~^ ~}" names)
    (show "elements with a class attribute"
          (count-if (lambda (e) (html:element-attr e "class")) elements))
    (show "data-* attributes used"
          (remove-duplicates
           (loop for e in elements
                 append (loop for (k . nil) in (html:html-element-attributes e)
                              when (and (> (length k) 5) (string= "data-" k :end2 5))
                                collect k))
           :test #'string=)))
  (stage "the stylesheet (make-css-theme + make-rule / make-media-rule / make-keyframes-rule / make-supports-rule)")
  (show "top-level nodes" (length (css:stylesheet-rules *sheet*)))
  (show "style rules (inside @media / @supports too)" (getf *sizes* :full-rules))
  (show "declarations using var(--…)"
        (labels ((walk (nodes)
                   (loop for n in nodes
                         sum (typecase n
                               (css:rule (count-if (lambda (d) (search "var(--" (css:declaration-value d)))
                                                   (css:rule-declarations n)))
                               (css:keyframes-rule 0)
                               (css:at-rule (walk (ignore-errors (css:at-rule-rules n))))
                               (t 0)))))
          (walk (css:stylesheet-rules *sheet*))))
  (let ((p (pretty *sheet*))
        (m (mini *sheet*)))
    (stage "pretty: (write-stylesheet sheet) — the whole stylesheet")
    (block-text p)
    (stage "minified: (write-stylesheet sheet :pretty nil) — first 600 characters")
    (note "~a…" (subseq m 0 (min 600 (length m))))
    (stage "sizes")
    (note "pretty:   ~6:d bytes, ~d lines" (utf-8-length p) (count #\Newline p))
    (note "minified: ~6:d bytes, 1 line (~d% of pretty)" (utf-8-length m)
          (percent (utf-8-length m) (utf-8-length p)))
    (show "minified has no newline" (null (find #\Newline m)))))

;;; ---------------------------------------------------------------------------
;;; Demo 2: scope-stylesheet, :prefix and :scope-rule.

(defun scope-label (node)
  (typecase node
    (css:scope-rule (format nil "@scope (~{~a~^, ~}) { ~d rules }"
                            (mapcar (lambda (s) (css:selector-text s :pretty nil))
                                    (css:scope-rule-start node))
                            (length (css:at-rule-rules node))))
    (t (rule-label node))))

(defun demo-scope ()
  (banner "Demo 2: scope-stylesheet — :prefix and :scope-rule (a frozen canvas inside a larger page)")
  (ensure-state)
  (let* ((scope "[data-canvas=\"css-demo\"]")
         (prefixed (css:scope-stylesheet *sheet* scope))
         (wrapped (css:scope-stylesheet *sheet* scope :method :scope-rule)))
    (stage "(scope-stylesheet sheet ~s) — :prefix, the default" scope)
    (note "selectors before -> after (theme, root-like and ordinary rules):")
    (loop for before in (css:stylesheet-rules *sheet*)
          for after in (css:stylesheet-rules prefixed)
          for i from 0
          when (member i '(0 1 2 3 4 5 6 8 9 13))
            do (note "  ~a~%      -> ~a" (rule-label before) (rule-label after)))
    (let ((small (find-if (lambda (n) (and (typep n 'css:media-rule)
                                           (search "max-width" (css:media-rule-query n))))
                          (css:stylesheet-rules prefixed))))
      (note "inside @media ~a:" (css:media-rule-query small))
      (dolist (child (subseq (css:at-rule-rules small) 0 3))
        (note "  ~a" (rule-label child))))
    (note "@keyframes is copied unchanged: ~a"
          (rule-label (find-if (lambda (n) (typep n 'css:keyframes-rule))
                               (css:stylesheet-rules prefixed))))
    (stage "excerpt, pretty (first 40 lines)")
    (block-text (pretty prefixed) :max-lines 40)
    (stage "(scope-stylesheet sheet ~s :method :scope-rule)" scope)
    (note "top level: each run of style / @media / @supports rules becomes one @scope;")
    (note "@keyframes stays outside, so it splits the runs:")
    (dolist (n (css:stylesheet-rules wrapped))
      (note "  ~a" (scope-label n)))
    (let ((first-scope (first (css:stylesheet-rules wrapped))))
      (note "inside the first @scope, the root-like selectors become :scope:")
      (dolist (child (subseq (css:at-rule-rules first-scope) 0 6))
        (note "  ~a" (rule-label child))))
    (stage "excerpt, pretty (first 30 lines)")
    (block-text (pretty wrapped) :max-lines 30)
    (stage "sizes (minified)")
    (note "unscoped ~:d, :prefix ~:d, :scope-rule ~:d bytes"
          (utf-8-length (mini *sheet*)) (utf-8-length (mini prefixed)) (utf-8-length (mini wrapped)))
    (show "the input stylesheet is unchanged"
          (string= (mini *sheet*) (mini (page-stylesheet))))))

;;; ---------------------------------------------------------------------------
;;; Demo 3: used-rules against the page tree.

(defun same-group-p (a b)
  (and (eq (type-of a) (type-of b))
       (typecase a
         (css:media-rule (string= (css:media-rule-query a) (css:media-rule-query b)))
         (css:supports-rule (string= (css:supports-rule-condition a) (css:supports-rule-condition b)))
         (t nil))))

(defun demo-used-rules ()
  (banner "Demo 3: used-rules — prune the stylesheet to what the page can use")
  (ensure-state)
  (let ((kept-top (css:stylesheet-rules *used*))
        (kept '()) (dropped '()))
    (dolist (node (css:stylesheet-rules *sheet*))
      (typecase node
        (css:rule
         (if (member node kept-top :test #'eq) (push node kept) (push node dropped)))
        ((or css:media-rule css:supports-rule)
         (let ((pruned (find-if (lambda (k) (same-group-p node k)) kept-top)))
           (if pruned
               (progn
                 (push node kept)
                 (dolist (child (css:at-rule-rules node))
                   (unless (member child (css:at-rule-rules pruned) :test #'eq)
                     (push (list :inside (rule-label node) child) dropped))))
               (push node dropped))))
        (t (push node kept))))
    (setf kept (nreverse kept) dropped (nreverse dropped))
    (stage "kept (~d top-level nodes; style rules are the same objects, EQ)" (length kept))
    (dolist (n kept)
      (note "  ~a" (rule-label n)))
    (stage "dropped (~d)" (length dropped))
    (dolist (d dropped)
      (if (and (consp d) (eq (first d) :inside))
          (note "  ~a   (inside ~a)" (rule-label (third d)) (second d))
          (note "  ~a" (rule-label d))))
    (stage "why")
    (note "aside, blockquote, #sidebar, .unused, .grid-only, .no-print: no element matches.")
    (note "@supports (display: grid) has no rule left, so it goes; @media print keeps")
    (note "\"aside, nav\" whole, because nav is on the page (a rule is kept or dropped as one).")
    (note "@keyframes pulse is kept although nothing animates: at-rules without")
    (note "selectors are always kept (the brief's conservative rule).")
    (note "Pseudo-classes and attribute selectors (a:hover, th[data-num],")
    (note "label:has(input[type=\"checkbox\"])) count as satisfied.")
    (stage "sizes before -> after")
    (destructuring-bind (&key full-rules full-pretty full-mini used-rules used-pretty used-mini) *sizes*
      (note "style rules: ~3d -> ~3d" full-rules used-rules)
      (note "pretty:   ~6:d -> ~6:d bytes (~d% smaller)" full-pretty used-pretty
            (- 100 (percent used-pretty full-pretty)))
      (note "minified: ~6:d -> ~6:d bytes (~d% smaller)" full-mini used-mini
            (- 100 (percent used-mini full-mini))))
    (show "pruning the pruned sheet again changes nothing"
          (string= (mini *used*) (mini (css:used-rules *used* *tree*))))))

;;; ---------------------------------------------------------------------------
;;; Demo 4: invalid input, conditions and restarts.

(defparameter +hostile+ "red;}body{x:y")

(defun report-condition (c)
  (note "condition: ~s" (type-of c))
  (note "report:    ~a" c)
  (when (typep c 'css:css-syntax-error)
    (note "input:     ~s" (css:css-error-input c))))

(defun demo-conditions ()
  (banner "Demo 4: invalid input — conditions, use-value, skip-invalid")
  (stage "(make-declaration \"color\" ~s) with no handler that fixes it" +hostile+)
  (handler-case (css:make-declaration "color" +hostile+)
    (css:css-syntax-error (c)
      (report-condition c)
      (note "a css-error: ~a, a css-syntax-error: ~a" (typep c 'css:css-error) (typep c 'css:css-syntax-error))))
  (let ((restarts nil))
    (handler-bind ((css:css-invalid-value
                     (lambda (c)
                       (setf restarts (loop for r in (compute-restarts c)
                                            for i below 2
                                            collect (list (restart-name r) (princ-to-string r))))
                       (css:skip-invalid c))))
      (css:make-declaration "color" +hostile+))
    (note "restarts offered at the signal:")
    (loop for (name text) in restarts do (note "  ~s — ~a" name text)))

  (stage "use-value: a handler supplies a replacement (validated again)")
  (let ((attempts '()))
    (let ((d (handler-bind ((css:css-invalid-value
                              (lambda (c)
                                (push (css:css-error-input c) attempts)
                                ;; First replacement is still bad; the second is accepted.
                                (use-value (if (= (length attempts) 1) "red;" "crimson") c))))
               (css:make-declaration "color" +hostile+))))
      (note "inputs refused, in order: ~s" (reverse attempts))
      (note "result: ~a" (mini (css:make-stylesheet (list (css:make-rule "p" (list d))))))))

  (stage "skip-invalid: the bad item is dropped, its container keeps the rest")
  (handler-bind ((css:css-syntax-error
                   (lambda (c)
                     (note "skipping ~s: ~s" (type-of c) (css:css-error-input c))
                     (css:skip-invalid c))))
    (let ((sheet (css:make-stylesheet
                  (list (css:make-rule "p" (list (css:make-declaration "color" +hostile+)
                                                 (css:make-declaration "margin" 0)))
                        (css:make-rule "p { } body" (list (css:make-declaration "color" "blue")))
                        (css:make-rule "em" (list (css:make-declaration "col or" "green")
                                                  (css:make-declaration "font-style" "italic")))
                        (css:make-media-rule "screen {" (list (css:make-rule "a" nil)))
                        (css:make-rule "strong" (list (css:make-declaration "color" "</style><script>")))))))
      (note "stylesheet left: ~a" (mini sheet))
      (note "(rule \"p { } body\" is NIL: a rule whose selector list is skipped is not a rule;")
      (note " @media \"screen {\" is NIL the same way; \"strong\" keeps no declarations)")))

  (stage "each refusal, one by one")
  (dolist (case (list (list "value" (lambda () (css:make-declaration "color" +hostile+)))
                      (list "value with </" (lambda () (css:make-declaration "content" "\"</style>\"")))
                      (list "property" (lambda () (css:make-declaration "col or" "red")))
                      (list "selector" (lambda () (css:make-rule "p { } body" nil)))
                      (list "selector (nesting)" (lambda () (css:make-rule "& > a" nil)))
                      (list "media query" (lambda () (css:make-media-rule "screen {" nil)))
                      (list "keyframes name" (lambda () (css:make-keyframes-rule "" nil)))
                      (list "keyframe selector" (lambda () (css:make-keyframe "halfway" nil)))))
    (destructuring-bind (label thunk) case
      (handler-case (progn (funcall thunk) (note "~a: accepted?!" label))
        (css:css-syntax-error (c)
          (note "~20a ~s  ~s" (concatenate 'string label ":") (type-of c) (css:css-error-input c))))))

  (stage "text that is data, not syntax, is escaped instead")
  (note "(css-string \"</style>\")  => ~a" (css:css-string "</style>"))
  (note "(css-ident \"2col\")         => ~a" (css:css-ident "2col"))
  (note "(css-url \"a b\\\".png\")      => ~a" (css:css-url "a b\".png"))
  (note "rule with content: (css-string ...): ~a"
        (mini (css:make-stylesheet
               (list (css:make-rule "q::before"
                                    (list (css:make-declaration "content" (css:css-string "</style>"))))))))

  (stage "compile-stylesheet with no backend: css-no-backend, then use-value")
  (let ((fallback (css:make-stylesheet (list (r "body" "margin" 0)))))
    (let ((result (handler-bind ((css:css-no-backend
                                   (lambda (c)
                                     (report-condition c)
                                     (note "restarts: ~s"
                                           (loop for r in (compute-restarts c) for i below 2
                                                 collect (restart-name r)))
                                     (use-value fallback c))))
                    (css:compile-stylesheet '(:lass (body :margin 0))))))
      (note "result: ~a (the fallback, EQ: ~a)" (mini result) (eq result fallback)))))

;;; ---------------------------------------------------------------------------
;;; Demo 5: the theme — light and dark through custom properties.

(defun demo-theme ()
  (banner "Demo 5: make-css-theme — light and dark through custom properties")
  (let* ((theme (page-theme))
         (theme-sheet (css:css-theme-stylesheet theme)))
    (stage "the maps (css-theme-light / css-theme-dark)")
    (loop for (property . light) in (css:css-theme-light theme)
          for dark = (cdr (assoc property (css:css-theme-dark theme) :test #'string=))
          do (note "~16a light ~8a  dark ~8a" property light dark))
    (stage "(css-theme-stylesheet theme), pretty")
    (block-text (pretty theme-sheet))
    (stage "how the page switches")
    (note "The rules never name a colour, only var(--…):")
    (let ((body (find-if (lambda (n) (and (typep n 'css:rule)
                                          (string= "body" (rule-label n))))
                         (css:stylesheet-rules *sheet*))))
      (note "  ~a" (mini (css:make-stylesheet (list body)))))
    (note "OS light, no data-theme      -> :root                       (light values)")
    (note "OS dark,  no data-theme      -> :root:not([data-theme=\"light\"]) in @media (dark values)")
    (note "data-theme=\"dark\" on <html>  -> :root[data-theme=\"dark\"]    (dark, whatever the OS)")
    (note "data-theme=\"light\" on <html> -> :root[data-theme=\"light\"]   (light: the @media rule no longer matches)")
    (note "color-scheme: light dark on :root also switches form controls and scrollbars.")
    (stage "another theme, same rules: only the custom properties change")
    (let* ((sepia (page-theme :light '(("bg" . "#fbf6ec") ("fg" . "#2b2118") ("muted" . "#6b5a48")
                                       ("surface" . "#f3ead9") ("border" . "#e0d3bc")
                                       ("border-strong" . "#a08d74") ("accent" . "#8a3b12")
                                       ("accent-hover" . "#6d2e0e") ("on-accent" . "#ffffff"))
                              :dark '(("bg" . "#1b1612") ("fg" . "#eadfce") ("muted" . "#b3a28c")
                                      ("surface" . "#251e18") ("border" . "#3d332a")
                                      ("border-strong" . "#6e604f") ("accent" . "#f0a46c")
                                      ("accent-hover" . "#f6c39c") ("on-accent" . "#1b1612"))))
           (a (page-stylesheet))
           (b (page-stylesheet :theme sepia))
           (n (length (css:stylesheet-rules theme-sheet)))
           (rest-a (mini (css:make-stylesheet (nthcdr n (css:stylesheet-rules a)))))
           (rest-b (mini (css:make-stylesheet (nthcdr n (css:stylesheet-rules b))))))
      (show "rules after the theme block identical" (string= rest-a rest-b))
      (note "sepia theme block, minified: ~a"
            (mini (css:css-theme-stylesheet sepia))))
    (stage "the same theme on a canvas instead of :root (:selector)")
    (block-text (mini (css:css-theme-stylesheet (page-theme :selector "[data-canvas]"))))))

;;; ---------------------------------------------------------------------------
;;; Demo 6: page.html — the pruned stylesheet inlined.

(defun no-external-urls-p (text)
  (notany (lambda (needle) (search needle text))
          '("http://" "https://" "//cdn" "url(" " src=" "@import")))

(defun demo-page ()
  (banner "Demo 6: page.html — the used rules inlined in <style>, nothing external")
  (ensure-state)
  (let* ((style (pretty *used*))
         (tree (page-tree :style style :sizes *sizes*))
         (html (format nil "~a~%" (html:serialize tree)))
         (path (asdf:system-relative-pathname "harness-demos-css" "page.html")))
    (show "used-rules on the final page = the inlined rules"
          (string= (mini *used*) (mini (css:used-rules *sheet* tree))))
    (show "class attributes in the page"
          (count-if (lambda (e) (html:element-attr e "class")) (page-elements tree)))
    (show "no external URLs" (no-external-urls-p html))
    (show "</ only where tags close (none inside <style>)"
          (null (search "</" style)))
    (write-file path html)
    (note "wrote harness/css/page.html: ~:d bytes (style ~:d bytes, ~d lines)"
          (utf-8-length html) (utf-8-length style) (count #\Newline style))
    (stage "page.html, first 30 lines")
    (block-text html :max-lines 30)))
