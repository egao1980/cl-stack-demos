(in-package #:harness-demos-css)

;;; Demos 7-11: the same page with its stylesheet written in LASS
;;; (harness/css/lass/*.lass), compiled by css-backend-lass 0.1.0 and layered
;;; on engels 0.2.0: (engels:stylesheet), then the page rules, then
;;; used-rules against the page. Writes page-lass.html.

(defun lass-path (name)
  (asdf:system-relative-pathname "harness-demos-css" (concatenate 'string "lass/" name)))

(defun file-text (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defun lass-forms-with-text (pathname)
  "Each top-level form of the LASS file PATHNAME with its source text, read
the way css-backend-lass reads LASS files (its user package, no #.)."
  (let ((text (file-text pathname)))
    (with-input-from-string (in text)
      (with-standard-io-syntax
        (let ((*package* (find-package '#:css-backend-lass-user))
              (*read-eval* nil)
              (eof (list nil)))
          (loop for start = (progn (peek-char t in nil) (file-position in))
                for form = (read in nil eof)
                until (eq form eof)
                collect (list form (string-trim '(#\Space #\Newline)
                                                (subseq text start (file-position in))))))))))

(defun let*-bindings (pathname)
  "The bindings of the outer (:let* bindings ...) form of PATHNAME."
  (let ((form (first (find-if (lambda (f) (and (consp (first f)) (eq :let* (first (first f)))))
                              (lass-forms-with-text pathname)))))
    (second form)))

(defun compile-in-page-scope (form)
  "FORM compiled where the page's parts are: inside the tokens' and page.lass's :let*."
  (css-backend-lass:compile-lass
   (list (list :let* (let*-bindings (lass-path "tokens.lass"))
               (list :let* (let*-bindings (lass-path "page.lass")) form)))
   :pathname (lass-path "parts/")))

(defun src (form)
  "FORM printed as LASS source (lowercase, no package prefixes)."
  (let ((*package* (find-package (quote #:harness-demos-css)))
        (*print-case* :downcase)
        (*print-pretty* nil))
    (prin1-to-string form)))

(defun lass-value (expression &key variables)
  "The CSS text css-backend-lass makes of one LASS value EXPRESSION."
  (let* ((sheet (css-backend-lass:compile-lass (list (list 'x :v expression))
                                               :variables variables))
         (rule (first (css:stylesheet-rules sheet))))
    (and rule (css:declaration-value (first (css:rule-declarations rule))))))

(defvar *page-lass* nil "The page rules compiled from lass/tokens.lass.")
(defvar *layered* nil "engels + the page rules.")
(defvar *layered-used* nil "*LAYERED* pruned to page-lass.html.")
(defvar *lass-sizes* nil)

(defparameter *lass-snippet*
  "(:let* ((space 8px)
        (gutter (* 2 #(space)))          ; 16.00px
        (accent \"#0b5cd5\"))
  (:let* ((accent-hover (darken #(accent) 10%))
          (accent-border (mix #(accent) \"#ffffff\" 25%)))
    (:include \"parts/theme.lass\")      ; --page-* light / dark
    (details
     :max-width (calc (- 100% (* 2 #(gutter))))
     :border 1px solid \"var(--page-border)\")))")

(defun lass-page-tree (&key (style "") sizes)
  (apply #'page-tree
         :style style
         :title "Stylesheets as data — LASS on engels"
         :heading "Stylesheets as data, in LASS"
         :lede "The same classless page, styled by engels 0.2.0 plus page rules written in LASS, pruned to the rules it uses."
         :snippet *lass-snippet*
         :model-text (list "The page rules are LASS files compiled by "
                           (el "code" nil "css-backend-lass") " into the css-protocol model and layered on "
                           (el "code" nil "(engels:stylesheet)")
                           ". Unit arithmetic, colour functions, " (el "code" nil ":let*") " and "
                           (el "code" nil ":include") " run at compile time; the model is then pruned and serialized like any other.")
         :theme-text (list "Engels keeps its colours in " (el "code" nil "--engels-*")
                           " custom properties; the page adds " (el "code" nil "--page-*")
                           " ones, derived with " (el "code" nil "darken") ", " (el "code" nil "lighten")
                           " and " (el "code" nil "mix") ", switched by "
                           (el "code" nil "prefers-color-scheme") " or " (el "code" nil "data-theme") ".")
         :footer "Built with engels 0.2.0, css-backend-lass 0.1.0, css-protocol 0.1.0 and html-protocol 0.1.0 · cl-stack-demos, harness/css"
         (when sizes (list :sizes sizes))))

(defun ensure-lass-state ()
  (unless *page-lass*
    (setf *page-lass* (css-backend-lass:compile-lass-file (lass-path "tokens.lass"))
          *layered* (css:merge-stylesheets (engels:stylesheet) *page-lass*)
          *layered-used* (css:used-rules *layered* (lass-page-tree))
          *lass-sizes* (list :full-rules (count-style-rules *layered*)
                             :full-pretty (utf-8-length (pretty *layered*))
                             :full-mini (utf-8-length (mini *layered*))
                             :used-rules (count-style-rules *layered-used*)
                             :used-pretty (utf-8-length (pretty *layered-used*))
                             :used-mini (utf-8-length (mini *layered-used*))))))

;;; ---------------------------------------------------------------------------
;;; Demo 7: LASS source next to the CSS it compiles to.

(defun show-form-and-css (file index)
  (destructuring-bind (form text) (nth index (lass-forms-with-text (lass-path file)))
    (stage "~a, form ~d" file (1+ index))
    (note "LASS:")
    (block-text text :indent 5)
    (note "CSS (pretty):")
    (block-text (pretty (compile-in-page-scope form)) :indent 5)))

(defun demo-lass-source ()
  (banner "Demo 7: css-backend-lass 0.1.0 — LASS source next to the CSS it compiles to")
  (ensure-lass-state)
  (stage "lass/tokens.lass — the shared variables file (bindings flow into :include, not out)")
  (block-text (file-text (lass-path "tokens.lass")))
  (stage "lass/page.lass — :let* of derived colours, then the parts")
  (block-text (file-text (lass-path "page.lass")))
  (show-form-and-css "parts/theme.lass" 0)
  (show-form-and-css "parts/theme.lass" 1)
  (show-form-and-css "parts/layout.lass" 0)
  (show-form-and-css "parts/layout.lass" 2)
  (show-form-and-css "parts/layout.lass" 4)
  (show-form-and-css "parts/components.lass" 1)
  (show-form-and-css "parts/components.lass" 2)
  (stage "the whole page sheet: (compile-lass-file \"lass/tokens.lass\")")
  (show "top-level nodes" (length (css:stylesheet-rules *page-lass*)))
  (show "style rules" (count-style-rules *page-lass*))
  (show "same text on a second compile"
        (string= (mini *page-lass*)
                 (mini (css-backend-lass:compile-lass-file (lass-path "tokens.lass")))))
  (stage "minified")
  (note "~a" (mini *page-lass*)))

;;; ---------------------------------------------------------------------------
;;; Demo 8: the classie functions.

(defparameter *classie-cases*
  '(("unit arithmetic" (+ 2in (ceil (* 85% "800px"))))
    ("percent of a length" (* 85% "800px"))
    ("percent of a percent" (* 50% 50%))
    ("pt to the left unit" (+ 1in 36pt))
    ("length / length" (/ 768px 16px))
    ("ceil (shown number)" (ceil 49.5%))
    ("ceiling (fraction)" (ceiling 49.5%))
    ("ceil of 0.85 * 16px" (ceil (* 0.85 "16px")))
    ("values" (values 1px solid "#d0d7de"))
    ("darken 20%" (darken "#fff" 20%))
    ("darken 10% (hover)" (darken "#0b5cd5" 10%))
    ("lighten 25% (dark accent)" (lighten "#0b5cd5" 25%))
    ("mix 25% with white (border)" (mix "#0b5cd5" "#ffffff" 25%))
    ("mix, default 50%" (mix "#007bff" "#fff"))
    ("fade-out 0.15" (fade-out "#0b5cd5" 0.15))
    ("fade-in 0.25" (fade-in "rgba(11, 92, 213, 0.5)" 0.25))
    ("engels: darken primary 7.5%" (darken "#007bff" 0.075))
    ("engels: darken primary 10%" (darken "#007bff" 0.1))
    ("engels: darken red 10%" (darken "#f44336" 0.1))))

(defun demo-classie-functions ()
  (banner "Demo 8: classie's functions, re-implemented in css-backend-lass (no cl-colors, no dufy)")
  (stage "(compile-lass '((x :v EXPRESSION))) -> the declaration value")
  (dolist (case *classie-cases*)
    (destructuring-bind (label expression) case
      (note "~30a ~45a => ~a" label (src expression) (lass-value expression))))
  (stage "the page's tokens and derived colours, resolved in order (:let*)")
  (let ((sheet (compile-in-page-scope
                (list* 'x
                       (loop for name in '("space" "gutter" "column" "radius" "accent"
                                           "accent-hover" "accent-border" "accent-wash" "bar"
                                           "accent-dark" "accent-dark-hover" "accent-dark-border"
                                           "accent-dark-wash" "bar-dark")
                             collect (intern (string-upcase name) :keyword)
                             collect (vector (intern (string-upcase name) '#:css-backend-lass-user)))))))
    (dolist (d (css:rule-declarations (first (css:stylesheet-rules sheet))))
      (note "~22a ~a" (format nil "#(~a)" (css:declaration-property d)) (css:declaration-value d))))
  (stage "engels 0.2.0 derives its palette the same way (engels:palette-colours)")
  (loop for (name . value) in (engels:palette-colours engels:*light-palette*)
        for dark = (cdr (assoc name (engels:palette-colours engels:*dark-palette*) :test #'string=))
        when (member name '("engels-primary" "engels-primary-hover" "engels-primary-active"
                            "engels-primary-active-border" "engels-input-focus-border"
                            "engels-input-focus-ring" "engels-red-dark")
                     :test #'string=)
          do (note "--~28a light ~24a dark ~a" name value dark)))

;;; ---------------------------------------------------------------------------
;;; Demo 9: calc(...) passes through.

(defun demo-calc ()
  (banner "Demo 9: calc(...) passes through — CSS arithmetic is not LASS arithmetic")
  (dolist (expression '((calc (- 100% (* 2 "16px")))
                        (calc (+ 2.25rem 2px))
                        (calc (- 100vw #(gutter)))
                        "calc(2.25rem + 2px)"))
    (note "~38a => ~a" (src expression) (lass-value expression :variables (list (cons (quote gutter) "16px")))))
  (note "LASS 0.6.0 parenthesises each calc operand; css-backend-lass keeps its output exactly.")
  (note "A string value is copied as is.")
  (note "outside calc the same operators evaluate: (- 100% 2rem) => ~s"
        (handler-case (lass-value '(- 100% 2rem))
          (css:css-invalid-value (c) (format nil "css-invalid-value ~s (rem is no unit of the arithmetic)"
                                             (css:css-error-input c)))))
  (stage "in the page (lass/parts/layout.lass) and in engels (forms.lass)")
  (note "~a" (mini (compile-in-page-scope
                    (first (nth 4 (lass-forms-with-text (lass-path "parts/layout.lass")))))))
  (let ((rule (find-if (lambda (n) (and (typep n 'css:rule)
                                        (string= "select:not([size]):not([multiple])" (rule-label n))))
                       (css:stylesheet-rules (engels:stylesheet)))))
    (note "~a" (mini (css:make-stylesheet (list rule))))))

;;; ---------------------------------------------------------------------------
;;; Demo 10: an invalid value, use-value and skip-invalid.

(defparameter *bad-lass*
  '((button :color "white"
            :background (darken "tomato" 10%)
            :border-radius 4px)))

(defun demo-lass-restarts ()
  (banner "Demo 10: an invalid LASS value — css-invalid-value, use-value, skip-invalid")
  (stage "source: ~a" (src (first *bad-lass*)))
  (note "colour names are not parsed (only #hex, rgb(), rgba()), so darken refuses \"tomato\"")
  (handler-case (css-backend-lass:compile-lass *bad-lass*)
    (css:css-syntax-error (c)
      (report-condition c)))
  (let ((restarts nil))
    (handler-bind ((css:css-invalid-value
                     (lambda (c)
                       (setf restarts (loop for r in (compute-restarts c)
                                            while (member (restart-name r) '(use-value css:skip-invalid))
                                            collect (list (restart-name r) (princ-to-string r))))
                       (css:skip-invalid c))))
      (css-backend-lass:compile-lass *bad-lass*))
    (note "restarts offered at the signal:")
    (loop for (name text) in restarts do (note "  ~s — ~a" name text)))
  (stage "use-value \"#E5533A\": the replacement text stands for the whole call")
  (let ((sheet (handler-bind ((css:css-invalid-value
                                (lambda (c) (use-value "#E5533A" c))))
                 (css-backend-lass:compile-lass *bad-lass*))))
    (note "~a" (mini sheet)))
  (stage "skip-invalid: the declaration that used the value is dropped, the rule stays")
  (let ((sheet (handler-bind ((css:css-invalid-value #'css:skip-invalid))
                 (css-backend-lass:compile-lass *bad-lass*))))
    (note "~a" (mini sheet)))
  (stage "the same restarts for an unbound #(name) and for a value css-protocol refuses")
  (handler-bind ((css:css-syntax-error
                   (lambda (c)
                     (note "skipping ~s: ~s" (type-of c) (css:css-error-input c))
                     (css:skip-invalid c))))
    (note "result: ~a"
          (mini (css-backend-lass:compile-lass
                 '((p :color #(no-such-token) :margin 0)
                   (em :content "a" :color "red;}body{x:y" :font-style italic)))))))

;;; ---------------------------------------------------------------------------
;;; Demo 11: engels + the page, pruned; page-lass.html.

(defun demo-lass-page ()
  (banner "Demo 11: engels + LASS page rules, pruned — page-lass.html")
  (ensure-lass-state)
  (let* ((engels (engels:stylesheet))
         (style (pretty *layered-used*))
         (tree (lass-page-tree :style style :sizes *lass-sizes*))
         (html (format nil "~a~%" (html:serialize tree)))
         (path (asdf:system-relative-pathname "harness-demos-css" "page-lass.html")))
    (stage "layering: (merge-stylesheets (engels:stylesheet) page-sheet), then used-rules")
    (note "engels first (its :root theme, Marx's element rules, the canvas vocabulary),")
    (note "then the page's --page-* theme and rules, so on equal specificity the page wins.")
    (show "engels top-level nodes" (length (css:stylesheet-rules engels)))
    (show "page top-level nodes" (length (css:stylesheet-rules *page-lass*)))
    (show "layered = engels ++ page"
          (string= (mini *layered*) (concatenate 'string (mini engels) (mini *page-lass*))))
    (stage "sizes (bytes, UTF-8)")
    (flet ((row (label sheet)
             (note "~34a ~4d rules  pretty ~7:d  minified ~7:d" label (count-style-rules sheet)
                   (utf-8-length (pretty sheet)) (utf-8-length (mini sheet)))))
      (row "engels 0.2.0 in full" engels)
      (row "page rules (LASS) alone" *page-lass*)
      (row "engels + page" *layered*)
      (row "engels + page, used-rules" *layered-used*))
    (note "(engels:css-octets): ~:d bytes = css/engels.css" (length (engels:css-octets)))
    (destructuring-bind (&key full-mini used-mini &allow-other-keys) *lass-sizes*
      (note "pruning keeps ~d% of the minified bytes" (percent used-mini full-mini)))
    (stage "what pruning dropped (top level)")
    (let ((kept (css:stylesheet-rules *layered-used*)))
      (dolist (n (css:stylesheet-rules *layered*))
        (when (and (typep n 'css:rule) (not (member n kept :test #'eq)))
          (note "  ~a" (rule-label n)))))
    (stage "checks")
    (show "used-rules on the final page = the inlined rules"
          (string= (mini *layered-used*) (mini (css:used-rules *layered* tree))))
    (show "engels' theme kept (:root, @media dark, data-theme)"
          (and (find ":root" (css:stylesheet-rules *layered-used*) :key #'rule-label :test #'string=)
               (find ":root[data-theme=\"dark\"]" (css:stylesheet-rules *layered-used*)
                     :key #'rule-label :test #'string=)
               t))
    (show "class attributes in the page"
          (count-if (lambda (e) (html:element-attr e "class")) (page-elements tree)))
    (show "no external URLs" (no-external-urls-p html))
    (show "no </ inside <style>" (null (search "</" style)))
    (write-file path html)
    (note "wrote harness/css/page-lass.html: ~:d bytes (style ~:d bytes, ~d lines)"
          (utf-8-length html) (utf-8-length style) (count #\Newline style))
    (stage "page-lass.html, first 24 lines")
    (block-text html :max-lines 24)))
