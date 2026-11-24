(in-package #:harness-demos-engrave)

;;;; A tour of engrave-protocol 0.1.0 (slice EN0): a score-protocol score, or
;;;; one of its views, becomes SVG pages whose element ids lead back to the
;;;; score, a timemap for playback cursors, and PDF through a printer. The
;;;; rendering happens in an engine behind the protocol. No native engine is
;;;; published yet (the first is planned as a separate backend), so every
;;;; section uses the reference NULL-ENGINE, which draws one placeholder group
;;;; per element and no music. Everything the protocol promises around the
;;;; engine is real: engines and their availability (1), layouts (2), pages
;;;; and their SVG (3), the id map (4), page splitting and system breaks (5),
;;;; views (6), unsupported notation (7), the conformance checks (8), the
;;;; timemap (9) and the PDF path (10). The score is the first line of OLD
;;;; HUNDREDTH in four parts (melody by Louis Bourgeois, 1551; words by William
;;;; Kethe, 1561; the lower parts are written for the demo). Every result with
;;;; a known answer is checked: RUN-DEMOS returns NIL when one differs or a
;;;; section stops on an error.

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  "Print LINES of prose, then a blank line."
  (format t "~&~{~a~%~}~%" lines))

(defun out (control &rest args)
  "FORMAT to *STANDARD-OUTPUT* with the padding at the end of each line removed."
  (let ((text (apply #'format nil control args)))
    (with-input-from-string (in text)
      (loop for (line missing-newline-p) = (multiple-value-list (read-line in nil))
            while line
            do (write-string (string-right-trim " " line))
               (unless missing-newline-p (terpri))))))

(defun code-text (object &key (margin 1000))
  "OBJECT printed as it reads in this package."
  (let ((*package* (find-package '#:harness-demos-engrave))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* margin))
    (prin1-to-string object)))

(defun indent (text columns)
  "TEXT with COLUMNS spaces added after each newline."
  (with-output-to-string (s)
    (loop for char across text
          do (write-char char s)
             (when (char= char #\Newline)
               (dotimes (i columns) (write-char #\Space s))))))

(defun same-p (a b)
  "EQUALP, except that strings compare case-sensitively."
  (typecase a
    (string (and (stringp b) (string= a b)))
    (cons (and (consp b) (same-p (car a) (car b)) (same-p (cdr a) (cdr b))))
    (t (equalp a b))))

(defun check (value expected what)
  "Count a check of VALUE against EXPECTED; print a line only when they differ."
  (incf *checks*)
  (unless (same-p value expected)
    (incf *mismatches*)
    (format t "~&  !! ~a: got ~a, expected ~a~%" what (code-text value) (code-text expected)))
  value)

(defun print-result (form value &optional note)
  "Print FORM => VALUE, on two lines when one is too long."
  (let ((ftext (indent (let ((line (code-text form)))
                         (if (> (length line) 82) (code-text form :margin 84) line))
                       2))
        (vtext (if note
                   (format nil "~a   ; ~a" (code-text value) note)
                   (code-text value))))
    (if (and (<= (+ 2 (length ftext) 4 (length vtext)) 84)
             (not (find #\Newline ftext)))
        (format t "~&  ~a => ~a~%" ftext vtext)
        (format t "~&  ~a~%    => ~a~%" ftext (indent vtext 7)))))

(defmacro show (form &optional (expected nil expected-p) note)
  "Print FORM and its value like a REPL transcript; check it against EXPECTED."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (print-result ',form ,value ,note)
       ,@(when expected-p `((check ,value ,expected (code-text ',form))))
       ,value)))

(defmacro shown (form)
  "Print FORM as code, then evaluate it. Its value is not printed."
  `(progn (format t "~&  ~a~%~%" (indent (code-text ',form :margin 96) 2))
          ,form))

(defun strip-identities (text)
  "TEXT without the \" {address}\" parts of unreadable objects."
  (with-output-to-string (s)
    (loop with i = 0
          while (< i (length text))
          do (let ((close (and (char= (char text i) #\Space)
                               (< (+ i 2) (length text))
                               (char= (char text (1+ i)) #\{)
                               (position #\} text :start i))))
               (if (and close (> close (+ i 2))
                        (every (lambda (c) (digit-char-p c 16)) (subseq text (+ i 2) close)))
                   (setf i (1+ close))
                   (progn (write-char (char text i) s) (incf i)))))))

(defun condition-line (c)
  "The type of the condition C and its report, on one line."
  (let* ((*package* (find-package '#:harness-demos-engrave))
         (*print-case* :downcase)
         (report (strip-identities (princ-to-string c))))
    (format nil "~(~a~): ~{~a~^ ~}" (type-of c)
            (remove "" (uiop:split-string report :separator '(#\Space #\Newline #\Tab))
                    :test #'string=))))

(defmacro show-signal (form type &key (checked t))
  "Print FORM and the condition of TYPE it signals. When CHECKED, count a
mismatch if it returns instead."
  `(handler-case (progn ,form
                        (format t "~&  ~a  (no condition)~%" (code-text ',form))
                        ,@(when checked `((check :returned :signalled (code-text ',form)))))
     (,type (c)
       (format t "~&  ~a~%    -> ~a~%" (indent (code-text ',form :margin 84) 2)
               (condition-line c)))))

(defmacro section (title &body body)
  "Print a banner, run BODY, and record the section as failed if it signals."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (e)
         (push ,title *failed-sections*)
         (format t "~&!! unexpected ~s, the rest of this section is skipped:~%   ~a~%"
                 (type-of e) e)))))

;;; The score -------------------------------------------------------------------
;;; OLD HUNDREDTH, the first line ("All people that on earth do dwell"), in G
;;; major and 4/4 with a half-note pickup, as an open score of four parts with
;;; one staff each. The soprano carries the words. Moderato, a quarter note =
;;; 96; the last chord, at the second half of bar 2, is slower (a quarter =
;;; 72) and has a fermata in every part.

(defparameter *parts*
  '(("P1" "Soprano" "S." :treble     (("G4" 1/2)) (("G4" 1/4) ("F#4" 1/4) ("E4" 1/4) ("D4" 1/4)) (("G4" 1/4) ("A4" 1/4) ("B4" 1/2)))
    ("P2" "Alto"    "A." :treble     (("D4" 1/2)) (("D4" 1/4) ("D4" 1/4) ("B3" 1/4) ("B3" 1/4))  (("D4" 1/4) ("D4" 1/4) ("D4" 1/2)))
    ("P3" "Tenor"   "T." :treble-8vb (("B3" 1/2)) (("B3" 1/4) ("A3" 1/4) ("G3" 1/4) ("G3" 1/4))  (("G3" 1/4) ("F#3" 1/4) ("G3" 1/2)))
    ("P4" "Bass"    "B." :bass       (("G2" 1/2)) (("B2" 1/4) ("D3" 1/4) ("E3" 1/4) ("G2" 1/4))  (("B2" 1/4) ("D3" 1/4) ("G2" 1/2))))
  "Part id, name, abbreviation, clef, then the bars from the pickup.")

(defparameter *words* '(("All" :single) ("peo" :begin) ("ple" :end) ("that" :single) ("on" :single)
                        ("earth" :single) ("do" :single) ("dwell." :single))
  "The soprano's syllables, one per note (William Kethe, 1561).")

(defun make-chorale (&key repeat)
  "The first line of OLD HUNDREDTH in four parts. With REPEAT, the line is
sung twice: a repeat from the pickup to the final bar."
  (let ((words *words*))
    (make-score
     :metadata '(:title "All people that on earth do dwell" :subtitle "OLD HUNDREDTH"
                 :composer "Louis Bourgeois, 1551" :lyricist "William Kethe, 1561")
     :measures (list (make-measure-info
                      :time (parse-meter "4/4") :key (parse-key "G major") :pickup-p t :length 1/2
                      :repeat-start repeat
                      :tempo (list (make-tempo-mark :tempo (make-tempo :bpm 96 :beat-unit 1/4)
                                                    :text "Moderato")))
                     (make-measure-info)
                     (make-measure-info
                      :barline :final :repeat-end (and repeat 2)
                      :tempo (list (make-tempo-mark :offset 1/2 :tempo (make-tempo :bpm 72 :beat-unit 1/4)
                                                    :text "rit."))))
     :parts (loop for (id name abbreviation clef . bars) in *parts*
                  collect (make-part
                           :id id :name name :abbreviation abbreviation
                           :staves (list (make-staff :clef clef))
                           :voices (list (make-voice
                                          :name name :staff 1
                                          :measures (loop for bar in bars
                                                          for b from 0
                                                          collect (loop for (pitch duration) in bar
                                                                        for j from 0
                                                                        collect (chorale-note pitch duration
                                                                                              (and (= b 2) (= j 2))
                                                                                              (and (string= id "P1") (pop words))))))))))))

(defun chorale-note (pitch duration last word)
  "A note of the chorale: a fermata on the LAST chord, the soprano's WORD (TEXT SYLLABIC)."
  (make-note-event :pitch (parse-pitch pitch) :duration duration
                   :fermata (and last (make-fermata))
                   :lyrics (and word (list (make-lyric :text (first word) :syllabic (second word))))))

(defvar *score* nil "The chorale.")

;;; Reading a score and its pages --------------------------------------------------

(defun parts (score) (fset:convert 'list (score-parts score)))
(defun measures (score) (fset:convert 'list (score-measure-map score)))
(defun measure-ids (score) (mapcar #'element-id (measures score)))

(defun voice-events (voice)
  (loop for events in (fset:convert 'list (voice-measures voice)) append (fset:convert 'list events)))

(defun part-events (score n)
  "The events of part N (from 0) of SCORE, in order."
  (voice-events (first (part-voices (nth n (parts score))))))

(defun first-of-kind (score class)
  (find-if (lambda (e) (typep e class)) (score-elements score)))

(defun event-label (score id)
  "The event ID as PART-ABBREVIATION:PITCH, e.g. S:G4."
  (let* ((event (find-element score id))
         (part (loop for p in (parts score)
                     when (member event (voice-events (first (part-voices p))))
                       return p)))
    (format nil "~a~a" (string-right-trim "." (or (and part (part-abbreviation part)) "?"))
            (if (typep event 'note-event) (pitch-name (note-pitch event)) "r"))))

(defun map-keys (map)
  "The keys of the FSet map MAP, sorted with STRING<."
  (let ((keys '()))
    (fset:do-map (k v map) (declare (ignore v)) (push k keys))
    (sort keys #'string<)))

(defun lookup (map key) (values (fset:lookup map key)))

(defun encode-page (page &key (pretty nil))
  "PAGE's SVG document as text."
  (xml:encode (svg-page-document page) :pretty pretty))

(defun attr (element name)
  (let ((a (find name (xml:xml-element-attributes element)
                 :key #'xml:xml-attribute-local-name :test #'string=)))
    (and a (xml:xml-attribute-value a))))

(defun child-elements (element)
  (remove-if-not #'xml:xml-element-p (xml:xml-element-children element)))

(defun print-tree (element &key (depth 0) (max-depth 99) (limit most-positive-fixnum))
  "ELEMENT and the elements under it, one per line: name, id, class and other
attributes. Prints at most LIMIT lines; returns how many were printed."
  (let ((printed 0))
    (labels ((walk (e d)
               (when (< printed limit)
                 (incf printed)
                 (out "  ~a<~a~{ ~a~}>~%" (make-string (* 2 d) :initial-element #\Space) (xml:xml-element-local-name e)
                      (loop for a in (xml:xml-element-attributes e)
                            unless (string= (xml:xml-attribute-local-name a) "xmlns")
                              collect (format nil "~a=~s" (xml:xml-attribute-local-name a)
                                              (xml:xml-attribute-value a))))
                 (let ((text (find-if #'xml:xml-text-p (xml:xml-element-children e))))
                   (when text
                     (out "  ~a  ~s~%" (make-string (* 2 d) :initial-element #\Space) (xml:xml-text-data text))))
                 (when (< d max-depth)
                   (dolist (c (child-elements e)) (walk c (1+ d)))))))
      (walk element depth))
    printed))

(defun count-elements (element)
  (1+ (reduce #'+ (mapcar #'count-elements (child-elements element)))))

(defun page-summary (pages)
  "Each page: its number, the systems with their measure ids, and the count of SVG elements."
  (dolist (page pages)
    (out "  page ~d  ~d SVG elements  systems ~{~a~^ ~}~%" (svg-page-number page)
         (count-elements (svg-page-root page))
         (mapcar (lambda (system) (format nil "~a" system)) (svg-page-systems page)))))

;;; 1. Engines -----------------------------------------------------------------------

(defclass unloaded-engine (engrave-engine) ()
  (:documentation "A stand-in for an engine over a native library that did not
load: ENGINE-AVAILABLE-P is false, so it never renders."))

(defmethod engine-name ((engine unloaded-engine)) "unloaded")
(defmethod engine-available-p ((engine unloaded-engine)) nil)

(defun demo-engines ()
  (para "An engine is an instance of a subclass of ENGRAVE-ENGINE. It implements one"
        "generic function, ENGINE-RENDER-PAGES; RENDER-SVG, RENDER-TIMEMAP and RENDER-PDF"
        "are the protocol's methods around it, so every engine gets the same checks,"
        "views and id translation. Real engines live in backends over native libraries."
        "None is published yet, so this demo uses NULL-ENGINE, the reference engine:"
        "deterministic placeholder SVG, one group per element, no music layout. It is"
        "what consumers and backends test the protocol with.")
  (let ((engine (make-null-engine)))
    (show (engine-name engine) "null")
    (show (engine-available-p engine) t)
    (show (eq (ensure-engine engine) engine) t "an available engine is returned as it is")
    (show (null-engine-unsupported engine) nil "it draws every element"))
  (format t "~%")
  (para "*ENGRAVE-ENGINE* is the engine a render function uses when it is given NIL."
        "It is NIL by default, and no backend sets it when loaded. ENSURE-ENGINE then"
        "signals ENGINE-UNAVAILABLE inside a USE-VALUE restart, whose value is checked"
        "the same way:")
  (show *engrave-engine* nil)
  (show-signal (ensure-engine nil) engine-unavailable)
  (show (engine-name (handler-bind ((engine-unavailable
                                      (lambda (c) (use-value (make-null-engine) c))))
                       (ensure-engine nil)))
        "null" "USE-VALUE with a null engine")
  (show (let ((*engrave-engine* (make-null-engine)))
          (engine-name (ensure-engine nil)))
        "null" "bound around the call")
  (format t "~%")
  (para "An engine over a native library whose library did not load answers NIL to"
        "ENGINE-AVAILABLE-P. UNLOADED-ENGINE (demo.lisp) plays one: rendering with it"
        "signals ENGINE-UNAVAILABLE before anything is drawn, and USE-VALUE swaps in"
        "another engine for the whole call:")
  (show-signal (ensure-engine (make-instance 'unloaded-engine)) engine-unavailable)
  (show-signal (render-svg (make-instance 'unloaded-engine) *score*) engine-unavailable)
  (show (length (handler-bind ((engine-unavailable (lambda (c) (use-value (make-null-engine) c))))
                  (render-svg (make-instance 'unloaded-engine) *score*)))
        1 "pages, from the null engine")
  (show-signal (ensure-engine "verovio") type-error))

;;; 2. Layout ------------------------------------------------------------------------

(defun demo-layout ()
  (para "A LAYOUT holds the page size, the margins (millimetres, whole tenths), the"
        "scale in percent and the system breaks. MAKE-LAYOUT checks each argument;"
        "ENSURE-LAYOUT takes NIL, a layout or a plist:")
  (let ((layout (make-layout)))
    (show (list (layout-page-size layout) (layout-margins layout) (layout-scale layout)
                (layout-system-breaks layout))
          '((210 297) (10 10 10 10) 100 :auto) "the defaults: A4"))
  (show (mapcar #'first +page-sizes+) '(:a3 :a4 :a5 :b4 :b5 :letter :legal :tabloid))
  (show (layout-page-size (make-layout :page-size :letter)) '(2159/10 2794/10) "215.9 x 279.4 mm")
  (show (layout-margins (make-layout :margins '(15 10 15 10))) '(15 10 15 10) "top right bottom left")
  (show (layout-scale (ensure-layout '(:page-size :a5 :scale 200))) 200)
  (format t "~%")
  (para "Each refusal is an INVALID-RENDER-OPTION naming the option and the value:")
  (show-signal (make-layout :page-size :quarto) invalid-render-option)
  (show-signal (make-layout :page-size '(210 1/3)) invalid-render-option)
  (show-signal (make-layout :margins 10.5) invalid-render-option)
  (show-signal (make-layout :margins '(10 110 10 100)) invalid-render-option)
  (show-signal (make-layout :scale 0) invalid-render-option)
  (show-signal (make-layout :system-breaks '("m-1" "m-1")) invalid-render-option)
  (show-signal (ensure-layout '(:scale)) invalid-render-option)
  (show-signal (ensure-layout '(:colour :blue)) invalid-render-option))

;;; 3. Pages ---------------------------------------------------------------------------

(defvar *pages* nil "The choral rendering of the score, on A4.")
(defvar *id-map* nil "Its id map.")

(defun svg-path ()
  (asdf:system-relative-pathname "harness-demos-engrave" "old-hundredth.svg"))

(defun demo-pages ()
  (para "The score: the first line of OLD HUNDREDTH (\"All people that on earth do"
        "dwell\") as an open score of four parts, one staff each, built in code"
        "(MAKE-CHORALE). G major, 4/4, a half-note pickup, Moderato with a quarter"
        "note at 96; the last chord is slower (\"rit.\", a quarter at 72) and has a"
        "fermata in every part. The soprano carries the words.")
  (dolist (part (parts *score*))
    (out "  ~a ~8a ~{~a~^ ~}~%" (element-id part) (part-name part)
         (mapcar (lambda (e) (format nil "~a:~a" (pitch-name (note-pitch e))
                                     (case (event-duration e) (1/4 "q") (1/2 "h"))))
                 (voice-events (first (part-voices part))))))
  (format t "~%")
  (show (length (score-elements *score*)) 61 "3 measures, 2 tempo marks, 4 parts, staves and voices, 32 notes, 4 fermatas, 8 lyrics")
  (show (check-score *score*) nil)
  (format t "~%")
  (para "RENDER-SVG engine score &key view page layout parts returns the pages and the"
        "id map. With the default layout (A4, 10 mm margins, scale 100) the null engine"
        "fits four 40 mm measures in a 190 mm line, and six systems of four staves on a"
        "page, so the three bars make one system on one page:")
  (multiple-value-bind (pages id-map) (render-svg (make-null-engine) *score*)
    (setf *pages* pages *id-map* id-map)
    (show (length pages) 1)
    (let ((page (first pages)))
      (show (svg-page-number page) 1)
      (show (svg-page-systems page) (list (measure-ids *score*)) "systems: measure ids")
      (show (xml:xml-document-p (svg-page-document page)) t "an xml-protocol tree, not a string")
      (show (xml:xml-element-namespace-uri (svg-page-root page)) +svg-namespace+)
      (show (list (attr (svg-page-root page) "viewBox") (attr (svg-page-root page) "width")
                  (attr (svg-page-root page) "height"))
            '("0 0 2100 2970" "210mm" "297mm") "user units of 1/10 mm")
      (format t "~%")
      (para "The page as a tree (PRINT-TREE in demo.lisp; the first 40 of its elements)."
            "Page 1 opens with the parts, each with its name, staff and voice; then each"
            "system, holding its measures, and in each measure its marks and the events"
            "of every voice, each event holding its marks and lyrics:")
      (print-tree (svg-page-root page) :limit 40)
      (out "  ...~%~%")
      (show (count-elements (svg-page-root page)) 71
            "the root, the parts group, a system, 61 element groups, 4 name texts, 3 rects")
      (let ((ids (svg-page-ids page)))
        (show (length ids) 62 "id attributes: 61 elements and system.1")
        (show (length (remove-duplicates ids :test #'string=)) 62 "all unique")
        (show (every #'valid-svg-id-p ids) t "all ASCII NCNames")
        (show (find "system.1" ids :test #'string=) "system.1"
              "an id the engine made up: no element id has a dot"))
      (let* ((note (first (part-events *score* 0)))
             (group (find-svg-element page (element-id note))))
        (show (list (xml:xml-element-local-name group) (attr group "id") (attr group "class"))
              (list "g" (element-id note) "note") "the soprano's first note: its own id")
        (show (mapcar (lambda (c) (attr c "class")) (child-elements group)) '("lyric")
              "the note's group holds its lyric's"))
      (show (find-svg-element page "e-zz") nil)
      (format t "~%")
      (para "ELEMENT-KIND is the class attribute each group gets:")
      (show (mapcar (lambda (class) (element-kind (first-of-kind *score* class)))
                    '(measure-info tempo-mark part staff voice note-event fermata lyric))
            '(:measure :tempo-mark :part :staff :voice :note :fermata :lyric))
      (format t "~%")
      (para "The same score and layout give the same bytes, and the page is written to"
            "old-hundredth.svg next to this demo:")
      (show (string= (encode-page page) (encode-page (first (render-svg (make-null-engine) *score*))))
            t)
      (with-open-file (s (svg-path) :direction :output :if-exists :supersede
                                    :external-format :utf-8)
        (write-string (encode-page page :pretty t) s))
      (show (subseq (encode-page page) 0 38) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      (show (and (search "<svg xmlns=\"http://www.w3.org/2000/svg\"" (encode-page page)) t) t
            "a standalone SVG document"))))

;;; 4. The id map ------------------------------------------------------------------------

(defun demo-id-map ()
  (para "The id map is an FSet map from the score's element ids to lists of"
        "(page . svg-id): where each element is drawn. An editor or a canvas uses it"
        "to go from a click on the page to the score element, and back. Every element"
        "of the choral view is a key; the null engine draws each one once, under its"
        "own id:")
  (show (fset:size *id-map*) 61)
  (show (equal (map-keys *id-map*) (sort (mapcar #'element-id (score-elements *score*)) #'string<)) t
        "every element of the score")
  (let ((s (first (part-events *score* 0)))
        (b (car (last (part-events *score* 3)))))
    (out "  id      kind         drawn at~%")
    (dolist (element (list (first (measures *score*)) (first-of-kind *score* 'tempo-mark)
                           (first (parts *score*)) (first-of-kind *score* 'staff)
                           (first-of-kind *score* 'voice) s (first (event-lyrics s))
                           b (event-fermata b)))
      (out "  ~7a ~12a ~a~%" (element-id element) (string-downcase (element-kind element))
           (code-text (lookup *id-map* (element-id element)))))
    (format t "~%")
    (show (every (lambda (id) (equal (lookup *id-map* id) (list (cons 1 id))))
                 (map-keys *id-map*))
          t "each value is ((1 . its own id))")))

;;; 5. Pages and system breaks -------------------------------------------------------------

(defun demo-breaks ()
  (para "A larger scale on a smaller page splits the line. On A5 at scale 200 a measure"
        "is 80 mm wide, so one fits a 128 mm line; a system of four staves is 90 mm"
        "high, so two fit a 190 mm page. Three systems make two pages:")
  (multiple-value-bind (pages id-map) (render-svg (make-null-engine) *score*
                                                  :layout '(:page-size :a5 :scale 200))
    (page-summary pages)
    (format t "~%")
    (show (mapcar #'svg-page-number pages) '(1 2))
    (show (reduce #'append (mapcar #'svg-page-systems pages)) (mapcar #'list (measure-ids *score*))
          "every measure once, in order")
    (let ((last-bar (third (measures *score*)))
          (last-note (car (last (part-events *score* 0)))))
      (show (lookup id-map (element-id last-bar)) (list (cons 2 (element-id last-bar))) "bar 2 is on page 2")
      (show (lookup id-map (element-id last-note)) (list (cons 2 (element-id last-note))))
      (show (lookup id-map "P1") '((1 . "P1")) "parts, staves and voices are drawn on page 1"))
    (format t "~%")
    (para ":PAGE asks for one page. The id map still covers the whole rendering, and"
          "the page is the same as in the full rendering:")
    (multiple-value-bind (one one-map) (render-svg (make-null-engine) *score*
                                                   :layout '(:page-size :a5 :scale 200) :page 2)
      (show (mapcar #'svg-page-number one) '(2))
      (show (string= (encode-page (first one)) (encode-page (second pages))) t)
      (show (fset:equal? one-map id-map) t))
    (show-signal (render-svg (make-null-engine) *score* :layout '(:page-size :a5 :scale 200) :page 3)
                 invalid-render-option)
    (show-signal (render-svg (make-null-engine) *score* :page 0) invalid-render-option))
  (format t "~%")
  (para ":SYSTEM-BREAKS is a list of measure ids: a system starts at each of them (and"
        "at the first measure). A page's SYSTEMS are the break plan: fed back, they give"
        "the same lines again, whatever the scale.")
  (let* ((ids (measure-ids *score*))
         (pages (render-svg (make-null-engine) *score*
                            :layout (list :system-breaks (list (second ids))))))
    (page-summary pages)
    (show (svg-page-systems (first pages)) (list (list (first ids)) (rest ids))
          "the pickup alone, then bars 1-2")
    (let* ((plan (reduce #'append (mapcar #'svg-page-systems pages)))
           (breaks (mapcar #'first (rest plan)))
           (again (render-svg (make-null-engine) *score*
                              :layout (list :system-breaks breaks))))
      (show breaks (list (second ids)))
      (show (equal (mapcar #'encode-page again) (mapcar #'encode-page pages)) t "the same bytes")))
  (show-signal (render-svg (make-null-engine) *score* :layout '(:system-breaks ("m-zz")))
               invalid-render-option))

;;; 6. Views ---------------------------------------------------------------------------

(defun demo-views ()
  (para "Views are score-protocol's. :CHORAL draws the score as notated (here four"
        "staves); :CONDENSED draws CONDENSED-VIEW, the hymnal reduction: soprano and"
        "alto on a treble staff, tenor and bass on a bass staff. The engine only ever"
        "sees the view; RENDER-SVG turns the view's ids back into the score's.")
  (multiple-value-bind (view view-map) (engrave-view *score* :view :condensed)
    (show (length (parts view)) 1)
    (show (mapcar #'staff-clef (part-staves (first (parts view)))) '(:treble :bass))
    (show (mapcar #'voice-stem (part-voices (first (parts view)))) '(:up :down :up :down))
    (show (lookup view-map (element-id (first (parts view)))) nil "the view's part is new")
    (multiple-value-bind (pages id-map) (render-svg (make-null-engine) *score* :view :condensed)
      (page-summary pages)
      (show (fset:size id-map) 53 "61 elements less the 4 parts and 4 staves")
      (show (lookup id-map "P1") nil "the source parts are not drawn")
      (let ((alto-note (first (part-events *score* 1))))
        (show (lookup id-map (element-id alto-note)) (list (cons 1 (element-id alto-note)))
              "the alto's first note, under its own id"))
      (show (check-engraving *score* pages id-map :view :condensed) nil)))
  (format t "~%")
  (para ":PARTS restricts the view to some parts (by id), here the outer voices:")
  (multiple-value-bind (pages id-map) (render-svg (make-null-engine) *score* :parts '("P1" "P4"))
    (page-summary pages)
    (show (fset:size id-map) 37 "5 in the measure map, 2 parts with their staves and voices, 16 notes, 2 fermatas, 8 lyrics")
    (show (lookup id-map "P2") nil)
    (show (check-engraving *score* pages id-map :parts '("P1" "P4")) nil))
  (show-signal (render-svg (make-null-engine) *score* :view :piano) invalid-render-option)
  (show-signal (render-svg (make-null-engine) *score* :parts '("P5")) invalid-render-option))

;;; 7. Unsupported notation --------------------------------------------------------------

(defun demo-unsupported ()
  (para "An engine that can't draw an element signals UNSUPPORTED-NOTATION inside a"
        "SKIP-ELEMENT restart (score-protocol's symbol, the one its MusicXML reader"
        "uses, so one handler serves both). MAKE-NULL-ENGINE :UNSUPPORTED names classes"
        "to refuse. Here fermatas and tempo marks: the conditions come in document"
        "order, and skipping draws the score without them.")
  (let ((engine (make-null-engine :unsupported '(fermata tempo-mark)))
        (seen '()))
    (show-signal (render-svg engine *score*) unsupported-notation)
    (multiple-value-bind (pages id-map)
        (handler-bind ((unsupported-notation
                         (lambda (c)
                           (push (list (unsupported-notation-element-id c) (unsupported-notation-kind c)) seen)
                           (invoke-restart 'skip-element))))
          (render-svg engine *score*))
      (out "  skipped, in order: ~{~a~^, ~}~%~%" (mapcar (lambda (s) (format nil "~a (~(~a~))" (first s) (second s)))
                                                     (reverse seen)))
      (show (length seen) 6 "two tempo marks, four fermatas")
      (let ((fermata (event-fermata (car (last (part-events *score* 0))))))
        (show (lookup id-map (element-id fermata)) nil "a skipped element maps to ()")
        (show (find-svg-element (first pages) (element-id fermata)) nil))
      (show (mapcar (lambda (i) (list (engraving-issue-kind i) (engraving-issue-id i)))
                    (check-engraving *score* pages id-map))
            (mapcar (lambda (s) (list :not-drawn (first s))) (reverse seen))
            "CHECK-ENGRAVING reports what was not drawn")))
  (format t "~%")
  (para "Skipping an element skips everything it holds, without a signal: refusing the"
        "VOICE class, the four voices are signalled and none of their 32 events, the"
        "soprano's 8 lyrics or the 4 fermatas is drawn:")
  (let ((count 0))
    (multiple-value-bind (pages id-map)
        (handler-bind ((unsupported-notation (lambda (c) (declare (ignore c)) (incf count) (skip-element))))
          (render-svg (make-null-engine :unsupported '(voice)) *score*))
      (declare (ignore pages))
      (show count 4)
      (show (count nil (mapcar (lambda (id) (lookup id-map id)) (map-keys id-map))) 48
            "4 voices + 32 events + 8 lyrics + 4 fermatas map to ()"))))

;;; 8. Conformance checks ------------------------------------------------------------------

(defun issue-list (issues)
  (mapcar (lambda (i) (list* (engraving-issue-kind i) (engraving-issue-id i) (engraving-issue-detail i)))
          issues))

(defun demo-checks ()
  (para "Engines are tested with conformance checks, not with golden SVG. CHECK-ENGRAVING"
        "says whether a rendering keeps the protocol's promises: pages numbered 1..n,"
        "SVG roots in the SVG namespace, valid and unique SVG ids, an id map with every"
        "expected element and nothing else, references to ids that exist, and systems"
        "covering every measure once. The null engine's renderings pass:")
  (show (check-engraving *score* *pages* *id-map*) nil)
  (multiple-value-bind (pages id-map) (render-svg (make-null-engine) *score* :layout '(:page-size :a5 :scale 200))
    (show (check-engraving *score* pages id-map) nil "the two-page rendering")
    (format t "~%")
    (para "Broken outputs, as a faulty engine might return them:")
    (let ((note (element-id (first (part-events *score* 0)))))
      (show (issue-list (check-engraving *score* (reverse pages) id-map))
            '((:page-number nil :index 0 :number 2)
              (:systems nil :expected ("m-1" "m-3" "m-4") :actual ("m-4" "m-1" "m-3")))
            "pages out of order")
      (show (issue-list (check-engraving *score* pages (fset:less id-map note)))
            (list (list :missing-element note))
            "an element missing from the map")
      (show (issue-list (check-engraving *score* pages (fset:with id-map "x-1" '((1 . "x-1")))))
            '((:unknown-element "x-1"))
            "a key that is no element of the score")
      (show (issue-list (check-engraving *score* pages (fset:with id-map note '((3 . "g-1")))))
            (list (list :dangling-ref note :ref '(3 . "g-1")))
            "a drawing on a page that does not exist"))))

;;; 9. Timemaps -------------------------------------------------------------------------------

(defun ms-text (ms)
  (if (integerp ms) (format nil "~d" ms) (format nil "~a (~,2f)" ms ms)))

(defun print-timemap (score timemap)
  (out "  onset ms          tempo  on                    off~%")
  (dolist (entry timemap)
    (out "  ~17a ~5a  ~21a ~{~a~^ ~}~%"
         (ms-text (timemap-entry-onset entry))
         (format nil "q=~d" (tempo-bpm (timemap-entry-tempo entry)))
         (format nil "~{~a~^ ~}" (mapcar (lambda (id) (event-label score id)) (timemap-entry-on entry)))
         (mapcar (lambda (id) (event-label score id)) (timemap-entry-off entry)))))

(defun demo-timemap ()
  (para "A timemap says when each event sounds, for a playback cursor: entries in"
        "onset order, each with the milliseconds from the start, the ids of the events"
        "that start and end there, and the tempo in force. It comes from the model"
        "(positions, durations, tempo marks), not from a layout, so every engine"
        "reports the same one. RENDER-TIMEMAP unfolds repeats by default. The line at a"
        "quarter = 96 is 625 ms a quarter; the last chord, at 72, is 833 1/3 ms a"
        "quarter. Fermatas add no time. Events are shown as part:pitch:")
  (let ((timemap (render-timemap (make-null-engine) *score*)))
    (print-timemap *score* timemap)
    (format t "~%")
    (show (length timemap) 9)
    (show (mapcar #'timemap-entry-onset timemap) '(0 1250 1875 2500 3125 3750 4375 5000 20000/3)
          "exact rationals")
    (show (timemap-entry-on (first timemap)) (mapcar (lambda (n) (element-id (first (part-events *score* n)))) '(0 1 2 3))
          "ids, in document order")
    (show (timemap-entry-off (car (last timemap))) (mapcar (lambda (n) (element-id (car (last (part-events *score* n))))) '(0 1 2 3)))
    (show (equalp (score-timemap *score*) timemap) t "no repeat: unfolding changes nothing")
    (show (check-timemap *score* timemap) nil))
  (format t "~%")
  (para "The same line with a repeat from the pickup to the final bar, so that it is"
        "sung twice. With :UNFOLD T (the default) every event appears once per pass,"
        "under its own id, and the timemap follows the playback order; with :UNFOLD NIL"
        "it is the score as written:")
  (let* ((twice (make-chorale :repeat t))
         (unfolded (render-timemap (make-null-engine) twice))
         (written (render-timemap (make-null-engine) twice :unfold nil))
         (s1 (element-id (first (part-events twice 0)))))
    (show (length written) 9)
    (show (length unfolded) 17 "the end of pass 1 is the start of pass 2: one entry")
    (let ((turn (nth 8 unfolded)))
      (show (mapcar (lambda (id) (event-label twice id)) (timemap-entry-off turn)) '("SB4" "AD4" "TG3" "BG2"))
      (show (mapcar (lambda (id) (event-label twice id)) (timemap-entry-on turn)) '("SG4" "AD4" "TB3" "BG2"))
      (show (ms-text (timemap-entry-onset turn)) "20000/3 (6666.67)")
      (show (tempo-bpm (timemap-entry-tempo turn)) 96 "the Moderato again, from the pickup's mark"))
    (show (count-if (lambda (e) (member s1 (timemap-entry-on e) :test #'string=)) unfolded) 2
          "the soprano's first note, on both passes")
    (show (ms-text (timemap-entry-onset (car (last unfolded)))) "40000/3 (13333.33)")
    (show (check-timemap twice unfolded) nil)
    (show (check-timemap twice written :unfold nil) nil)
    (format t "~%")
    (para "CHECK-TIMEMAP on broken timemaps: the written one checked as unfolded, two"
          "entries swapped, an id that is no timed event:")
    (show (length (check-timemap twice written)) 32 "every event once where twice is expected")
    (show (engraving-issue-detail (first (check-timemap twice written)))
          '(:expected 2 :on 1 :off 1))
    (let ((timemap (render-timemap (make-null-engine) *score*)))
      (show (mapcar #'engraving-issue-kind
                    (check-timemap *score* (list* (second timemap) (first timemap) (cddr timemap))))
            '(:onset-order :off-before-on :off-before-on :off-before-on :off-before-on))
      (let ((head (first timemap)))
        (show (issue-list (check-timemap *score* (cons (make-timemap-entry :onset 0 :on (append (timemap-entry-on head) (list "P1"))
                                                                           :off '() :tempo (timemap-entry-tempo head))
                                                       (rest timemap))))
              '((:unknown-event "P1" :entry 0)) "a part id among the first notes"))))
  (show-signal (funcall 'make-timemap-entry :onset -1 :on '() :off '()) type-error)
  (show-signal (score-timemap "a score") type-error))

;;; 10. PDF ------------------------------------------------------------------------------------

(defclass svg-text-printer () ()
  (:documentation "A printer that returns the pages' SVG text as octets: not a PDF."))

(defmethod print-pages ((printer svg-text-printer) pages layout)
  (declare (ignore layout))
  (babel:string-to-octets (format nil "~{~a~}" (mapcar #'encode-page pages)) :encoding :utf-8))

(defun demo-pdf ()
  (para "RENDER-PDF renders the SVG pages and hands them to a printer, an object with a"
        "PRINT-PAGES method; the protocol checks that what comes back is a PDF. EN0"
        "ships no printer (the browser printer is planned), and *PDF-PRINTER* is NIL."
        "Without a printer nothing is rendered: PRINTER-UNAVAILABLE, inside USE-VALUE.")
  (show *pdf-printer* nil)
  (show-signal (render-pdf (make-null-engine) *score*) printer-unavailable)
  (show (block nil
          (handler-bind ((printer-unavailable (lambda (c) (return (and (find-restart 'use-value c) t)))))
            (render-pdf (make-null-engine) *score*)))
        t "USE-VALUE is there while the handler runs")
  (format t "~%")
  (para "A printer that returns something other than a PDF (SVG-TEXT-PRINTER in"
        "demo.lisp returns the pages' SVG text) is refused with ENGINE-RENDER-ERROR:")
  (show-signal (render-pdf (make-null-engine) *score* :printer (make-instance 'svg-text-printer))
               engine-render-error)
  (show-signal (handler-bind ((printer-unavailable
                                (lambda (c) (use-value (make-instance 'svg-text-printer) c))))
                 (render-pdf (make-null-engine) *score*))
               engine-render-error))

;;; Running -------------------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()))
    (format t "~&engrave-protocol ~a (slice EN0) on score-protocol ~a and xml-protocol ~a~%"
            (asdf:component-version (asdf:find-system "engrave-protocol"))
            (asdf:component-version (asdf:find-system "score-protocol"))
            (asdf:component-version (asdf:find-system "xml-protocol")))
    (setf *score* (make-chorale))
    (section "1. Engines: the reference engine, availability, *ENGRAVE-ENGINE*" (demo-engines))
    (section "2. Layout: page size, margins, scale, system breaks" (demo-layout))
    (section "3. Pages: RENDER-SVG and the SVG it returns" (demo-pages))
    (section "4. The id map: score element ids to (page . svg-id)" (demo-id-map))
    (section "5. Page splitting, :PAGE and system breaks" (demo-breaks))
    (section "6. Views: :CHORAL, :CONDENSED and :PARTS" (demo-views))
    (section "7. Unsupported notation and SKIP-ELEMENT" (demo-unsupported))
    (section "8. Conformance: CHECK-ENGRAVING" (demo-checks))
    (section "9. Timemaps: RENDER-TIMEMAP, SCORE-TIMEMAP, CHECK-TIMEMAP" (demo-timemap))
    (section "10. PDF: RENDER-PDF and the printer protocol" (demo-pdf))
    (banner "Summary")
    (format t "~d results checked against expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
