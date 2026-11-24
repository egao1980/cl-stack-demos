(in-package #:harness-demos-musicxml)

;;;; A tour of score-backend-musicxml 0.1.0, slice SC2 of the score family:
;;;; MusicXML 4.0 partwise scores read into score-protocol scores and written
;;;; back, as text (.musicxml) or as a compressed archive (.mxl). Two
;;;; public-domain hymns, re-encoded by hand for this demo, are the input:
;;;; scores/new-britain.musicxml (NEW BRITAIN, "Amazing Grace": American folk
;;;; melody, 1829; words by John Newton, 1779) and
;;;; scores/old-hundredth-satb.musicxml (the first line of OLD HUNDREDTH: melody
;;;; by Louis Bourgeois, 1551; words by William Kethe, 1561; the lower parts
;;;; written for the demo). The sections cover reading (1), hymnal voices (2),
;;;; chord symbols (3), content outside the model (4), writing (5), an optional
;;;; schema check (6) and external entities (7). Every result with a known
;;;; answer is also checked: RUN-DEMOS returns NIL when one differs or a
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
  "OBJECT printed as it reads in this package, which uses SCORE-PROTOCOL and
SCORE-BACKEND-MUSICXML."
  (let ((*package* (find-package '#:harness-demos-musicxml))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* margin))
    (prin1-to-string object)))

(defun indent (text columns)
  "TEXT with COLUMNS spaces added after each newline, for printing it at that column."
  (with-output-to-string (s)
    (loop for char across text
          do (write-char char s)
             (when (char= char #\Newline)
               (dotimes (i columns) (write-char #\Space s))))))

(defun value-text (value)
  "VALUE as printed, with the name of a music-theory value after it."
  (let ((name (typecase value
                (music-theory:chord-symbol (ignore-errors (print-chord-symbol value)))
                (music-theory:spelled-pitch (pitch-name value)))))
    (if name
        (format nil "~a   ; ~a" (strip-identities (code-text value)) name)
        (strip-identities (code-text value)))))

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
    (format t "~&  !! ~a: got ~a, expected ~a~%" what (value-text value) (value-text expected)))
  value)

(defun print-result (form value &optional note)
  "Print FORM => VALUE, on two lines when one is too long. NOTE, when given, is
a comment after the value."
  (let ((ftext (indent (let ((line (code-text form)))
                         (if (> (length line) 82) (code-text form :margin 84) line))
                       2))
        (vtext (if note
                   (format nil "~a   ; ~a" (strip-identities (code-text value)) note)
                   (value-text value))))
    (if (and (<= (+ 2 (length ftext) 4 (length vtext)) 84)
             (not (find #\Newline ftext)))
        (format t "~&  ~a => ~a~%" ftext vtext)
        (format t "~&  ~a~%    => ~a~%" ftext (indent vtext 7)))))

(defmacro show (form &optional (expected nil expected-p) note)
  "Print FORM and its value like a REPL transcript; check the value against
EXPECTED when one is given. NOTE is a comment printed after the value."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (print-result ',form ,value ,note)
       ,@(when expected-p `((check ,value ,expected (code-text ',form))))
       ,value)))

(defmacro shown (form)
  "Print FORM as code, then evaluate it. Its value is not printed."
  `(progn (format t "~&  ~a~%" (indent (code-text ',form :margin 96) 2))
          ,form))

(defun strip-identities (text)
  "TEXT without the \" {address}\" parts that PRINT-UNREADABLE-OBJECT writes for
:IDENTITY T. Addresses differ from run to run; the rest of a report doesn't."
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
  "The type of the condition C and its report, on one line: runs of blanks and
newlines in the report become one space, object addresses and the run's temp
directory are left out."
  (let* ((*package* (find-package '#:harness-demos-musicxml))
         (*print-case* :downcase)
         (report (masked (strip-identities (princ-to-string c)))))
    (format nil "~(~a~): ~{~a~^ ~}"
            (let ((type (type-of c)))
              (if (eq (symbol-package type) (find-package '#:harness-demos-musicxml))
                  (symbol-name type)
                  (code-text type)))
            (remove "" (uiop:split-string report :separator '(#\Space #\Newline #\Tab))
                    :test #'string=))))

(defmacro show-signal (form type &key (checked t))
  "Print FORM and the condition of TYPE it signals, and return that condition.
When CHECKED, count a mismatch if it returns instead."
  `(handler-case (progn ,form
                        (format t "~&  ~a  (no condition)~%" (code-text ',form))
                        ,@(when checked `((check :returned :signalled (code-text ',form))))
                        nil)
     (,type (c)
       (format t "~&  ~a~%    -> ~a~%" (indent (code-text ',form :margin 84) 2)
               (condition-line c))
       c)))

(defun signals-of (thunk type)
  "(values result conditions): THUNK's value and every condition of TYPE it
signalled, in order. Warnings among them are muffled."
  (let ((seen '()))
    (values (handler-bind ((condition (lambda (c)
                                        (when (typep c type)
                                          (push c seen)
                                          (when (find-restart 'muffle-warning c)
                                            (muffle-warning c))))))
              (funcall thunk))
            (nreverse seen))))

(defmacro section (title &body body)
  "Print a banner, run BODY, and record the section as failed if it signals."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (e)
         (push ,title *failed-sections*)
         (format t "~&!! unexpected ~s, the rest of this section is skipped:~%   ~a~%"
                 (type-of e) (masked (princ-to-string e)))))))

;;; Scores and documents the sections share -----------------------------------

(defvar *hymn-text* nil "scores/new-britain.musicxml, as text.")
(defvar *hymn* nil "That file as READ-MUSICXML reads it (section 1).")
(defvar *as-written* nil "The SATB excerpt read without splitting (section 2).")
(defvar *satb* nil "The SATB excerpt read with :SPLIT-VOICES :AUTO (section 2).")
(defvar *trill* nil "The hymn with a trill (section 4).")
(defvar *skipped* nil "*TRILL* read with SKIP-ELEMENT (section 4).")
(defvar *written* nil "WRITE-MUSICXML of *HYMN* (section 5).")
(defvar *mxl* nil "WRITE-MXL of *HYMN* (section 5).")
(defvar *xxe* nil "A document with an external entity (section 7).")
(defvar *unused* nil "One that declares it and never uses it (section 7).")
(defvar *parameter* nil "One with an external parameter entity (section 7).")
(defvar *laughs* nil "The billion laughs (section 7).")

;;; Files ---------------------------------------------------------------------

(defvar *taken-ids* nil "The ids of *HYMN* that came from id attributes (section 1).")
(defvar *tmp* nil "This run's temp directory (printed as <tmp>).")

(defun score-file (name)
  "The pathname of scores/NAME in this demo's directory."
  (asdf:system-relative-pathname "harness-demos-musicxml" (format nil "scores/~a" name)))

(defun tmp-path (name)
  (merge-pathnames name *tmp*))

(defun slashed (string)
  (substitute #\/ #\\ string))

(defun replace-all (text old new)
  (with-output-to-string (s)
    (loop with start = 0
          for hit = (search old text :start2 start)
          do (write-string text s :start start :end (or hit (length text)))
             (if hit
                 (progn (write-string new s) (setf start (+ hit (length old))))
                 (loop-finish)))))

(defun masked (text)
  "TEXT with this run's temp directory written as <tmp>/."
  (if *tmp*
      (let ((dir (namestring *tmp*)))
        (replace-all (replace-all text dir "<tmp>/") (slashed dir) "<tmp>/"))
      text))

(defun file-text (path)
  "The text of the file PATH, read as UTF-8."
  (with-open-file (in path :external-format :utf-8)
    (let* ((s (make-string (file-length in)))
           (n (read-sequence s in)))
      (subseq s 0 n))))

(defun write-text (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out))
  path)

(defun text-lines (text)
  "The lines of TEXT, without a last empty line after a final newline."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (if (and lines (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun print-lines (text &key (from 1) to (numbers t))
  "Print lines FROM..TO (1-based, inclusive) of TEXT, indented, numbered when NUMBERS."
  (let ((lines (text-lines text)))
    (loop for line in lines
          for k from 1
          when (and (>= k from) (or (null to) (<= k to)))
            do (if numbers
                   (out "  ~3d  ~a~%" k (masked line))
                   (out "    ~a~%" (masked line))))))

(defun octets-text (octets)
  (babel:octets-to-string octets :encoding :utf-8))

;;; Reading a score -------------------------------------------------------------

(defun lst (x)
  "X as a list: score-protocol keeps some collections as FSet seqs."
  (if (listp x) x (fset:convert 'list x)))

(defun dur-name (d)
  (let ((row (assoc d '((1 "w") (3/4 "h.") (1/2 "h") (3/8 "q.") (1/4 "q") (1/8 "e") (1/16 "s")))))
    (if row (second row) (format nil "~a" d))))

(defun event-text (e)
  "A short spelling of the event E: G4:h, G4+D4:q, rest:q."
  (let ((d (dur-name (event-duration e))))
    (etypecase e
      (rest-event (format nil "rest:~a~:[~;(hidden)~]" d (rest-event-hidden-p e)))
      (note-event (format nil "~a:~a" (pitch-name (note-pitch e)) d))
      (chord-event (format nil "~{~a~^+~}:~a" (mapcar (lambda (n) (pitch-name (note-pitch n)))
                                                      (chord-event-notes e))
                           d)))))

(defun all-events (score)
  "(bar event voice) for every event of SCORE, voice by voice, in time order."
  (let ((first (score-first-bar score)))
    (loop for part in (lst (score-parts score))
          nconc (loop for voice in (part-voices part)
                      nconc (loop for events in (lst (voice-measures voice))
                                  for bar from first
                                  nconc (loop for e in (lst events) collect (list bar e voice)))))))

(defun lyric-text* (l)
  (format nil "~a~a~:[~;_~]" (lyric-text l)
          (if (member (lyric-syllabic l) '(:begin :middle)) "-" "")
          (lyric-extender l)))

(defun verse-line (score verse)
  "The words of VERSE as sung, rebuilt from the syllables of SCORE's lyrics."
  (with-output-to-string (s)
    (let ((joined t))
      (loop for (nil e) in (all-events score)
            for l = (find verse (event-lyrics e) :key #'lyric-verse)
            when l
              do (unless joined (write-char #\Space s))
                 (write-string (lyric-text l) s)
                 (setf joined (member (lyric-syllabic l) '(:begin :middle)))
                 (when joined (write-char #\- s))))))

(defun class-word (object)
  (string-downcase (symbol-name (class-name (class-of object)))))

(defun file-ids (text)
  "(element-name id) for every id attribute in the MusicXML TEXT, in document order."
  (let ((document (handler-bind ((xml-protocol:xml-entity-error #'continue))
                    (xml-protocol:decode text)))
        (found '()))
    (labels ((walk (element)
               (let ((id (xml-protocol:xml-attr element "id")))
                 (when id (push (list (xml-protocol:xml-element-local-name element) id) found)))
               (dolist (child (xml-protocol:xml-element-children element))
                 (when (xml-protocol:xml-element-p child) (walk child)))))
      (walk (xml-protocol:document-root-element document)))
    (nreverse found)))


(defun demo-reading ()
  (para "READ-MUSICXML takes a pathname, a string of MusicXML, UTF-8 octets or a"
        "character stream, and returns a score-protocol SCORE: an immutable value with"
        "a measure map, parts with staves and voices of events, a chord-symbol track"
        "and the spanners. Durations are rationals in whole notes (1/4 is a quarter)."
        ""
        "The input is NEW BRITAIN, the tune of \"Amazing Grace\" (American folk melody,"
        "1829; words by John Newton, 1779), re-encoded by hand for this demo: the first"
        "phrase pair, sung twice. It has a one-beat pickup, two verses of lyrics,"
        "<harmony> chord symbols, a tie, two slurs, a dynamic, a tempo mark and a repeat"
        "with first and second endings. Some elements carry id attributes. The file,"
        "scores/new-britain.musicxml:")
  (setf *hymn-text* (file-text (score-file "new-britain.musicxml")))
  (print-lines *hymn-text*)
  (format t "~%")
  (shown (setf *hymn* (read-musicxml (score-file "new-britain.musicxml"))))
  (format t "~%")
  (format t "  (score-metadata *hymn*)~%")
  (loop for (key value) on (score-metadata *hymn*) by #'cddr
        do (out "    ~12a ~s~%" (format nil "~(~s~)" key) value))
  (check (score-metadata *hymn*)
         '(:title "Amazing Grace" :subtitle "NEW BRITAIN" :composer "American folk melody, 1829"
           :lyricist "John Newton, 1779" :copyright "Public domain. This encoding: MIT.")
         "metadata")
  (para ""
        "<work-title> is the title and <movement-title> the subtitle. <encoding> is"
        "bookkeeping and is dropped without a word, as are layout and playback elements"
        "(<beam>, placement, the left barline's bar-style).")

  (format t "~&The part, its staves and voices:~%~%")
  (let ((parts (lst (score-parts *hymn*))))
    (check (length parts) 1 "parts")
    (dolist (part parts)
      (out "  part ~s ~s: staves ~{~a~^, ~}; voices ~{~a~^, ~}~%"
           (element-id part) (part-name part)
           (mapcar (lambda (st) (format nil "~a (~(~s~))" (element-id st) (staff-clef st)))
                   (part-staves part))
           (mapcar (lambda (v) (format nil "~a (staff ~d)" (element-id v) (voice-staff v)))
                   (part-voices part)))))

  (format t "~%The measure map, one MEASURE-INFO per bar. The pickup is bar 0:~%~%")
  (out "  bar  id      time  key      length  marks~%")
  (loop for m in (lst (score-measure-map *hymn*))
        for bar from (score-first-bar *hymn*)
        do (out "  ~3d  ~7a ~5a ~8a ~7a ~{~a~^; ~}~%"
                bar (element-id m)
                (if (measure-info-time m) (meter-string (measure-info-time m)) "")
                (if (measure-info-key m) (key-name (measure-info-key m)) "")
                (bar-length *hymn* bar)
                (remove nil
                        (list (and (measure-info-pickup-p m) "pickup")
                              (loop for tm in (measure-info-tempo m)
                                    collect (format nil "tempo ~a ~s ~a = ~a"
                                                    (element-id tm) (tempo-mark-text tm)
                                                    (dur-name (tempo-beat-unit (tempo-mark-tempo tm)))
                                                    (tempo-bpm (tempo-mark-tempo tm)))
                                      into marks
                                    finally (return (and marks (format nil "~{~a~}" marks))))
                              (and (measure-info-repeat-start m) "repeat start")
                              (and (measure-info-ending m)
                                   (format nil "ending ~a passes ~a"
                                           (element-id (measure-info-ending m))
                                           (ending-passes (measure-info-ending m))))
                              (and (measure-info-repeat-end m)
                                   (format nil "repeat end, ~d times" (measure-info-repeat-end m)))
                              (and (not (eq (measure-info-barline m) :regular))
                                   (format nil "barline ~(~s~)" (measure-info-barline m)))))))
  (check (length (lst (score-measure-map *hymn*))) 10 "measures")
  (check (list (score-first-bar *hymn*) (score-last-bar *hymn*) (score-duration *hymn*))
         '(0 9 7) "first bar, last bar, duration")

  (format t "~%Every event, with its marks and its lyrics (verse 1, verse 2); a trailing~%~
             \"-\" continues a word and \"_\" is an extender:~%~%")
  (out "  bar  pos   id          event   marks                     verse 1   verse 2~%")
  (let ((events (all-events *hymn*)))
    (loop for (bar e) in events
          do (out "  ~3d  ~5a ~11a ~7a ~25a ~9a ~a~%"
                  bar (cdr (element-position *hymn* (element-id e))) (element-id e) (event-text e)
                  (format nil "~{~a~^, ~}"
                          (remove nil
                                  (list (and (typep e 'sounding-event) (note-tie* e)
                                             (format nil "tie ~a -> ~a" (element-id (note-tie* e))
                                                     (tie-end (note-tie* e))))
                                        (and (event-dynamic e)
                                             (format nil "~(~a~) ~a" (dynamic-mark-value (event-dynamic e))
                                                     (element-id (event-dynamic e))))
                                        (and (event-fermata e)
                                             (format nil "fermata ~a" (element-id (event-fermata e)))))))
                  (let ((l (find 1 (event-lyrics e) :key #'lyric-verse))) (if l (lyric-text* l) ""))
                  (let ((l (find 2 (event-lyrics e) :key #'lyric-verse))) (if l (lyric-text* l) ""))))
    (check (length events) 20 "events")
    (check (loop for (nil e) in events sum (length (event-lyrics e))) 28 "lyrics"))
  (format t "~%")
  (show (verse-line *hymn* 1) "A-maz-ing grace! How sweet the sound That saved a wretch like me!")
  (show (verse-line *hymn* 2) "grace that taught my heart to fear, And grace my fears re-lieved; 'Twas"
        "verse 2's pickup, 'Twas, is in the first ending")

  (format t "~%Chord symbols are music-theory CHORD-SYMBOL values anchored at (bar . offset):~%~%")
  (let ((anchors (lst (score-chord-symbols *hymn*))))
    (dolist (a anchors)
      (out "  ~12a ~11a ~a~%" (code-text (anchor-position a)) (element-id a)
           (print-chord-symbol (anchor-symbol a))))
    (check (mapcar (lambda (a) (print-chord-symbol (anchor-symbol a))) anchors)
           '("N.C." "G" "G7/B" "C" "G" "G" "Em" "D" "D7" "D") "chord symbols"))
  (para ""
        "G7/B sits at (2 . 1/2): its <harmony> comes after the half note in the file, and"
        "a harmony's position is the reading cursor there. <kind>none</kind> is N.C.")
  (format t "~&The spanners: slurs name their first and last events by id.~%~%")
  (dolist (sp (lst (score-spanners *hymn*)))
    (out "  ~13a ~(~s~) from ~a to ~a~%" (element-id sp) (spanner-kind sp) (spanner-start sp) (spanner-end sp)))
  (check (mapcar #'element-id (lst (score-spanners *hymn*))) '("amazing-slur" "sp-1v") "spanner ids")

  (para ""
        "Ids. An element takes the id attribute of the MusicXML element that carries it"
        "when the value is a valid score id (VALID-ID-P: ASCII letters, digits, - and _,"
        "starting with a letter) and no earlier element took it. A measure's carrier is"
        "the first part's <measure>, with a slice id M.P where P is the part's id. Every"
        "id attribute in the file, and what became of it:")
  (let ((rows (file-ids *hymn-text*))
        (taken-ids (quote ())))
    (dolist (row rows)
      (destructuring-bind (name id) row
        (let* ((dot (position #\. id))
               (taken (cond ((string= name "part") nil)
                            ((and dot (string= name "measure")) (subseq id 0 dot))
                            ((find-element *hymn* id :errorp nil) id)))
               (element (and taken (find-element *hymn* taken :errorp nil))))
          (when element (push taken taken-ids))
          (out "  ~28a ~a~%" (format nil "<~a id=~s>" name id)
               (cond ((string= name "part") "the part's reference to its <score-part>")
                     (element (format nil "~a ~s" (class-word element) taken))
                     (t (format nil "not taken: (valid-id-p ~s) is NIL" id)))))))
    (check (length rows) 14 "id attributes in the file")
    (setf *taken-ids* taken-ids))
  (format t "~%")
  (show (valid-id-p "held.note") nil "an NCName, but \".\" is kept for slice ids")
  (show (element-id (second (find 7 (all-events *hymn*) :key #'first))) "e-1e"
        "so the held D5 gets a minted id")
  (let* ((elements (score-elements *hymn*))
         (given (remove-if-not (lambda (e) (member (element-id e) *taken-ids* :test (function string=))) elements)))
    (para ""
          (format nil "Of the ~d elements, ~d took ids from the file. MAKE-SCORE minted the other ~d,"
                  (length elements) (length given) (- (length elements) (length given)))
          "a kind prefix and a base-36 serial, in document order (the measure map, then the"
          "part, its staves and voices, each event followed by its marks and lyrics, then"
          "the chord symbols and the spanners). The first twenty:")
    (out "  ~{~a~^ ~}~%" (mapcar #'element-id (subseq elements 0 20)))
    (check (list (length elements) (length given)) '(79 12) "elements, ids from the file"))
  (format t "~%")
  (show (score-id-counter *hymn*) 67 "the highest serial minted: 79 - 12 = 67")
  (show (score-equal *hymn* (read-musicxml (score-file "new-britain.musicxml"))) t
        "reading again gives the same ids")

  (para "" "The other kinds of source read alike:")
  (show (score-equal *hymn* (read-musicxml *hymn-text*)) t "a string")
  (show (score-equal *hymn* (read-musicxml (babel:string-to-octets *hymn-text* :encoding :utf-8))) t
        "UTF-8 octets")
  (show (score-equal *hymn* (with-input-from-string (in *hymn-text*) (read-musicxml in))) t
        "a character stream"))

(defun note-tie* (e)
  "The tie of a note event, or of the first tied note of a chord event."
  (typecase e
    (note-event (note-tie e))
    (chord-event (some #'note-tie (chord-event-notes e)))))

;;; 2. Hymnal voices ---------------------------------------------------------------

(defparameter +satb-labels+ '("S" "A" "T" "B"))

(defun voice-lines (score)
  "Print each voice of SCORE's first part: id, staff, stem, then its bars."
  (let ((part (first (lst (score-parts score)))))
    (loop for voice in (part-voices part)
          for k from 0
          do (out "  ~a ~5a staff ~d ~8a |~{ ~a |~}~%"
                  (if (= (length (part-voices part)) 4) (nth k +satb-labels+) " ")
                  (element-id voice) (voice-staff voice)
                  (if (voice-stem voice) (format nil "~(~s~)" (voice-stem voice)) "nil")
                  (mapcar (lambda (events)
                            (format nil "~{~a~^ ~}" (mapcar #'event-text (lst events))))
                          (lst (voice-measures voice)))))))

(defun voice-pitches (score k)
  "The events of voice K (0-based) of SCORE's first part as EVENT-TEXT strings, bar by bar."
  (mapcar (lambda (events) (mapcar #'event-text (lst events)))
          (lst (voice-measures (nth k (part-voices (first (lst (score-parts score)))))))))

(defun demo-voices ()
  (para "A hymnal prints soprano and alto on the treble staff and tenor and bass on the"
        "bass staff. Where two voices move together they share a stem: one chord carries"
        "both. Where their rhythms part, the staff switches to two voices (MusicXML"
        "<voice> 1 and 2, stems up and down). The excerpt is the first line of OLD"
        "HUNDREDTH (melody by Louis Bourgeois, 1551; words \"All people that on earth do"
        "dwell\" by William Kethe, 1561), with alto, tenor and bass written for the demo."
        "Bars 0 and 1 are shared-stem chords. The last note of bar 1 is a unison: soprano"
        "and alto both sing D4, printed as one note. In bar 2 the alto holds half notes"
        "under the soprano's quarters, as voice 2. From scores/old-hundredth-satb.musicxml,"
        "bar 1 of the treble staff and bar 2:")
  (let ((text (file-text (score-file "old-hundredth-satb.musicxml"))))
    (print-lines text :from 32 :to 39)
    (format t "    ...~%")
    (print-lines text :from 50 :to 57)
    (format t "    ...~%~%"))
  (para "Read as written, the part has the file's three voices: 1 and 2 on the treble"
        "staff, 5 on the bass staff, holding chords:")
  (shown (setf *as-written* (read-musicxml (score-file "old-hundredth-satb.musicxml"))))
  (format t "~%")
  (voice-lines *as-written*)
  (check (mapcar #'voice-staff (part-voices (first (lst (score-parts *as-written*)))))
         '(1 1 2) "staves of the voices as written")
  (para ""
        "With :SPLIT-VOICES :AUTO, READ-MUSICXML calls SPLIT-VOICES on the score it read."
        "Per staff, the upper voice keeps the top note of each shared-stem chord and a"
        "lower voice gets the bottom one. The bass staff had one voice, so its lower voice"
        "is new. A single note in a shared-stem bar is a unison: both voices sing it, and"
        "the lower voice's copy is a guess, signalled as LOW-CONFIDENCE-GUESS:")
  (multiple-value-bind (split guesses)
      (signals-of (lambda () (read-musicxml (score-file "old-hundredth-satb.musicxml") :split-voices :auto))
                  'low-confidence-guess)
    (setf *satb* split)
    (format t "  (read-musicxml (score-file \"old-hundredth-satb.musicxml\") :split-voices :auto)~%~%")
    (voice-lines split)
    (check (voice-pitches split 0) '(("G4:h") ("G4:q" "F#4:q" "E4:q" "D4:q") ("G4:q" "A4:q" "B4:h")) "soprano")
    (check (voice-pitches split 1) '(("D4:h") ("D4:q" "D4:q" "B3:q" "D4:q") ("D4:h" "D4:h")) "alto")
    (check (voice-pitches split 2) '(("B3:h") ("B3:q" "A3:q" "G3:q" "G3:q") ("G3:q" "F#3:q" "G3:h")) "tenor")
    (check (voice-pitches split 3) '(("G2:h") ("G2:q" "D3:q" "E3:q" "B2:q") ("B2:q" "D3:q" "G2:h")) "bass")
    (check (mapcar #'voice-stem (part-voices (first (lst (score-parts split))))) '(:up :down :up :down) "stems")
    (format t "~%")
    (dolist (g guesses)
      (format t "  signalled: ~a~%" (condition-line g)))
    (check (mapcar (lambda (g) (list (low-confidence-guess-what g) (low-confidence-guess-confidence g))) guesses)
           '(("unison" 1/2)) "guesses")
    (let* ((id (low-confidence-guess-element-id (first guesses)))
           (copy (find-element split id)))
      (format t "~%")
      (show (element-position *satb* (low-confidence-guess-element-id (first guesses))) '(1 . 3/4)
            "bar 1, beat 4: the alto's D4")
      (show (element-confidence (find-element *satb* (low-confidence-guess-element-id (first guesses)))) 1/2)
      (show (provenance-backend (element-provenance (find-element *satb* (low-confidence-guess-element-id (first guesses)))))
            "score-backend-musicxml")
      (show (mapcar #'element-id (uncertain-elements *satb*)) (list id) "the one uncertain element")
      (check (event-text copy) "D4:q" "the copy")))
  (para ""
        "Ids follow the notes. The soprano keeps the chord events' ids, the alto takes"
        "the ids of the lower chord notes, and the new bass voice is minted:")
  (let* ((written *as-written*)
         (chord (first (lst (first (lst (voice-measures (first (part-voices (first (lst (score-parts written))))))))))))
    (out "  as written, bar 0 treble: chord ~a, notes ~{~a~^ ~}~%" (element-id chord)
         (mapcar (lambda (n) (format nil "~a=~a" (element-id n) (pitch-name (note-pitch n)))) (chord-event-notes chord)))
    (let ((voices (part-voices (first (lst (score-parts *satb*))))))
      (out "  split, bar 0: soprano ~a, alto ~a; voice ids ~{~a~^ ~}~%"
           (element-id (first (lst (first (lst (voice-measures (first voices)))))))
           (element-id (first (lst (first (lst (voice-measures (second voices)))))))
           (mapcar #'element-id voices))
      (check (element-id (first (lst (first (lst (voice-measures (first voices)))))))
             (element-id chord) "soprano keeps the chord's id")
      (check (element-id (first (lst (first (lst (voice-measures (second voices)))))))
             (element-id (second (chord-event-notes chord))) "alto takes the lower note's id")))
  (format t "~%")
  (show (score-equal (split-voices *as-written*) *satb*) t "the same as calling SPLIT-VOICES yourself")
  (show (check-score *satb*) nil "no issues")
  (show (score-equal (split-voices *satb*) *satb*) t "a split score has nothing left to split"))

;;; 3. Chord symbols ----------------------------------------------------------------

(defun harmony-xml (root alter kind &key degrees bass (kind-attributes ""))
  "A <harmony> element: ROOT with ALTER, KIND, DEGREES as (value alter type), BASS as (step alter)."
  (with-output-to-string (s)
    (format s "<harmony>")
    (format s "<root><root-step>~a</root-step>~:[~;<root-alter>~a</root-alter>~]</root>"
            root (/= alter 0) alter)
    (format s "<kind~a>~a</kind>" kind-attributes kind)
    (when bass
      (format s "<bass><bass-step>~a</bass-step>~:[~;<bass-alter>~a</bass-alter>~]</bass>"
              (first bass) (/= (second bass) 0) (second bass)))
    (loop for (value alter type) in degrees
          do (format s "<degree><degree-value>~a</degree-value><degree-alter>~a</degree-alter>~
<degree-type>~a</degree-type></degree>" value alter type))
    (format s "</harmony>")))

(defun harmony-element (xml)
  (xml-protocol:document-root-element (xml-protocol:decode xml)))

(defun compact (element)
  (xml-protocol:encode element :pretty nil :declaration nil))

(defun degrees-text (degrees)
  (format nil "~{~a~^, ~}" (mapcar (lambda (d) (format nil "~(~a~) ~d ~d" (first d) (second d) (third d)))
                                   degrees)))

(defparameter +harmony-rows+
  '(;; root alter kind degrees bass  reads-as  written-kind
    ("C" 0 "major" () nil "C" "major")
    ("A" 0 "minor" () nil "Am" "minor")
    ("F" 1 "diminished" () nil "F#m(b5)" "diminished")
    ("E" -1 "augmented" () nil "Eb+" "augmented")
    ("G" 0 "dominant" () nil "G7" "dominant")
    ("F" 0 "major-seventh" () nil "Fmaj7" "major-seventh")
    ("D" 0 "minor-seventh" () nil "Dm7" "minor-seventh")
    ("B" 0 "half-diminished" () nil "Bm7(b5)" "half-diminished")
    ("C" 1 "diminished-seventh" () nil "C#dim7" "diminished-seventh")
    ("A" 0 "major-minor" () nil "Ammaj7" "major-minor")
    ("B" -1 "major-sixth" () nil "Bb6" "major-sixth")
    ("A" 0 "dominant-ninth" () nil "A9" "dominant-ninth")
    ("D" 0 "suspended-fourth" () nil "Dsus4" "suspended-fourth")
    ("E" 0 "power" () nil "E5" "power")
    ("D" -1 "Neapolitan" () nil "Db" "major")
    ("G" 0 "dominant" () ("B" 0) "G7/B" "dominant")
    ("D" 0 "dominant" ((9 -1 "add")) nil "D7(b9)" "dominant")
    ("A" 0 "major" ((3 0 "subtract") (4 0 "add")) ("E" 0) "Asus4/E" "suspended-fourth")
    ("A" 0 "major" ((4 0 "add") (3 0 "subtract")) ("E" 0) "A(add4,omit3)/E" "major")
    ("C" 0 "none" () nil "N.C." "none"))
  "<harmony> contents, the chord symbol each reads as, and the <kind> it is written with.")

(defun demo-chords ()
  (para "A <harmony> is a root, a <kind>, optional <degree>s and an optional bass."
        "HARMONY-TO-CHORD-SYMBOL turns the element into a music-theory CHORD-SYMBOL, and"
        "CHORD-SYMBOL-TO-HARMONY turns a symbol back into an element. The chord-level"
        "mapping is music-theory 0.5.1's (CHORD-SYMBOL-MUSICXML-KIND and"
        "MUSICXML-KIND-CHORD-SYMBOL); this backend only adapts it to XML. Each row reads"
        "a <harmony>, prints the symbol, writes it back, and reads that again:")
  (out "  <kind> (root, degrees, bass)               reads as          written as~%")
  (loop for (root alter kind degrees bass text written-kind) in +harmony-rows+
        for element = (harmony-element (harmony-xml root alter kind :degrees degrees :bass bass))
        for symbol = (harmony-to-chord-symbol element)
        for back = (chord-symbol-to-harmony symbol)
        for (out-kind out-degrees) = (multiple-value-list (chord-symbol-musicxml-kind symbol))
        do (out "  ~a~44t ~17a ~a~@[ (~a)~]~%"
                (format nil "~a (~a~a~@[; ~a~]~@[; /~a~])" kind root
                        (case alter (1 "#") (-1 "b") (t ""))
                        (and degrees (degrees-text (mapcar (lambda (d) (list (third d) (first d) (second d))) degrees)))
                        (and bass (first bass)))
                (print-chord-symbol symbol) out-kind (and out-degrees (degrees-text out-degrees)))
           (check (print-chord-symbol symbol) text (format nil "~a reads as" kind))
           (check out-kind written-kind (format nil "~a written as" text))
           (check (harmony-to-chord-symbol back) symbol (format nil "~a round trip" text)))
  (para ""
        "Neapolitan, and the other kinds named after their use, are spellings of chords"
        "the model already has: Neapolitan on Db is a Db major triad, written back as"
        "major. Degrees are ordered: major with subtract 3, add 4 is a suspension, Asus4/E,"
        "which music-theory writes with its own kind, suspended-fourth. In the other"
        "order, add 4, subtract 3, it is an added fourth and an omitted third. The"
        "D7(b9) row as XML:")
  (let ((symbol (parse-chord-symbol "D7(b9)")))
    (print-lines (xml-protocol:encode (chord-symbol-to-harmony symbol) :declaration nil) :numbers nil)
    (format t "~%")
    (show (multiple-value-list (chord-symbol-musicxml-kind (parse-chord-symbol "D7(b9)")))
          '("dominant" ((:add 9 -1))) "music-theory's side of the mapping")
    (show (print-chord-symbol (musicxml-kind-chord-symbol "major" (parse-pitch-class "A")
                                                          :bass (parse-pitch-class "E")
                                                          :degrees '((:subtract 3 0) (:add 4 0))))
          "Asus4/E"))
  (para ""
        "use-symbols. MusicXML's <kind> has display attributes: use-symbols=\"yes\" asks for"
        "symbols (Δ for major seventh, - for minor, ° and ø), and text gives the exact"
        "text to print. Both say how the chord was printed, not which chord it is, so"
        "the reader ignores them and the writer never emits them. Three spellings of a"
        "C major seventh:")
  (let* ((plain (harmony-xml "C" 0 "major-seventh"))
         (symbols (harmony-xml "C" 0 "major-seventh" :kind-attributes " use-symbols=\"yes\""))
         (text (harmony-xml "C" 0 "major-seventh" :kind-attributes " text=\"&#916;7\""))
         (read (mapcar (lambda (x) (harmony-to-chord-symbol (harmony-element x))) (list plain symbols text))))
    (loop for x in (list plain symbols text)
          for s in read
          do (out "    ~a~%      => ~a~%" x (print-chord-symbol s)))
    (check (every (lambda (s) (equalp s (first read))) read) t "the three read alike")
    (check (compact (chord-symbol-to-harmony (first read))) plain "written plain")
    (format t "~%")
    (show (print-chord-symbol (harmony-to-chord-symbol (harmony-element (harmony-xml "C" 0 "major-seventh" :kind-attributes " use-symbols=\"yes\"")))
                              :dialect :jazz :accidentals :unicode)
          "CΔ7" "symbols are the printer's choice: music-theory's :jazz dialect"))
  (para ""
        "A symbol parsed from text remembers that text and its dialect. MusicXML has no"
        "place for them, so CHORD-SYMBOL-TO-HARMONY returns what it lost as a second value"
        "(WRITE-MUSICXML turns that into a LOSSY-CONVERSION warning):")
  (show (nth-value 1 (chord-symbol-to-harmony (parse-chord-symbol "C-7" :dialect :jazz)))
        '("chord-symbol text"))
  (show (print-chord-symbol (harmony-to-chord-symbol (chord-symbol-to-harmony (parse-chord-symbol "C-7" :dialect :jazz))))
        "Cm7" "the same chord, printed in the default dialect")
  (para ""
        "A harmony the model can't hold is MUSICXML-UNSUPPORTED (section 4), here from a"
        "direct call, so with no path. <kind>other</kind> spells a chord by its degrees"
        "alone:")
  (show-signal (harmony-to-chord-symbol
                (harmony-element "<harmony><root><root-step>C</root-step></root><kind>other</kind></harmony>"))
               musicxml-unsupported)
  (show (handler-bind ((musicxml-unsupported #'skip-element))
          (harmony-to-chord-symbol
           (harmony-element "<harmony><root><root-step>C</root-step></root><kind>other</kind></harmony>")))
        nil "SKIP-ELEMENT drops it"))

;;; 4. Content outside the model --------------------------------------------------------

(defparameter +grace-line+ "        <lyric number=\"1\"><text>grace!</text></lyric>"
  "The line of new-britain.musicxml that the trill goes before.")

(defun trill-text ()
  "The hymn with a trill on \"grace!\" (bar 2): an <ornaments> element the model has no place for."
  (replace-all *hymn-text* +grace-line+
               (format nil "        <notations><ornaments><trill-mark/></ornaments></notations>~%~a"
                       +grace-line+)))

(defun demo-unsupported ()
  (para "The reader maps every MusicXML element it knows to the model, drops layout,"
        "playback and bookkeeping elements without a word, and treats everything else as"
        "content the model can't hold. Here is the hymn again with one such element, a"
        "trill on \"grace!\" in bar 2:")
  (let* ((text (trill-text))
         (k (position-if (lambda (line) (search "trill-mark" line)) (text-lines text))))
    (print-lines text :from k :to (+ k 2))
    (format t "~%")
    (para "By default (:UNSUPPORTED :ERROR) the read stops with MUSICXML-UNSUPPORTED, a"
          "SCORE-FORMAT-ERROR naming the element and its path:")
    (shown (setf *trill* (trill-text)))
    (let ((c (show-signal (read-musicxml *trill*) musicxml-unsupported)))
      (when c
        (format t "~%")
        (show (musicxml-unsupported-element c) "ornaments")
        (show (score-format-error-position c)
              "/score-partwise[1]/part[1]/measure[3]/note[1]/notations[1]/ornaments[1]")
        (show (score-format-error-format c) :musicxml)
        (show (score-error-reason c) "outside the MusicXML mapping")))
    (format t "~%")
    (para "The error is signalled inside a SKIP-ELEMENT restart. A handler that invokes it"
          "drops the element, and reading goes on as if it weren't in the file:")
    (show (block nil
            (handler-bind ((musicxml-unsupported
                             (lambda (c) (return (mapcar #'restart-name (subseq (compute-restarts c) 0 1))))))
              (read-musicxml *trill*)))
          '(skip-element) "the innermost restart")
    (let ((skipped (handler-bind ((musicxml-unsupported #'skip-element))
                     (read-musicxml *trill*))))
      (shown (setf *skipped* (handler-bind ((musicxml-unsupported #'skip-element))
                                       (read-musicxml *trill*))))
      (format t "~%")
      (show (score-equal *skipped* *hymn*) t "the hymn without the trill, ids and all")
      (show (string= (write-musicxml *skipped*) (write-musicxml *hymn*)) t "byte for byte when written")
      (show (search "ornament" (write-musicxml *skipped*)) nil "no trace of it")
      (check (score-equal skipped *hymn*) t "skipped"))
    (para ""
          "The :UNSUPPORTED keyword sets the policy for the whole read. :WARN drops each"
          "such element with a LOSSY-CONVERSION warning naming it. Its report says \"to"
          "musicxml\" although nothing is being written (egao1980/score-protocol#9). :SKIP"
          "drops the element silently:")
    (multiple-value-bind (score warnings)
        (signals-of (lambda () (read-musicxml *trill* :unsupported :warn)) 'warning)
      (format t "  (read-musicxml *trill* :unsupported :warn)~%")
      (dolist (w warnings) (format t "    warns: ~a~%" (condition-line w)))
      (format t "    => a score, SCORE-EQUAL to *hymn*: ~a~%~%" (code-text (score-equal score *hymn*)))
      (check (mapcar #'lossy-conversion-what warnings) '("<ornaments>") "the :warn warnings")
      (check (score-equal score *hymn*) t ":warn score"))
    (multiple-value-bind (score seen)
        (signals-of (lambda () (read-musicxml *trill* :unsupported :skip)) 'condition)
      (format t "  (read-musicxml *trill* :unsupported :skip)~%    conditions signalled: ~d~%    => a score, SCORE-EQUAL to *hymn*: ~a~%~%"
              (length seen) (code-text (score-equal score *hymn*)))
      (check (length seen) 0 ":skip signals")
      (check (score-equal score *hymn*) t ":skip score"))
    (show-signal (read-musicxml *trill* :unsupported :ignore) type-error)
    (para ""
          "Through serdes-protocol the error arrives as SERDES-DECODE-ERROR (serdes-protocol"
          "translates errors with HANDLER-CASE, so SKIP-ELEMENT is gone by then). Call"
          "READ-MUSICXML to handle unsupported content.")
    (show-signal (serdes-protocol:decode *trill* :format :musicxml) serdes-protocol:serdes-decode-error)))

;;; 5. Writing -------------------------------------------------------------------------

(defun id-carriers (text)
  "((element-name . count) ...) of the id attributes in the MusicXML TEXT."
  (let ((counts '()))
    (loop for (name) in (file-ids text)
          do (let ((row (assoc name counts :test #'string=)))
               (if row (incf (cdr row)) (push (cons name 1) counts))))
    (nreverse counts)))

(defun carried-ids (text)
  "The element ids the id attributes of the MusicXML TEXT carry: every attribute but
the <part> reference, with a measure's slice id M.P read as M."
  (loop for (name id) in (file-ids text)
        unless (string= name "part")
          collect (if (string= name "measure") (subseq id 0 (position #\. id)) id)))

(defun demo-writing ()
  (para "WRITE-MUSICXML returns the MusicXML text (or writes it to a pathname or a"
        "stream). It writes the DOCTYPE, which no reader needs to fetch, a <player> per"
        "voice, and every element id as an id attribute on its carrier. The first lines"
        "for the hymn:")
  (shown (setf *written* (write-musicxml *hymn*)))
  (format t "~%")
  (print-lines *written* :to 22)
  (format t "        ...~%~%")
  (let ((carriers (id-carriers *written*)))
    (format t "  id attributes in the written file, by element:~%    ~{~a~^, ~}~%~%"
            (mapcar (lambda (row) (format nil "~a ~d" (car row) (cdr row))) carriers))
    (check (reduce #'+ carriers :key #'cdr) 80 "id attributes written"))
  (show (length (text-lines *written*)) 482 (format nil "lines; the input has ~d" (length (text-lines *hymn-text*))))
  (para ""
        "Each of the 79 elements has its id on its carrier (the part's id is on <score-part>"
        "and, as the reference, on <part>), so a read gives the same score back, ids"
        "included. The written text is also a fixed point: written twice it is the same"
        "bytes, and reading and writing it again changes nothing.")
  (show (score-equal (read-musicxml *written*) *hymn*) t "read, write, read")
  (show (string= (write-musicxml *hymn*) (write-musicxml *hymn*)) t "byte-stable")
  (show (string= (write-musicxml (read-musicxml *written*)) *written*) t "a fixed point")
  (show (set-exclusive-or (carried-ids *written*) (mapcar #'element-id (score-elements *hymn*))
                          :test #'string=)
        nil "every element id is in the file, once")
  (format t "~%")
  (show (pathname-name (write-musicxml *hymn* (tmp-path "new-britain.musicxml"))) "new-britain"
        "with a pathname it writes the file")
  (show (score-equal (read-musicxml (tmp-path "new-britain.musicxml")) *hymn*) t)
  (para ""
        "A score whose elements have provenance or confidence loses them in MusicXML."
        "Writing warns once per kind of loss. The split SATB score of section 2 has one"
        "guess:")
  (multiple-value-bind (text warnings) (signals-of (lambda () (write-musicxml *satb*)) 'lossy-conversion)
    (format t "  (write-musicxml *satb*)~%")
    (dolist (w warnings) (format t "    warns: ~a~%" (condition-line w)))
    (check (mapcar #'lossy-conversion-what warnings) '("provenance and confidence") "satb write warnings")
    (let ((back (read-musicxml text)))
      (check (length (uncertain-elements back)) 0 "the guess's confidence is gone")
      (format t "~%")
      (show (length (uncertain-elements (read-musicxml (handler-bind ((warning #'muffle-warning))
                                                          (write-musicxml *satb*)))))
            0 "the voices are kept, the confidence isn't")))

  (para ""
        ".mxl. WRITE-MXL stores the same text in a zip archive: an uncompressed mimetype"
        "entry first, as MusicXML 4.0 asks, then META-INF/container.xml naming the"
        "rootfile, then the score. Every entry is stored with no timestamp, so the bytes"
        "depend on the score only.")
  (shown (setf *mxl* (write-mxl *hymn*)))
  (format t "~%")
  (show (length *mxl*) (length (write-mxl *hymn*)) "octets")
  (show (equalp (write-mxl *hymn*) *mxl*) t "byte-stable")
  (let ((archive (compression-protocol:open-archive *mxl* :format :zip)))
    (unwind-protect
         (let ((entries (compression-protocol:archive-entries archive)))
           (format t "~%  entry                    method  size~%")
           (dolist (e entries)
             (out "  ~24a ~6d  ~d~%" (compression-protocol:archive-entry-name e)
                  (compression-protocol:archive-entry-method e)
                  (compression-protocol:archive-entry-uncompressed-size e)))
           (check (mapcar #'compression-protocol:archive-entry-name entries)
                  '("mimetype" "META-INF/container.xml" "score.musicxml") "entries")
           (check (every (lambda (e) (eql 0 (compression-protocol:archive-entry-method e))) entries) t "stored")
           (format t "~%  mimetype: ~a~%  META-INF/container.xml:~%"
                   (octets-text (compression-protocol:read-entry archive "mimetype")))
           (print-lines (octets-text (compression-protocol:read-entry archive "META-INF/container.xml")) :numbers nil)
           (check (octets-text (compression-protocol:read-entry archive "score.musicxml")) *written* "the score entry"))
      (compression-protocol:close-archive archive)))
  (format t "~%")
  (show (score-equal (read-mxl *mxl*) *hymn*) t "READ-MXL finds the rootfile through container.xml")
  (show (pathname-type (write-mxl *hymn* (tmp-path "new-britain.mxl"))) "mxl")
  (show (score-equal (read-mxl (tmp-path "new-britain.mxl")) *hymn*) t)
  (show-signal (read-mxl (babel:string-to-octets *written* :encoding :utf-8)) score-format-error)

  (para ""
        "serdes-protocol. Loading the system registers two formats, :MUSICXML (text) and"
        ":MXL (binary). ENCODE and DECODE delegate to the functions above:")
  (show (serdes-protocol:format-media-type :musicxml) "application/vnd.recordare.musicxml+xml")
  (show (serdes-protocol:format-media-type :mxl) "application/vnd.recordare.musicxml")
  (show (serdes-protocol:find-format-for-media-type "application/vnd.recordare.musicxml") :mxl)
  (show (serdes-protocol:format-binary-p :mxl) t)
  (show (string= (serdes-protocol:encode *hymn* :format :musicxml) *written*) t)
  (show (score-equal (serdes-protocol:decode *written* :format :musicxml) *hymn*) t)
  (show (equalp (serdes-protocol:encode *hymn* :format :mxl) *mxl*) t)
  (show (score-equal (serdes-protocol:decode *mxl* :format :mxl) *hymn*) t))

;;; 6. Schema check -------------------------------------------------------------------

(defun program-available-p (program)
  (handler-case (zerop (stack-process:run (list program "--version") :output nil :error-output nil))
    (error () nil)))

(defun xmllint (text schema catalog)
  "(values exit-code message): xmllint validating TEXT against SCHEMA, offline."
  (multiple-value-bind (code stdout stderr)
      (stack-process:run (list "xmllint" "--nonet" "--noout" "--schema" schema "-")
                         :input text :output :string :error-output :string
                         :env (list (cons "XML_CATALOG_FILES" catalog)))
    (declare (ignore stdout))
    (values code (string-trim '(#\Newline #\Return #\Space) stderr))))

(defun demo-schema ()
  (para "The MusicXML 4.0 XSD is not part of this repository. To check the written files"
        "against it, put the schema/ directory of github.com/w3c/musicxml (tag v4.0:"
        "musicxml.xsd with xml.xsd, xlink.xsd, container.xsd and catalog.xml beside it)"
        "somewhere local and set MUSICXML_XSD to its musicxml.xsd. xmllint then runs"
        "with --nonet and the schema's catalog, so nothing is fetched.")
  (let* ((xsd (uiop:getenv "MUSICXML_XSD"))
         (xsd-file (and xsd (plusp (length xsd)) (probe-file xsd)))
         (xmllint (program-available-p "xmllint")))
    (cond
      ((null xsd-file)
       (format t "  skipped: MUSICXML_XSD ~:[is not set~;does not name a file~].~%" (and xsd (plusp (length xsd)))))
      ((not xmllint)
       (format t "  skipped: xmllint is not on PATH.~%"))
      (t
       (let ((catalog (namestring (merge-pathnames "catalog.xml" xsd-file)))
             (container-xsd (probe-file (merge-pathnames "container.xsd" xsd-file))))
         (format t "  MUSICXML_XSD names a musicxml.xsd and xmllint is on PATH.~%~%")
         (loop for (what text schema) in
                   (list (list "the hymn as written (section 5)" (write-musicxml *hymn*) xsd-file)
                         (list "the split SATB excerpt as written" (handler-bind ((warning #'muffle-warning))
                                                                     (write-musicxml *satb*))
                               xsd-file)
                         (list "the hymn read back from .mxl, written again"
                               (write-musicxml (read-mxl (write-mxl *hymn*))) xsd-file)
                         (and container-xsd
                              (list "META-INF/container.xml (container.xsd)"
                                    (let ((archive (compression-protocol:open-archive (write-mxl *hymn*) :format :zip)))
                                      (unwind-protect
                                           (octets-text (compression-protocol:read-entry archive "META-INF/container.xml"))
                                        (compression-protocol:close-archive archive)))
                                    container-xsd)))
               when what
                 do (multiple-value-bind (code message) (xmllint text (namestring schema) catalog)
                      (format t "  ~a:~%    xmllint exit ~d: ~a~%" what code message)
                      (check code 0 what))))))))

;;; 7. External entities ------------------------------------------------------------

(defun entity-doc (doctype &optional (title "&secret;"))
  "A one-measure MusicXML document with DOCTYPE and TITLE as its movement title."
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%~a~%<score-partwise version=\"4.0\">~
<movement-title>~a</movement-title><part-list><score-part id=\"P1\"><part-name>T</part-name>~
</score-part></part-list><part id=\"P1\"><measure number=\"1\"/></part></score-partwise>~%"
          doctype title))

(defun demo-entities ()
  (para "XML external entities (XXE) let a document pull in a local file or a URL. The"
        "reader parses with xml-backend-native, which never fetches anything: it refuses"
        "an external entity at its declaration, and the reader reports that as a"
        "SCORE-FORMAT-ERROR whose CAUSE is the XML-ENTITY-ERROR. The secret here is a"
        "file in the run's temp directory:")
  (write-text (tmp-path "secret.txt") "the secret is 42")
  (let* ((url (format nil "file://~a~a" (if (char= (char (slashed (namestring *tmp*)) 0) #\/) "" "/")
                      (slashed (namestring (tmp-path "secret.txt")))))
         (doc (entity-doc (format nil "<!DOCTYPE score-partwise [~%  <!ENTITY secret SYSTEM ~s>~%]>" url))))
    (print-lines doc :to 4 :numbers t)
    (format t "  ...~%~%")
    (setf *xxe* doc)
    (let ((c (show-signal (read-musicxml *xxe*) score-format-error)))
      (when c
        (format t "~%")
        (show (condition-line (score-format-error-cause c))
              "xml-protocol:xml-entity-error: XML error: external entity declaration at 3:24")
        (show (score-format-error-position c) '(3 . 24) "line 3, column 24: the declaration")
        (show (search "42" (princ-to-string c)) nil "the secret is nowhere in the report"))))
  (para ""
        "The refusal comes at the declaration, before any reference is read: a"
        "document that declares the entity and never uses it is refused just the same."
        "A parameter entity naming a URL is refused at its declaration too. Had either"
        "been fetched, the error would be about a file or a connection (port 9 has no"
        "listener):")
  (let ((unused (entity-doc (format nil "<!DOCTYPE score-partwise [<!ENTITY secret SYSTEM \"file:///etc/hostname\">]>") "Hymn"))
        (parameter (entity-doc "<!DOCTYPE score-partwise [<!ENTITY % remote SYSTEM \"http://127.0.0.1:9/evil.dtd\"> %remote;]>" "Hymn")))
    (format t "    ~a~%" (second (text-lines unused)))
    (setf *unused* unused)
    (show-signal (read-musicxml *unused*) score-format-error)
    (format t "~%    ~a~%" (second (text-lines parameter)))
    (setf *parameter* parameter)
    (let ((c (show-signal (read-musicxml *parameter*) score-format-error)))
      (when c
        (check (typep (score-format-error-cause c) 'xml-protocol:xml-entity-error) t "parameter entity cause"))))
  (para ""
        "Internal entities are not expanded either: xml-backend-native knows only the"
        "predefined entities and numeric character references. So the \"billion laughs\""
        "document, eight nested entities that would expand to 20 million characters,"
        "stops at its first reference:")
  (let ((laughs (entity-doc (format nil "<!DOCTYPE score-partwise [<!ENTITY a \"ha\">~{<!ENTITY ~a \"~a\">~}]>"
                                    (loop for (name previous) in '(("b" "a") ("c" "b") ("d" "c") ("e" "d")
                                                                   ("f" "e") ("g" "f") ("h" "g"))
                                          nconc (list name (format nil "~v@{&~a;~:*~}" 10 previous))))
                            "&h;")))
    (setf *laughs* laughs)
    (format t "    ~a...~%" (subseq (second (text-lines laughs)) 0 70))
    (let ((c (show-signal (read-musicxml *laughs*) score-format-error)))
      (when c
        (check (typep (score-format-error-cause c) 'xml-protocol:xml-entity-error) t "billion laughs cause"))))
  (para ""
        "The DOCTYPE every MusicXML file starts with names an external DTD subset by URL."
        "The parser refuses to fetch it with an XML-ENTITY-ERROR that offers CONTINUE,"
        "and READ-MUSICXML takes that restart, so the files of sections 1 and 2 read"
        "without a network. Through xml-protocol directly:")
  (let* ((seen '())
         (document (handler-bind ((xml-protocol:xml-entity-error
                                    (lambda (c) (push c seen) (continue c))))
                     (xml-protocol:decode *hymn-text*))))
    (format t "  (handler-bind ((xml-protocol:xml-entity-error #'continue))~%    (xml-protocol:decode *hymn-text*))~%")
    (dolist (c seen) (format t "    signalled: ~a, CONTINUE taken~%" (condition-line c)))
    (check (length seen) 1 "the DTD refusal")
    (check (xml-protocol:xml-element-local-name (xml-protocol:document-root-element document))
           "score-partwise" "decoded anyway")))

;;; The tour -------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*tmp* (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "harness-demos-musicxml-~36r"
                                         (random (expt 36 8) (make-random-state t)))
                                 (uiop:temporary-directory)))))
    (ensure-directories-exist *tmp*)
    (unwind-protect
         (progn
           (format t "~&score-backend-musicxml ~a (slice SC2: MusicXML 4.0 partwise, .musicxml and .mxl)~%~
                      on score-protocol ~a and music-theory ~a~%"
                   (asdf:component-version (asdf:find-system "score-backend-musicxml"))
                   (asdf:component-version (asdf:find-system "score-protocol"))
                   (asdf:component-version (asdf:find-system "music-theory")))
           (section "1. Reading a hymn" (demo-reading))
           (section "2. Hymnal voices: SPLIT-VOICES" (demo-voices))
           (section "3. Chord symbols: <harmony> and music-theory" (demo-chords))
           (section "4. Content outside the model: MUSICXML-UNSUPPORTED" (demo-unsupported))
           (section "5. Writing: .musicxml, .mxl and serdes formats" (demo-writing))
           (section "6. Schema check (optional)" (demo-schema))
           (section "7. External entities are refused" (demo-entities))
           (banner "Summary")
           (format t "~d results checked against expected values: ~d mismatched.~%~
                      ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
                   *checks* *mismatches* (reverse *failed-sections*))
           (finish-output)
           (and (zerop *mismatches*) (null *failed-sections*)))
      (uiop:delete-directory-tree *tmp* :validate t :if-does-not-exist :ignore))))
