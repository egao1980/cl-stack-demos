(in-package #:harness-demos-score)

;;;; A tour of score-protocol 0.1.0, slice SC0: the canonical, immutable model
;;;; of a notated score. One public-domain hymn tune, NEW BRITAIN ("Amazing
;;;; Grace": American folk melody, 1829; words by John Newton, 1779), is built
;;;; in code (section 1). The sections then cover its element ids (2), bars
;;;; and positions (3), validation (4), provenance and confidence (5) and the
;;;; canonical s-expression form (6). Every result with a known answer is also
;;;; checked: RUN-DEMOS returns NIL when one differs or a section stops on an
;;;; error.

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
  "OBJECT printed as it reads in this package, which uses SCORE-PROTOCOL."
  (let ((*package* (find-package '#:harness-demos-score))
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
                (music-theory:spelled-pitch (pitch-name value))
                (music-theory:meter (meter-string value))
                (music-theory:key (key-name value))
                (music-theory:chord-symbol (ignore-errors (print-chord-symbol value))))))
    (if name
        (format nil "~a   ; ~a" (code-text value) name)
        (code-text value))))

(defun same-p (a b)
  "EQUALP, except that strings compare case-sensitively (\"e-a\" is not \"E-A\")."
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
                   (format nil "~a   ; ~a" (code-text value) note)
                   (value-text value))))
    (if (and (<= (+ 2 (length ftext) 4 (length vtext)) 84)
             (not (find #\Newline ftext)))
        (format t "~&  ~a => ~a~%" ftext vtext)
        (format t "~&  ~a~%    => ~a~%" ftext vtext))))

(defmacro show (form &optional (expected nil expected-p) note)
  "Print FORM and its value like a REPL transcript; check the value against
EXPECTED when one is given. NOTE is a comment printed after the value."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (print-result ',form ,value ,note)
       ,@(when expected-p `((check ,value ,expected (code-text ',form))))
       ,value)))

(defmacro show-values (form expected &optional note)
  "SHOW for a form with several values, printed as (values ...). EXPECTED is
the list of values."
  (let ((vals (gensym "VALUES")))
    `(let ((,vals (multiple-value-list ,form)))
       (print-result ',form (cons 'values ,vals) ,note)
       (check ,vals ,expected (code-text ',form))
       ,vals)))

(defmacro shown (form)
  "Print FORM as code, then evaluate it. Its value is not printed."
  `(progn (format t "~&  ~a~%~%" (indent (code-text ',form :margin 96) 2))
          ,form))

(defun condition-line (c)
  "The type of the condition C and its report, on one line: runs of blanks and
newlines in the report become one space."
  (let* ((*package* (find-package '#:harness-demos-score))
         (*print-case* :downcase)
         (report (princ-to-string c)))
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

;;; The hymn tune as data -------------------------------------------------------
;;; NEW BRITAIN in G major, 3/4, with a one-beat pickup. Bars 1-15 are sung
;;; twice, once per verse: bar 16 is the first ending (it ends verse 1 and
;;; picks up verse 2), bar 17 the second. A note is (PITCH DURATION FLAG...):
;;; :MELISMA continues the syllable before it, :HELD is the stop note of a tie
;;; (no new syllable), :TIE starts a tie to the next note, :BREATH and
;;; :FERMATA add those marks, :MP and :MF a dynamic. Durations are whole
;;; notes: 1/4 is a quarter.

(defparameter *melody*
  '((("D4" 1/4 :mp))                                    ; 0  the pickup
    (("G4" 1/2) ("B4" 1/8) ("G4" 1/8 :melisma))         ; 1
    (("B4" 1/2) ("A4" 1/4))                             ; 2
    (("G4" 1/2) ("E4" 1/4))                             ; 3
    (("D4" 1/2) ("D4" 1/4))                             ; 4
    (("G4" 1/2) ("B4" 1/8) ("G4" 1/8 :melisma))         ; 5
    (("B4" 1/2) ("A4" 1/4))                             ; 6
    (("D5" 3/4 :tie))                                   ; 7
    (("D5" 1/2 :held :breath) ("B4" 1/4 :mf))           ; 8
    (("D5" 1/2) ("B4" 1/8) ("D5" 1/8 :melisma))         ; 9
    (("B4" 1/2) ("G4" 1/4))                             ; 10
    (("D4" 1/2) ("E4" 1/4))                             ; 11
    (("G4" 1/2) ("E4" 1/4 :melisma))                    ; 12
    (("D4" 1/2 :melisma) ("D4" 1/4))                    ; 13
    (("G4" 1/2) ("B4" 1/8) ("G4" 1/8 :melisma))         ; 14
    (("B4" 1/2) ("A4" 1/4))                             ; 15
    (("G4" 1/2) ("D4" 1/4))                             ; 16 first ending
    (("G4" 3/4 :fermata)))                              ; 17 second ending
  "The melody, bar by bar from the pickup (bar 0).")

(defparameter *bass*
  '(((:rest 1/4))
    (("G2" 3/4 :mp)) (("G2" 1/2) ("B2" 1/4)) (("C3" 3/4)) (("G2" 3/4))
    (("G2" 3/4)) (("E2" 3/4)) (("D2" 3/4)) (("F#2" 3/4))
    (("G2" 3/4)) (("G2" 1/2) ("B2" 1/4)) (("C3" 3/4)) (("G2" 1/2) ("E2" 1/4))
    (("D2" 3/4)) (("G2" 3/4)) (("D2" 3/4)) (("G2" 1/2) (:rest 1/4))
    ((("G2" "D3") 3/4 :fermata)))
  "A bass line for the second staff. A list of pitches is a chord.")

(defparameter *chords*
  '(((0 . 0) "N.C.") ((1 . 0) "G") ((2 . 1/2) "G7/B") ((3 . 0) "C") ((4 . 0) "G")
    ((6 . 0) "Em") ((7 . 0) "D") ((8 . 0) "D7/F#") ((9 . 0) "G") ((10 . 1/2) "G7/B")
    ((11 . 0) "C") ((12 . 0) "G") ((12 . 1/2) "Em") ((13 . 0) "D") ((14 . 0) "G")
    ((15 . 0) "D7") ((16 . 0) "G") ((17 . 0) "G"))
  "Chord symbols at (bar . offset) positions. (8 . 0) is under the tied D5:
no note starts there.")

(defparameter *measure-marks*
  '((0 :time "3/4" :key "G major" :pickup-p t :length 1/4 :tempo (84 "Moderato"))
    (1 :repeat-start t :rehearsal "A")
    (9 :rehearsal "B")
    (16 :ending (1) :repeat-end 2)
    (17 :ending (2) :barline :final))
  "What each bar's MEASURE-INFO holds beyond the defaults.")

(defparameter *verses*
  '("A- maz- ing grace! how sweet the sound That saved a wretch like me! I once was lost, but now am found; Was blind, but now I see."
    "'Twas grace that taught my heart to fear, And grace my fears re- lieved; How pre- cious did that grace ap- pear The hour I first be- lieved!")
  "John Newton's first two verses (1779). A trailing hyphen continues a word.")

(defparameter *metadata*
  '(:title "Amazing Grace" :subtitle "NEW BRITAIN"
    :composer "American folk melody, 1829" :lyricist "John Newton, 1779"))

(defun flag-p (spec flag)
  "True when the note SPEC carries FLAG."
  (and (member flag (cddr spec)) t))

(defun melody-notes ()
  "Every melody note as (BAR INDEX SPEC), in time order."
  (loop for specs in *melody*
        for bar from 0
        nconc (loop for spec in specs
                    for j from 0
                    collect (list bar j spec))))

(defun syllables (text)
  "TEXT split into (SYLLABLE SYLLABIC) pairs. A token that ends in a hyphen
begins or continues a word; the token after the last one ends it."
  (loop with continued = nil
        for token in (remove "" (uiop:split-string text :separator " ") :test #'string=)
        for hyphen = (char= #\- (char token (1- (length token))))
        collect (list (string-right-trim "-" token)
                      (cond ((and continued hyphen) :middle)
                            (hyphen :begin)
                            (continued :end)
                            (t :single)))
        do (setf continued hyphen)))

(defun syllable-slots (verse)
  "The (BAR INDEX) of each note that takes a syllable of VERSE, in singing
order. Verse 1 starts on the pickup and ends in the first ending; verse 2
starts on the first ending's last note and ends in the second ending."
  (let ((body (loop for (bar j spec) in (melody-notes)
                    when (and (<= 1 bar 15)
                              (not (flag-p spec :melisma))
                              (not (flag-p spec :held)))
                      collect (list bar j))))
    (if (= verse 1)
        (append '((0 0)) body '((16 0)))
        (append '((16 1)) body '((17 0))))))

(defun continued-p (bar j)
  "True when the melody note after (BAR J) continues its syllable: a melisma
or the stop note of a tie. Its syllable then gets an extender line."
  (let ((next (second (member (list bar j) (melody-notes)
                              :key (lambda (note) (list (first note) (second note)))
                              :test #'equal))))
    (and next (or (flag-p (third next) :melisma) (flag-p (third next) :held)))))

;;; Building the score ------------------------------------------------------------
;;; ANNOTATE is a function of a key such as (:NOTE bar j), (:LYRIC verse bar
;;; j), (:ANCHOR position), (:BREATH bar j), (:FERMATA bar j) or (:TEMPO bar).
;;; It returns extra constructor arguments (:PROVENANCE, :CONFIDENCE), or NIL.
;;; Section 5 uses it to play a recognition step.

(defun hymn-lyrics (annotate)
  "A hash table from (BAR INDEX) to the lyrics sung on that melody note."
  (let ((table (make-hash-table :test #'equal)))
    (loop for text in *verses*
          for verse from 1
          for syllables = (syllables text)
          for slots = (syllable-slots verse)
          do (assert (= (length syllables) (length slots)) ()
                     "verse ~d has ~d syllables for ~d notes"
                     verse (length syllables) (length slots))
             (loop for (syllable syllabic) in syllables
                   for (bar j) in slots
                   do (push (apply #'make-lyric :verse verse :text syllable :syllabic syllabic
                                                :extender (continued-p bar j)
                                   (funcall annotate (list :lyric verse bar j)))
                            (gethash (list bar j) table))))
    table))

(defun dynamic-of (flags)
  "A DYNAMIC-MARK for the dynamic among FLAGS, or NIL."
  (let ((value (find-if (lambda (flag) (member flag '(:p :mp :mf :f))) flags)))
    (and value (make-dynamic-mark :value value))))

(defun melody-event (bar j spec lyrics annotate)
  (destructuring-bind (pitch duration &rest flags) spec
    (apply #'make-note-event
           :pitch (parse-pitch pitch) :duration duration
           :dynamic (dynamic-of flags)
           :fermata (and (member :fermata flags)
                         (apply #'make-fermata (funcall annotate (list :fermata bar j))))
           :breath-mark (and (member :breath flags)
                             (apply #'make-breath-mark (funcall annotate (list :breath bar j))))
           :lyrics (gethash (list bar j) lyrics)
           (funcall annotate (list :note bar j)))))

(defun bass-event (spec)
  (destructuring-bind (pitch duration &rest flags) spec
    (let ((fermata (and (member :fermata flags) (make-fermata))))
      (etypecase pitch
        (keyword (make-rest-event :duration duration))
        (cons (make-chord-event
               :notes (mapcar (lambda (name) (make-chord-note :pitch (parse-pitch name))) pitch)
               :duration duration :fermata fermata))
        (string (make-note-event :pitch (parse-pitch pitch) :duration duration
                                 :dynamic (dynamic-of flags) :fermata fermata))))))

(defun hymn-measure (bar annotate)
  (destructuring-bind (&key time key pickup-p length tempo repeat-start rehearsal
                         ending repeat-end (barline :regular))
      (rest (assoc bar *measure-marks*))
    (make-measure-info
     :time (and time (parse-meter time)) :key (and key (parse-key key))
     :pickup-p pickup-p :length length :barline barline
     :repeat-start repeat-start :repeat-end repeat-end
     :rehearsal (and rehearsal (make-rehearsal-mark :text rehearsal))
     :ending (and ending (make-ending :passes ending))
     :tempo (and tempo
                 (list (apply #'make-tempo-mark
                              :tempo (make-tempo :bpm (first tempo) :beat-unit 1/4)
                              :text (second tempo)
                              (funcall annotate (list :tempo bar))))))))

(defun build-hymn (&key (annotate (constantly nil)))
  "Step 1: every element but the tie and the slurs, which name notes by id.
MAKE-SCORE mints an id for each element, in document order."
  (let ((lyrics (hymn-lyrics annotate)))
    (make-score
     :metadata *metadata*
     :measures (loop for bar from 0 below (length *melody*)
                     collect (hymn-measure bar annotate))
     :parts (list (make-part
                   :name "Hymn tune"
                   :staves (list (make-staff :clef :treble) (make-staff :clef :bass))
                   :voices (list (make-voice
                                  :name "Melody" :staff 1
                                  :measures (loop for specs in *melody*
                                                  for bar from 0
                                                  collect (loop for spec in specs
                                                                for j from 0
                                                                collect (melody-event bar j spec lyrics annotate))))
                                 (make-voice
                                  :name "Bass" :staff 2
                                  :measures (mapcar (lambda (specs) (mapcar #'bass-event specs))
                                                    *bass*)))))
     :chord-symbols (loop for (position text) in *chords*
                          collect (apply #'make-chord-symbol-anchor
                                         :position position :symbol (parse-chord-symbol text)
                                         (funcall annotate (list :anchor position)))))))

;;; Reading and rebuilding ------------------------------------------------------

(defun voice-at (score n)
  "Voice N (0 the melody, 1 the bass) of the score's only part."
  (nth n (part-voices (fset:first (score-parts score)))))

(defun event-at (score voice bar j)
  "Event J of BAR in voice VOICE (0 the melody, 1 the bass)."
  (fset:lookup (fset:lookup (voice-measures (voice-at score voice))
                            (- bar (score-first-bar score)))
               j))

(defun melody-at (score bar j) (event-at score 0 bar j))
(defun bass-at (score bar j) (event-at score 1 bar j))

(defun copy-note (note &rest changes)
  "A NOTE-EVENT like NOTE, with its id, but with CHANGES (constructor arguments)."
  (apply #'make-note-event
         (append changes
                 (list :id (element-id note) :pitch (note-pitch note)
                       :duration (event-duration note) :tuplet (event-tuplet note)
                       :grace (event-grace note) :accidental (note-accidental note)
                       :tie (note-tie note) :articulations (event-articulations note)
                       :fermata (event-fermata note) :breath-mark (event-breath-mark note)
                       :dynamic (event-dynamic note) :lyrics (event-lyrics note)
                       :provenance (element-provenance note)
                       :confidence (element-confidence note)))))

(defun copy-measure (measure &rest changes)
  "A MEASURE-INFO like MEASURE, with its id, but with CHANGES."
  (apply #'make-measure-info
         (append changes
                 (list :id (element-id measure) :number (measure-info-number measure)
                       :time (measure-info-time measure) :key (measure-info-key measure)
                       :pickup-p (measure-info-pickup-p measure)
                       :length (measure-info-length measure)
                       :barline (measure-info-barline measure)
                       :repeat-start (measure-info-repeat-start measure)
                       :repeat-end (measure-info-repeat-end measure)
                       :ending (measure-info-ending measure) :jump (measure-info-jump measure)
                       :rehearsal (measure-info-rehearsal measure)
                       :tempo (measure-info-tempo measure)
                       :provenance (element-provenance measure)
                       :confidence (element-confidence measure)))))

(defun replace-event (score new)
  "SCORE with the event that has NEW's id replaced by NEW, through COPY-SCORE.
Only the path down to the event is rebuilt: its voice measure, its voice and
its part. Every other element is passed on as it is."
  (let* ((id (element-id new))
         (old (find-element score id))
         (voice (element-parent score id))
         (part (element-parent score (element-id voice)))
         (index (- (car (element-position score id)) (score-first-bar score)))
         (measures (voice-measures voice))
         (events (fset:lookup measures index))
         (j (loop for i below (fset:size events)
                  when (eq (fset:lookup events i) old) return i))
         (new-voice (make-voice :id (element-id voice) :name (voice-name voice)
                                :staff (voice-staff voice) :stem (voice-stem voice)
                                :measures (fset:with measures index (fset:with events j new))
                                :provenance (element-provenance voice)
                                :confidence (element-confidence voice)))
         (new-part (make-part :id (element-id part) :name (part-name part)
                              :abbreviation (part-abbreviation part)
                              :staves (part-staves part)
                              :voices (substitute new-voice voice (part-voices part))
                              :provenance (element-provenance part)
                              :confidence (element-confidence part))))
    (copy-score score :parts (substitute new-part part (fset:convert 'list (score-parts score))))))

(defun replace-measure (score new)
  "SCORE with the measure that has NEW's id replaced by NEW."
  (let ((old (find-element score (element-id new))))
    (copy-score score :measures (substitute new old (fset:convert 'list (score-measure-map score))))))

(defun slur-runs ()
  "((START-BAR START-J) (END-BAR END-J)) for each melisma of the melody: from
the note that takes the syllable to the last note that continues it."
  (let ((runs '()) (start nil) (end nil))
    (loop for (bar j spec) in (melody-notes)
          do (cond ((flag-p spec :melisma) (setf end (list bar j)))
                   (t (when end (push (list start end) runs))
                      (setf start (list bar j) end nil))))
    (when end (push (list start end) runs))
    (nreverse runs)))

(defun tie-notes ()
  "((BAR J) of the note that starts the tie, (BAR J) of its stop note)."
  (let ((notes (melody-notes)))
    (loop for (note next) on notes
          when (flag-p (third note) :tie)
            return (list (subseq note 0 2) (subseq next 0 2)))))

(defun add-ties-and-slurs (score)
  "Step 2: the tie and the slurs, which point at the notes by their minted ids.
The note that starts the tie is rebuilt (same id); COPY-SCORE adds the rest."
  (destructuring-bind ((b1 j1) (b2 j2)) (tie-notes)
    (let* ((start (melody-at score b1 j1))
           (stop (melody-at score b2 j2))
           (tied (replace-event score (copy-note start :tie (make-tie :end (element-id stop))))))
      (copy-score tied
                  :spanners (loop for ((sb sj) (eb ej)) in (slur-runs)
                                  collect (make-spanner :kind :slur
                                                        :start (element-id (melody-at tied sb sj))
                                                        :end (element-id (melody-at tied eb ej))))))))

(defun make-hymn (&key (annotate (constantly nil)))
  (add-ties-and-slurs (build-hymn :annotate annotate)))

(defvar *draft* nil "The hymn after step 1: no tie, no slurs.")
(defvar *hymn* nil "The hymn.")

;;; Printing a score --------------------------------------------------------------

(defun duration-text (duration)
  (case duration (1/8 "e") (1/4 "q") (3/8 "q.") (1/2 "h") (3/4 "h.") (t (princ-to-string duration))))

(defun chord-text (anchor)
  (let ((offset (cdr (anchor-position anchor)))
        (text (print-chord-symbol (anchor-symbol anchor))))
    (if (zerop offset) text (format nil "~a@~a" text offset))))

(defun slur-ends (score)
  "Two hash tables of event ids: where slurs start, where they end."
  (let ((starts (make-hash-table :test #'equal))
        (ends (make-hash-table :test #'equal)))
    (fset:do-seq (spanner (score-spanners score))
      (setf (gethash (spanner-start spanner) starts) t
            (gethash (spanner-end spanner) ends) t))
    (values starts ends)))

(defun event-text (event starts ends)
  "EVENT as PITCH:DURATION with its marks: ~ tie, ( ) slur, ' breath mark,
^ fermata, [mp] a dynamic."
  (format nil "~a:~a~:[~;~~~]~:[~;(~]~:[~;)~]~:[~;'~]~:[~;^~]~@[[~(~a~)]~]"
          (etypecase event
            (rest-event "r")
            (chord-event (format nil "<~{~a~^ ~}>"
                                 (mapcar (lambda (n) (pitch-name (note-pitch n)))
                                         (chord-event-notes event))))
            (note-event (pitch-name (note-pitch event))))
          (duration-text (event-duration event))
          (and (typep event 'note-event) (note-tie event))
          (gethash (element-id event) starts)
          (gethash (element-id event) ends)
          (and (typep event 'sounding-event) (event-breath-mark event))
          (event-fermata event)
          (and (event-dynamic event) (dynamic-mark-value (event-dynamic event)))))

(defun lyric-token (lyric)
  "A syllable as printed in a hymnal: a hyphen after one that a word goes on
from, an underscore for an extender line."
  (format nil "~a~:[~;-~]~:[~;_~]" (lyric-text lyric)
          (member (lyric-syllabic lyric) '(:begin :middle)) (lyric-extender lyric)))

(defun verse-text (events verse)
  (format nil "~{~a~^ ~}"
          (loop for event in events
                nconc (loop for lyric in (event-lyrics event)
                            when (= (lyric-verse lyric) verse)
                              collect (lyric-token lyric)))))

(defun print-score-table (score)
  (multiple-value-bind (starts ends) (slur-ends score)
    (let ((anchors (fset:convert 'list (score-chord-symbols score))))
      (out "  bar  chords      melody              verse 1     verse 2         bass~%")
      (loop for bar from (score-first-bar score) to (score-last-bar score)
            for melody = (fset:convert 'list (fset:lookup (voice-measures (voice-at score 0))
                                                          (- bar (score-first-bar score))))
            for bass = (fset:convert 'list (fset:lookup (voice-measures (voice-at score 1))
                                                        (- bar (score-first-bar score))))
            do (out "  ~3d  ~11a ~19a ~11a ~15a ~{~a~^ ~}~%"
                    bar
                    (format nil "~{~a~^ ~}"
                            (mapcar #'chord-text
                                    (remove bar anchors :key (lambda (a) (car (anchor-position a)))
                                                        :test-not #'eql)))
                    (format nil "~{~a~^ ~}" (mapcar (lambda (e) (event-text e starts ends)) melody))
                    (verse-text melody 1) (verse-text melody 2)
                    (mapcar (lambda (e) (event-text e starts ends)) bass))))))

;;; 1. Building the score ---------------------------------------------------------

(defun demo-build ()
  (para "A SCORE is an immutable value: metadata, a measure map shared by all parts,"
        "the parts (each with staves and voices of events), a chord-symbol track and"
        "the spanners. Pitches, meters, keys, tempi and chord symbols are music-theory"
        "values. Durations and offsets are rationals in whole notes (1/4 is a quarter)."
        ""
        "The demo builds NEW BRITAIN, the tune of \"Amazing Grace\" (American folk"
        "melody, 1829; words by John Newton, 1779), on two staves: the melody with two"
        "verses of lyrics, and a bass line. 3/4, G major, a one-beat pickup. Bars 1-15"
        "are repeated, once per verse, with a first and a second ending. The tune is"
        "data (*MELODY*, *BASS*, *CHORDS*, *MEASURE-MARKS*, *VERSES* in demo.lisp); the"
        "constructors turn it into elements:")
  (shown (make-measure-info :time (parse-meter "3/4") :key (parse-key "G major")
                            :pickup-p t :length 1/4
                            :tempo (list (make-tempo-mark :tempo (make-tempo :bpm 84 :beat-unit 1/4)
                                                          :text "Moderato"))))
  (shown (make-note-event :pitch (parse-pitch "B4") :duration 1/8
                          :lyrics (list (make-lyric :verse 1 :text "ing" :syllabic :end :extender t)
                                        (make-lyric :verse 2 :text "that" :extender t))))
  (shown (make-chord-symbol-anchor :position '(2 . 1/2) :symbol (parse-chord-symbol "G7/B")))
  (para ""
        "Ties and slurs refer to notes by id. So the score is built in two steps. Step 1"
        "(BUILD-HYMN) calls MAKE-SCORE with everything else and no ids at all: MAKE-SCORE"
        "mints one for every element. Step 2 (ADD-TIES-AND-SLURS) reads the minted ids"
        "of the notes and adds the tie (by rebuilding its start note, same id) and five"
        "slurs over the melismas, through COPY-SCORE.")
  (setf *draft* (build-hymn)
        *hymn* (add-ties-and-slurs *draft*))
  (show (score-metadata *hymn*) *metadata*)
  (show (fset:size (score-measure-map *hymn*)) 18)
  (show (mapcar #'part-name (fset:convert 'list (score-parts *hymn*))) '("Hymn tune"))
  (show (mapcar #'staff-clef (part-staves (fset:first (score-parts *hymn*)))) '(:treble :bass))
  (show (mapcar #'voice-name (part-voices (fset:first (score-parts *hymn*)))) '("Melody" "Bass"))
  (show (fset:size (score-chord-symbols *hymn*)) 18)
  (show (fset:size (score-spanners *hymn*)) 5)
  (show (length (score-elements *draft*)) 169 "elements after step 1")
  (show (length (score-elements *hymn*)) 175 "+ 1 tie, 5 slurs")
  (para ""
        "The whole score, read back through the readers. A note is PITCH:DURATION (e"
        "eighth, q quarter, h half, h. dotted half); ~ starts a tie, ( and ) mark slur"
        "ends, ' is a breath mark, ^ a fermata, [mp] a dynamic. A syllable with a"
        "hyphen continues the word, _ is an extender line. A chord @1/2 is anchored"
        "half a note into the bar. Bar 8's D7/F# sits under the tied D5, where no note"
        "starts: chord symbols are anchored to positions, not to notes.")
  (print-score-table *hymn*)
  (format t "~%")
  (let* ((counts (make-hash-table)))
    (dolist (element (score-elements *hymn*))
      (incf (gethash (class-name (class-of element)) counts 0)))
    (para "Elements by class:")
    (let ((expected '((measure-info 18) (tempo-mark 1) (rehearsal-mark 2) (ending 2)
                      (part 1) (staff 2) (voice 2) (note-event 56) (rest-event 2)
                      (chord-event 1) (chord-note 2) (tie 1) (lyric 56) (dynamic-mark 3)
                      (fermata 2) (breath-mark 1) (chord-symbol-anchor 18) (spanner 5))))
      (loop for (class n) in expected
            do (out "  ~22a ~3d~%" (string-downcase class) (gethash class counts 0))
               (check (gethash class counts 0) n (format nil "count of ~(~a~)" class)))
      (check (hash-table-count counts) (length expected) "number of classes")))
  (format t "~%")
  (show (check-score *hymn*) nil "no issues: section 4"))

;;; 2. Ids ------------------------------------------------------------------------

(defun id-prefix (id) (subseq id 0 (position #\- id)))

(defun id-serial (id)
  "The serial of a minted id, its base-36 part after the hyphen."
  (parse-integer id :start (1+ (position #\- id)) :radix 36))

(defun ncname-p (string)
  "True when STRING is an XML NCName in ASCII: a letter or _ first, then letters,
digits, -, _ and ."
  (flet ((letter-p (c) (or (char<= #\a c #\z) (char<= #\A c #\Z) (char= c #\_))))
    (and (plusp (length string))
         (letter-p (char string 0))
         (every (lambda (c) (or (letter-p c) (char<= #\0 c #\9) (find c "-."))) string))))

(defun print-wrapped (label ids)
  "LABEL, then IDS wrapped at 80 columns under it."
  (let ((column 16))
    (format t "  ~13a" label)
    (dolist (id ids)
      (when (> (+ column 1 (length id)) 80)
        (format t "~%~15t")
        (setf column 16))
      (format t " ~a" id)
      (incf column (1+ (length id))))
    (terpri)))

(defun print-ids-in-order (score)
  "Every id of SCORE in document order, grouped: the measure map, the part, each
bar of the melody, the bass, the chord symbols, the spanners."
  (let ((groups '()) (voice nil))
    (flet ((add (label id)
             (if (equal (car (first groups)) label)
                 (push id (cdr (first groups)))
                 (push (list label id) groups))))
      (dolist (element (score-elements score))
        (let ((id (element-id element)))
          (typecase element
            ((or measure-info ending jump-mark rehearsal-mark tempo-mark) (add "measure map" id))
            ((or part staff) (add "part, staves" id))
            (voice (setf voice element) (add (string-downcase (voice-name voice)) id))
            (chord-symbol-anchor (add "chord symbols" id))
            (spanner (add "spanners" id))
            (t (add (if (string= (voice-name voice) "Melody")
                        (format nil "  bar ~d" (car (element-position score id)))
                        "bass")
                    id))))))
    (loop for (label . ids) in (reverse groups)
          do (print-wrapped label (reverse ids)))))

(defun demo-ids ()
  (para "Every element has an id: a string, unique in the score. MAKE-SCORE mints the"
        "missing ones as PREFIX-SERIAL, the serial in base 36 (lower case), counting up"
        "in document order. The prefix names the kind of element: m measure, tm tempo"
        "mark, rh rehearsal mark, en ending, p part, st staff, v voice, e event (note,"
        "rest or chord), n chord note, ti tie, ly lyric, dy dynamic, fe fermata, br"
        "breath mark, h chord symbol, sp spanner. Ids are opaque: code asks"
        "FIND-ELEMENT, it never parses one. Document order is a pre-order walk: the"
        "measures (each followed by its marks), the part, its staves and voices, each"
        "event followed by its marks and lyrics, then chord symbols and spanners."
        ""
        "Every id of the hymn, in document order (SCORE-ELEMENTS):")
  (print-ids-in-order *hymn*)
  (let* ((elements (score-elements *hymn*))
         (ids (mapcar #'element-id elements))
         (step-1 (mapcar #'element-id (score-elements *draft*))))
    (format t "~%")
    (para "The prefixes, with the class each one stands for:")
    (out "  prefix  count  first  last   classes~%")
    (let ((rows '()))
      (loop for element in elements
            for id = (element-id element)
            for class = (string-downcase (class-name (class-of element)))
            for row = (assoc (id-prefix id) rows :test #'string=)
            do (if row
                   (progn (incf (second row))
                          (setf (fourth row) id)
                          (pushnew class (fifth row) :test #'string=))
                   (push (list (id-prefix id) 1 id id (list class)) rows)))
      (loop for (prefix count first last classes) in (reverse rows)
            do (out "  ~6a  ~5d  ~5a  ~5a  ~{~a~^, ~}~%" prefix count first last (reverse classes)))
      (check (mapcar #'first (reverse rows))
             '("m" "tm" "rh" "en" "p" "st" "v" "e" "dy" "ly" "ti" "br" "fe" "n" "h" "sp")
             "prefixes in order of first appearance"))
    (format t "~%")
    (para "The counter and the serials. Step 1 numbered its 169 elements 1 to 169 in"
          "document order. COPY-SCORE in step 2 passed the counter on, so the tie and the"
          "slurs got 170 to 175, although the tie sits inside a note of bar 7:")
    (show (score-id-counter *draft*) 169)
    (show (score-id-counter *hymn*) 175)
    (show (equal (mapcar #'id-serial step-1) (loop for i from 1 to 169 collect i)) t
          "serials 1, 2, ... 169")
    (let ((tie (note-tie (melody-at *hymn* 7 0))))
      (show (element-id tie) "ti-4q")
      (show (id-serial (element-id tie)) 170)
      (show (tie-end tie) (element-id (melody-at *hymn* 8 0)))
      (show (element-parent *hymn* (element-id tie)) (melody-at *hymn* 7 0)))
    (show (mapcar #'element-id (fset:convert 'list (score-spanners *hymn*)))
          '("sp-4r" "sp-4s" "sp-4t" "sp-4u" "sp-4v"))
    (show (= (length ids) (length (remove-duplicates ids :test #'string=))) t "all unique")
    (show (every #'valid-id-p ids) t)
    (show (every #'ncname-p ids) t "so MusicXML and MEI can carry them as xml ids")
    (show (list (valid-id-p "P1") (valid-id-p "tenor-1") (valid-id-p "1e") (valid-id-p "m-3.p-4")
                (valid-id-p "e a"))
          '(t t nil nil nil))
    (show-signal (make-note-event :id "1st" :pitch (parse-pitch "G4") :duration 1/4) type-error)
    (format t "~%")
    (para "Minting is deterministic: the same input gets the same ids, with no clock,"
          "randomness or global state.")
    (show (equal ids (mapcar #'element-id (score-elements (make-hymn)))) t)
    (show (score-equal *hymn* (make-hymn)) t)
    (format t "~%")
    (para "Stable through COPY-SCORE. A copy with new metadata keeps every id and the"
          "counter, and shares every element with the original (EQ):")
    (let ((copy (copy-score *hymn* :metadata '(:title "Amazing Grace" :arranger "demo"))))
      (show (getf (score-metadata copy) :arranger) "demo")
      (show (equal ids (mapcar #'element-id (score-elements copy))) t)
      (show (every #'eq elements (score-elements copy)) t)
      (show (score-id-counter copy) 175))
    (format t "~%")
    (para "Untouched subtrees stay EQ after a rebuild. Step 2 rebuilt the note that starts"
          "the tie, and with it the path above it: the melody voice and the part. Of the"
          "169 elements of step 1, the other 166 are the same objects in the hymn. The"
          "three rebuilt ones kept their ids:")
    (let ((rebuilt (remove-if (lambda (element)
                                (eq element (find-element *hymn* (element-id element))))
                              (score-elements *draft*))))
      (show (- 169 (length rebuilt)) 166 "EQ in both scores")
      (show (mapcar #'element-id rebuilt)
            (list (element-id (fset:first (score-parts *hymn*)))
                  (element-id (voice-at *hymn* 0))
                  (element-id (melody-at *hymn* 7 0))))
      (show (mapcar (lambda (element) (class-name (class-of element))) rebuilt)
            '(part voice note-event))
      (show (eq (bar-measure *draft* 7) (bar-measure *hymn* 7)) t)
      (show (eq (voice-at *draft* 1) (voice-at *hymn* 1)) t "the bass voice")
      (show (eq (melody-at *draft* 7 0) (melody-at *hymn* 7 0)) nil "the tied note")
      (show (equal (event-lyrics (melody-at *draft* 7 0)) (event-lyrics (melody-at *hymn* 7 0)))
            t "its lyrics: the same objects"))
    (format t "~%")
    (para "A deleted element's id is never reused. Removing the Em of bar 12 and then"
          "adding a C there gives the C a new serial, one past the counter, not the"
          "Em's id:")
    (let* ((anchors (fset:convert 'list (score-chord-symbols *hymn*)))
           (em (find '(12 . 1/2) anchors :key #'anchor-position :test #'equal))
           (fewer (copy-score *hymn* :chord-symbols (remove em anchors)))
           (more (copy-score fewer :chord-symbols
                             (append (remove em anchors)
                                     (list (make-chord-symbol-anchor
                                            :position '(12 . 1/2)
                                            :symbol (parse-chord-symbol "C"))))))
           (c (find '(12 . 1/2) (fset:convert 'list (score-chord-symbols more))
                    :key #'anchor-position :test #'equal)))
      (show (element-id em) "h-4k")
      (show (find-element fewer (element-id em) :errorp nil) nil)
      (show (score-id-counter fewer) 175 "deleting mints nothing")
      (show (element-id c) "h-4w")
      (show (id-serial (element-id c)) 176)
      (show (find-element more (element-id em) :errorp nil) nil))
    (format t "~%")
    (para "Lookups. FIND-ELEMENT and ELEMENT-PARENT go from an id to its element and up"
          "the tree. An unknown id signals UNKNOWN-ELEMENT, with a USE-VALUE restart:")
    (let* ((note (melody-at *hymn* 1 1))
           (lyric (first (event-lyrics note))))
      (show (lyric-text (find-element *hymn* (element-id lyric))) "ing")
      (show (loop for id = (element-id lyric) then (element-id parent)
                  for parent = (element-parent *hymn* id)
                  while parent collect (element-id parent))
            (list (element-id note) (element-id (voice-at *hymn* 0))
                  (element-id (fset:first (score-parts *hymn*))))
            "lyric -> note -> voice -> part")
      (show (find-element *hymn* "e-zzz" :errorp nil) nil)
      (show-signal (find-element *hymn* "e-zzz") unknown-element)
      (show (handler-bind ((unknown-element (lambda (c) (use-value :missing c))))
              (find-element *hymn* "e-zzz"))
            :missing))
    (format t "~%")
    (para "Slice ids. A measure spans every part, so one voice's share of a measure (a"
          "part's <measure> in MusicXML) is named by joining two ids with a dot. A slice"
          "id is an NCName that no element can have, since element ids have no dot:")
    (let* ((measure (bar-measure *hymn* 9))
           (bass (voice-at *hymn* 1))
           (slice (slice-id (element-id measure) (element-id bass))))
      (show (slice-id (element-id measure) (element-id bass)) "m-c.v-3h" "bar 9 of the bass")
      (show (list (ncname-p slice) (valid-id-p slice)) '(t nil))
      (show-values (find-slice *hymn* "m-c.v-3h") (list measure bass))
      (show (find-slice *hymn* "m-c.e-26" :errorp nil) nil "e-26 is an event: no slice")
      (show (element-id (melody-at *hymn* 9 0)) "e-26")
      (show-signal (slice-id "m-1" "not an id") type-error))))

;;; 3. Bars and positions ---------------------------------------------------------

(defun measure-marks-text (measure)
  (format nil "~{~a~^ ~}"
          (remove nil
                  (list (and (measure-info-pickup-p measure) "pickup")
                        (and (measure-info-repeat-start measure) "|:")
                        (let ((ending (measure-info-ending measure)))
                          (and ending (format nil "[~{~a~^,~}." (ending-passes ending))))
                        (and (measure-info-repeat-end measure)
                             (format nil ":| x~d" (measure-info-repeat-end measure)))
                        (let ((mark (measure-info-rehearsal measure)))
                          (and mark (format nil "rehearsal ~a" (rehearsal-mark-text mark))))
                        (let ((tempo (first (measure-info-tempo measure))))
                          (and tempo (format nil "~a q=~a" (tempo-mark-text tempo)
                                             (tempo-bpm (tempo-mark-tempo tempo)))))
                        (and (not (eq (measure-info-barline measure) :regular))
                             (format nil "~(~a~) barline" (measure-info-barline measure)))))))

(defun demo-positions ()
  (para "Bar numbers are indices into the measure map. When the first measure is a"
        "pickup it is bar 0, so the first full bar is bar 1. Time and key signatures are"
        "inherited from the latest measure that has one. BAR-LENGTH is the meter's bar,"
        "or the measure's own LENGTH (the pickup's quarter). BAR-START adds them up:")
  (show (score-first-bar *hymn*) 0)
  (show (score-last-bar *hymn*) 17)
  (show (score-duration *hymn*) 13 "1/4 + 17 bars of 3/4")
  (out "~%  bar  id     time  key      length  start  marks~%")
  (loop for bar from 0 to 17
        for measure = (bar-measure *hymn* bar)
        for length = (bar-length *hymn* bar)
        for start = (bar-start *hymn* bar)
        do (out "  ~3d  ~5a  ~4a  ~7a  ~6a  ~5a  ~a~%"
                bar (element-id measure) (meter-string (bar-time *hymn* bar))
                (key-name (bar-key *hymn* bar)) length start (measure-marks-text measure))
           (check length (if (zerop bar) 1/4 3/4) (format nil "bar-length ~d" bar))
           (check start (if (zerop bar) 0 (+ 1/4 (* (1- bar) 3/4))) (format nil "bar-start ~d" bar))
           (check (meter-string (bar-time *hymn* bar)) "3/4" (format nil "bar-time ~d" bar))
           (check (key-name (bar-key *hymn* bar)) "G major" (format nil "bar-key ~d" bar)))
  (show (measure-info-time (bar-measure *hymn* 5)) nil "inherited: nothing stored")
  (format t "~%")
  (para "A position is (BAR . OFFSET), the offset a rational in whole notes from the"
        "start of the bar. ABSOLUTE-OFFSET turns it into whole notes from the start of"
        "the score, OFFSET-POSITION goes back. The end of a bar is the same instant as"
        "the start of the next, and OFFSET-POSITION gives the next bar. In a score with"
        "a pickup, (1 . 0) is the pickup's length:")
  (show (absolute-offset *hymn* '(0 . 0)) 0)
  (show (absolute-offset *hymn* '(1 . 0)) 1/4 "the pickup's length")
  (show (absolute-offset *hymn* '(1 . 5/8)) 7/8 "the G eighth of \"-ing\"")
  (show (absolute-offset *hymn* '(9 . 5/8)) 55/8)
  (show (absolute-offset *hymn* '(1 . 3/4)) 1 "the end of bar 1 ...")
  (show (absolute-offset *hymn* '(2 . 0)) 1 "... is the start of bar 2")
  (show (offset-position *hymn* 1) '(2 . 0))
  (show (offset-position *hymn* 1/8) '(0 . 1/8))
  (show (offset-position *hymn* 55/8) '(9 . 5/8))
  (show (offset-position *hymn* 13) '(17 . 3/4) "the very end")
  (format t "~%")
  (para "ELEMENT-POSITION places any element: an event by its voice measure and the"
        "durations before it, a mark or lyric at its event, a tie at its start note, a"
        "slur at its first event, a measure mark at its bar. Parts, staves and voices"
        "have no position:")
  (let* ((ing (melody-at *hymn* 1 1))
         (tie (note-tie (melody-at *hymn* 7 0)))
         (slur (fset:lookup (score-spanners *hymn*) 3))
         (tempo (first (measure-info-tempo (bar-measure *hymn* 0))))
         (b (measure-info-rehearsal (bar-measure *hymn* 9)))
         (anchor (fset:lookup (score-chord-symbols *hymn*) 7)))
    (out "  element  what                                   position~%")
    (loop for (element what expected)
            in (list (list ing "B4 eighth, \"-ing\"" '(1 . 1/2))
                     (list (second (event-lyrics ing)) "its verse-2 lyric \"that\"" '(1 . 1/2))
                     (list (event-breath-mark (melody-at *hymn* 8 0)) "breath mark on the held D5" '(8 . 0))
                     (list (melody-at *hymn* 8 1) "B4 quarter, \"I\"" '(8 . 1/2))
                     (list tie "the tie D5 ~ D5" '(7 . 0))
                     (list slur "the slur over \"found\"" '(12 . 0))
                     (list tempo "tempo mark Moderato" '(0 . 0))
                     (list b "rehearsal mark B" '(9 . 0))
                     (list (measure-info-ending (bar-measure *hymn* 17)) "second ending" '(17 . 0))
                     (list anchor "chord symbol D7/F#" '(8 . 0))
                     (list (bass-at *hymn* 16 1) "bass rest, first ending" '(16 . 1/2))
                     (list (voice-at *hymn* 1) "the bass voice" nil))
          for position = (element-position *hymn* (element-id element))
          do (out "  ~7a  ~37a  ~a~%" (element-id element) what (code-text position))
             (check position expected (format nil "element-position ~a" what))))
  (format t "~%")
  (para "Round trips. OFFSET-POSITION of ABSOLUTE-OFFSET gives back the position of every"
        "event (59 of them, both voices), and the other way round every offset on the"
        "1/24 grid from 0 to 13 comes back (313 offsets, triplet eighths included):")
  (let ((events (remove-if-not (lambda (e) (typep e 'event)) (score-elements *hymn*))))
    (show (length events) 59)
    (show (every (lambda (event)
                   (let ((position (element-position *hymn* (element-id event))))
                     (equal (offset-position *hymn* (absolute-offset *hymn* position)) position)))
                 events)
          t))
  (show (loop for k from 0 to (* 13 24)
              always (= (absolute-offset *hymn* (offset-position *hymn* (/ k 24))) (/ k 24)))
        t)
  (show (loop for bar from 0 to 17
              always (equal (offset-position *hymn* (bar-start *hymn* bar)) (cons bar 0)))
        t "every bar-start is (bar . 0)")
  (format t "~%")
  (para "INVALID-POSITION. A bar outside 0..17, an offset past its bar (the pickup is"
        "only a quarter long), or an absolute offset outside 0..13 signals"
        "INVALID-POSITION with the argument, inside a USE-VALUE restart. A position"
        "that isn't a cons, or a float offset, is a TYPE-ERROR:")
  (show-signal (absolute-offset *hymn* '(0 . 1/2)) invalid-position)
  (show-signal (absolute-offset *hymn* '(18 . 0)) invalid-position)
  (show-signal (offset-position *hymn* 14) invalid-position)
  (show-signal (offset-position *hymn* -1/8) invalid-position)
  (show-signal (bar-length *hymn* 18) invalid-position)
  (show-signal (bar-start *hymn* -1) invalid-position)
  (show-signal (absolute-offset *hymn* 3) type-error)
  (show-signal (offset-position *hymn* 0.5) type-error)
  (show (handler-case (absolute-offset *hymn* '(18 . 0))
          (invalid-position (c) (invalid-position-position c)))
        '(18 . 0))
  (para ""
        "A handler can answer with USE-VALUE. Here a past-the-end offset is clamped to"
        "the last position, and a bar past the end to the score's duration:")
  (show (handler-bind ((invalid-position
                         (lambda (c) (use-value (cons 17 (bar-length *hymn* 17)) c))))
          (offset-position *hymn* 20))
        '(17 . 3/4))
  (show (handler-bind ((invalid-position
                         (lambda (c) (use-value (score-duration *hymn*) c))))
          (list (absolute-offset *hymn* '(4 . 1/4)) (absolute-offset *hymn* '(25 . 0))))
        '(11/4 13))
  (show (let ((empty (make-score)))
          (list (score-first-bar empty) (score-last-bar empty) (score-duration empty)
                (handler-case (absolute-offset empty '(1 . 0))
                  (invalid-position () :invalid))))
        '(1 0 0 :invalid)
        "no measures: every position is invalid"))

;;; 4. Validation -----------------------------------------------------------------

(defvar *broken* nil "The hymn with four mistakes in it.")

(defun print-issues (score issues)
  (out "  kind               id     bar  the id names                     detail~%")
  (dolist (issue (sort (copy-list issues) #'string< :key (lambda (i) (symbol-name (score-issue-kind i)))))
    (let* ((element (find-element score (score-issue-id issue) :errorp nil))
           (parent (and element (element-parent score (element-id element)))))
      (out "  ~17a  ~5a  ~3@a  ~31a  ~a~%"
           (code-text (score-issue-kind issue)) (score-issue-id issue)
           (or (score-issue-bar issue) "-")
           (typecase element
             (lyric (format nil "lyric ~s on ~(~a~) ~a" (lyric-text element)
                            (class-name (class-of parent)) (element-id parent)))
             (tie (format nil "tie ~a ~a ~~ ~a ~a"
                          (element-id parent) (pitch-name (note-pitch parent))
                          (tie-end element)
                          (pitch-name (note-pitch (find-element score (tie-end element))))))
             (voice (format nil "voice ~s" (voice-name element)))
             (ending (format nil "ending ~{~a~^,~}. over ~d measures" (ending-passes element)
                             (ending-measures element)))
             (t (code-text element)))
           (code-text (score-issue-detail issue))))))

(defun demo-validation ()
  (para "MAKE-SCORE accepts any score whose values have the right types. Whether they"
        "fit together is reported, not refused: CHECK-SCORE returns a list of"
        "SCORE-ISSUEs (kind, subject id, bar, detail) and never signals. The hymn has"
        "none. Here is a copy with four mistakes, each made with REPLACE-EVENT or"
        "REPLACE-MEASURE (demo.lisp: rebuild one element, keep its id):")
  (show (check-score *hymn*) nil)
  (setf *broken* *hymn*)
  (para ""
        "1. An overfull bar: the E4 quarter of bar 3 becomes a half note.")
  (shown (setf *broken* (replace-event *broken* (copy-note (melody-at *broken* 3 1) :duration 1/2))))
  (para "2. A tie to a different pitch: the D5 that the bar-7 tie ends on becomes C5.")
  (shown (setf *broken* (replace-event *broken* (copy-note (melody-at *broken* 8 0)
                                                           :pitch (parse-pitch "C5")))))
  (para "3. A lyric on a rest: the bass rest of the pickup gets a syllable.")
  (shown (setf *broken* (replace-event *broken*
                                       (make-rest-event :id (element-id (bass-at *broken* 0 0))
                                                        :duration 1/4
                                                        :lyrics (list (make-lyric :text "Oh"))))))
  (para "4. An ending past the end: the second ending, on the last bar, claims three"
        "measures.")
  (shown (setf *broken* (let ((last (bar-measure *broken* 17)))
                          (replace-measure *broken*
                                           (copy-measure last :ending (make-ending :id (element-id (measure-info-ending last))
                                                                                   :passes '(2) :measures 3))))))
  (let ((issues (check-score *broken*)))
    (show (length issues) 4)
    (show (sort (mapcar #'score-issue-kind issues) #'string<)
          '(:bad-ending :measure-duration :misplaced-lyric :tie-pitch))
    (format t "~%")
    (print-issues *broken* issues)
    (format t "~%")
    (let ((by-kind (lambda (kind) (find kind issues :key #'score-issue-kind))))
      (show (score-issue-detail (funcall by-kind :measure-duration)) '(:expected 3/4 :actual 1))
      (show (score-issue-bar (funcall by-kind :measure-duration)) 3)
      (show (score-issue-id (funcall by-kind :measure-duration)) (element-id (voice-at *broken* 0)))
      (show (score-issue-id (funcall by-kind :tie-pitch))
            (element-id (note-tie (melody-at *broken* 7 0))))
      (show (score-issue-bar (funcall by-kind :bad-ending)) 17)))
  (format t "~%")
  (para "CHECK-SCORE never signals, so even the broken score goes through it with no"
        "handler firing. It works from its own bar table, not ABSOLUTE-OFFSET:")
  (show (let ((signals 0))
          (handler-bind ((condition (lambda (c) (declare (ignore c)) (incf signals))))
            (check-score *broken*))
          signals)
        0)
  (format t "~%")
  (para "VALIDATE-SCORE is the strict entry point: it returns a valid score as it is,"
        "and otherwise signals SCORE-INVARIANT-VIOLATION with the issues, inside a"
        "CONTINUE restart that returns the score as it is:")
  (show (eq (validate-score *hymn*) *hymn*) t)
  (show-signal (validate-score *broken*) score-invariant-violation)
  (show (handler-case (validate-score *broken*)
          (score-invariant-violation (c) (length (score-invariant-violation-issues c))))
        4)
  (let ((seen '()))
    (show (eq (handler-bind ((score-invariant-violation
                               (lambda (c)
                                 (setf seen (mapcar #'score-issue-kind
                                                    (score-invariant-violation-issues c)))
                                 (continue c))))
                (validate-score *broken*))
              *broken*)
          t "resumed with CONTINUE")
    (show (sort seen #'string<) '(:bad-ending :measure-duration :misplaced-lyric :tie-pitch)))
  (show (typep (make-condition 'score-invariant-violation) 'score-error) t))

;;; 5. Provenance and confidence ----------------------------------------------------

(defparameter *recognition*
  '(((:breath 8 0) 0.38 (1092 1340 14 22))
    ((:note 9 1) 0.55 (312 1612 22 64))
    ((:lyric 2 9 1) 0.41 (300 1760 70 26))
    ((:note 12 1) 0.72 (848 1630 20 58))
    ((:anchor (8 . 0)) 0.66 (1040 1250 96 30))
    ((:fermata 17 0) 0.93 (1500 2300 40 22))
    ((:tempo 0) 0.85 (120 210 260 34)))
  "What a recognition step was unsure of: element key, confidence, bounding box
on the page (x y width height, in the page's own units).")

(defun recognise (key)
  "The ANNOTATE function of a pretend recognition step: provenance :OMR on every
note, lyric and chord symbol, and a confidence and a bounding box on the
elements of *RECOGNITION*."
  (let ((entry (assoc key *recognition* :test #'equal)))
    (cond (entry
           (list :provenance (make-provenance :source :omr :backend "demo-omr" :page 1
                                              :bbox (third entry))
                 :confidence (second entry)))
          ((member (first key) '(:note :lyric :anchor))
           (list :provenance (make-provenance :source :omr :backend "demo-omr" :page 1))))))

(defun element-what (score element)
  (typecase element
    (note-event (format nil "~a ~a" (pitch-name (note-pitch element))
                        (duration-text (event-duration element))))
    (lyric (format nil "verse ~d ~s" (lyric-verse element) (lyric-text element)))
    (chord-symbol-anchor (print-chord-symbol (anchor-symbol element)))
    (breath-mark (format nil "on ~a" (element-id (element-parent score (element-id element)))))
    (fermata (format nil "on ~a" (element-id (element-parent score (element-id element)))))
    (tempo-mark (tempo-mark-text element))
    (t "")))

(defun print-uncertain (score elements)
  (out "  id     class                what           position   confidence  page  bbox~%")
  (dolist (element elements)
    (let ((provenance (element-provenance element)))
      (out "  ~5a  ~19a  ~13a  ~9a  ~10a  ~4a  ~a~%"
           (element-id element) (string-downcase (class-name (class-of element))) (element-what score element)
           (code-text (element-position score (element-id element)))
           (element-confidence element)
           (provenance-page provenance) (code-text (provenance-bbox provenance))))))

(defun demo-provenance ()
  (para "Every element can say where it came from and how sure its producer was."
        "PROVENANCE is a struct: SOURCE (:user, :musicxml, :midi, :omr, :transcription"
        "...), BACKEND, PAGE, BBOX, TIME-RANGE. CONFIDENCE is NIL (not recorded:"
        "certain) or a real in 0..1. A recognizer marks its guesses, and a review step"
        "asks UNCERTAIN-ELEMENTS for the ones below a threshold (default 0.8)."
        ""
        "Here the same builder plays a recognition step reading the tune from page 1:"
        "every note, lyric and chord symbol gets provenance :OMR, and seven elements a"
        "confidence and a bounding box (*RECOGNITION* in demo.lisp):")
  (let* ((read (make-hymn :annotate #'recognise))
         (uncertain (uncertain-elements read)))
    (show (equal (mapcar #'element-id (score-elements read))
                 (mapcar #'element-id (score-elements *hymn*)))
          t "the same ids: same input, same order")
    (show (score-equal read *hymn*) nil "provenance and confidence count")
    (show (provenance-source (element-provenance (melody-at read 1 0))) :omr)
    (show (element-confidence (melody-at read 1 0)) nil "certain")
    (show (element-provenance (melody-at *hymn* 1 0)) nil "built by hand: none")
    (format t "~%")
    (para "(uncertain-elements read), in document order:")
    (print-uncertain read uncertain)
    (format t "~%")
    (show (mapcar #'element-confidence uncertain) '(0.38 0.55 0.41 0.72 0.66))
    (show (mapcar #'element-confidence (uncertain-elements read :below 0.5)) '(0.38 0.41))
    (show (mapcar #'element-confidence (uncertain-elements read :below 1))
          '(0.85 0.38 0.55 0.41 0.72 0.93 0.66) "the tempo mark comes first: measures lead")
    (show (uncertain-elements read :below 0) nil)
    (show (uncertain-elements *hymn*) nil)
    (show (count-if #'element-provenance (score-elements read)) 114 "37 notes, 56 lyrics, 18 chords, 3 marks")
    (show (count-if #'element-confidence (score-elements read)) 7)
    (let ((note (melody-at read 9 1)))
      (show (element-provenance note)
            (make-provenance :source :omr :backend "demo-omr" :page 1 :bbox '(312 1612 22 64))))
    (format t "~%")
    (para "A reviewer who confirms the B4 eighth of bar 9 rebuilds it with the same id and"
          "confidence 1 (SC1's edits will do this, setting provenance :USER):")
    (let* ((note (melody-at read 9 1))
           (fixed (replace-event read (copy-note note :confidence 1
                                                      :provenance (make-provenance :source :user)))))
      (show (element-id (melody-at fixed 9 1)) (element-id note))
      (show (mapcar #'element-id (uncertain-elements fixed))
            (remove (element-id note) (mapcar #'element-id uncertain) :test #'string=)))
    (show-signal (make-lyric :text "la" :confidence 1.5) type-error)))

;;; 6. The canonical s-expression form ----------------------------------------------

(defun sexp-text (form &key (margin 96))
  (let ((*package* (find-package '#:harness-demos-score))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* margin))
    (prin1-to-string form)))

(defun print-head (text lines)
  "The first LINES lines of TEXT, indented, then a count of the rest."
  (let ((all (uiop:split-string text :separator (string #\Newline))))
    (loop for line in all
          repeat lines
          do (out "  ~a~%" line))
    (when (> (length all) lines)
      (out "  ... ~d more lines~%" (- (length all) lines)))))

(defmacro show-format-error (form)
  "Print FORM (a malformed score form) and what SEXP-TO-SCORE says about it."
  `(let ((form ,form))
     (out "  ~a~%" (indent (sexp-text form :margin 84) 2))
     (handler-case (progn (sexp-to-score form)
                          (format t "    (no condition)~%")
                          (check :returned :signalled "sexp-to-score"))
       (score-format-error (c)
         (out "    -> ~a~%       position: ~a~%"
              (condition-line c) (sexp-text (score-format-error-position c) :margin 1000))
         (check (score-format-error-format c) :sexp "score-format-error-format")))))

(defun demo-sexp ()
  (para "SCORE-TO-SEXP writes the reference serialization: a form of conses, keywords,"
        "strings and numbers, with every id and the id counter. Keys come in a fixed"
        "order and defaults are left out, so equal scores give EQUAL forms. Pitches,"
        "meters and keys are written as music-theory text (\"F#2\", \"3/4\", \"G major\")."
        "The hymn's form starts like this:")
  (let* ((form (score-to-sexp *hymn*))
         (text (with-standard-io-syntax (prin1-to-string form))))
    (print-head (sexp-text form) 24)
    (format t "~%")
    (out "  ~d characters when printed with PRIN1 under WITH-STANDARD-IO-SYNTAX.~%" (length text))
    (show (first form) :score)
    (show (getf (rest form) :id-counter) 175)
    (format t "~%")
    (para "ELEMENT-TO-SEXP writes one element and everything inside it: the B4 eighth of"
          "bar 1 with its two lyrics, and the chord symbol in bar 8:")
    (out "  ~a~%" (indent (sexp-text (element-to-sexp (melody-at *hymn* 1 1)) :margin 84) 2))
    (out "  ~a~%" (indent (sexp-text (element-to-sexp (fset:lookup (score-chord-symbols *hymn*) 7))
                                     :margin 84) 2))
    (show (element-id (sexp-to-element (element-to-sexp (melody-at *hymn* 1 1))))
          (element-id (melody-at *hymn* 1 1)))
    (format t "~%")
    (para "Reading it back. The text goes through READ with *READ-EVAL* NIL, then"
          "SEXP-TO-SCORE. The copy is SCORE-EQUAL (model equality: content, ids,"
          "provenance and confidence), has the same counter, writes the same form, and"
          "mints the same next id:")
    (let* ((read-form (with-standard-io-syntax
                        (let ((*read-eval* nil)) (read-from-string text))))
           (copy (sexp-to-score read-form)))
      (show (equal read-form form) t)
      (show (score-equal copy *hymn*) t)
      (show (eq copy *hymn*) nil)
      (show (score-id-counter copy) 175)
      (show (equal (score-to-sexp copy) form) t)
      (show (check-score copy) nil)
      (flet ((next-id (score)
               (element-id (find '(5 . 0) (fset:convert 'list
                                           (score-chord-symbols
                                            (copy-score score :chord-symbols
                                                        (cons (make-chord-symbol-anchor
                                                               :position '(5 . 0)
                                                               :symbol (parse-chord-symbol "G/B"))
                                                              (fset:convert 'list (score-chord-symbols score))))))
                                 :key #'anchor-position :test #'equal))))
        (show (list (next-id *hymn*) (next-id copy)) '("h-4w" "h-4w"))))
    (para ""
          "SCORE-EQUAL ignores the counter, which is bookkeeping, and counts everything"
          "else:")
    (show (score-equal *hymn* (sexp-to-score (list* :score :id-counter 999
                                                    (let ((plist (copy-list (rest form))))
                                                      (remf plist :id-counter)
                                                      plist))))
          t "the same score with counter 999")
    (show (score-equal *hymn* *draft*) nil "no tie, no slurs")
    (show (score-equal *hymn* *broken*) nil)
    (show (score-equal *broken* (sexp-to-score (score-to-sexp *broken*))) t "invalid scores round-trip too"))
  (format t "~%")
  (para "Reader errors. A form that isn't a kind keyword and a proper plist, a kind the"
        "key doesn't take, an unknown key or a key given twice signals"
        "SCORE-FORMAT-ERROR (FORMAT :SEXP). Its POSITION is the offending sub-form:")
  (show-format-error '(:score :measures ((:measure :time "3/4" :key "G major" :pickup t :lenght 1/4))
                      :parts ()))
  (show-format-error '(:score :measures ((:measure :time "3/4"))
                      :parts ((:part :staves ((:staff)) :voices ((:voice :measures (((:note :pitch "G4" :duration 3/4 :fermata (:tie :end "e-9"))))))))))
  (show-format-error '(:score :measures ((:measure :time "3/4"))
                      :parts ((:part :staves ((:staff)) :voices ((:voice :measures (((:note :pitch "G4" :duration 3/4 :lyrics ((:lyric :text "la" :verse)))))))))))
  (show-format-error '(:score :measures ((:measure :time "3/4" :time "6/8")) :parts ()))
  (show-format-error '(:scor :measures () :parts ()))
  (format t "~%")
  (para "Errors from the constructors and from music-theory's parsers are not wrapped:"
        "they reach the caller with their own restarts. A pitch that doesn't parse can"
        "be replaced with USE-VALUE, a duration that is out of range with STORE-VALUE,"
        "and the reading goes on:")
  (let ((bad-pitch '(:score :measures ((:measure :time "3/4" :key "G major"))
                     :parts ((:part :staves ((:staff))
                              :voices ((:voice :measures (((:note :pitch "H4" :duration 3/4)))))))))
        (bad-duration '(:score :measures ((:measure :time "3/4" :key "G major"))
                        :parts ((:part :staves ((:staff))
                                 :voices ((:voice :measures (((:rest :duration -3/4))))))))))
    (show-signal (sexp-to-score bad-pitch) music-theory:invalid-pitch)
    (show (let ((score (handler-bind ((music-theory:invalid-pitch
                                        (lambda (c) (use-value (parse-pitch "B4") c))))
                         (sexp-to-score bad-pitch))))
            (pitch-name (note-pitch (event-at score 0 1 0))))
          "B4")
    (show-signal (sexp-to-score bad-duration) type-error)
    (show (let ((score (handler-bind ((type-error (lambda (c) (store-value 3/4 c))))
                         (sexp-to-score bad-duration))))
            (list (event-duration (event-at score 0 1 0)) (check-score score)))
          '(3/4 nil))))

;;; Running -------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()))
    (format t "~&score-protocol ~a (slice SC0, the canonical score model) on music-theory ~a~%"
            (asdf:component-version (asdf:find-system "score-protocol"))
            (asdf:component-version (asdf:find-system "music-theory")))
    (section "1. Building a score: a hymn tune in code" (demo-build))
    (section "2. Element ids" (demo-ids))
    (section "3. Bars and positions" (demo-positions))
    (section "4. Validation: CHECK-SCORE and VALIDATE-SCORE" (demo-validation))
    (section "5. Provenance and confidence" (demo-provenance))
    (section "6. The canonical s-expression form" (demo-sexp))
    (banner "Summary")
    (format t "~d results checked against expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
