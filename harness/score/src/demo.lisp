(in-package #:harness-demos-score)

;;;; A tour of score-protocol 0.3.0. Slice SC0 is the canonical, immutable
;;;; model of a notated score; slice SC1 the edit-operation algebra and views;
;;;; slice SC3 drafts, whose ties and spanners name elements not minted yet.
;;;; One public-domain hymn tune, NEW BRITAIN ("Amazing Grace": American folk
;;;; melody, 1829; words by John Newton, 1779), is built in code (section 1).
;;;; The sections then cover its element ids (2), bars and positions (3),
;;;; validation (4), provenance and confidence (5), the canonical s-expression
;;;; form (6), editing (7), undo and composition (8), re-barring (9),
;;;; transposition (10), views (11) and the score builder (12). Section 11 also
;;;; builds a four-part fragment of OLD HUNDREDTH (melody by Louis Bourgeois,
;;;; 1551); section 12 the hymn's first line as a score of two parts. Every result
;;;; with a known answer is also checked: RUN-DEMOS returns NIL when one
;;;; differs or a section stops on an error.

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
newlines in the report become one space, and object addresses are left out."
  (let* ((*package* (find-package '#:harness-demos-score))
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

(defun build-hymn (&key (annotate (constantly nil)) spanners)
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
                                         (funcall annotate (list :anchor position))))
     :spanners spanners)))

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

(defun rebuild-event (score new)
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

(defun rebuild-measure (score new)
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
           (tied (rebuild-event score (copy-note start :tie (make-tie :end (element-id stop))))))
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
                        (and (measure-info-jump measure)
                             (format nil "~{~(~a~)~^ ~}"
                                     (mapcar #'jump-mark-kind (measure-info-jump measure))))
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
        "none. Here is a copy with four mistakes, each made with REBUILD-EVENT or"
        "REBUILD-MEASURE (demo.lisp: rebuild one element through COPY-SCORE, keep its"
        "id). The edit operations of section 7 can't make them: an edit that adds an"
        "issue is refused with EDIT-BREAKS-SCORE.")
  (show (check-score *hymn*) nil)
  (setf *broken* *hymn*)
  (para ""
        "1. An overfull bar: the E4 quarter of bar 3 becomes a half note.")
  (shown (setf *broken* (rebuild-event *broken* (copy-note (melody-at *broken* 3 1) :duration 1/2))))
  (para "2. A tie to a different pitch: the D5 that the bar-7 tie ends on becomes C5.")
  (shown (setf *broken* (rebuild-event *broken* (copy-note (melody-at *broken* 8 0)
                                                           :pitch (parse-pitch "C5")))))
  (para "3. A lyric on a rest: the bass rest of the pickup gets a syllable.")
  (shown (setf *broken* (rebuild-event *broken*
                                       (make-rest-event :id (element-id (bass-at *broken* 0 0))
                                                        :duration 1/4
                                                        :lyrics (list (make-lyric :text "Oh"))))))
  (para "4. An ending past the end: the second ending, on the last bar, claims three"
        "measures.")
  (shown (setf *broken* (let ((last (bar-measure *broken* 17)))
                          (rebuild-measure *broken*
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
           (fixed (rebuild-event read (copy-note note :confidence 1
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

;;; 7. Editing ----------------------------------------------------------------------
;;; SC1: an EDIT-OPERATION is a value; APPLY-EDIT returns the new score and a
;;; change set. The helpers print a change set with what each id names, and
;;; count how much of the old score the new one shares.

(defvar *edit-log* '() "(BEFORE OP AFTER CHANGES) for each step of section 7, newest first.")
(defvar *edited* nil "The hymn after the seven edits of section 7.")

(defun measure-id (score bar) (element-id (bar-measure score bar)))
(defun melody-id (score bar j) (element-id (melody-at score bar j)))
(defun voice-id (score n) (element-id (voice-at score n)))

(defun bar-text (score voice bar)
  "The events of BAR in voice VOICE (0 the melody, 1 the bass), written as in
the score table of section 1."
  (multiple-value-bind (starts ends) (slur-ends score)
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (event) (event-text event starts ends))
                    (fset:convert 'list (fset:lookup (voice-measures (voice-at score voice))
                                                     (- bar (score-first-bar score))))))))

(defun bar-chords (score bar)
  "The chord symbols of BAR, as in the score table."
  (format nil "~{~a~^ ~}"
          (mapcar #'chord-text
                  (remove bar (fset:convert 'list (score-chord-symbols score))
                          :key (lambda (anchor) (car (anchor-position anchor)))
                          :test-not #'eql))))

(defun id-what (score id)
  "What the element of SCORE with ID is, in a few words."
  (let ((element (find-element score id :errorp nil))
        (none (make-hash-table)))
    (typecase element
      (null "not in the score")
      (measure-info (format nil "bar ~d" (car (element-position score id))))
      (voice (format nil "voice ~a" (voice-name element)))
      (event (format nil "~a in bar ~d" (event-text element none none)
                     (car (element-position score id))))
      (tie (format nil "tie to ~a" (tie-end element)))
      (lyric (format nil "lyric ~s" (lyric-text element)))
      (spanner (format nil "~(~a~) ~a to ~a" (spanner-kind element)
                       (spanner-start element) (spanner-end element)))
      (chord-symbol-anchor (format nil "~a in bar ~d" (print-chord-symbol (anchor-symbol element))
                                   (car (anchor-position element))))
      (jump-mark (format nil "~(~a~)" (jump-mark-kind element)))
      (rehearsal-mark (format nil "rehearsal ~a" (rehearsal-mark-text element)))
      (t (string-downcase (class-name (class-of element)))))))

(defun print-items (label items &key (width 86))
  "LABEL, then ITEMS separated by commas and wrapped at WIDTH columns under it."
  (let ((lines '()) (line ""))
    (loop for (item . more) on items
          for text = (format nil "~a~:[~;,~]" item more)
          do (cond ((string= line "") (setf line text))
                   ((> (+ 12 (length line) 1 (length text)) width)
                    (push line lines)
                    (setf line text))
                   (t (setf line (concatenate 'string line " " text)))))
    (push (if (string= line "") "-" line) lines)
    (loop for text in (reverse lines)
          for first = t then nil
          do (out "  ~10a~a~%" (if first label "") text))))

(defun print-change-set (before after changes)
  "The three lists of CHANGES, each id with what it names: in AFTER, or in
BEFORE for a removed one."
  (flet ((described (ids score)
           (mapcar (lambda (id) (format nil "~a (~a)" id (id-what score id))) ids)))
    (print-items "created" (described (change-set-created changes) after))
    (print-items "removed" (described (change-set-removed changes) before))
    (print-items "modified" (described (change-set-modified changes) after))))

(defun sharing (before after)
  "(values shared kept rebuilt): of the elements of AFTER whose id BEFORE also
has, how many are BEFORE's own objects (EQ), how many there are, and the ids of
the others in document order."
  (let ((old (make-hash-table :test #'equal))
        (shared 0) (kept 0) (rebuilt '()))
    (dolist (element (score-elements before))
      (setf (gethash (element-id element) old) element))
    (dolist (element (score-elements after))
      (let ((same-id (gethash (element-id element) old)))
        (when same-id
          (incf kept)
          (if (eq same-id element)
              (incf shared)
              (push (element-id element) rebuilt)))))
    (values shared kept (nreverse rebuilt))))

(defun report-sharing (before after)
  (multiple-value-bind (shared kept rebuilt) (sharing before after)
    (out "  EQ        ~d of the ~d elements kept are the old objects~%" shared kept)
    (print-items "rebuilt" rebuilt)
    (out "  counter   ~d -> ~d~%" (score-id-counter before) (score-id-counter after))))

(defun run-step (score op bars expected)
  "Apply OP to SCORE and print BARS of the melody before and after, the change
set, the sharing and the counter. Check the change set (created removed
modified) against EXPECTED, log the step and return the new score."
  (multiple-value-bind (after changes) (apply-edit score op)
    (dolist (bar bars)
      (out "  bar ~2d  before  ~a~%          after   ~a~%" bar (bar-text score 0 bar) (bar-text after 0 bar)))
    (when bars (format t "~%"))
    (print-change-set score after changes)
    (report-sharing score after)
    (format t "~%")
    (check (list (change-set-created changes) (change-set-removed changes)
                 (change-set-modified changes))
           expected "change set")
    (check (check-score after) nil "check-score after the edit")
    (push (list score op after changes) *edit-log*)
    after))

(defmacro edit-step (score op-form bars expected)
  "Print OP-FORM and the canonical form of its operation, then RUN-STEP."
  `(let ((op ,op-form))
     (out "  ~a~%    => ~a~%~%" (indent (code-text ',op-form :margin 84) 2)
          (indent (sexp-text (edit-to-sexp op) :margin 80) 7))
     (run-step ,score op ,bars ,expected)))

(defun last-changes () (fourth (first *edit-log*)))

(defun restarts-around (thunk type)
  "The names of the first three restarts around the first condition of TYPE
that THUNK signals: the innermost, which the edit establishes."
  (block nil
    (handler-bind ((condition (lambda (c)
                                (when (typep c type)
                                  (return (subseq (mapcar #'restart-name (compute-restarts c))
                                                  0 3))))))
      (funcall thunk)
      :no-condition)))

(defun demo-editing ()
  (para "SC1 makes every change to a score a value. An EDIT-OPERATION names one change"
        "and APPLY-EDIT returns (values new-score change-set). The score passed in never"
        "changes. The new one shares every untouched element with it. Operations name"
        "their targets by id, never by place, so a logged operation means the same"
        "thing after other edits. The change set lists the ids the edit CREATED,"
        "REMOVED and MODIFIED. An element is modified when its own slots change or its"
        "children's ids do. So when a note changes pitch, its voice is not modified."
        "When a lyric's text changes, the note it sits on is not modified either."
        ""
        "Seven edits on the hymn, each applied to the result of the one before. Each"
        "prints the operation's constructor call and its canonical form, the bars it"
        "touches, the change set, and how much of the old score the new one shares:")
  (setf *edit-log* '())
  (let ((score *hymn*))
    (para "1. SET-PITCH. Bar 15's A4 becomes F#4, a leading note into the last bar. The"
          "note keeps its id and its lyrics, and only the note is modified. The voice"
          "and the part above it are rebuilt to hold the new note. Every other element"
          "of the score is the old score's own object:")
    (setf score (edit-step score (make-set-pitch :id (melody-id score 15 1) :pitch (parse-pitch "F#4"))
                           '(15) '(() () ("e-37"))))
    (show (eq (voice-at score 1) (voice-at *hymn* 1)) t "the bass voice")
    (show (every #'eq (fset:convert 'list (score-measure-map score))
                 (fset:convert 'list (score-measure-map *hymn*)))
          t "every measure")
    (show (eq (melody-at score 15 0) (melody-at *hymn* 15 0)) t "the B4 beside it")
    (format t "~%")
    (para "2. SET-DURATION across a barline. Bar 12's E4, the end of a melisma, becomes a"
          "half note. It starts on the third beat, so it runs over the barline. The first"
          "piece is the note itself, with its id and its marks. The piece in bar 13 is a"
          "new note, and a new tie joins the two. The longer note covers the first beat"
          "of bar 13, so the D4 that started there is removed, and the rest of its time"
          "becomes a quarter rest. The slur that ended on that D4 has lost an endpoint,"
          "and the cascade removes it. The new ids continue the series. The counter was"
          "175, and the tie, the piece and the rest get serials 176, 177 and 178, in"
          "document order:")
    (setf score (edit-step score (make-set-duration :id (melody-id score 12 1) :duration 1/2)
                           '(12 13) '(("ti-4w" "e-4x" "e-4y") ("e-2t" "sp-4u") ("v-r" "e-2s"))))
    (show (mapcar #'id-serial (change-set-created (last-changes))) '(176 177 178))
    (show (tie-end (note-tie (melody-at score 12 1))) "e-4x")
    (format t "~%")
    (para "3. A shorter SET-DURATION. The final G4 of the second ending goes from a dotted"
          "half to a half. The time it gave up is a gap, and the gap is filled with a"
          "quarter rest. Its fermata and its lyric stay on the note:")
    (setf score (edit-step score (make-set-duration :id (melody-id score 17 0) :duration 1/2)
                           '(17) '(("e-4z") () ("v-r" "e-3e"))))
    (para "4. INSERT-EVENT into a full bar. Bar 16, the first ending, holds G4 h and D4 q."
          "An E4 eighth goes in before the D4. Everything after it in the bar moves an"
          "eighth later. That would make the bar an eighth too long, and there is no"
          "trailing rest to absorb it. So APPLY-EDIT signals MEASURE-OVERFULL, with the"
          "voice, the bar and the excess:")
    (let ((op (make-insert-event :voice (voice-id score 0) :measure (measure-id score 16) :offset 1/2
                                 :event (make-note-event :pitch (parse-pitch "E4") :duration 1/8))))
      (show-signal (apply-edit score op) measure-overfull)
      (show (handler-case (apply-edit score op)
              (measure-overfull (c)
                (list (measure-overfull-voice c) (measure-overfull-bar c) (measure-overfull-excess c))))
            '("v-r" 16 1/8))
      (show (restarts-around (lambda () (apply-edit score op)) 'measure-overfull)
            '(reflow skip-edit use-value))
      (format t "~%")
      (para "The REFLOW restart applies the same operation with REFLOW true. The E4 takes"
            "its eighth, and the rest of the voice moves an eighth later and re-flows into"
            "the bars. The D4 now crosses the barline, so it becomes two tied eighths. The"
            "G4 of bar 17 moves, keeping its id. The quarter rest from step 3 is cut at"
            "the end of the score, and what is left becomes a new eighth rest. REFLOW"
            "follows the voice's time line bar after bar, so it pushes music from the first"
            "ending into the second. Without the rest at the end, a note would be pushed"
            "past the end of the score, which is EDIT-POSITION-INVALID.")
      (let ((reflowed (handler-bind ((measure-overfull #'reflow)) (apply-edit score op))))
        (show (score-equal reflowed (apply-edit score (make-insert-event :voice (edit-voice op) :measure (edit-measure op)
                                                                         :offset 1/2 :event (edit-event op) :reflow t)))
              t "the restart = the operation with :reflow t")
        (format t "~%")))
    (setf score (edit-step score (make-insert-event :voice (voice-id score 0) :measure (measure-id score 16) :offset 1/2
                                                    :event (make-note-event :pitch (parse-pitch "E4") :duration 1/8)
                                                    :reflow t)
                           '(16 17) '(("e-50" "ti-51" "e-52" "e-53") ("e-4z") ("v-r" "e-3c"))))
    (para "5. ADD-SPANNER: a slur over the new E4 and the D4. A spanner joins the end of"
          "SCORE-SPANNERS. Spanners are children of the score, which has no id, so"
          "nothing is modified and nothing is rebuilt:")
    (setf score (edit-step score (make-add-spanner :spanner (make-spanner :kind :slur :start (melody-id score 16 1)
                                                                           :end (melody-id score 16 2)))
                           '(16) '(("sp-54") () ())))
    (para "6. ADD-MARK: a fermata on \"see.\", the last note of verse 1. The note is"
          "modified, because a child was added to it:")
    (setf score (edit-step score (make-add-mark :target (melody-id score 16 0) :mark (make-fermata))
                           '(16) '(("fe-55") () ("e-3a"))))
    (para "7. Setting a lyric is ADD-MARK of a LYRIC. Many hymnals change \"saved a wretch"
          "like me\" to \"saved a soul like me\". A lyric of the same verse on the note is"
          "replaced, and the new lyric, built without an id, takes the old one's id"
          "(ly-1s). So the lyric is modified, but the note is not, because its shallow"
          "form holds only the lyric's id. The note is still a new object, since it holds"
          "a new lyric:")
    (setf score (edit-step score (make-add-mark :target (melody-id score 6 0) :mark (make-lyric :verse 1 :text "soul"))
                           '() '(() () ("ly-1s"))))
    (show (mapcar #'lyric-text (event-lyrics (melody-at score 6 0))) '("soul" "fears"))
    (show (mapcar #'element-id (event-lyrics (melody-at score 6 0))) '("ly-1s" "ly-1t"))
    (setf *edited* score)
    (format t "~%")
    (para "The hymn after the seven edits (compare section 1):")
    (print-score-table *edited*)
    (format t "~%")
    (show (score-id-counter *edited*) 185)
    (show (length (score-elements *edited*)) 182 "175 + 9 created - 2 removed")
    (show (check-score *edited*) nil))
  (format t "~%")
  (para "Refusals. A failed precondition or rule is an INVALID-EDIT, a SCORE-ERROR that"
        "names the operation. Some failures have their own subtype. Every refusal is"
        "signalled before anything changes, inside the restarts SKIP-EDIT and USE-VALUE:")
  (show-signal (apply-edit *hymn* (make-set-pitch :id "e-zz" :pitch (parse-pitch "C5"))) edit-target-missing)
  (show-signal (apply-edit *hymn* (make-set-pitch :id (voice-id *hymn* 0) :pitch (parse-pitch "C5"))) invalid-edit)
  (show-signal (apply-edit *hymn* (make-insert-event :voice (voice-id *hymn* 0) :measure (measure-id *hymn* 5)
                                                     :offset 1/4
                                                     :event (make-note-event :pitch (parse-pitch "A4") :duration 1/8)))
               edit-position-invalid)
  (show-signal (apply-edit *hymn* (make-add-mark :target (element-id (bass-at *hymn* 0 0))
                                                 :mark (make-lyric :text "Oh")))
               edit-breaks-score)
  (show (typep (make-condition 'edit-breaks-score) 'score-invariant-violation) t)
  (show (typep (make-condition 'edit-target-missing) 'unknown-element) t)
  (format t "~%")
  (para "SKIP-EDIT returns the score as it was, with an empty change set. USE-VALUE"
        "applies another operation instead:")
  (show (multiple-value-bind (score changes)
            (handler-bind ((invalid-edit #'skip-edit))
              (apply-edit *hymn* (make-set-pitch :id "e-zz" :pitch (parse-pitch "C5"))))
          (list (eq score *hymn*) (change-set-empty-p changes)))
        '(t t))
  (show (multiple-value-bind (score changes)
            (handler-bind ((edit-target-missing
                             (lambda (c)
                               (use-value (make-set-pitch :id (melody-id *hymn* 15 1) :pitch (parse-pitch "F#4")) c))))
              (apply-edit *hymn* (make-set-pitch :id "e-zz" :pitch (parse-pitch "F#4"))))
          (list (bar-text score 0 15) (change-set-modified changes)))
        '("B4:h F#4:q" ("e-37"))))

;;; 8. Undo, composition, the canonical form of operations -------------------------

(defun op-kind (op) (first (edit-to-sexp op)))

(defun wrap-words (text width)
  "TEXT with newlines put between words so no line is longer than WIDTH."
  (let ((lines '()) (line ""))
    (dolist (word (uiop:split-string text :separator " "))
      (cond ((string= line "") (setf line word))
            ((> (+ (length line) 1 (length word)) width) (push line lines) (setf line word))
            (t (setf line (concatenate 'string line " " word)))))
    (push line lines)
    (format nil "~{~a~^~%~}" (reverse lines))))

(defun demo-undo ()
  (para "INVERT-EDIT score op returns an operation that undoes OP on SCORE. It is read"
        "off the pair (before, after), so it undoes what was actually applied, whatever"
        "restart or cascade led there, and it restores removed elements with their own"
        "ids. It is built from the library's own classes, mostly restore-style"
        "operations in a COMPOSITE-EDIT, so it serialises like any other operation."
        ""
        "The inverse of each step of section 7, taken on the score the step was applied"
        "to:")
  (let* ((log (reverse *edit-log*))
         (inverses (loop for (before op) in log collect (invert-edit before op))))
    (out "  step  operation       inverse: ops  form size  kinds~%")
    (loop for (nil op) in log
          for inverse in inverses
          for i from 1
          for members = (edit-ops inverse)
          do (out "  ~4d  ~14a  ~12d  ~9d  ~a~%" i (string-downcase (op-kind op))
                  (length members) (length (with-standard-io-syntax (prin1-to-string (edit-to-sexp inverse))))
                  (indent (wrap-words (format nil "~{~(~a~)~^ ~}"
                                              (remove-duplicates (mapcar #'op-kind members) :from-end t))
                                      38)
                          47)))
    (check (mapcar (lambda (inverse) (length (edit-ops inverse))) inverses) '(1 61 1 1 1 1 1)
           "inverse sizes")
    (format t "~%")
    (para "Most inverses are a single restore. Step 1's restores bar 15 of the melody as it"
          "was, with the note's own id, lyrics and provenance:")
    (out "  ~a~%~%" (indent (sexp-text (edit-to-sexp (first inverses)) :margin 84) 2))
    (para "Step 2's inverse holds 61 operations. It removes every chord symbol, silences"
          "both voices, strips the measures and puts all of it back. The step removed a"
          "slur from the middle of SCORE-SPANNERS, and the direct restore (remove the"
          "spanners after it, add them back) was judged to collide with itself. It"
          "doesn't, since the removals run first. The result is still exact, as the next"
          "lines show, but the inverse is the size of the score: score-protocol#7.")
    (para "Undo: the inverses applied in reverse order, newest first. Each one gives a"
          "score SCORE-EQUAL to the score before its step. Its change set mirrors the"
          "step's: CREATED is the step's REMOVED, REMOVED its CREATED, and MODIFIED holds"
          "the same ids:")
    (let ((score *edited*))
      (loop for (before nil nil changes) in *edit-log*
            for inverse in (reverse inverses)
            for i downfrom (length *edit-log*)
            do (multiple-value-bind (undone undo) (apply-edit score inverse)
                 (let ((mirrored (and (equal (change-set-created undo) (change-set-removed changes))
                                      (equal (change-set-removed undo) (change-set-created changes))
                                      (null (set-exclusive-or (change-set-modified undo)
                                                              (change-set-modified changes)
                                                              :test #'string=)))))
                   (out "  undo step ~d: score-equal to the score before it: ~(~a~); change set mirrored: ~(~a~)~%"
                        i (score-equal undone before) mirrored)
                   (check (score-equal undone before) t (format nil "undo step ~d" i))
                   (check mirrored t (format nil "undo step ~d change set" i)))
                 (setf score undone)))
      (format t "~%")
      (show (score-equal score *hymn*) t "back to the hymn of section 1")
      (show (eq score *hymn*) nil "an equal value, not the same object")
      (show (score-id-counter score) 185 "serials are never reused"))
    (format t "~%")
    (para "COMPOSE-EDITS makes one operation of several. Its members are applied in"
          "order, each to the score the one before produced. The cascade and the check"
          "run once, at the end. The seven steps as one operation:")
    (let* ((ops (mapcar #'second log))
           (composite (apply #'compose-edits ops)))
      (out "  ~a~%~%" (indent (sexp-text (edit-to-sexp composite) :margin 84) 2))
      (multiple-value-bind (after changes) (apply-edit *hymn* composite)
        (show (score-equal after *edited*) t "the same score as step by step")
        (format t "~%")
        (print-change-set *hymn* after changes)
        (format t "~%")
        (para "The composite's change set runs from the hymn to the result. The rest made in"
              "step 3 and cut in step 4 doesn't appear in it. Its inverse is again one"
              "operation, and it undoes all seven steps:")
        (show (score-equal (apply-edit after (invert-edit *hymn* composite)) *hymn*) t))
      (show (eq (compose-edits (first ops)) (first ops)) t "one operation is itself")
      (show (length (edit-ops (compose-edits composite (first ops)))) 8 "composites are spliced")
      (show (multiple-value-bind (score changes) (apply-edit *hymn* (compose-edits))
              (list (score-equal score *hymn*) (change-set-empty-p changes)))
            '(t t) "the empty composite")
      (format t "~%")
      (para "EDIT-TO-SEXP writes an operation in the canonical form, the way sexp.lisp"
            "writes elements: the kind keyword, then the slots as keys in a fixed order,"
            "with the defaults left out. Given elements are element forms, and pitches,"
            "meters, keys and intervals are music-theory text. The text is read back with"
            "*READ-EVAL* NIL and SEXP-TO-EDIT. The copy writes the same form, and it"
            "applies to the hymn exactly as the original does:")
      (let* ((form (edit-to-sexp composite))
             (text (with-standard-io-syntax (prin1-to-string form)))
             (read-form (with-standard-io-syntax
                          (let ((*read-eval* nil)) (read-from-string text))))
             (copy (sexp-to-edit read-form)))
        (show (length text) 637 "characters")
        (show (equal (edit-to-sexp copy) form) t)
        (show (eq copy composite) nil)
        (multiple-value-bind (a ca) (apply-edit *hymn* composite)
          (multiple-value-bind (b cb) (apply-edit *hymn* copy)
            (show (list (score-equal a b) (equalp ca cb)) '(t t) "equal scores, equal change sets"))))
      (show (every (lambda (inverse)
                     (equal (edit-to-sexp (sexp-to-edit (edit-to-sexp inverse))) (edit-to-sexp inverse)))
                   inverses)
            t "the inverses round-trip too")
      (format t "~%")
      (para "A form with an unknown kind or key signals SCORE-FORMAT-ERROR (FORMAT :SEXP),"
            "with the offending form as POSITION:")
      (flet ((format-error (form)
               (out "  ~a~%" (indent (sexp-text form :margin 84) 2))
               (handler-case (progn (sexp-to-edit form)
                                    (format t "    (no condition)~%")
                                    (check :returned :signalled "sexp-to-edit"))
                 (score-format-error (c)
                   (out "    -> ~a~%       position: ~a~%" (condition-line c)
                        (sexp-text (score-format-error-position c) :margin 1000))
                   (check (score-format-error-format c) :sexp "score-format-error-format")))))
        (format-error '(:set-pitch :id "e-37" :pitch "F#4" :colour :red))
        (format-error '(:set-pitches :id "e-37" :pitch "F#4"))
        (format-error '(:composite :ops ((:set-pitch :id "e-37" :pitch "F#4") (:add-mark :target "e-3a"
                                                                                         :mark (:fermata :shape)))))))))

;;; 9. Re-barring -------------------------------------------------------------------

(defun print-bar-table (score bars)
  (out "  bar  id     time  melody                  bass               chords~%")
  (dolist (bar bars)
    (out "  ~3d  ~5a  ~4a  ~22a  ~17a  ~a~%" bar (measure-id score bar)
         (meter-string (bar-time score bar)) (bar-text score 0 bar) (bar-text score 1 bar)
         (bar-chords score bar))))

(defun demo-rebarring ()
  (para "SET-TIME re-bars a region of the score. The region runs from the measure to the"
        "next measure that has a time signature of its own. In the hymn only bar 0 has"
        "one, so a SET-TIME on bar 1 would re-bar the whole tune. Giving bar 5 its own"
        "3/4 first ends the region at bar 4. That changes only bar 5's measure:")
  (multiple-value-bind (bounded changes)
      (apply-edit *hymn* (make-set-time :measure (measure-id *hymn* 5) :time (parse-meter "3/4")))
    (show (change-set-modified changes) '("m-8"))
    (show (meter-string (measure-info-time (bar-measure bounded 5))) "3/4")
    (format t "~%")
    (para "3/4 to 6/8 on bars 1-4. A 6/8 bar holds six eighths, the same length as a 3/4"
          "bar. Every barline stays where it was, so the measures are kept, nothing"
          "moves and nothing is tied. Only bar 1's own TIME changes. Beaming and beat"
          "grouping are left to the engraver:")
    (multiple-value-bind (six-eight changes)
        (apply-edit bounded (make-set-time :measure (measure-id bounded 1) :time (parse-meter "6/8")))
      (show (list (change-set-created changes) (change-set-removed changes) (change-set-modified changes))
            '(() () ("m-3")))
      (show (loop for bar from 1 to 4
                  always (and (eq (fset:lookup (voice-measures (voice-at six-eight 0)) bar)
                                  (fset:lookup (voice-measures (voice-at bounded 0)) bar))
                              (eq (fset:lookup (voice-measures (voice-at six-eight 1)) bar)
                                  (fset:lookup (voice-measures (voice-at bounded 1)) bar))))
            t "bars 1-4 of both voices are the same objects")
      (show (mapcar (lambda (bar) (meter-string (bar-time six-eight bar))) '(1 2 3 4 5)) '("6/8" "6/8" "6/8" "6/8" "3/4")))
    (format t "~%")
    (para "3/4 to 2/4 on the same bars. The region lasts 3 whole notes, which makes six"
          "bars of 2/4. Every voice keeps its time line, and the music re-flows into the"
          "new bars. A measure is kept when a new bar starts where it started: bar 1"
          "(offset 0) and old bar 3 (offset 3/2) keep their ids, and with them anything"
          "on them. Old bars 2 and 4 started inside a new bar, so they are removed. The"
          "other four bars are new measures. A note that now crosses a barline is split"
          "into tied pieces, so ties flow across the new barlines. Chord symbols in the"
          "region keep their absolute offsets: G7/B moves from (2 . 1/2) to (3 . 1/4)."
          "Those after the region stay with their measures, two bars later. Before and"
          "after:")
    (print-bar-table bounded '(0 1 2 3 4 5 6))
    (format t "~%")
    (multiple-value-bind (two-four changes)
        (apply-edit bounded (make-set-time :measure (measure-id bounded 1) :time (parse-meter "2/4")))
      (print-bar-table two-four '(0 1 2 3 4 5 6 7 8))
      (format t "~%")
      (print-change-set bounded two-four changes)
      (format t "~%")
      (show (score-last-bar two-four) 19)
      (show (mapcar (lambda (bar) (measure-id two-four bar)) '(1 4 7)) '("m-3" "m-6" "m-8")
            "kept: old bars 1 and 3, and bar 5")
      (show (count-if (lambda (id) (string= (id-prefix id) "ti")) (change-set-created changes)) 6
            "new ties: 2 in the melody, 4 in the bass")
      (show (mapcar (lambda (bar) (bar-chords two-four bar)) '(3 4 5)) '("G7/B@1/4" "C" "G@1/4"))
      (show (check-score two-four) nil)
      (show (score-equal (apply-edit two-four (invert-edit bounded (make-set-time :measure (measure-id bounded 1)
                                                                                   :time (parse-meter "2/4"))))
                         bounded)
            t "and it undoes")))
  (format t "~%")
  (para "A refusal. SET-TIME on bar 8 to 2/4 would re-bar bars 8-17. Bar 9 starts 3/4"
        "after bar 8, inside a 2/4 bar, so it would be removed. But bar 9 carries"
        "rehearsal mark B, and re-barring would lose it. Bar 17, with the second"
        "ending, would go the same way. The operation is refused before anything"
        "changes:")
  (let ((op (make-set-time :measure (measure-id *hymn* 8) :time (parse-meter "2/4"))))
    (show-signal (apply-edit *hymn* op) invalid-edit)
    (show (handler-case (apply-edit *hymn* op) (invalid-edit (c) (eq (invalid-edit-op c) op))) t
          "the condition names the operation")
    (show (multiple-value-bind (score changes) (handler-bind ((invalid-edit #'skip-edit)) (apply-edit *hymn* op))
            (list (eq score *hymn*) (change-set-empty-p changes)))
          '(t t) "SKIP-EDIT")))

;;; 10. Transposition ---------------------------------------------------------------

(defun print-chords (label score)
  (print-items label (mapcar #'chord-text (fset:convert 'list (score-chord-symbols score)))))

(defun demo-transposition ()
  (para "TRANSPOSE moves the notes, the key signatures and the chord symbols. The"
        "spelling comes from music-theory 0.5.0. For an interval it is TRANSPOSE-PITCH"
        "and TRANSPOSE-CHORD-SYMBOL. For a target key it is TRANSPOSE-TO-KEY, whose"
        "letter moves at most three steps. Every note keeps its id and its accidental"
        "display. A chord symbol loses its TEXT, so it prints from its parts."
        ""
        "Up a major second: G major becomes A major.")
  (let ((m2 (parse-interval "M2")))
    (multiple-value-bind (up changes) (apply-edit *hymn* (make-transpose :interval m2))
      (show (interval-name m2) "M2")
      (show (key-name (bar-key up 0)) "A major")
      (print-chords "before" *hymn*)
      (print-chords "after" up)
      (format t "~%")
      (print-bar-table up '(7 8))
      (format t "~%")
      (show (list (length (change-set-created changes)) (length (change-set-removed changes))
                  (length (change-set-modified changes)))
            '(0 0 77) "58 notes and chord notes, 18 chord symbols, bar 0's key")
      (show (tie-end (note-tie (melody-at up 7 0))) (melody-id up 8 0) "the tie still joins E5 to E5")
      (show (check-score up) nil)))
  (format t "~%")
  (para "To a key. With TO-KEY, every pitch goes from the key in force at FROM (G major)"
        "to TO-KEY by TRANSPOSE-TO-KEY: it keeps its scale degree and alteration. Chord"
        "symbols move the same way. G major to Ab major:")
  (multiple-value-bind (ab) (apply-edit *hymn* (make-transpose :to-key (parse-key "Ab major")))
    (show (key-name (bar-key ab 0)) "Ab major")
    (print-chords "after" ab)
    (format t "~%")
    (print-bar-table ab '(7 8 17))
    (format t "~%")
    (show (check-score ab) nil)
    (format t "~%")
    (para "Up an augmented unison, G major becomes G# major, with eight sharps. That is"
          "not one of the 30 keys, and PARSE-KEY refuses it. For each key it moves,"
          "TRANSPOSE takes (choose-enharmonic-key K' :max-accidentals 7). That returns K'"
          "when K' is one of the 30 keys, and otherwise the one of them that sounds the"
          "same. Then it respells the notes and chord symbols from K' to that key. So"
          "the result is Ab major, spelled for Ab major, not G# major with eight sharps:")
    (show-signal (parse-key "G# major") music-theory:invalid-key)
    (let ((g-sharp (make-key :tonic (parse-pitch-class "G#") :mode :major)))
      (show (key-name (choose-enharmonic-key g-sharp :max-accidentals 7)) "Ab major" "4 flats, not 8 sharps")
      (show (key-signature (choose-enharmonic-key g-sharp)) -4))
    (show (key-name (choose-enharmonic-key (parse-key "F# major"))) "F# major" "6 sharps: kept")
    (show (key-name (choose-enharmonic-key (parse-key "C# major"))) "Db major")
    (show (key-name (choose-enharmonic-key (parse-key "C# major") :max-accidentals 7)) "C# major")
    (multiple-value-bind (a1) (apply-edit *hymn* (make-transpose :interval (parse-interval "A1")))
      (show (key-name (bar-key a1 0)) "Ab major")
      (show (bar-text a1 0 8) "Eb5:h' C5:q[mf]")
      (show (score-equal a1 ab) t "the same score as TO-KEY Ab major")))
  (format t "~%")
  (para "A range. FROM and TO limit the transposition to bars 9-15, the second half of"
        "the verse, up a major second. Bar 9 gets A major as its own key. Bar 16, the"
        "measure after TO, keeps its signatures: it gets G major as its own key, so the"
        "endings are still in G. Chord symbols move only inside the range:")
  (multiple-value-bind (part-way)
      (apply-edit *hymn* (make-transpose :interval (parse-interval "M2")
                                         :from (measure-id *hymn* 9) :to (measure-id *hymn* 15)))
    (out "  bar  key       own key  melody~%")
    (loop for bar from 8 to 17
          do (out "  ~3d  ~8a  ~7a  ~a~%" bar (key-name (bar-key part-way bar))
                  (if (measure-info-key (bar-measure part-way bar)) "yes" "-")
                  (bar-text part-way 0 bar)))
    (format t "~%")
    (print-chords "chords" part-way)
    (show (mapcar (lambda (bar) (key-name (measure-info-key (bar-measure part-way bar)))) '(9 16))
          '("A major" "G major"))
    (show (check-score part-way) nil))
  (format t "~%")
  (para "Exactly one of INTERVAL and TO-KEY, and FROM not after TO:")
  (show-signal (apply-edit *hymn* (make-transpose :interval (parse-interval "M2") :to-key (parse-key "A major")))
               invalid-edit)
  (show-signal (apply-edit *hymn* (make-transpose :interval (parse-interval "M2") :from (measure-id *hymn* 9)
                                                  :to (measure-id *hymn* 3)))
               edit-position-invalid))

;;; 11. Views -----------------------------------------------------------------------
;;; OLD HUNDREDTH: the melody of Psalm 100 in the Genevan Psalter, by Louis
;;; Bourgeois (1551), in G major and the even rhythm of modern hymnals. The
;;; first line only, words "All people that on earth do dwell" (William Kethe,
;;; 1561). The alto, tenor and bass are a plain chordal setting written for
;;; this demo. Each voice is its own part, as in an open choral score.

(defparameter *chorale*
  '(("Soprano" "S." :treble     (("G4" 1/2)) (("G4" 1/4) ("F#4" 1/4) ("E4" 1/4) ("D4" 1/4)) (("G4" 1/4) ("A4" 1/4) ("B4" 1/2)))
    ("Alto"    "A." :treble     (("D4" 1/2)) (("D4" 1/4) ("D4" 1/4) ("B3" 1/4) ("B3" 1/4))  (("D4" 1/4) ("D4" 1/4) ("D4" 1/2)))
    ("Tenor"   "T." :treble-8vb (("B3" 1/2)) (("B3" 1/4) ("A3" 1/4) ("G3" 1/4) ("G3" 1/4))  (("G3" 1/4) ("F#3" 1/4) ("G3" 1/2)))
    ("Bass"    "B." :bass       (("G2" 1/2)) (("B2" 1/4) ("D3" 1/4) ("E3" 1/4) ("G2" 1/4))  (("B2" 1/4) ("D3" 1/4) ("G2" 1/2))))
  "Name, abbreviation, clef, then the bars from the half-note pickup.")

(defun build-chorale ()
  (make-score :metadata '(:title "All people that on earth do dwell" :subtitle "OLD HUNDREDTH"
                          :composer "Louis Bourgeois, 1551" :lyricist "William Kethe, 1561")
              :measures (list (make-measure-info :time (parse-meter "4/4") :key (parse-key "G major")
                                                 :pickup-p t :length 1/2)
                              (make-measure-info)
                              (make-measure-info :barline :final))
              :parts (loop for (name abbreviation clef . bars) in *chorale*
                           collect (make-part
                                    :name name :abbreviation abbreviation
                                    :staves (list (make-staff :clef clef))
                                    :voices (list (make-voice
                                                   :name name :staff 1
                                                   :measures (loop for bar in bars
                                                                   collect (loop for (pitch duration) in bar
                                                                                 collect (make-note-event :pitch (parse-pitch pitch)
                                                                                                          :duration duration)))))))))

(defun all-voices (score)
  (loop for part in (fset:convert 'list (score-parts score)) append (part-voices part)))

(defun voice-events (voice)
  (loop for events in (fset:convert 'list (voice-measures voice)) append (fset:convert 'list events)))

(defun all-events (score)
  (loop for voice in (all-voices score) append (voice-events voice)))

(defun voice-line (voice &key (bars 4))
  "The first BARS bars of VOICE, | between bars."
  (let* ((none (make-hash-table))
         (all (fset:convert 'list (voice-measures voice)))
         (shown (subseq all 0 (min bars (length all)))))
    (format nil "~{~a~^ | ~}~:[~; | ...~]"
            (mapcar (lambda (events)
                      (format nil "~{~a~^ ~}" (mapcar (lambda (e) (event-text e none none))
                                                      (fset:convert 'list events))))
                    shown)
            (> (length all) bars))))

(defun print-parts (score)
  "Each part of SCORE: id, name, staves, then each voice with its staff, stem and music."
  (dolist (part (fset:convert 'list (score-parts score)))
    (out "  part ~a ~a, staves ~{~(~a~)~^ ~}~%" (element-id part)
         (if (part-name part) (format nil "~s" (part-name part)) "(no name)")
         (mapcar #'staff-clef (part-staves part)))
    (dolist (voice (part-voices part))
      (out "    voice ~5a ~8a staff ~d  stem ~5a  ~a~%" (element-id voice) (voice-name voice)
           (voice-staff voice) (string-downcase (or (voice-stem voice) "-")) (voice-line voice)))))

(defun id-map-identity-p (id-map)
  (block nil
    (fset:do-map (view-id source-id id-map)
      (unless (string= view-id source-id) (return nil)))
    t))

(defvar *form* nil "The hymn as an A section with a Fine, then a four-bar interlude and a D.C. al Fine.")

(defparameter *interlude*
  '((("B4" 1/2) ("D5" 1/4)) (("C5" 1/2) ("A4" 1/4)) (("B4" 1/2) ("G4" 1/4)) (("A4" 3/4)))
  "A four-bar interlude for the melody, bars 18-21.")
(defparameter *interlude-bass* '((("G2" 3/4)) (("C3" 3/4)) (("G2" 3/4)) (("D3" 3/4))))
(defparameter *interlude-chords* '((18 "G") (19 "C") (20 "G") (21 "D7")))

(defun note-bars (bars)
  (mapcar (lambda (bar)
            (mapcar (lambda (spec) (make-note-event :pitch (parse-pitch (first spec)) :duration (second spec)))
                    bar))
          bars))

(defun form-edit (score)
  "One operation that turns the hymn into an A B A form: Fine on the second
ending, then a four-bar interlude with rehearsal mark C and D.C. al Fine."
  (apply #'compose-edits
         (make-add-mark :target (measure-id score 17) :mark (make-jump-mark :kind :fine))
         (make-insert-measures
          :measure-infos (list (make-measure-info :rehearsal (make-rehearsal-mark :text "C"))
                               (make-measure-info) (make-measure-info)
                               (make-measure-info :barline :final
                                                  :jump (list (make-jump-mark :kind :da-capo-al-fine))))
          :contents (list (cons (voice-id score 0) (note-bars *interlude*))
                          (cons (voice-id score 1) (note-bars *interlude-bass*))))
         (loop for (bar text) in *interlude-chords*
               collect (make-add-chord-symbol
                        :anchor (make-chord-symbol-anchor :position (cons bar 0)
                                                          :symbol (parse-chord-symbol text))))))

(defun playback-order (view id-map passes source)
  "(pass . source-bar) for each bar of the unfolded VIEW."
  (loop for bar from (score-first-bar view) to (score-last-bar view)
        for id = (measure-id view bar)
        collect (cons (fset:lookup passes id)
                      (car (element-position source (fset:lookup id-map id))))))

(defun order-runs (order)
  "ORDER as runs of consecutive bars played on the same pass: (first last pass)."
  (let ((runs '()))
    (dolist (entry order)
      (destructuring-bind (pass . bar) entry
        (let ((run (first runs)))
          (if (and run (= pass (third run)) (= bar (1+ (second run))))
              (setf (second run) bar)
              (push (list bar bar pass) runs)))))
    (nreverse runs)))

(defun demo-views ()
  (para "A view is a pure function of a source score. It returns a new score and an ID"
        "MAP, an FSet map from each view id that comes from the source to the source id."
        "Elements carried over unchanged are the source's own objects (EQ). Elements a"
        "view makes, and every copy after the first, get minted ids that continue the"
        "source's counter. The view of a valid score is valid."
        ""
        "EXTRACT-PART takes a part or a voice. The view keeps the source's measure map,"
        "every chord symbol, and the spanners whose two ends it keeps. The melody voice"
        "on its own:")
  (multiple-value-bind (melody id-map) (extract-part *hymn* (voice-id *hymn* 0))
    (print-parts melody)
    (format t "~%")
    (show (eq (voice-at melody 0) (voice-at *hymn* 0)) t "already staff 1, stem NIL: the same voice")
    (show (list (fset:size (score-spanners melody)) (fset:size (score-chord-symbols melody))) '(5 18)
          "the five slurs, every chord symbol")
    (show (list (fset:size id-map) (id-map-identity-p id-map)) '(147 t) "every id maps to itself")
    (show (check-score melody) nil))
  (format t "~%")
  (para "The bass voice moves from staff 2 to staff 1 of a part with one staff, the bass"
        "clef. The voice is rebuilt, since its STAFF changed, but its events are the"
        "source's. No slur has both ends in the bass:")
  (multiple-value-bind (bass) (extract-part *hymn* (voice-id *hymn* 1))
    (print-parts bass)
    (format t "~%")
    (show (every #'eq (voice-events (voice-at bass 0)) (voice-events (voice-at *hymn* 1))) t)
    (show (fset:size (score-spanners bass)) 0))
  (show (multiple-value-bind (whole) (extract-part *hymn* (element-id (fset:first (score-parts *hymn*))))
          (list (score-equal whole *hymn*) (eq (fset:first (score-parts whole)) (fset:first (score-parts *hymn*)))))
        '(t t) "the whole part: the hymn again")
  (show-signal (extract-part *hymn* "v-zz") unknown-element)
  (show (multiple-value-bind (view)
            (handler-bind ((unknown-element (lambda (c) (use-value (voice-id *hymn* 1) c))))
              (extract-part *hymn* "v-zz"))
          (voice-name (voice-at view 0)))
        "Bass" "USE-VALUE: another id")
  (format t "~%")
  (para "CONDENSED-VIEW puts up to four voices on two staves, the hymnal reduction. The"
        "hymn has two voices, so this uses a four-part fragment of OLD HUNDREDTH: the"
        "first line of Louis Bourgeois's melody (1551), with an alto, tenor and bass"
        "written for this demo. It is an open score, with one part per voice:")
  (let ((chorale (build-chorale)))
    (print-parts chorale)
    (format t "~%")
    (show (check-score chorale) nil)
    (show (score-id-counter chorale) 47)
    (format t "~%")
    (para "The condensed view has one new part with a treble and a bass staff. Soprano"
          "and alto share staff 1, tenor and bass share staff 2. The upper voice of each"
          "staff gets STEM :UP and the lower STEM :DOWN. The part, minted, has no name,"
          "since the voices come from four parts:")
    (multiple-value-bind (condensed id-map) (condensed-view chorale)
      (print-parts condensed)
      (format t "~%")
      (let ((part (fset:first (score-parts condensed))))
        (show (list (element-id part) (mapcar #'element-id (part-staves part))) '("p-1c" ("st-1d" "st-1e"))
              "serials 48-50"))
      (show (every #'eq (all-events condensed) (all-events chorale))
            t "every event is the source's")
      (show (every #'eq (fset:convert 'list (score-measure-map condensed))
                   (fset:convert 'list (score-measure-map chorale)))
            t "the source's measures")
      (show (list (fset:size id-map) (id-map-identity-p id-map)) '(39 t)
            "3 measures, 4 voices, 32 notes; not the part or staves")
      (show (check-score condensed) nil)
      (format t "~%")
      (para "And EXTRACT-PART of a voice of the condensed view gives the alto line on its"
            "own staff, with STEM NIL again:")
      (multiple-value-bind (alto) (extract-part condensed (element-id (second (all-voices condensed))))
        (print-parts alto)
        (show (every #'eq (voice-events (voice-at alto 0)) (voice-events (second (all-voices chorale)))) t)))
    (show-signal (condensed-view chorale :voices (let ((s (element-id (first (all-voices chorale))))) (list s s)))
                 score-error))
  (format t "~%")
  (para "UNFOLD expands repeats, endings and jumps into playback order. For a score with"
        "all three, one composite edit turns the hymn into an A B A form. A Fine goes on"
        "the second ending (bar 17). Then come a four-bar interlude (rehearsal C, bars"
        "18-21, appended with INSERT-MEASURES) and a D.C. al Fine on its last bar. The"
        "interlude's chords are added too:")
  (multiple-value-bind (form changes) (apply-edit *hymn* (form-edit *hymn*))
    (setf *form* form)
    (print-change-set *hymn* form changes)
    (format t "~%")
    (out "  bar  id     marks~%")
    (dolist (bar '(0 1 9 16 17 18 21))
      (out "  ~3d  ~5a  ~a~%" bar (measure-id form bar) (measure-marks-text (bar-measure form bar))))
    (format t "~%")
    (print-bar-table form '(18 19 20 21))
    (show (check-score form) nil))
  (format t "~%")
  (para "The walk plays bars 0-16 and repeats from bar 1, where the repeat starts. On the"
        "second pass it skips the first ending and plays the second (bar 17). Fine"
        "doesn't stop it yet, because no jump has asked for it. It goes on through the"
        "interlude, and D.C. al Fine sends it back to the start. After a jump, repeats"
        "are not taken again and only the last ending of each group is played, so it"
        "plays 0-15 and bar 17 again, then stops at the Fine. PASS counts how many times"
        "a source measure has been played. The playback order, as (pass . bar) for each"
        "bar of the view:")
  (multiple-value-bind (view id-map passes) (unfold *form*)
    (let ((order (playback-order view id-map passes *form*)))
      (print-items "order" (mapcar (lambda (entry) (format nil "(~d . ~d)" (car entry) (cdr entry))) order))
      (format t "~%")
      (print-items "runs" (mapcar (lambda (run)
                                    (destructuring-bind (first last pass) run
                                      (if (= first last)
                                          (format nil "~d (pass ~d)" first pass)
                                          (format nil "~d-~d (pass ~d)" first last pass))))
                                  (order-runs order)))
      (format t "~%")
      (check (order-runs order) '((0 16 1) (1 15 2) (17 21 1) (0 0 2) (1 15 3) (17 17 2)) "playback runs")
      (show (length order) 54)
      (show (score-duration view) 79/2 "whole notes of playback")
      (show (fset:size (score-parts view)) 1)
      (show (loop for measure in (fset:convert 'list (score-measure-map view))
                  never (or (measure-info-repeat-start measure) (measure-info-repeat-end measure)
                            (measure-info-ending measure) (measure-info-jump measure)))
            t "no repeat signs, endings or jumps left")
      (show (check-score view) nil)
      (format t "~%")
      (para "Each view bar is a copy of its source bar. Repeat signs, endings and jump marks"
            "are gone, since the view has nothing left to navigate. The first copy of an"
            "element keeps its id, and later copies get minted ids (the source's counter"
            "was 197). The id map sends every copy to its source, and PASSES gives its"
            "pass. So a playback cursor at a view onset knows the source element and the"
            "pass, which is enough for a score follower to highlight the note, or to pick"
            "the verse. The melody's onset -> element-id map at the turns of the walk:")
      (out "  view bar  source bar  pass  onset    view id  source id  note~%")
      (let ((none (make-hash-table)))
        (dolist (bar '(15 16 17 32 33 36 37 53))
          (dolist (event (fset:convert 'list (fset:lookup (voice-measures (voice-at view 0)) bar)))
            (let ((id (element-id event)))
              (out "  ~8d  ~10d  ~4d  ~7a  ~7a  ~9a  ~a~%" bar
                   (car (element-position *form* (fset:lookup id-map (measure-id view bar))))
                   (fset:lookup passes id)
                   (absolute-offset view (element-position view id))
                   id (fset:lookup id-map id) (event-text event none none))))))
      (format t "~%")
      (show (fset:lookup id-map (melody-id view 17 0)) (melody-id *form* 1 0) "view bar 17 is bar 1 again")
      (show (fset:lookup passes (melody-id view 38 0)) 3 "bar 1 for the third time, after the D.C.")
      (show (list (score-id-counter *form*) (score-id-counter view)) '(197 501))
      (show (score-equal (unfold view) view) t "unfolding a view gives the same measures again")))
  (format t "~%")
  (para "The hymn alone, with its repeat and two endings and no jump, unfolds to 33 bars:")
  (multiple-value-bind (view id-map passes) (unfold *hymn*)
    (show (order-runs (playback-order view id-map passes *hymn*)) '((0 16 1) (1 15 2) (17 17 1))
          "(first last pass)")))

;;; 12. Drafts: the score builder (0.3.0) ---------------------------------------------
;;; score-protocol 0.3.0 (SC3). The first line of NEW BRITAIN as a score of two
;;; parts, built in one MAKE-SCORE call: its tie and slurs name their notes
;;; through the DRAFT-REFs of a SCORE-BUILDER. A key names each element that a
;;; reference needs: (:MELODY bar j) a melody note, :MELODY and :BASS the parts.

(defparameter *duet-melody*
  (append (subseq *melody* 0 8) '((("D5" 1/2 :held) (:rest 1/4))))
  "The melody of the pickup and bars 1-8. The line ends on the held D5: the B4
that picks up the next line is a rest here.")

(defparameter *duet-tie* '((:melody 7 0) (:melody 8 0))
  "The D5 of bar 7 is tied to the D5 that starts bar 8.")

(defparameter *duet-slurs* '(((:melody 1 1) (:melody 1 2)) ((:melody 5 1) (:melody 5 2)))
  "The two melismas of the line, as the keys of their first and last notes.")

(defparameter *duet-prefer* '((:melody . "P1") (:bass . "P2"))
  "Preferred ids of the parts, as a reader would take them from its file.")

(defun duet-prefer (key) (cdr (assoc key *duet-prefer*)))

(defun duet-carrier-p (key)
  "True when the element of KEY is one a reference names: a part, or a note at
the end of the tie or of a slur."
  (and (or (assoc key *duet-prefer*)
           (equal key (second *duet-tie*))
           (some (lambda (slur) (member key slur :test #'equal)) *duet-slurs*))
       t))

(defun builder-functions (builder)
  "The CARRIER and TARGET functions of BUILD-DUET for BUILDER: both return the
ref of the key, with the part's preferred id."
  (flet ((ref (key) (builder-ref builder key :prefer (duet-prefer key))))
    (values #'ref #'ref)))

(defun duet-event (spec key lyrics carrier target)
  (destructuring-bind (pitch duration &rest flags) spec
    (if (eq pitch :rest)
        (make-rest-event :duration duration)
        (make-note-event :id (and (duet-carrier-p key) (funcall carrier key))
                         :pitch (parse-pitch pitch) :duration duration
                         :dynamic (dynamic-of flags)
                         :tie (and (equal key (first *duet-tie*))
                                   (make-tie :end (funcall target (second *duet-tie*))))
                         :lyrics lyrics))))

(defun build-duet (carrier target)
  "The first line of NEW BRITAIN as two parts, the melody (with verse 1) and the
bass, in one MAKE-SCORE call. CARRIER maps the key of an element that a
reference names to its :ID (a DRAFT-REF, an id string or NIL). TARGET maps a key
to what a tie's END or a slur's START or END holds for it."
  (let ((lyrics (hymn-lyrics (constantly nil))))
    (flet ((verse-1 (bar j)
             (remove 1 (gethash (list bar j) lyrics) :key #'lyric-verse :test-not #'eql))
           (voice (name staff measures)
             (make-voice :name name :staff staff :measures measures)))
      (make-score
       :metadata '(:title "Amazing Grace" :subtitle "NEW BRITAIN, the first line"
                   :composer "American folk melody, 1829" :lyricist "John Newton, 1779")
       :measures (loop for bar from 0 to 8
                       collect (case bar
                                 (0 (hymn-measure 0 (constantly nil)))
                                 (8 (make-measure-info :barline :final))
                                 (t (make-measure-info))))
       :parts (list (make-part
                     :id (funcall carrier :melody) :name "Melody"
                     :staves (list (make-staff :clef :treble))
                     :voices (list (voice "Melody" 1
                                          (loop for specs in *duet-melody*
                                                for bar from 0
                                                collect (loop for spec in specs
                                                              for j from 0
                                                              collect (duet-event spec (list :melody bar j)
                                                                                  (verse-1 bar j)
                                                                                  carrier target))))))
                    (make-part
                     :id (funcall carrier :bass) :name "Bass"
                     :staves (list (make-staff :clef :bass))
                     :voices (list (voice "Bass" 1 (mapcar (lambda (specs) (mapcar #'bass-event specs))
                                                           (subseq *bass* 0 9))))))
       :chord-symbols (loop for (position text) in *chords*
                            when (<= (car position) 8)
                              collect (make-chord-symbol-anchor :position position
                                                                :symbol (parse-chord-symbol text)))
       :spanners (loop for (start end) in *duet-slurs*
                       collect (make-spanner :kind :slur :start (funcall target start)
                                             :end (funcall target end)))))))

(defun duet-part (score n) (fset:lookup (score-parts score) n))
(defun duet-voice (score n) (first (part-voices (duet-part score n))))

(defun duet-element (score key)
  "The element of SCORE that KEY names."
  (if (consp key)
      (destructuring-bind (voice bar j) key
        (fset:lookup (fset:lookup (voice-measures (duet-voice score (if (eq voice :melody) 0 1)))
                                  (- bar (score-first-bar score)))
                     j))
      (duet-part score (if (eq key :melody) 0 1))))

(defun print-duet (score)
  "SCORE bar by bar: chord symbols, the melody part, verse 1 and the bass part."
  (multiple-value-bind (starts ends) (slur-ends score)
    (out "  bar  chords     ~19a verse 1        ~a~%"
         (format nil "melody (~a)" (element-id (duet-part score 0)))
         (format nil "bass (~a)" (element-id (duet-part score 1))))
    (loop for bar from (score-first-bar score) to (score-last-bar score)
          for i from 0
          for melody = (fset:convert 'list (fset:lookup (voice-measures (duet-voice score 0)) i))
          for bass = (fset:convert 'list (fset:lookup (voice-measures (duet-voice score 1)) i))
          do (out "  ~3d  ~10a ~19a ~14a ~{~a~^ ~}~%" bar
                  (format nil "~{~a~^ ~}"
                          (mapcar #'chord-text
                                  (remove bar (fset:convert 'list (score-chord-symbols score))
                                          :key (lambda (a) (car (anchor-position a))) :test-not #'eql)))
                  (format nil "~{~a~^ ~}" (mapcar (lambda (e) (event-text e starts ends)) melody))
                  (verse-text melody 1)
                  (mapcar (lambda (e) (event-text e starts ends)) bass)))))

(defun print-duet-ids (score)
  "Every id of SCORE in document order, grouped like PRINT-IDS-IN-ORDER: the
measure map, each part with its staff and voice, the melody bar by bar, the
bass, the chord symbols, the slurs."
  (let ((groups '()) (voice nil))
    (flet ((add (label id)
             (if (equal (car (first groups)) label)
                 (push id (cdr (first groups)))
                 (push (list label id) groups))))
      (dolist (element (score-elements score))
        (let ((id (element-id element)))
          (typecase element
            ((or measure-info tempo-mark) (add "measure map" id))
            (part (add (format nil "part ~a" (part-name element)) id))
            ((or staff voice) (when (voice-p element) (setf voice element))
             (add (car (first groups)) id))
            (chord-symbol-anchor (add "chord symbols" id))
            (spanner (add "slurs" id))
            (t (add (if (string= (voice-name voice) "Melody")
                        (format nil "  bar ~d" (car (element-position score id)))
                        "  bass")
                    id))))))
    (loop for (label . ids) in (reverse groups)
          do (print-wrapped label (reverse ids)))))

(defun voice-p (object) (typep object 'voice))

(defun all-strings-p (score)
  "True when every id, tie end and spanner end of SCORE is a string: a score
holds no DRAFT-REF."
  (every (lambda (element)
           (and (stringp (element-id element))
                (typecase element
                  (tie (stringp (tie-end element)))
                  (spanner (and (stringp (spanner-start element)) (stringp (spanner-end element))))
                  (t t))))
         (score-elements score)))

(defun ref-text (ref)
  "A DRAFT-REF as it prints, without its address."
  (strip-identities (code-text ref)))

(defun shared-part-ref (builder)
  "BUILD-DUET with BUILDER's refs, except that both parts carry the ref of :PART."
  (build-duet (lambda (key) (builder-ref builder (if (keywordp key) :part key)))
              (lambda (key) (builder-ref builder key))))

(defvar *duet* nil "The first line of NEW BRITAIN as two parts, built from a draft.")

(defun hymn-in-one-call ()
  "Section 1's hymn from one draft: the tie and the five slurs refer to their
notes through a builder, so MAKE-SCORE runs once."
  (let* ((b (make-score-builder))
         (tie (tie-notes))
         (runs (slur-runs))
         (carriers (cons (second tie) (reduce #'append runs))))
    (build-hymn :annotate (lambda (key)
                            (when (eq (first key) :note)
                              (let ((at (rest key)))
                                (append (when (member at carriers :test #'equal)
                                          (list :id (builder-ref b at)))
                                        (when (equal at (first tie))
                                          (list :tie (make-tie :end (builder-ref b (second tie)))))))))
                :spanners (loop for (start end) in runs
                                collect (make-spanner :kind :slur :start (builder-ref b start)
                                                      :end (builder-ref b end))))))

(defun demo-builder ()
  (para "score-protocol 0.3.0 adds drafts (slice SC3). Section 1 built the hymn in two"
        "steps, because a tie or a slur names its notes by id and MAKE-SCORE mints the"
        "ids. A reader of a file meets this in every score: a tie is written on a note"
        "before the note it ends on. A SCORE-BUILDER hands out DRAFT-REFs, placeholders"
        "for ids that are not minted yet. An element CARRIES a ref when the ref is its"
        ":ID (every constructor takes one), and a tie's END or a spanner's START and END"
        "REFER to it. MAKE-SCORE puts the minted ids in their place in the same call."
        ""
        "BUILDER-REF makes a ref on first use of a key and returns the same one after:")
  (let ((b (make-score-builder))
        (other (make-score-builder)))
    (show (ref-text (builder-ref b '(:melody 8 0))) "#<draft-ref (:melody 8 0)>")
    (show (eq (builder-ref b '(:melody 8 0)) (builder-ref b (list :melody 8 0))) t
          "keys compare with EQUAL")
    (show (eq (builder-ref b :melody) (builder-ref other :melody)) nil
          "another builder, another ref")
    (show (draft-ref-prefer (builder-ref b :melody :prefer "P1")) "P1")
    (show (draft-ref-prefer (builder-ref b :melody)) "P1" "PREFER NIL keeps it")
    (show-signal (builder-ref b :bass :prefer "1st") type-error)
    (show (builder-id b :melody :errorp nil) nil "no score has carried it yet")
    (show-signal (builder-id b :melody) unresolved-reference))
  (format t "~%")
  (para "A score of two parts: the first line of NEW BRITAIN, the pickup and bars 1-8,"
        "with the melody and verse 1 in part \"Melody\" and the bass line in part"
        "\"Bass\". The line ends on the held D5 (the B4 that picks up the next line is"
        "a rest here). BUILD-DUET writes the draft in document order. The D5 of bar 7"
        "is tied forward to a note that does not exist yet, so its tie refers to a ref;"
        "the D5 of bar 8, built after it, carries that ref. Each slur refers to two"
        "refs, and the parts carry refs that prefer the ids P1 and P2, as a reader"
        "would take them from a file. The pieces of the draft:")
  (let ((b (make-score-builder)))
    (shown (make-note-event :pitch (parse-pitch "D5") :duration 3/4
                            :tie (make-tie :end (builder-ref b '(:melody 8 0)))
                            :lyrics (list (make-lyric :text "me!" :extender t))))
    (shown (make-note-event :id (builder-ref b '(:melody 8 0)) :pitch (parse-pitch "D5")
                            :duration 1/2))
    (shown (make-spanner :kind :slur :start (builder-ref b '(:melody 1 1))
                         :end (builder-ref b '(:melody 1 2))))
    (para "and (MAKE-PART :ID (BUILDER-REF B :MELODY :PREFER \"P1\") ...) for the melody part."
          "One call builds the score:")
    (shown (setf *duet* (multiple-value-call #'build-duet (builder-functions b))))
    (print-duet *duet*)
    (format t "~%")
    (show (check-score *duet*) nil)
    (show (all-strings-p *duet*) t "every id, tie end and slur end is a string: no ref is left")
    (show (length (score-elements *duet*)) 71)
    (format t "~%")
    (para "Every id in document order. The carriers of P1 and P2 took their preferred"
          "ids. Every other element was minted, counting up in document order: unlike"
          "section 2, where the tie got serial 170, the tie here gets the serial of its"
          "place in bar 7.")
    (print-duet-ids *duet*)
    (format t "~%")
    (let* ((ids (mapcar #'element-id (score-elements *duet*)))
           (minted (remove-if-not (lambda (id) (find #\- id)) ids)))
      (show (remove-if (lambda (id) (find #\- id)) ids) '("P1" "P2") "the preferred ids")
      (show (equal (mapcar #'id-serial minted) (loop for i from 1 to 69 collect i)) t
            "the 69 minted ids have serials 1 to 69, in document order")
      (show (score-id-counter *duet*) 69 "P1 and P2 have no serial: the counter ignores them"))
    (format t "~%")
    (para "Each ref, its key, the id its carrier got (BUILDER-ID) and what refers to it."
          "The tie (an element of bar 7) comes before the note it ends on:")
    (let ((tie (note-tie (duet-element *duet* '(:melody 7 0)))))
      (out "  key            id     carrier            referred to by~%")
      (dolist (key (list :melody :bass '(:melody 1 1) '(:melody 1 2) '(:melody 5 1) '(:melody 5 2)
                         '(:melody 8 0)))
        (let ((id (builder-id b key)))
          (out "  ~14a ~6a ~18a ~a~%" (code-text key) id
               (if (keywordp key) (format nil "part ~s" (part-name (find-element *duet* id)))
                   (id-what *duet* id))
               (or (loop for spanner in (fset:convert 'list (score-spanners *duet*))
                         when (or (equal id (spanner-start spanner)) (equal id (spanner-end spanner)))
                           return (format nil "slur ~a ~:[(end)~;(start)~]" (element-id spanner)
                                          (equal id (spanner-start spanner))))
                   (and (equal id (tie-end tie)) (format nil "tie ~a in bar 7" (element-id tie)))
                   "-"))
          (check (element-id (duet-element *duet* key)) id (format nil "builder-id ~s" key))))
      (format t "~%")
      (show (tie-end tie) (builder-id b '(:melody 8 0)))
      (show (< (id-serial (element-id tie)) (id-serial (tie-end tie))) t
            "the tie points forward in document order")))
  (format t "~%")
  (para "The builder file's promise: MAKE-SCORE of a draft gives the score a caller would"
        "get by building twice. The first build gives each carrier its preferred id (or"
        "NIL) and every reference the placeholder \"zz\", to read off the minted ids. The"
        "second build sets each reference to the id its note got in the first:")
  (let* ((placeholders (build-duet #'duet-prefer (constantly "zz")))
         (resolved (build-duet #'duet-prefer
                               (lambda (key) (element-id (duet-element placeholders key))))))
    (show (tie-end (note-tie (duet-element placeholders '(:melody 7 0)))) "zz")
    (show (mapcar #'score-issue-kind (check-score placeholders)) '(:spanner-endpoint :spanner-endpoint :tie-target)
          "two slurs and a tie to nothing")
    (show (score-equal resolved *duet*) t)
    (show (equal (mapcar #'element-id (score-elements resolved))
                 (mapcar #'element-id (score-elements *duet*)))
          t "the same ids in the same order"))
  (format t "~%")
  (para "Preferred ids. A given id always wins, the first carrier in document order wins"
        "a preferred id, and a preferred id that has a serial raises the counter as a"
        "given one does:")
  (flet ((part-ids (score) (list (element-id (duet-part score 0)) (element-id (duet-part score 1)))))
    (let* ((b (make-score-builder))
           (given (build-duet (lambda (key) (if (eq key :bass) "P1" (builder-ref b key :prefer (duet-prefer key))))
                              (lambda (key) (builder-ref b key)))))
      (show (part-ids given) '("p-b" "P1") "the bass is given P1, so the melody's part is minted"))
    (let* ((b (make-score-builder))
           (both (build-duet (lambda (key) (builder-ref b key :prefer (if (keywordp key) "P1" nil)))
                             (lambda (key) (builder-ref b key)))))
      (show (part-ids both) '("P1" "p-1b") "both prefer P1: the melody comes first"))
    (let* ((b (make-score-builder))
           (high (build-duet (lambda (key) (builder-ref b key :prefer (if (eq key :melody) "p-zz" (duet-prefer key))))
                             (lambda (key) (builder-ref b key)))))
      (show (part-ids high) '("p-zz" "P2"))
      (show (element-id (first (score-elements high))) "m-100"
            "p-zz is serial 1295, so minting starts at 1296, even before the part")
      (show (score-id-counter high) (+ 1295 69))))
  (format t "~%")
  (para "Refs that can't be resolved signal UNRESOLVED-REFERENCE. A ref that no element"
        "carries is signalled once per ref, in document order, inside a USE-VALUE"
        "restart whose value, an id string, stands for it. Here COPY-SCORE adds a slur"
        "over bar 3 whose ends are refs nobody carries, and a handler answers with the"
        "ids of the notes:")
  (let ((b (make-score-builder))
        (keys '()))
    (flet ((slur-over-bar-3 ()
             (copy-score *duet*
                         :spanners (append (fset:convert 'list (score-spanners *duet*))
                                           (list (make-spanner :kind :slur
                                                               :start (builder-ref b '(:melody 3 0))
                                                               :end (builder-ref b '(:melody 3 1))))))))
      (show-signal (slur-over-bar-3) unresolved-reference)
      (let ((score (handler-bind ((unresolved-reference
                                    (lambda (c)
                                      (push (unresolved-reference-key c) keys)
                                      (use-value (element-id (duet-element *duet* (unresolved-reference-key c)))
                                                 c))))
                     (slur-over-bar-3))))
        (show (reverse keys) '((:melody 3 0) (:melody 3 1)) "signalled in document order")
        (show (fset:size (score-spanners score)) 3)
        (show (check-score score) nil)
        (show (builder-id b '(:melody 3 0) :errorp nil) nil
              "no element carried that ref, so the builder has no id for it"))))
  (format t "~%")
  (para "A ref carried by two elements is an error with no restart, and a build that"
        "signals records no ids. SHARED-PART-REF builds the duet with both parts"
        "carrying the ref of the key :PART:")
  (let ((b (make-score-builder)))
    (show-signal (shared-part-ref b) unresolved-reference)
    (show (handler-case (shared-part-ref b)
            (unresolved-reference (c) (and (find-restart 'use-value c) t)))
          nil "no USE-VALUE here")
    (show (builder-id b '(:melody 8 0) :errorp nil) nil "nothing recorded, although that note was fine"))
  (format t "~%")
  (para "Refs are for building. An edit whose elements hold a ref is refused before its"
        "other preconditions:")
  (let ((b (make-score-builder)))
    (show-signal (apply-edit *duet* (make-add-mark :target (element-id (duet-element *duet* '(:melody 8 0)))
                                                   :mark (make-fermata :id (builder-ref b :fermata))))
                 invalid-edit))
  (format t "~%")
  (para "An edit on the built score. ADD-MARK puts a fermata on the held D5 that ends"
        "the line (its id from BUILDER-ID would do as well). The change set names one"
        "element, and every subtree the edit does not touch is the old object (EQ):"
        "the whole bass part, the measure map, the chord symbols, the slurs, the"
        "melody's staff and its voice measures for bars 0-7. Only the path down to the"
        "note is rebuilt: part P1, its voice, bar 8's voice measure and the note.")
  (let* ((before *duet*)
         (target (element-id (duet-element before '(:melody 8 0)))))
    (shown (apply-edit *duet* (make-add-mark :target target :mark (make-fermata))))
    (multiple-value-bind (after changes) (apply-edit before (make-add-mark :target target :mark (make-fermata)))
      (out "  bar 8  before  ~a~%         after   ~a~%~%"
           (bar-of-duet before 8) (bar-of-duet after 8))
      (print-change-set before after changes)
      (report-sharing before after)
      (format t "~%")
      (flet ((all-eq (a b) (and (= (length a) (length b)) (every #'eq a b) t))
             (seq (s) (fset:convert 'list s)))
        (let ((p1 (duet-part before 0)) (p1* (duet-part after 0)))
          (out "  subtree                                          EQ~%")
          (dolist (row (list (list "part P2, the bass, and everything in it"
                                   (eq (duet-part before 1) (duet-part after 1)) t)
                             (list "the measure map (9 measures, 1 tempo mark)"
                                   (all-eq (seq (score-measure-map before)) (seq (score-measure-map after))) t)
                             (list "the 8 chord symbols"
                                   (all-eq (seq (score-chord-symbols before)) (seq (score-chord-symbols after))) t)
                             (list "the 2 slurs"
                                   (all-eq (seq (score-spanners before)) (seq (score-spanners after))) t)
                             (list "P1's staff"
                                   (all-eq (part-staves p1) (part-staves p1*)) t)
                             (list "P1's voice measures for bars 0-7"
                                   (all-eq (subseq (seq (voice-measures (duet-voice before 0))) 0 8)
                                           (subseq (seq (voice-measures (duet-voice after 0))) 0 8))
                                   t)
                             (list "part P1" (eq p1 p1*) nil)
                             (list "P1's voice" (eq (duet-voice before 0) (duet-voice after 0)) nil)
                             (list "bar 8's voice measure"
                                   (eq (fset:lookup (voice-measures (duet-voice before 0)) 8)
                                       (fset:lookup (voice-measures (duet-voice after 0)) 8))
                                   nil)
                             (list "the D5 itself"
                                   (eq (find-element before target) (find-element after target)) nil)))
            (destructuring-bind (label value expected) row
              (out "  ~48a ~a~%" label (if value "T" "NIL"))
              (check value expected label))))
        (format t "~%")
        (show (list (change-set-created changes) (change-set-removed changes) (change-set-modified changes))
              (list (list (element-id (event-fermata (find-element after target)))) '() (list target)))
        (show (score-id-counter after) 70 "the fermata took the next serial"))))
  (format t "~%")
  (para "Section 1's hymn in one call. With a builder, the tie and the five slurs go"
        "into the draft, and MAKE-SCORE runs once (HYMN-IN-ONE-CALL):")
  (let* ((one (hymn-in-one-call))
         (ids (mapcar #'element-id (score-elements one))))
    (show (length ids) 175)
    (show (equal (mapcar #'id-serial ids) (loop for i from 1 to 175 collect i)) t
          "serials 1 to 175 in document order")
    (show (element-id (note-tie (melody-at one 7 0))) "ti-1y" "serial 70, in its place in bar 7")
    (show (element-id (note-tie (melody-at *hymn* 7 0))) "ti-4q" "two steps: serial 170")
    (show (equal (mapcar #'type-of (score-elements one)) (mapcar #'type-of (score-elements *hymn*))) t
          "the same elements in the same order")
    (show (score-equal one *hymn*) nil "only the ids differ")
    (show (check-score one) nil))
  (format t "~%")
  (para "Two smaller changes in 0.3.0. ABSOLUTE-OFFSET says what is wrong with a"
        "position (section 3 shows the first two; 0.2.0 said \"outside the score\" for"
        "all three):")
  (show-signal (absolute-offset *hymn* '(0 . 1/2)) invalid-position)
  (show-signal (absolute-offset *hymn* '(18 . 0)) invalid-position)
  (show-signal (absolute-offset *hymn* '(3 . -1/4)) invalid-position)
  (para ""
        "LOSSY-CONVERSION has a direction, :TO when a score is written (the default)"
        "and :FROM when a file is read, and its report names it:")
  (show (princ-to-string (make-condition 'lossy-conversion :format :midi :what "ids"))
        "lossy conversion to midi: ids dropped")
  (show (princ-to-string (make-condition 'lossy-conversion :format :musicxml :direction :from
                                                           :what "a trill"))
        "lossy conversion from musicxml: a trill dropped"))

(defun bar-of-duet (score bar)
  "The events of BAR in the melody part of the duet SCORE."
  (multiple-value-bind (starts ends) (slur-ends score)
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (event) (event-text event starts ends))
                    (fset:convert 'list (fset:lookup (voice-measures (duet-voice score 0))
                                                     (- bar (score-first-bar score))))))))

;;; Running -------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()))
    (format t "~&score-protocol ~a (slices SC0, the score model; SC1, edits and views; SC3, drafts and the score builder) on music-theory ~a~%"
            (asdf:component-version (asdf:find-system "score-protocol"))
            (asdf:component-version (asdf:find-system "music-theory")))
    (section "1. Building a score: a hymn tune in code" (demo-build))
    (section "2. Element ids" (demo-ids))
    (section "3. Bars and positions" (demo-positions))
    (section "4. Validation: CHECK-SCORE and VALIDATE-SCORE" (demo-validation))
    (section "5. Provenance and confidence" (demo-provenance))
    (section "6. The canonical s-expression form" (demo-sexp))
    (section "7. Editing: operations as values" (demo-editing))
    (section "8. Undo, composition and the canonical form of operations" (demo-undo))
    (section "9. Re-barring with SET-TIME" (demo-rebarring))
    (section "10. Transposition" (demo-transposition))
    (section "11. Views: EXTRACT-PART, CONDENSED-VIEW and UNFOLD" (demo-views))
    (section "12. Drafts: the score builder and forward references (0.3.0)" (demo-builder))
    (banner "Summary")
    (format t "~d results checked against expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
