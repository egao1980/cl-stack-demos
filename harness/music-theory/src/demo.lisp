(in-package #:harness-demos-music-theory)

;;;; A short tour of music-theory 0.4.0. Sections 1-6 cover slice MT0 (spelled
;;;; pitches, the line of fifths and intervals), sections 7-11 slice MT1 (meters,
;;;; the metric hierarchy, dots and tuplets, tie splitting, tempo), sections 12-16
;;;; slice MT2 (keys and scales, scale degrees, Roman numerals, mixture, a
;;;; progression), sections 17-22 slice MT3 (chord symbols: a chart, dialects,
;;;; Unicode and slash chords, essential tones, transposition, errors). Every
;;;; result with a known answer is also checked: RUN-DEMOS returns NIL when one
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

(defun code-text (object)
  "OBJECT printed as it reads in this package, which uses MUSIC-THEORY."
  (let ((*package* (find-package '#:harness-demos-music-theory))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 1000))
    (prin1-to-string object)))

(defun indent (text columns)
  "TEXT with COLUMNS spaces added after each newline, for printing it at that column."
  (with-output-to-string (s)
    (loop for char across text
          do (write-char char s)
             (when (char= char #\Newline)
               (dotimes (i columns) (write-char #\Space s))))))

(defun value-text (value)
  "VALUE as printed, with the name of a pitch, pitch class, interval, meter, key or
Roman numeral after it."
  (let ((name (typecase value
                (spelled-pitch (pitch-name value))
                (pitch-class-name (pitch-class-string value))
                (interval (ignore-errors (interval-name value)))
                (meter (meter-string value))
                (key (key-name value))
                (roman-numeral (ignore-errors (roman-name value)))
                (chord-symbol (ignore-errors (print-chord-symbol value))))))
    (if name
        (format nil "~a   ; ~a" (code-text value) name)
        (code-text value))))

(defun same-p (a b)
  "EQUALP, except that strings compare case-sensitively (\"m3\" is not \"M3\")."
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
  (let ((ftext (indent (code-text form) 2))
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

(defmacro show-signal (form &key (checked t))
  "Print FORM and the MUSIC-THEORY-ERROR it signals. When CHECKED, count a
mismatch if it returns instead."
  `(handler-case (progn ,form
                        (format t "~&  ~a  (no condition)~%" (code-text ',form))
                        ,@(when checked `((check :returned :signalled (code-text ',form)))))
     (music-theory-error (c)
       (format t "~&  ~a~%    -> ~a~%" (code-text ',form) c))))

(defmacro section (title &body body)
  "Print a banner, run BODY, and record the section as failed if it signals."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (e)
         (push ,title *failed-sections*)
         (format t "~&!! unexpected ~s, the rest of this section is skipped:~%   ~a~%"
                 (type-of e) e)))))

;;; Helpers -------------------------------------------------------------------

(defun all-spellings ()
  "The 35 spelled pitch classes with at most two accidentals, in line-of-fifths order."
  (loop for fifths from -15 to 19 collect (fifths-pitch-class fifths)))

(defun at-octave (pitch-class octave)
  "The SPELLED-PITCH of PITCH-CLASS in OCTAVE."
  (make-spelled-pitch :letter (pitch-class-name-letter pitch-class)
                      :alter (pitch-class-name-alter pitch-class)
                      :octave octave))

(defun names (pitches &optional (accidentals :ascii))
  (mapcar (lambda (p) (pitch-name p :accidentals accidentals)) pitches))

(defun quality-text (interval)
  (string-downcase (symbol-name (interval-quality interval))))

(defun pitches (&rest texts)
  (mapcar #'parse-pitch texts))

;;; 1. Spelled pitches ----------------------------------------------------------

(defparameter *pitch-inputs*
  ;; (text ascii-name midi)
  '(("F#4" "F#4" 66) ("Bbb3" "Bbb3" 57) ("C-1" "C-1" 0) ("C##5" "C##5" 74)
    ("Fx5" "F##5" 79) ("F♯4" "F#4" 66) ("B𝄫3" "Bbb3" 57) ("e♭4" "Eb4" 63)
    (" ab5 " "Ab5" 80) ("B-1" "B-1" 11) ("G9" "G9" 127)))

(defun demo-pitches ()
  (para "A SPELLED-PITCH is a value: LETTER 0-6 (C D E F G A B), ALTER -2..2 (double flat"
        "to double sharp) and a scientific OCTAVE. It is an immutable struct compared with"
        "EQUALP. PARSE-PITCH reads a name and PITCH-NAME prints one, in ASCII by default.")
  (show (parse-pitch "F#4") (make-spelled-pitch :letter 3 :alter 1 :octave 4))
  (show (pitch-name (make-spelled-pitch :letter 6 :alter -2 :octave 3)) "Bbb3")
  (show (pitch-name (parse-pitch "F#4") :accidentals :unicode) "F♯4")
  (show (pitch-name (parse-pitch "Bbb3") :accidentals :unicode) "B𝄫3")
  (terpri)
  (para "The parser takes # b ## bb x and the Unicode signs ♯ ♭ 𝄪 𝄫, either letter case and"
        "surrounding blanks. \"-\" is never a flat: in \"B-1\" it starts octave -1.")
  (format t "  ~12a~7@a~6@a~7@a   ~8a~9a~4@a~%" "text" "letter" "alter" "octave"
          "ASCII" "Unicode" "MIDI")
  (loop for (text ascii midi) in *pitch-inputs*
        for p = (parse-pitch text)
        do (format t "  ~12a~7d~6d~7d   ~8a~9a~4d~%" (prin1-to-string text)
                   (spelled-pitch-letter p) (spelled-pitch-alter p) (spelled-pitch-octave p)
                   (pitch-name p) (pitch-name p :accidentals :unicode) (pitch-midi p))
           (check (pitch-name p) ascii (format nil "name of ~s" text))
           (check (pitch-midi p) midi (format nil "MIDI of ~s" text)))
  (let ((total 0) (ok 0))
    (dolist (pc (all-spellings))
      (loop for octave from -1 to 9
            for p = (at-octave pc octave)
            for ascii = (pitch-name p)
            do (incf total)
               (when (and (string= ascii (pitch-name (parse-pitch ascii)))
                          (equalp p (parse-pitch (pitch-name p :accidentals :unicode))))
                 (incf ok))))
    (format t "~%Round trip, 35 spellings x octaves -1..9: ~d of ~d names parse back to the same~%~
               pitch and print the same, in ASCII and in Unicode.~%~%" ok total)
    (check ok total "round trips"))
  (let* ((p (parse-pitch "Bbb3"))
         (text (with-standard-io-syntax (prin1-to-string p)))
         (back (let ((*read-eval* nil)) (read-from-string text))))
    (para "Values print readably, so they can be written out and read back (or used as keys"
          "in EQUALP hash tables):"
          (format nil "  ~a" text)
          (format nil "  read back, EQUALP to the original: ~a" (equalp back p)))
    (check (equalp back p) t "read back"))
  (para "A PITCH-CLASS-NAME is the spelling without an octave.")
  (show (parse-pitch-class "Eb") (make-pitch-class-name :letter 2 :alter -1))
  (show (pitch-spelling (parse-pitch "C##5")) (parse-pitch-class "C##"))
  (show (pitch-class-string (parse-pitch-class "C##") :accidentals :unicode) "C𝄪"))

;;; 2. MIDI numbers and the scientific octave ---------------------------------

(defparameter *midi-rows*
  ;; (name midi pitch-class diatonic-index)
  '(("C-1" 0 0 -7) ("A0" 21 9 5) ("B3" 59 11 27) ("B#3" 60 0 27) ("C4" 60 0 28)
    ("Dbb4" 60 0 29) ("Cb4" 59 11 28) ("A4" 69 9 33) ("G9" 127 7 67)))

(defun demo-midi ()
  (para "PITCH-MIDI is 12 x (octave + 1) + the letter's semitones + ALTER, so C4 = 60 and"
        "C-1 = 0. The octave number belongs to the letter, not to the sound: B#3 sounds as"
        "C4 and Cb4 sounds as B3. PITCH-DIATONIC-INDEX (letter + 7 x octave) is the staff"
        "position, which ignores accidentals; PITCH-CLASS is the unspelled class 0-11.")
  (format t "  ~6a~6@a~8@a~10@a~%" "pitch" "MIDI" "class" "staff")
  (loop for (name midi pc index) in *midi-rows*
        for p = (parse-pitch name)
        do (format t "  ~6a~6d~8d~10d~%" name (pitch-midi p) (pitch-class p)
                   (pitch-diatonic-index p))
           (check (list (pitch-midi p) (pitch-class p) (pitch-diatonic-index p))
                  (list midi pc index) name))
  (terpri)
  (para "B#3, C4 and Dbb4 are one key on a keyboard but three different notes on paper:")
  (show (pitch-midi (parse-pitch "B#3")) 60)
  (show (pitch-midi (parse-pitch "C4")) 60)
  (show (equalp (parse-pitch "B#3") (parse-pitch "C4")) nil)
  (show (pitch-enharmonic-p (parse-pitch "B#3") (parse-pitch "C4")) t)
  (show (interval-name (interval-between (parse-pitch "C4") (parse-pitch "B#3"))) "-d2")
  (terpri)
  (para "Two pitches are enharmonic when their MIDI numbers are equal. Once a pitch class is"
        "involved, only the pitch classes are compared:")
  (show (pitch-enharmonic-p (parse-pitch "F#4") (parse-pitch "Gb4")) t)
  (show (pitch-enharmonic-p (parse-pitch "F#4") (parse-pitch "Gb5")) nil)
  (show (pitch-enharmonic-p (parse-pitch "F#4") (parse-pitch-class "Gb")) t))

;;; 3. The line of fifths -----------------------------------------------------

(defun demo-fifths ()
  (para "Every spelling has an integer position on the line of fifths: C 0, G 1, D 2, and"
        "F -1, Bb -2 the other way. A sharp adds 7 and a flat subtracts 7. With at most two"
        "accidentals the line runs from Fbb (-15) to B## (19), 35 spellings."
        "FIFTHS-PITCH-CLASS gives the spelling at a position, PITCH-FIFTHS the position.")
  (loop for label in '("double flats" "flats" "naturals" "sharps" "double sharps")
        for start from -15 by 7
        do (out "  ~14a~{~4d ~4a~}~%" label
                (loop for f from start below (+ start 7)
                      collect f
                      collect (pitch-class-string (fifths-pitch-class f)))))
  (format t "~%  ~{~a~^ ~}~%~%"
          (mapcar (lambda (pc) (pitch-class-string pc :accidentals :unicode)) (all-spellings)))
  (let ((ok (loop for f from -15 to 19
                  for pc = (fifths-pitch-class f)
                  count (and (= f (pitch-fifths pc))
                             (= (pitch-class pc) (mod (* 7 f) 12))))))
    (format t "All 35 positions: (pitch-fifths (fifths-pitch-class n)) = n, and the pitch class~%~
               is 7n mod 12 (~d of 35).~%~%" ok)
    (check ok 35 "line of fifths"))
  (para "Twelve steps along the line give the same pitch class again, spelled differently:"
        "the line never closes into a circle on paper (B# at 12 is not C at 0). Within two"
        "accidentals each pitch class has three spellings, except G#/Ab with two.")
  (format t "  ~5a  ~a~%" "class" "spellings (position)")
  (dotimes (pc 12)
    (let ((group (remove pc (all-spellings) :key #'pitch-class :test-not #'=)))
      (out "  ~5d  ~{~16a~}~%" pc
              (mapcar (lambda (s) (format nil "~a (~d)" (pitch-class-string s) (pitch-fifths s)))
                      group))
      (check (length group) (if (= pc 8) 2 3) (format nil "spellings of class ~d" pc))))
  (terpri)
  (para "An interval is a fixed distance on the line. From C up to each spelling within the"
        "octave:")
  (let* ((c4 (parse-pitch "C4"))
         (row (loop for d from -7 to 7
                    for to = (at-octave (fifths-pitch-class d) 4)
                    collect (list d (pitch-class-string (pitch-spelling to))
                                  (interval-name (interval-between c4 to))))))
    (out "  distance ~{~4d~}~%  to       ~{~4@a~}~%  interval ~{~4@a~}~%~%"
            (mapcar #'first row) (mapcar #'second row) (mapcar #'third row))
    (check (mapcar #'third row)
           '("d1" "d5" "m2" "m6" "m3" "m7" "P4" "P1" "P5" "M2" "M6" "M3" "M7" "A4" "A1")
           "intervals from C"))
  ;; From every spelling to every spelling, placed 0-6 steps above: the distance
  ;; on the line and the simple interval name determine each other.
  (let ((by-distance (make-hash-table))
        (by-name (make-hash-table :test #'equal))
        (pairs 0) (named 0))
    (dolist (from (all-spellings))
      (dolist (to (all-spellings))
        (incf pairs)
        (let* ((p (at-octave from 4))
               (q (at-octave to (if (>= (pitch-class-name-letter to)
                                        (pitch-class-name-letter from))
                                    4 5)))
               (name (handler-case (interval-name (interval-between p q))
                       (invalid-interval () nil)))
               (d (- (pitch-fifths to) (pitch-fifths from))))
          (when name
            (incf named)
            (pushnew name (gethash d by-distance) :test #'string=)
            (pushnew d (gethash name by-name))))))
    (let ((one-to-one (and (loop for v being the hash-values of by-distance
                                 always (= 1 (length v)))
                           (loop for v being the hash-values of by-name
                                 always (= 1 (length v))))))
      (format t "All ~d ordered pairs of the 35 spellings, the second placed 0-6 steps above the~%~
                 first: ~d have a named interval, with ~d distinct names. ~:[Some distance names~%~
                 two intervals.~;Each distance names one~%~
                 interval and each interval has one distance.~]~%"
              pairs named (hash-table-count by-name) one-to-one)
      (check one-to-one t "distance <-> interval"))))

;;; 4. Intervals ----------------------------------------------------------------

(defparameter *interval-rows*
  ;; (from to name)
  '(("Eb4" "A4" "A4") ("C4" "G5" "P12") ("C4" "E4" "M3") ("E4" "C4" "-M3")
    ("B3" "F4" "d5") ("F4" "B4" "A4") ("C4" "C5" "P8") ("D4" "F#5" "M10")
    ("E4" "D#4" "-m2") ("C4" "Cb4" "d1") ("C4" "C#4" "A1") ("G#3" "Fb4" "dd7")
    ("A4" "C3" "-M13")))

(defun demo-intervals ()
  (para "An INTERVAL is STEPS (signed diatonic steps; compound intervals are kept, so P12 is"
        "not P5) plus SEMITONES. Quality and number are derived, never stored."
        "INTERVAL-BETWEEN measures from the first pitch to the second.")
  (show (interval-between (parse-pitch "Eb4") (parse-pitch "A4"))
        (make-interval :steps 3 :semitones 6))
  (show (interval-name (interval-between (parse-pitch "C4") (parse-pitch "G5"))) "P12")
  (terpri)
  (format t "  ~11a~6@a~6@a  ~6a~18a~4@a~5@a  ~9a~a~%"
          "from  to" "steps" "semis" "name" "quality" "num" "dir" "compound" "simple")
  (loop for (from to name) in *interval-rows*
        for i = (interval-between (parse-pitch from) (parse-pitch to))
        do (format t "  ~11a~6d~6d  ~6a~18a~4d~5d  ~9a~a~%"
                   (format nil "~a ~a" from to)
                   (interval-steps i) (interval-semitones i) (interval-name i)
                   (quality-text i) (interval-number i) (interval-direction i)
                   (if (interval-compound-p i) "yes" "no")
                   (interval-name (simple-interval i)))
           (check (interval-name i) name (format nil "~a to ~a" from to)))
  (terpri)
  (para "A unison has no direction and is read as ascending, so C4 to Cb4 is a diminished"
        "unison (d1), not a descending augmented one. PARSE-INTERVAL accepts exactly what"
        "INTERVAL-NAME prints: an optional \"-\", a quality (dd d m M P A AA) and a number.")
  (show (parse-interval "-P5") (make-interval :steps -4 :semitones -7))
  (show (parse-interval "AA4") (make-interval :steps 3 :semitones 7))
  (show (interval-semitones (parse-interval "dd7")) 8)
  (let ((ok 0) (ascending 0) (descending 0) (rejected '()))
    (dolist (quality '("dd" "d" "m" "P" "M" "A" "AA"))
      (loop for number from 1 to 15
            for text = (format nil "~a~d" quality number)
            for i = (handler-case (parse-interval text) (invalid-interval () nil))
            do (if (null i)
                   (push text rejected)
                   (progn
                     (incf ascending)
                     (when (string= text (interval-name i)) (incf ok))
                     (when (> number 1)
                       (incf descending)
                       (when (string= (format nil "-~a" text)
                                      (interval-name (parse-interval (format nil "-~a" text))))
                         (incf ok)))))))
    (setf rejected (reverse rejected))
    (format t "~%Numbers 1-15 with each quality: ~d names (~d ascending, ~d descending) parse and~%~
               print back unchanged. The ~d that put m or M on a unison, fourth, fifth or octave~%~
               (or their compounds), or P on any other number, are rejected:~%  ~{~a~^ ~}~%~%"
            ok ascending descending (length rejected) rejected)
    (check (list ok ascending descending (length rejected)) '(161 83 78 22) "name round trips"))
  (para "SIMPLE-INTERVAL removes whole octaves while the interval is wider than one (an"
        "octave itself stays). INVERT-INTERVAL complements the simple interval within the"
        "octave: the numbers add up to 9, M and m swap, A and d swap, P stays P."
        "REVERSE-INTERVAL turns the direction around.")
  (format t "  ~8a~10a~12a~a~%" "name" "simple" "inversion" "reversal")
  (loop for (name simple inversion reversal)
          in '(("P12" "P5" "P4" "-P12") ("P15" "P8" "P1" "-P15") ("P8" "P8" "P1" "-P8")
               ("-M9" "-M2" "-m7" "M9") ("M10" "M3" "m6" "-M10") ("M3" "M3" "m6" "-M3")
               ("m3" "m3" "M6" "-m3") ("A4" "A4" "d5" "-A4") ("M7" "M7" "m2" "-M7")
               ("P1" "P1" "P8" "P1") ("A1" "A1" "d8" "d1") ("AA2" "AA2" "dd7" "-AA2"))
        for i = (parse-interval name)
        for row = (list (interval-name (simple-interval i))
                        (interval-name (invert-interval i))
                        (interval-name (reverse-interval i)))
        do (format t "  ~8a~10a~12a~a~%" name (first row) (second row) (third row))
           (check row (list simple inversion reversal) name))
  (let ((total 0) (ok 0))
    (dolist (quality '("dd" "d" "m" "P" "M" "A" "AA"))
      (loop for number from 1 to 8
            for i = (handler-case (parse-interval (format nil "~a~d" quality number))
                      (invalid-interval () nil))
            when i
              do (incf total)
                 (let ((inv (invert-interval i)))
                   (when (and (= 9 (+ (interval-number i) (interval-number inv)))
                              (= 12 (+ (interval-semitones i) (interval-semitones inv)))
                              (equalp i (invert-interval inv)))
                     (incf ok)))))
    (format t "~%All ~d simple ascending intervals: numbers add up to 9, semitones to 12, and~%~
               inverting twice gives the interval back (~d of ~d).~%~%" total ok total)
    (check ok total "inversions"))
  (para "Reversing A1 gives d1 because a unison is read as ascending. INTERVAL-BETWEEN agrees:")
  (show (interval-name (interval-between (parse-pitch "C#4") (parse-pitch "C4"))) "d1"))

;;; 5. Transposition keeps the spelling -----------------------------------------

(defun scale-row (label pitches)
  (out "  ~18a~{~6a~}~%" label (names pitches)))

(defun demo-transpose ()
  (para "TRANSPOSE-PITCH moves the letter and octave by STEPS and lets the accidental make up"
        "the SEMITONES. The spelling is exact: the same key on the keyboard gets a different"
        "name after a different interval.")
  (show (pitch-name (transpose-pitch (parse-pitch "F#4") (parse-interval "M2"))) "G#4")
  (show (pitch-name (transpose-pitch (parse-pitch "F#4") (parse-interval "d3"))) "Ab4")
  (show (pitch-name (transpose-pitch (parse-pitch "C4") (parse-interval "A1"))) "C#4")
  (show (pitch-name (transpose-pitch (parse-pitch "C4") (parse-interval "m2"))) "Db4")
  (show (pitch-name (transpose-pitch (parse-pitch "D4") (parse-interval "-P5"))) "G3")
  (show (pitch-name (transpose-pitch (parse-pitch "Bb3") (parse-interval "P12"))) "F5")
  (terpri)
  (para "The C major scale up an augmented fourth is F# major, with E# as its seventh degree"
        "(one letter per step). Up a diminished fifth it is Gb major, with Cb. The two rows"
        "sound the same note by note but share no spelling, and the steps between neighbours"
        "are the same in all three rows.")
  (let* ((c-major (pitches "C4" "D4" "E4" "F4" "G4" "A4" "B4" "C5"))
         (up-a4 (mapcar (lambda (p) (transpose-pitch p (parse-interval "A4"))) c-major))
         (up-d5 (mapcar (lambda (p) (transpose-pitch p (parse-interval "d5"))) c-major))
         (steps (lambda (row)
                  (loop for (a b) on row while b
                        collect (interval-name (interval-between a b))))))
    (scale-row "C major" c-major)
    (scale-row "up A4: F# major" up-a4)
    (scale-row "up d5: Gb major" up-d5)
    (out "  ~18a~{~6a~}~%" "MIDI (both)" (mapcar #'pitch-midi up-a4))
    (out "  ~18a   ~{~6a~}~%~%" "steps (all rows)" (funcall steps c-major))
    (check (names up-a4) '("F#4" "G#4" "A#4" "B4" "C#5" "D#5" "E#5" "F#5") "C major up A4")
    (check (names up-d5) '("Gb4" "Ab4" "Bb4" "Cb5" "Db5" "Eb5" "F5" "Gb5") "C major up d5")
    (check (every #'pitch-enharmonic-p up-a4 up-d5) t "enharmonic rows")
    (check (notany #'equalp up-a4 up-d5) t "no shared spelling")
    (check (list (funcall steps up-a4) (funcall steps up-d5))
           (list (funcall steps c-major) (funcall steps c-major)) "same steps"))
  (para "Twelve perfect fifths up from C1 walk the line of fifths from 0 to 12 and end on"
        "B#7, which sounds as C8 but is not C8:")
  (let* ((fifth (parse-interval "P5"))
         (stack (loop repeat 13
                      for p = (parse-pitch "C1") then (transpose-pitch p fifth)
                      collect p)))
    (out "  name   ~{~5a~}~%  fifths ~{~5a~}~%~%" (names stack) (mapcar #'pitch-fifths stack))
    (check (pitch-name (car (last stack))) "B#7" "twelve fifths"))
  (show (pitch-midi (parse-pitch "B#7")) 108)
  (show (pitch-enharmonic-p (parse-pitch "B#7") (parse-pitch "C8")) t)
  (show (equalp (parse-pitch "B#7") (parse-pitch "C8")) nil)
  (let* ((pitches (loop for octave from 3 to 5
                        nconc (mapcar (lambda (pc) (at-octave pc octave)) (all-spellings))))
         (pairs 0) (ok 0))
    (dolist (p pitches)
      (dolist (q pitches)
        (incf pairs)
        (let ((i (interval-between p q)))
          (when (and (equalp q (transpose-pitch p i))
                     (equalp p (transpose-pitch q (reverse-interval i))))
            (incf ok)))))
    (format t "~%All 35 spellings in octaves 3-5, every ordered pair p, q (~d pairs): p moved by~%~
               (interval-between p q) is q, and q moved by its reversal is p (~d of ~d).~%"
            pairs ok pairs)
    (check ok pairs "transpose round trips")))

;;; 6. Conditions and restarts --------------------------------------------------

(defun demo-conditions ()
  (para "Bad text signals INVALID-PITCH or INVALID-INTERVAL (both MUSIC-THEORY-ERROR) with"
        "the text and a short reason, inside a USE-VALUE restart whose value the parse"
        "function returns. Here HANDLER-BIND answers NIL for every input it cannot read:")
  (let* ((inputs '("C4" "H4" "Eb" "G#10" "Bb3" "F#4x" "" "Cbbb4" "C♯♭4" "C♯4"))
         (parsed (handler-bind ((invalid-pitch
                                  (lambda (c)
                                    (format t "  ~8s ~a~%" (music-theory-error-text c)
                                            (music-theory-error-reason c))
                                    (use-value nil c))))
                   (mapcar #'parse-pitch inputs))))
    (format t "  => ~{~a~^ ~}~%~%" (mapcar (lambda (p) (if p (pitch-name p) "nil")) parsed))
    (check (mapcar (lambda (p) (and p (pitch-name p))) parsed)
           '("C4" nil nil nil "Bb3" nil nil nil nil "C#4") "parsed with use-value"))
  (para "\"Cbbb4\" has an octave; its fault is the third flat. Since 0.3.0 the reason says"
        "so, for three accidentals and for a mix of sharps and flats alike (it used to be"
        "\"missing octave\", egao1980/music-theory#4). PARSE-PITCH-CLASS and PARSE-KEY"
        "share the scanner:")
  (loop for (form thunk)
          in (list (list '(parse-pitch "Cbbb4") (lambda () (parse-pitch "Cbbb4")))
                   (list '(parse-pitch "C♯♭4") (lambda () (parse-pitch "C♯♭4")))
                   (list '(parse-pitch "Cx#4") (lambda () (parse-pitch "Cx#4")))
                   (list '(parse-pitch-class "Ebbb") (lambda () (parse-pitch-class "Ebbb")))
                   (list '(parse-key "F#b minor") (lambda () (parse-key "F#b minor"))))
        for reason = (handler-case (progn (funcall thunk) :returned)
                       (music-theory-error (c) (music-theory-error-reason c)))
        do (out "  ~36a -> ~a~%" (code-text form) reason)
           (check reason "too many or mixed accidentals" (code-text form)))
  (terpri)
  (block restart
    (handler-bind ((invalid-pitch
                     (lambda (c)
                       (let ((r (first (compute-restarts c))))
                         (format t "The innermost restart of ~s is ~s:~%  ~a~%"
                                 (music-theory-error-text c) (restart-name r) r))
                       (return-from restart))))
      (parse-pitch "H4")))
  (para ""
        "Invoked interactively, the restart reads replacement text from *QUERY-IO* and"
        "parses that instead (here the reply is scripted):")
  (let* ((reply "Bb4")
         (prompt (make-string-output-stream))
         (p (handler-bind ((invalid-pitch
                             (lambda (c)
                               (let ((*query-io* (make-two-way-stream
                                                  (make-string-input-stream
                                                   (format nil "~a~%" reply))
                                                  prompt)))
                                 (invoke-restart-interactively (find-restart 'use-value c))))))
              (parse-pitch "Bb"))))
    (format t "  (parse-pitch \"Bb\")  prompt ~s  reply ~s  => ~a~%~%"
            (get-output-stream-string prompt) reply (pitch-name p))
    (check (pitch-name p) "Bb4" "interactive use-value"))
  (para "The same restart on PARSE-INTERVAL; M4 does not exist, so the handler supplies P4:")
  (show (handler-bind ((invalid-interval (lambda (c) (use-value (parse-interval "P4") c))))
          (interval-name (parse-interval "M4")))
        "P4")
  (terpri)
  (para "TRANSPOSE-PITCH signals INVALID-PITCH (TEXT NIL) when the result needs more than two"
        "accidentals; it offers no restart, so the caller decides. G# major has F## as its"
        "seventh degree, so up an augmented unison that note would need three sharps:")
  (let* ((a1 (parse-interval "A1"))
         (g-sharp (pitches "G#4" "A#4" "B#4" "C#5" "D#5" "E#5" "F##5" "G#5"))
         (failure nil)
         (up (mapcar (lambda (p)
                       (handler-case (pitch-name (transpose-pitch p a1))
                         (invalid-pitch (c) (setf failure (list (pitch-name p) c)) "--")))
                     g-sharp)))
    (out "  ~10a~{~6a~}~%  ~10a~{~6a~}~%" "G# major" (names g-sharp) "up A1" up)
    (format t "  ~a up A1 -> ~a~%~%" (first failure) (second failure))
    (check up '("G##4" "A##4" "B##4" "C##5" "D##5" "E##5" "--" "G##5") "G# major up A1"))
  (para "Some intervals are too far out for a name (beyond doubly augmented or doubly"
        "diminished). They still exist and still transpose; only naming them signals:")
  (show (interval-between (parse-pitch "Cb4") (parse-pitch "C##4"))
        (make-interval :steps 0 :semitones 3))
  (show (pitch-name (transpose-pitch (parse-pitch "Cb4") (make-interval :steps 0 :semitones 3)))
        "C##4")
  (terpri)
  (para "One handler on MUSIC-THEORY-ERROR catches all of them:")
  (loop for (form thunk)
          in (list (list '(parse-pitch "H4") (lambda () (parse-pitch "H4")))
                   (list '(parse-pitch-class "Ebbb") (lambda () (parse-pitch-class "Ebbb")))
                   (list '(parse-interval "P3") (lambda () (parse-interval "P3")))
                   (list '(parse-interval "-P1") (lambda () (parse-interval "-P1")))
                   (list '(fifths-pitch-class 20) (lambda () (fifths-pitch-class 20)))
                   (list '(interval-name (make-interval :steps 0 :semitones 3))
                         (lambda () (interval-name (make-interval :steps 0 :semitones 3))))
                   (list '(transpose-pitch (parse-pitch "Fbb4") (parse-interval "d1"))
                         (lambda () (transpose-pitch (parse-pitch "Fbb4") (parse-interval "d1")))))
        do (handler-case (progn (funcall thunk)
                                (format t "  ~a  (no condition)~%" (code-text form))
                                (check :returned :signalled (code-text form)))
             (music-theory-error (c)
               (format t "  ~a~%    -> ~a~%" (code-text form) c)))))

;;; 7. Meters (MT1) -------------------------------------------------------------

(defparameter *meter-rows*
  ;; (text meter-string bar beats beat-unit compound-p)
  '(("4/4" "4/4" 1 (1/4 1/4 1/4 1/4) 1/4 nil)
    ("C" "4/4" 1 (1/4 1/4 1/4 1/4) 1/4 nil)
    ("C|" "2/2" 1 (1/2 1/2) 1/2 nil)
    ("3/4" "3/4" 3/4 (1/4 1/4 1/4) 1/4 nil)
    ("5/4" "5/4" 5/4 (1/4 1/4 1/4 1/4 1/4) 1/4 nil)
    ("3/8" "3/8" 3/8 (1/8 1/8 1/8) 1/8 nil)
    ("6/8" "6/8" 3/4 (3/8 3/8) 3/8 t)
    ("9/8" "9/8" 9/8 (3/8 3/8 3/8) 3/8 t)
    ("12/8" "12/8" 3/2 (3/8 3/8 3/8 3/8) 3/8 t)
    ("6/4" "6/4" 3/2 (3/4 3/4) 3/4 t)
    ("2+3/8" "2+3/8" 5/8 (1/4 3/8) nil nil)
    ("3+2+2/8" "3+2+2/8" 7/8 (3/8 1/4 1/4) nil nil)))

(defun demo-meters ()
  (para "Sections 7-11 are slice MT1, new in 0.2.0. Durations and positions are exact"
        "rationals in whole notes: 1/4 is a quarter, 3/8 a dotted quarter, 1 a whole note."
        "A METER is a list of NUMERATORS over a DENOMINATOR: (4) over 4 for 4/4, (2 3) over"
        "8 for the additive 2+3/8. PARSE-METER also reads the symbols C (4/4) and C| (2/2);"
        "METER-STRING always prints numbers.")
  (show (parse-meter "4/4") (make-meter :numerators '(4) :denominator 4))
  (show (parse-meter "2+3/8") (make-meter :numerators '(2 3) :denominator 8))
  (show (meter-string (parse-meter "C|")) "2/2")
  (terpri)
  (para "METER-BEATS lists the beats of one bar. A single numerator above 3 that divides"
        "by 3 is compound: 6/8 has two dotted-quarter beats, not six eighths. Several"
        "numerators make one beat each. Anything else is simple, one beat per count, so"
        "3/8 has three eighth beats. BEAT-UNIT is the common beat, or NIL when the beats"
        "differ, and METER-MEASURE-DURATION is the length of a bar.")
  (show (meter-beats (parse-meter "6/8")) '(3/8 3/8))
  (show (compound-p (parse-meter "6/8")) t)
  (show (beat-unit (parse-meter "12/8")) 3/8)
  (show (meter-measure-duration (parse-meter "12/8")) 3/2)
  (show (meter-beats (parse-meter "2+3/8")) '(1/4 3/8))
  (show (beat-unit (parse-meter "2+3/8")) nil)
  (show (compound-p (parse-meter "2+3/8")) nil)
  (terpri)
  (format t "  ~10a~9a~6a~22a~6a~a~%" "text" "meter" "bar" "beats" "unit" "compound-p")
  (loop for (text name bar beats unit compound) in *meter-rows*
        for m = (parse-meter text)
        for row = (list (meter-string m) (meter-measure-duration m) (meter-beats m)
                        (beat-unit m) (compound-p m))
        do (out "  ~10a~9a~6a~22a~6a~a~%" (prin1-to-string text) (first row) (second row)
                (format nil "~{~a~^ ~}" (third row)) (code-text (fourth row))
                (code-text (fifth row)))
           (check row (list name bar beats unit compound) text))
  (terpri)
  (para "Bad text signals INVALID-METER, a MUSIC-THEORY-ERROR, inside the same USE-VALUE"
        "restart as the pitch parsers. Here the handler answers NIL:")
  (let* ((inputs '("6/8" "4/5" "0/4" "65/4" "4 / 4" "4/4x" "2++3/8" ""))
         (parsed (handler-bind ((invalid-meter
                                  (lambda (c)
                                    (format t "  ~9s ~a~%" (music-theory-error-text c)
                                            (music-theory-error-reason c))
                                    (use-value nil c))))
                   (mapcar #'parse-meter inputs))))
    (format t "  => ~{~a~^ ~}~%~%" (mapcar (lambda (m) (if m (meter-string m) "nil")) parsed))
    (check (mapcar (lambda (m) (and m (meter-string m))) parsed)
           '("6/8" nil nil nil nil nil nil nil) "meters parsed with use-value"))
  (show (handler-bind ((invalid-meter (lambda (c) (use-value (parse-meter "4/4") c))))
          (meter-string (parse-meter "4/5")))
        "4/4")
  (terpri)
  (para "A leading zero is accepted; whether it should be is open"
        "(egao1980/music-theory#3):")
  (show (meter-string (parse-meter "04/4"))))

;;; 8. The metric hierarchy -----------------------------------------------------

(defparameter *level-names*
  '((0 . "downbeat") (1 . "group starts") (2 . "beats") (3 . "eighths") (4 . "sixteenths"))
  "Row labels of the weight maps, whose meters have the eighth grid at level 3.")

(defun bar-weights (meter)
  "METRIC-WEIGHT of each 1/16 position of one bar of METER."
  (loop for k below (* 16 (meter-measure-duration meter))
        collect (metric-weight meter (/ k 16))))

(defun weight-map (text expected)
  "Print the weights of one bar of TEXT, then a row of dots per level, weakest
first: a position has a dot in the row of its weight and every row above it."
  (let* ((weights (bar-weights (parse-meter text)))
         (levels (sort (remove-duplicates (copy-list weights)) #'>)))
    (out "  ~18a~{~d ~}~%" text weights)
    (dolist (level levels)
      (out "  ~18a~{~a ~}~%"
           (format nil "  ~d ~a" level (cdr (assoc level *level-names*)))
           (mapcar (lambda (w) (if (<= w level) "*" " ")) weights)))
    (terpri)
    (check weights expected (format nil "weights of ~a" text))))

(defun demo-metric ()
  (para "METRIC-WEIGHT gives each position in a bar a level, 0 the strongest:"
        "  0    the downbeat"
        "  1    the start of a beat group: beats pair up when there are four or more of"
        "       an even count (beat 3 of 4/4 and of 12/8), and in an additive meter"
        "       every beat starts a group"
        "  2    any other beat"
        "  3-8  subdivisions: a plain beat halves and a dotted beat splits in three,"
        "       then every part halves again"
        "  9    off every grid, such as a triplet eighth in 4/4"
        ""
        "One bar on the 1/16 grid. Each position has a dot in the row of its own weight"
        "and in every row above it, so the stronger the position, the taller its column.")
  (weight-map "4/4" '(0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4))
  (weight-map "6/8" '(0 4 3 4 3 4 2 4 3 4 3 4))
  (weight-map "2+3/8" '(0 4 3 4 1 4 3 4 3 4))
  (para "In 4/4 each quarter beat halves into eighths; in 6/8 each dotted-quarter beat"
        "splits into three. Both then halve into 16ths. In 2+3/8 the first beat halves,"
        "the second splits in three and starts a group (1). A 6/8 bar has no level 1:"
        "its two beats do not pair up."
        ""
        "More meters, weights only. In 3/4 nothing pairs up (three beats is odd), 12/8"
        "has its half bar on beat 3, and in 2/2 the beat is a half note, so the eighths"
        "weigh 4 and the 16ths 5. A beat of n units (n > 1) splits into its n units: the"
        "dotted beats above into three, and the five-eighth group of 3+5/8 into five"
        "eighths, all weighing 3. Before 0.3.0 that group was halved at 5/16 and its"
        "eighths weighed 9 (egao1980/music-theory#7):")
  (loop for (text expected) in '(("3/4" (0 4 3 4 2 4 3 4 2 4 3 4))
                                 ("12/8" (0 4 3 4 3 4 2 4 3 4 3 4 1 4 3 4 3 4 2 4 3 4 3 4))
                                 ("2/2" (0 5 4 5 3 5 4 5 2 5 4 5 3 5 4 5))
                                 ("3+5/8" (0 4 3 4 3 4 1 4 3 4 3 4 3 4 3 4)))
        for weights = (bar-weights (parse-meter text))
        do (out "  ~18a~{~d ~}~%" text weights)
           (check weights expected (format nil "weights of ~a" text)))
  (terpri)
  (para "The same offset means different things in different meters. The offset is taken"
        "modulo the bar, so later bars and pickups need no arithmetic:")
  (show (metric-weight (parse-meter "4/4") 1/4) 2 "beat 2")
  (show (metric-weight (parse-meter "6/8") 1/4) 3 "the third eighth of beat 1")
  (show (metric-weight (parse-meter "2+3/8") 1/4) 1 "the second group")
  (show (metric-weight (parse-meter "4/4") 5/4) 2 "beat 2 of bar 2")
  (show (metric-weight (parse-meter "4/4") -1/4) 2 "beat 4 of the bar before")
  (show (metric-weight (parse-meter "4/4") 1/12) 9 "a triplet eighth"))

;;; 9. Dots and tuplets ---------------------------------------------------------

(defparameter *note-names*
  '((2 . "breve") (1 . "whole") (1/2 . "half") (1/4 . "quarter") (1/8 . "eighth")
    (1/16 . "16th") (1/32 . "32nd") (1/64 . "64th") (1/128 . "128th")))

(defun duration-name (duration)
  "DURATION as one written value (\"double-dotted quarter\"), or NIL."
  (multiple-value-bind (base dots) (duration-dots duration)
    (and base
         (format nil "~[~;dotted ~;double-dotted ~;triple-dotted ~]~a"
                 dots (cdr (assoc base *note-names*))))))

(defparameter *dot-rows*
  ;; (duration base dots)
  '((1/4 1/4 0) (3/8 1/4 1) (7/16 1/4 2) (15/32 1/4 3) (3/16 1/8 1) (3/4 1/2 1)
    (3/2 1 1) (2 2 0) (3/256 1/128 1) (5/8 nil nil) (1/12 nil nil) (1/20 nil nil)))

(defun demo-dots ()
  (para "A dot adds half the note's value and a second dot half of that again: a dotted"
        "note is 3/2 of its base, a double-dotted one 7/4. DURATION-DOTS finds the base"
        "(a breve, 2, down to a 128th, 1/128) and the dots, at most MAX-DOTS (default 3),"
        "or NIL and NIL when the duration is not one written value.")
  (show-values (duration-dots 3/8) '(1/4 1) "dotted quarter")
  (show-values (duration-dots 7/16) '(1/4 2) "double-dotted quarter")
  (show-values (duration-dots 7/16 :max-dots 1) '(nil nil))
  (terpri)
  (format t "  ~10a~7a~6a~a~%" "duration" "base" "dots" "written as")
  (loop for (duration base dots) in *dot-rows*
        for row = (multiple-value-list (duration-dots duration))
        do (out "  ~10a~7a~6a~a~%" duration (code-text (first row)) (code-text (second row))
                (or (duration-name duration) "--"))
           (check row (list base dots) (format nil "dots of ~a" duration)))
  (terpri)
  (para "5/8 needs a tie (a half and an eighth). 1/12 and 1/20 need a tuplet: three"
        "eighths in the time of two fill a quarter, so each lasts 1/12. TUPLET-DURATION"
        "gives the sounding length of a written value under a ratio n/m, \"n in the time"
        "of m\". DURATION-DOTS with :TUPLET turns a sounding length back into the written"
        "value.")
  (show (tuplet-duration 1/8 3/2) 1/12 "triplet eighth")
  (show (tuplet-duration 1/4 3/2) 1/6 "triplet quarter")
  (show (tuplet-duration 1/16 5/4) 1/20 "quintuplet 16th")
  (show (tuplet-duration 1/8 2/3) 3/16 "a duplet eighth in 6/8 lasts a dotted eighth")
  (show (tuplet-duration 1/8 3/2 :dots 1) 1/8 "a dotted triplet eighth lasts an eighth")
  (show (* 3 (tuplet-duration 1/8 3/2)) 1/4)
  (show-values (duration-dots 1/12 :tuplet 3/2) '(1/8 0))
  (show-values (duration-dots 1/20 :tuplet 5/4) '(1/16 0))
  (let ((total 0) (ok 0))
    (dolist (base '(2 1 1/2 1/4 1/8 1/16 1/32 1/64 1/128))
      (dotimes (dots 4)
        (dolist (ratio '(1 3/2 5/4 7/4 2/3))
          (incf total)
          (when (equal (multiple-value-list
                        (duration-dots (tuplet-duration base ratio :dots dots) :tuplet ratio))
                       (list base dots))
            (incf ok)))))
    (format t "~%9 bases (breve to 128th) x 0-3 dots x the ratios 1, 3/2, 5/4, 7/4 and 2/3:~%~
               DURATION-DOTS with :TUPLET gives back the base and dots of TUPLET-DURATION~%~
               (~d of ~d).~%" ok total)
    (check ok total "tuplet round trips")))

;;; 10. Tie splitting -------------------------------------------------------------

(defun sixteenths (duration)
  (* 16 duration))

(defun piece-starts (onset pieces)
  "The offset of each of PIECES, tied one after another from ONSET."
  (let ((at onset))
    (mapcar (lambda (piece) (prog1 at (incf at piece))) pieces)))

(defun strip (spans cells)
  "A row of CELLS two-character cells. Each span (START LENGTH HEAD) puts HEAD
in its first cell and dashes in the rest."
  (let ((row (make-string (* 2 cells) :initial-element #\Space)))
    (loop for (start length head) in spans
          do (fill row #\- :start (* 2 start) :end (* 2 (+ start length)))
             (setf (char row (* 2 start)) head))
    row))

(defun barred (row bar-cells)
  "ROW with a | before each bar of BAR-CELLS cells and after the last one."
  (with-output-to-string (s)
    (loop for i from 0 below (length row) by (* 2 bar-cells)
          do (format s "|~a" (subseq row i (+ i (* 2 bar-cells)))))
    (write-char #\| s)))

(defun written-as (onset pieces bar)
  "The names of PIECES joined by ~ for a tie, with | where a barline falls."
  (with-output-to-string (s)
    (loop for piece in pieces
          for at in (piece-starts onset pieces)
          for first = t then nil
          do (unless first
               (write-string (if (zerop (mod at bar)) " ~ | " " ~ ") s))
             (write-string (duration-name piece) s))))

(defun show-rhythm (text notes expected)
  "Draw NOTES, a list of (offset duration), in the meter TEXT on the 1/16 grid,
as given and tied by SPLIT-DURATION, then list each note's pieces. EXPECTED is
the list of piece lists."
  (let* ((meter (parse-meter text))
         (bar (meter-measure-duration meter))
         (bar-cells (sixteenths bar))
         (bars (ceiling (loop for (onset duration) in notes maximize (+ onset duration)) bar))
         (splits (loop for (onset duration) in notes
                       collect (split-duration duration :meter meter :offset onset)))
         (row (lambda (spans) (barred (strip spans (* bars bar-cells)) bar-cells))))
    (out "  ~10a~a~%" text
         (barred (format nil "~{~d ~}" (loop repeat bars append (bar-weights meter))) bar-cells))
    (out "  ~10a~a~%" "as given"
         (funcall row (loop for (onset duration) in notes
                            collect (list (sixteenths onset) (sixteenths duration) #\o))))
    (out "  ~10a~a~%~%" "tied"
         (funcall row (loop for (onset nil) in notes
                            for pieces in splits
                            append (loop for piece in pieces
                                         for at in (piece-starts onset pieces)
                                         for head = #\o then #\~
                                         collect (list (sixteenths at) (sixteenths piece) head)))))
    (format t "  ~8a~10a~19a~a~%" "offset" "duration" "tied pieces" "written as")
    (loop for (onset duration) in notes
          for pieces in splits
          do (out "  ~8a~10a~19a~a~%" onset duration (format nil "~{~a~^ + ~}" pieces)
                  (written-as onset pieces bar)))
    (terpri)
    (check splits expected (format nil "~a rhythm" text))))

(defun split-sweep (texts)
  "Split every 1/16-grid onset of one bar and every duration 1/16..2 in each meter
of TEXTS. Returns the notes whose pieces pass every check, and the notes tried."
  (let ((total 0) (ok 0))
    (dolist (text texts)
      (let* ((meter (parse-meter text))
             (bar (meter-measure-duration meter)))
        (loop for k below (sixteenths bar)
              for onset = (/ k 16)
              do (loop for j from 1 to 32
                       for duration = (/ j 16)
                       for pieces = (split-duration duration :meter meter :offset onset)
                       for starts = (piece-starts onset pieces)
                       do (incf total)
                          (when (and (= duration (reduce #'+ pieces))
                                     (every (lambda (p) (duration-dots p :max-dots 1)) pieces)
                                     (every (lambda (at p) (<= (+ (mod at bar) p) bar))
                                            starts pieces)
                                     (every (lambda (at p)
                                              (loop for i from 1 below (sixteenths p)
                                                    always (>= (metric-weight meter (+ at (/ i 16)))
                                                               (metric-weight meter at))))
                                            starts pieces))
                            (incf ok))))))
    (values ok total)))

(defun demo-ties ()
  (para "A note longer than one written value, or one that would hide a strong beat, is"
        "written as tied notes. SPLIT-DURATION gives the pieces of a duration that starts"
        "OFFSET after a barline (METER defaults to 4/4 and MAX-DOTS to 1):"
        "  1. cut at every barline;"
        "  2. keep a segment whole when it is one written value, nothing inside it is"
        "     stronger than its start, and (in a compound meter) a segment that crosses"
        "     a beat starts on a beat and lasts whole beats;"
        "  3. otherwise cut it at the strongest position inside and try both parts.")
  (show (split-duration 1/2 :offset 1/8) '(1/8 1/4 1/8))
  (show (split-duration 1/2 :meter (parse-meter "6/8")) '(3/8 1/8))
  (terpri)
  (para "A syncopated two-bar rhythm in 4/4. The top row is the metric weight of"
        "section 8, then the notes as given and as tied: o starts a note, ~ starts a"
        "tied piece, - holds it. Each 16th is two characters wide.")
  (show-rhythm "4/4" '((0 1/8) (1/8 1/2) (5/8 1/4) (7/8 3/8) (5/4 3/4))
               '((1/8) (1/8 1/4 1/8) (1/8 1/8) (1/8 1/4) (1/4 1/2)))
  (para "Every tie falls on a position stronger than the start of its note: beat 2 and"
        "the half bar for the half note, beat 4 for the quarter, the barline for the"
        "dotted quarter, and the half bar of bar 2 for the dotted half."
        ""
        "Two bars of 6/8. The half note from the downbeat becomes a dotted quarter tied"
        "to an eighth, so the second beat shows. A quarter inside a beat stays whole;"
        "the middle one of three quarters crosses beat 2 and is split:")
  (show-rhythm "6/8" '((0 1/2) (1/2 1/4) (3/4 1/4) (1 1/4) (5/4 1/4))
               '((3/8 1/8) (1/4) (1/4) (1/8 1/8) (1/4)))
  (para "More cases. A dotted half from the downbeat stays one note, unless dots are"
        "off. In 3/4 no beat is stronger than beat 2 until the next bar. An offset past"
        "the bar, or before it for a pickup, counts from the barline before it.")
  (show (split-duration 3/4) '(3/4))
  (show (split-duration 3/4 :max-dots 0) '(1/2 1/4))
  (show (split-duration 1/2 :offset 1/4) '(1/4 1/4) "the half bar shows")
  (show (split-duration 1/2 :meter (parse-meter "3/4") :offset 1/4) '(1/2))
  (show (split-duration 9/4 :offset 1/2) '(1/2 1 3/4) "two barlines")
  (show (split-duration 1/2 :offset -1/4) '(1/4 1/4) "a pickup")
  (show (split-duration 9/8 :meter (parse-meter "12/8")) '(3/4 3/8))
  (show (split-duration 1/2 :meter (parse-meter "2+3/8") :offset 1/8) '(1/8 3/8))
  (let ((meters '("4/4" "3/4" "2/2" "6/8" "9/8" "12/8" "2+3/8" "3+2+2/8" "3+5/8")))
    (multiple-value-bind (ok total) (split-sweep meters)
      (format t "~%Every onset on the 1/16 grid of one bar, with every duration from 1/16 to 2, in~%~
                 ~{~a~#[~; and ~:;, ~]~}~%~
                 (~d notes): the pieces add up to the duration, each is one written value with~%~
                 at most one dot, none crosses a barline, and nothing inside a piece is stronger~%~
                 than its start (~d of ~d).~%~%"
              meters total ok total)
      (check ok total "split sweep")))
  (para "SPLIT-DURATION writes plain values only. A tuplet length cannot be cut into"
        "them, so it signals MUSIC-THEORY-ERROR:")
  (show-signal (split-duration 1/12))
  (terpri)
  (para "Two bars of 3+5/8. Since 0.3.0 the five-eighth group has an eighth grid (section"
        "8), so an eighth inside it is one note; in 0.2.0 SPLIT-DURATION rejected it as"
        "\"not notatable without a tuplet\" (egao1980/music-theory#7). The quarter that"
        "crosses the start of the group is tied there; the half note that starts on it"
        "stays whole:")
  (show-rhythm "3+5/8" '((0 1/4) (1/4 1/4) (1/2 1/8) (5/8 3/8) (1 3/8) (11/8 1/2) (15/8 1/8))
               '((1/4) (1/8 1/8) (1/8) (3/8) (3/8) (1/2) (1/8))))

;;; 11. Tempo and phrase windows --------------------------------------------------

(defparameter *tempo-rows*
  ;; (duration label seconds-at-72)
  '((1/16 "16th" 5/24) (1/8 "eighth" 5/12) (1/4 "quarter" 5/6) (3/8 "dotted quarter" 5/4)
    (1/2 "half" 5/3) (1 "whole, a 4/4 bar" 10/3) (3 "three 4/4 bars" 10)))

(defun demo-tempo ()
  (para "A TEMPO is BPM beats per minute of BEAT-UNIT, a duration: ♩ = 72 is 72 quarters"
        "(1/4) a minute, and 6/8 is usually counted in dotted quarters (3/8)."
        "DURATION-SECONDS and SECONDS-DURATION convert exactly, in rationals.")
  (show (make-tempo :bpm 72) (make-tempo :bpm 72 :beat-unit 1/4))
  (show (duration-seconds 1/4 (make-tempo :bpm 72)) 5/6)
  (show (seconds-duration 10 (make-tempo :bpm 72)) 3 "three whole notes")
  (show (seconds-duration 2.5 (make-tempo :bpm 72)) 3/4 "a float goes through RATIONALIZE")
  (show (duration-seconds 3/4 (make-tempo :bpm 60 :beat-unit 3/8)) 2
        "a 6/8 bar at dotted quarter = 60")
  (terpri)
  (format t "  At ♩ = 72:~%  ~24a~10a~a~%" "duration" "seconds" "decimal")
  (let ((tempo (make-tempo :bpm 72)))
    (loop for (duration label seconds) in *tempo-rows*
          for s = (duration-seconds duration tempo)
          do (out "  ~24a~10a~,4f~%" (format nil "~a (~a)" duration label) s (float s))
             (check s seconds (format nil "~a at 72" duration))))
  (let ((tempi (list (make-tempo :bpm 72) (make-tempo :bpm 96) (make-tempo :bpm 132)
                     (make-tempo :bpm 72.5) (make-tempo :bpm 60 :beat-unit 3/8)))
        (total 0) (ok 0))
    (dolist (tempo tempi)
      (loop for k from 1 to 64
            for d = (/ k 16)
            do (incf total)
               (when (eql d (seconds-duration (duration-seconds d tempo) tempo))
                 (incf ok))))
    (format t "~%Every duration from 1/16 to 4 in 16ths, at ♩ = 72, 96, 132 and 72.5 and with a~%~
               dotted-quarter beat at 60: SECONDS-DURATION of DURATION-SECONDS gives it back~%~
               exactly (~d of ~d).~%~%" ok total)
    (check ok total "tempo round trips"))
  (para "Phrase windows. A singer or a wind player has to breathe at least every 8"
        "seconds. At ♩ = 96 a four-bar phrase in 4/4 lasts 10 seconds, too long."
        "MAX-WINDOW-DURATION gives the longest duration that fits, rounded down to a"
        "multiple of GRID when one is given:")
  (show (duration-seconds 4 (make-tempo :bpm 96)) 10 "four 4/4 bars")
  (show (max-window-duration (make-tempo :bpm 96) 8) 16/5 "12.8 quarters")
  (show (max-window-duration (make-tempo :bpm 96) 8 :grid 1/8) 25/8 "25 eighths")
  (show (duration-seconds 25/8 (make-tempo :bpm 96)) 125/16 "7.8125 seconds")
  (show (max-window-duration (make-tempo :bpm 96) 8 :grid 1) 3 "three whole bars")
  (terpri)
  (para "On an eighth-note grid the phrase can run three bars and one eighth: the breath"
        "comes by the second eighth of bar 4 at the latest. A note held that long is"
        "tied across three barlines:")
  (show (split-duration 25/8) '(1 1 1 1/8))
  (terpri)
  (para "The same 8-second window at other tempi, on the eighth-note grid:")
  (format t "  ~6a~8a~9a~17a~a~%" "♩ =" "window" "eighths" "4/4 bars + 8ths" "seconds")
  (loop for (bpm eighths) in '((60 16) (72 19) (96 25) (120 32) (144 38))
        for tempo = (make-tempo :bpm bpm)
        for window = (max-window-duration tempo 8 :grid 1/8)
        for n = (* 8 window)
        do (out "  ~6a~8a~9a~17a~,4f~%" bpm window n
                (multiple-value-bind (bars rest) (floor n 8) (format nil "~d + ~d" bars rest))
                (float (duration-seconds window tempo)))
           (check n eighths (format nil "window at ~a" bpm))
           (check (and (<= (duration-seconds window tempo) 8)
                       (> (duration-seconds (+ window 1/8) tempo) 8))
                  t (format nil "window at ~a is the longest" bpm))))

;;; 12. Keys, key signatures and scales (MT2) ---------------------------------------

(defun class-names (classes)
  (mapcar #'pitch-class-string classes))

(defun thirty-keys ()
  "The 30 keys with signatures -7..7: each major key, then its relative minor."
  (loop for f from -7 to 7
        collect (make-key :tonic (fifths-pitch-class f) :mode :major)
        collect (make-key :tonic (fifths-pitch-class (+ f 3)) :mode :minor)))

(defun signature-accidentals (key)
  "The altered notes of KEY's natural scale in key-signature order: sharps up the
line of fifths (F# C# G# ...), flats down it (Bb Eb Ab ...)."
  (class-names (sort (remove 0 (scale-degrees key) :key #'pitch-class-name-alter)
                     (if (plusp (key-signature key)) #'< #'>)
                     :key #'pitch-fifths)))

(defun fifths-run (signature)
  "The accidentals of SIGNATURE read off the line of fifths: SIGNATURE sharps from
F# (position 6) up, or -SIGNATURE flats from Bb (position -2) down."
  (loop for k below (abs signature)
        collect (pitch-class-string
                 (fifths-pitch-class (if (plusp signature) (+ 6 k) (- -2 k))))))

(defun differ-only-at (scale other degree alter)
  "True when OTHER is SCALE with degree DEGREE moved by ALTER semitones, letter kept."
  (loop for d from 1
        for a in scale
        for b in other
        always (if (= d degree)
                   (and (= (pitch-class-name-letter a) (pitch-class-name-letter b))
                        (= alter (- (pitch-class-name-alter b) (pitch-class-name-alter a))))
                   (equalp a b))))

(defparameter *key-forms*
  ;; (text key-name)
  '(("G" "G major") ("g" "G minor") ("f#" "F# minor") ("bb" "Bb minor") ("Gm" "G minor")
    ("Ebm" "Eb minor") ("eb MAJOR" "Eb major") ("B♭ minor" "Bb minor")
    (" a  minor " "A minor")))

(defparameter *signature-rows*
  ;; (signature major minor)
  '((-7 "Cb major" "Ab minor") (-6 "Gb major" "Eb minor") (-5 "Db major" "Bb minor")
    (-4 "Ab major" "F minor") (-3 "Eb major" "C minor") (-2 "Bb major" "G minor")
    (-1 "F major" "D minor") (0 "C major" "A minor") (1 "G major" "E minor")
    (2 "D major" "B minor") (3 "A major" "F# minor") (4 "E major" "C# minor")
    (5 "B major" "G# minor") (6 "F# major" "D# minor") (7 "C# major" "A# minor")))

(defparameter *scale-rows*
  ;; (key variant degrees)
  '(("A minor" :natural ("A" "B" "C" "D" "E" "F" "G"))
    ("A minor" :harmonic ("A" "B" "C" "D" "E" "F" "G#"))
    ("A minor" :melodic ("A" "B" "C" "D" "E" "F#" "G#"))
    ("C minor" :natural ("C" "D" "Eb" "F" "G" "Ab" "Bb"))
    ("C minor" :harmonic ("C" "D" "Eb" "F" "G" "Ab" "B"))
    ("C minor" :melodic ("C" "D" "Eb" "F" "G" "A" "B"))
    ("F# minor" :natural ("F#" "G#" "A" "B" "C#" "D" "E"))
    ("F# minor" :harmonic ("F#" "G#" "A" "B" "C#" "D" "E#"))
    ("F# minor" :melodic ("F#" "G#" "A" "B" "C#" "D#" "E#"))))

(defun demo-keys ()
  (para "Sections 12-16 are slice MT2, new in 0.3.0. A KEY is a TONIC, a PITCH-CLASS-NAME,"
        "and a MODE, :MAJOR or :MINOR. PARSE-KEY reads \"Eb major\" or \"F# minor\", and"
        "KEY-NAME prints that form, in ASCII or in Unicode:")
  (show (parse-key "Eb major")
        (make-key :tonic (make-pitch-class-name :letter 2 :alter -1) :mode :major))
  (show (key-name (parse-key "F# minor")) "F# minor")
  (show (key-name (parse-key "F# minor") :accidentals :unicode) "F♯ minor")
  (terpri)
  (para "Short forms: with no mode word an uppercase letter is major and a lowercase one"
        "minor, and a trailing \"m\" is minor. With a mode word neither the letter's case"
        "nor the word's matters:")
  (format t "  ~14a~a~%" "text" "key")
  (loop for (text name) in *key-forms*
        for key = (parse-key text)
        do (format t "  ~14a~a~%" (prin1-to-string text) (key-name key))
           (check (key-name key) name (format nil "key ~s" text)))
  (terpri)
  (para "KEY-SIGNATURE counts sharps (positive) or flats (negative). It is the tonic's"
        "position on the line of fifths for a major key, and three less for a minor key:"
        "A is at 3, so A minor has no sharps, like C major. The last column lists the"
        "altered notes of each scale; they are the run of the line of fifths from F# up,"
        "or from Bb down, that a key signature shows:")
  (format t "  ~5a~11a~11a~a~%" "sig" "major" "minor" "sharps or flats")
  (loop for (sig major minor) in *signature-rows*
        for major-key = (parse-key major)
        for minor-key = (parse-key minor)
        for accidentals = (signature-accidentals major-key)
        do (out "  ~3@a  ~11a~11a~{~a~^ ~}~%" sig (key-name major-key) (key-name minor-key)
                accidentals)
           (check (list (key-signature major-key) (key-signature minor-key)) (list sig sig) major)
           (check accidentals (fifths-run sig) (format nil "accidentals of ~a" major))
           (check (signature-accidentals minor-key) accidentals
                  (format nil "accidentals of ~a" minor)))
  (let ((ok (count-if (lambda (key)
                        (and (equalp key (parse-key (key-name key)))
                             (equalp key (parse-key (key-name key :accidentals :unicode)))))
                      (thirty-keys))))
    (format t "~%All 30 keys parse back from their names, ASCII and Unicode (~d of 30).~%~%" ok)
    (check ok 30 "key name round trips"))
  (para "These 30 are the valid keys. Anything else signals INVALID-KEY (a"
        "MUSIC-THEORY-ERROR) with a reason, inside the USE-VALUE restart of the other"
        "parsers. G# major would need eight sharps (F## among them), Fb major eight flats:")
  (let* ((inputs '("Eb major" "G#" "Fb" "e# minor" "Gmaj" "G maj" "Gminor" "GM" "H minor"
                   "Cbbb major" ""))
         (parsed (handler-bind ((invalid-key
                                  (lambda (c)
                                    (format t "  ~13s ~a~%" (music-theory-error-text c)
                                            (music-theory-error-reason c))
                                    (use-value nil c))))
                   (mapcar #'parse-key inputs))))
    (format t "  => ~{~a~^, ~}~%~%" (mapcar (lambda (k) (if k (key-name k) "nil")) parsed))
    (check (mapcar (lambda (k) (and k (key-name k))) parsed)
           (cons "Eb major" (make-list 10)) "keys parsed with use-value"))
  (show (handler-bind ((invalid-key (lambda (c) (use-value (parse-key "Ab") c))))
          (key-name (parse-key "G#")))
        "Ab major")
  (terpri)
  (para "MAKE-KEY builds any key, and the functions of sections 13-15 accept it, because"
        "an applied chord can tonicize one (V/V in C# major is V of G# major). Only"
        "KEY-SIGNATURE refuses:")
  (show (mapcar #'pitch-class-string (scale-degrees (make-key :tonic (parse-pitch-class "G#"))))
        '("G#" "A#" "B#" "C#" "D#" "E#" "F##"))
  (show-signal (key-signature (make-key :tonic (parse-pitch-class "G#"))))
  (terpri)
  (para "SCALE-DEGREES lists degrees 1-7 of a key, one letter per degree. A major key has"
        "one scale. A minor key has three: :NATURAL (the default), :HARMONIC with a raised"
        "7th, and :MELODIC, the ascending form with a raised 6th and 7th.")
  (show (mapcar #'pitch-class-string (scale-degrees (parse-key "Eb")))
        '("Eb" "F" "G" "Ab" "Bb" "C" "D"))
  (show (mapcar #'pitch-class-string (scale-degrees (parse-key "C#")))
        '("C#" "D#" "E#" "F#" "G#" "A#" "B#"))
  (terpri)
  (out "  ~10a~10a~{~5a~}~%" "key" "variant" '(1 2 3 4 5 6 7))
  (loop for (name variant degrees) in *scale-rows*
        for previous = nil then current
        for current = name
        for row = (class-names (scale-degrees (parse-key name) :variant variant))
        do (out "  ~10a~10a~{~5a~}~%" (if (equal name previous) "" name)
                (string-downcase (symbol-name variant)) row)
           (check row degrees (format nil "~a ~(~a~)" name variant)))
  (let ((ok (count-if (lambda (key)
                        (let ((natural (scale-degrees key))
                              (harmonic (scale-degrees key :variant :harmonic))
                              (melodic (scale-degrees key :variant :melodic))
                              (major (scale-degrees (make-key :tonic (key-tonic key)))))
                          (and (differ-only-at natural harmonic 7 1)
                               (differ-only-at major melodic 3 -1))))
                      (remove :major (thirty-keys) :key #'key-mode))))
    (format t "~%All 15 minor keys: the harmonic scale is the natural one with degree 7 raised a~%~
               semitone, and the melodic scale is the parallel major with degree 3 lowered a~%~
               semitone (~d of 15).~%~%" ok)
    (check ok 15 "minor scale variants"))
  (para "Church modes are reserved for later. Any other variant is a TYPE-ERROR:")
  (let ((expected (handler-case (progn (scale-degrees (parse-key "C") :variant :dorian) :returned)
                    (type-error (c) (type-error-expected-type c)))))
    (format t "  ~a~%    -> type-error, expected type ~a~%"
            (code-text '(scale-degrees (parse-key "C") :variant :dorian)) (code-text expected))
    (check expected '(member :natural :harmonic :melodic) "variant type")))

;;; 13. Scale degrees ---------------------------------------------------------------

(defun degree-label (degree alteration)
  "DEGREE with ALTERATION written as an accidental before it: 4, #4, b6, bb7."
  (format nil "~a~d"
          (subseq (pitch-class-string (make-pitch-class-name :letter 0 :alter alteration)) 1)
          degree))

(defun degree-labels (texts key &optional (variant :natural))
  "The DEGREE-LABEL of each pitch class in TEXTS, in KEY and VARIANT."
  (mapcar (lambda (text)
            (multiple-value-bind (degree alteration)
                (degree-of (parse-pitch-class text) key :variant variant)
              (degree-label degree alteration)))
          texts))

(defun demo-degrees ()
  (para "DEGREE-OF gives a note's scale degree in a key, its ALTERATION against the scale"
        "(0 for a note of the scale) and, for a SPELLED-PITCH, the octave of the tonic at"
        "or below it. The letter decides the degree: F#4 and Gb4 sound the same, but in"
        "D major F# is degree 3 and Gb is degree 4 lowered.")
  (show-values (degree-of (parse-pitch "F#4") (parse-key "D")) '(3 0 4))
  (show-values (degree-of (parse-pitch "Gb4") (parse-key "D")) '(4 -1 4))
  (show-values (degree-of (parse-pitch "C#5") (parse-key "B")) '(2 0 4) "the tonic below is B4")
  (show-values (degree-of (parse-pitch "B#3") (parse-key "C")) '(7 1 3) "the tonic below is C3")
  (show-values (degree-of (parse-pitch-class "Fb") (parse-key "F#")) '(1 -2 nil)
               "a class has no octave")
  (terpri)
  (para "The alteration is measured against the scale of the variant: G# is a raised 7th"
        "in natural A minor and the plain 7th in harmonic A minor.")
  (show-values (degree-of (parse-pitch "G#4") (parse-key "a")) '(7 1 3))
  (show-values (degree-of (parse-pitch "G#4") (parse-key "a") :variant :harmonic) '(7 0 3))
  (terpri)
  (para "So every chromatic note is an altered degree. The chromatic scale in C major,"
        "spelled with sharps going up and with flats coming down, then in A minor against"
        "each variant:")
  (let* ((c (parse-key "C"))
         (a (parse-key "a"))
         (up '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
         (down '("C" "B" "Bb" "A" "Ab" "G" "Gb" "F" "E" "Eb" "D" "Db"))
         (minor '("A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"))
         (rows (list (list "C major" up nil)
                     (list "" (degree-labels up c)
                           '("1" "#1" "2" "#2" "3" "4" "#4" "5" "#5" "6" "#6" "7"))
                     (list "" down nil)
                     (list "" (degree-labels down c)
                           '("1" "7" "b7" "6" "b6" "5" "b5" "4" "3" "b3" "2" "b2"))
                     nil
                     (list "A minor" minor nil)
                     (list "natural" (degree-labels minor a)
                           '("1" "#1" "2" "3" "#3" "4" "#4" "5" "6" "#6" "7" "#7"))
                     (list "harmonic" (degree-labels minor a :harmonic)
                           '("1" "#1" "2" "3" "#3" "4" "#4" "5" "6" "#6" "b7" "7"))
                     (list "melodic" (degree-labels minor a :melodic)
                           '("1" "#1" "2" "3" "#3" "4" "#4" "5" "b6" "6" "b7" "7")))))
    (dolist (row rows)
      (destructuring-bind (&optional (label "") cells expected) row
        (out "  ~11a~{~5a~}~%" label cells)
        (when expected
          (check cells expected (format nil "degrees of ~{~a~^ ~}" cells))))))
  (terpri)
  (para "PITCH-OF-DEGREE is the inverse: a degree, moved by an optional ALTERATION, as a"
        "PITCH-CLASS-NAME, or with OCTAVE as a SPELLED-PITCH in the scale that starts on"
        "the tonic in that octave. Degree 2 of B major in octave 4 is C#5, above B4:")
  (show (pitch-of-degree 2 (parse-key "B") :octave 4) (parse-pitch "C#5"))
  (show (pitch-of-degree 7 (parse-key "C#") :octave 4) (parse-pitch "B#4"))
  (show (pitch-of-degree 4 (parse-key "C") :alteration 1) (parse-pitch-class "F#"))
  (show (pitch-of-degree 6 (parse-key "c") :alteration 1) (parse-pitch-class "A"))
  (show (pitch-of-degree 7 (parse-key "a") :variant :harmonic) (parse-pitch-class "G#"))
  (para ""
        "Degree 7 of C# major is B#. Raised two more semitones it would be B###, with"
        "three sharps, so PITCH-OF-DEGREE signals INVALID-PITCH:")
  (show-signal (pitch-of-degree 7 (parse-key "C#") :alteration 2))
  (terpri)
  (let ((total 0) (ok 0))
    (dolist (key (thirty-keys))
      (dolist (variant '(:natural :harmonic :melodic))
        (dolist (pc (all-spellings))
          (incf total)
          (multiple-value-bind (d a o) (degree-of pc key :variant variant)
            (when (and (null o)
                       (equalp pc (pitch-of-degree d key :alteration a :variant variant)))
              (incf ok)))
          (loop for octave from 2 to 5
                for p = (at-octave pc octave)
                do (incf total)
                   (multiple-value-bind (d a o) (degree-of p key :variant variant)
                     (when (equalp p (pitch-of-degree d key :alteration a :octave o
                                                            :variant variant))
                       (incf ok)))))))
    (format t "All 30 keys, the three variants, and the 35 spellings as classes and in octaves~%~
               2-5 (~d cases): PITCH-OF-DEGREE of the three values of DEGREE-OF gives the~%~
               note back (~d of ~d).~%" total ok total)
    (check (list total ok) '(15750 15750) "degree round trips")))

;;; 14. Roman numerals --------------------------------------------------------------

(defparameter *roman-qualities*
  '(:major :minor :diminished :augmented
    :dominant-7 :major-7 :minor-7 :minor-major-7 :half-diminished-7 :diminished-7
    :augmented-7 :augmented-major-7)
  "The twelve qualities a Roman numeral can name: four triads, then eight sevenths.")

(defun triad-p (roman)
  (member (roman-numeral-quality roman) (subseq *roman-qualities* 0 4)))

(defun revised (roman &key (inversion (roman-numeral-inversion roman))
                           (applied-to (roman-numeral-applied-to roman)))
  "ROMAN with another INVERSION or APPLIED-TO."
  (make-roman-numeral :degree (roman-numeral-degree roman)
                      :alteration (roman-numeral-alteration roman)
                      :quality (roman-numeral-quality roman)
                      :inversion inversion :applied-to applied-to))

(defun roman-tones (roman key)
  "The tones of ROMAN's chord in KEY, root first. CHORD-FOR puts the third, fifth
or seventh in the bass for inversions 1-3, so they are the root and those basses."
  (class-names (cons (chord-symbol-root (chord-for roman key))
                     (loop for inversion from 1 to (if (triad-p roman) 2 3)
                           collect (chord-symbol-bass
                                    (chord-for (revised roman :inversion inversion) key))))))

(defun chord-text (roman key)
  "ROMAN's chord in KEY as its tones from the root, then /bass for an inversion."
  (let ((bass (chord-symbol-bass (chord-for roman key))))
    (format nil "~{~a~^ ~}~@[ /~a~]" (roman-tones roman key) (and bass (pitch-class-string bass)))))

(defun chord (root quality &optional bass)
  "A CHORD-SYMBOL with ROOT, QUALITY and BASS (pitch-class text), the slots a Roman
numeral uses."
  (make-chord-symbol :root (parse-pitch-class root) :quality quality
                     :bass (and bass (parse-pitch-class bass))))

(defun all-numerals ()
  "Every well-formed ROMAN-NUMERAL without a target: 7 degrees x 5 alterations x 44
qualities and inversions (4 triads in 3 positions, 8 sevenths in 4)."
  (loop for degree from 1 to 7
        nconc (loop for alteration from -2 to 2
                    nconc (loop for quality in *roman-qualities*
                                for top = (if (member quality (subseq *roman-qualities* 0 4))
                                              2 3)
                                nconc (loop for inversion from 0 to top
                                            collect (make-roman-numeral
                                                     :degree degree :alteration alteration
                                                     :quality quality :inversion inversion))))))

(defun all-targets ()
  "Every target an applied numeral can have: a root-position major or minor triad
on any degree, with any alteration (70)."
  (loop for degree from 1 to 7
        nconc (loop for alteration from -2 to 2
                    nconc (loop for quality in '(:major :minor)
                                collect (make-roman-numeral :degree degree :alteration alteration
                                                            :quality quality)))))

(defparameter *roman-rows*
  ;; (text ascii unicode in-c-major in-a-minor)
  '(("I" "I" "I" "C E G" "A C# E")
    ("ii6" "ii6" "ii6" "D F A /F" "B D F# /D")
    ("V7" "V7" "V7" "G B D F" "E G# B D")
    ("vii°7" "viio7" "vii°7" "B D F Ab" "G# B D F")
    ("V/V" "V/V" "V/V" "D F# A" "B D# F#")
    ("vii°7/V" "viio7/V" "vii°7/V" "F# A C Eb" "D# F# A C")
    ("bVI" "bVI" "♭VI" "Ab C Eb" "Fb Ab Cb")
    ("N6" "bII6" "♭II6" "Db F Ab /F" "Bb D F /D")))

(defparameter *minor-rows*
  ;; (text chord-in-a-minor), printed in pairs
  '(("VI" "F A C") ("vi" "F# A C#") ("VII" "G B D") ("viio" "G# B D")
    ("v" "E G B") ("V" "E G# B") ("iio6" "B D F /D") ("bVI" "Fb Ab Cb")))

(defparameter *roman-spellings*
  ;; (text ascii unicode)
  '(("V6/5" "V65" "V65") ("V2" "V42" "V42") ("V4/2" "V42" "V42") ("iiø7" "ii/o7" "iiø7")
    ("viio7" "viio7" "vii°7") ("♭VI" "bVI" "♭VI") ("#ivo7" "#ivo7" "♯iv°7")
    ("N" "bII" "♭II") ("N64" "bII64" "♭II64") ("IM7" "IM7" "IM7") ("IVM65" "IVM65" "IVM65")
    ("III+" "III+" "III+") ("vii/o7/V" "vii/o7/V" "viiø7/V") ("V6/5/V" "V65/V" "V65/V")))

(defparameter *reading-rows*
  ;; (root quality bass tones literal target applied), in C major
  '(("D" :major nil "D F# A" "II" "V" "V/V")
    ("D" :dominant-7 nil "D F# A C" "II7" "V" "V7/V")
    ("D" :dominant-7 "F#" "D F# A C /F#" "II65" "V" "V65/V")
    ("F#" :diminished-7 nil "F# A C Eb" "#ivo7" "V" "viio7/V")
    ("E" :dominant-7 nil "E G# B D" "III7" "vi" "V7/vi")
    ("A" :dominant-7 "C#" "A C# E G /C#" "VI65" "ii" "V65/ii")
    ("C" :dominant-7 "Bb" "C E G Bb /Bb" "I42" "IV" "V42/IV")))

(defun demo-roman ()
  (para "A ROMAN-NUMERAL names a triad or seventh chord by the scale degree of its root."
        "It is data with no key: DEGREE 1-7, a prefix ALTERATION (b is -1, # is +1),"
        "QUALITY, INVERSION 0-3, and APPLIED-TO, the target of an applied chord such as"
        "V/V. PARSE-ROMAN reads the usual text. The case gives major or minor, a marker"
        "o or ° diminished, /o or ø half-diminished, + augmented, M a major seventh, and"
        "the figure the inversion: 6 and 64 for triads, 7 65 43 42 for sevenths. N is"
        "the Neapolitan, bII.")
  (show (parse-roman "ii6") (make-roman-numeral :degree 2 :quality :minor :inversion 1))
  (terpri)
  (format t "  ~10a~8a~12a~20a~11a~a~%"
          "text" "degree" "alteration" "quality" "inversion" "applied-to")
  (loop for (text expected) in '(("V7" (5 0 :dominant-7 0 nil))
                                 ("vii°7/V" (7 0 :diminished-7 0 "V"))
                                 ("bVI" (6 -1 :major 0 nil))
                                 ("N6" (2 -1 :major 1 nil))
                                 ("iiø65" (2 0 :half-diminished-7 1 nil))
                                 ("V42/IV" (5 0 :dominant-7 3 "IV")))
        for r = (parse-roman text)
        for target = (roman-numeral-applied-to r)
        for row = (list (roman-numeral-degree r) (roman-numeral-alteration r)
                        (roman-numeral-quality r) (roman-numeral-inversion r)
                        (and target (roman-name target)))
        do (out "  ~10a~8a~12a~20a~11a~a~%" text (first row) (second row)
                (code-text (third row)) (fourth row) (or (fifth row) "nil"))
           (check row expected text))
  (terpri)
  (para "CHORD-FOR spells the chord of a numeral in a key: ROOT, QUALITY and, for an"
        "inversion, BASS. Below, the tones are listed from the root, with /bass for an"
        "inversion. An applied numeral is read in the key its target tonicizes: V/V in"
        "C major is V of G major. ROMAN-NAME prints the canonical text, ASCII or Unicode:")
  (format t "  ~10a~10a~10a~14a~a~%" "text" "ASCII" "Unicode" "in C major" "in A minor")
  (loop for (text ascii unicode in-c in-a) in *roman-rows*
        for r = (parse-roman text)
        for row = (list (roman-name r) (roman-name r :accidentals :unicode)
                        (chord-text r (parse-key "C")) (chord-text r (parse-key "a")))
        do (out "  ~10a~10a~10a~14a~a~%" text (first row) (second row) (third row) (fourth row))
           (check row (list ascii unicode in-c in-a) text))
  (terpri)
  (para "In a minor key the case of the numeral decides degrees 6 and 7: vi, vii° and"
        "vii°7 sit on the raised notes (F# and G# in A minor), VI and VII on the natural"
        "ones (F and G). A prefix accidental moves that default root, so bVI in A minor"
        "is Fb major; F major is plain VI. The other degrees follow the natural scale:"
        "ii in A minor is B minor, and the diatonic triad there is ii° (B D F).")
  (let ((a (parse-key "a")))
    (loop for ((text-1 expected-1) (text-2 expected-2)) on *minor-rows* by #'cddr
          for chord-1 = (chord-text (parse-roman text-1) a)
          for chord-2 = (chord-text (parse-roman text-2) a)
          do (out "  ~6a~16a~6a~a~%" text-1 chord-1 text-2 chord-2)
             (check (list chord-1 chord-2) (list expected-1 expected-2)
                    (format nil "~a and ~a in A minor" text-1 text-2))))
  (terpri)
  (para "PARSE-ROMAN takes other spellings of the same numeral. ROMAN-NAME prints the"
        "canonical one, and both of its forms parse back to the same value:")
  (format t "  ~11a~11a~a~%" "text" "ASCII" "Unicode")
  (loop for (text ascii unicode) in *roman-spellings*
        for r = (parse-roman text)
        for row = (list (roman-name r) (roman-name r :accidentals :unicode))
        do (out "  ~11a~11a~a~%" text (first row) (second row))
           (check row (list ascii unicode) text)
           (check (list (parse-roman ascii) (parse-roman unicode)) (list r r)
                  (format nil "~a parsed back" text)))
  (terpri)
  (para "Other text signals INVALID-ROMAN-NUMERAL with a reason, in the same USE-VALUE"
        "restart. There is no ninth (V9) and no applied chord of an applied chord"
        "(V/V/V); a target is a plain major or minor triad, not viio or V7; augmented"
        "sixths (Ger65) are not numerals:")
  (let* ((inputs '("V65/V" "VIII" "Vi" "Io" "v+" "IM" "iiø" "V9" "N7" "V/V/V" "V/viio"
                   "V7/V7" "V/" "Ger65" " V 7" ""))
         (parsed (handler-bind ((invalid-roman-numeral
                                  (lambda (c)
                                    (format t "  ~9s ~a~%" (music-theory-error-text c)
                                            (music-theory-error-reason c))
                                    (use-value nil c))))
                   (mapcar #'parse-roman inputs))))
    (format t "  => ~{~a~^ ~}~%~%" (mapcar (lambda (r) (if r (roman-name r) "nil")) parsed))
    (check (mapcar (lambda (r) (and r (roman-name r))) parsed)
           (cons "V65/V" (make-list 15)) "numerals parsed with use-value"))
  (para "ROMAN-FOR goes back from a chord to a numeral. It reads the chord literally: a"
        "chromatic chord gets a prefix accidental and never an applied reading, because"
        "choosing one is analysis. With :APPLIED-TO it reads the chord in the key that"
        "target tonicizes. CHORD below is this demo's shorthand for MAKE-CHORD-SYMBOL"
        "(a third argument names the BASS):"
        "  (chord \"F#\" :diminished-7)"
        "  = (make-chord-symbol :root (parse-pitch-class \"F#\") :quality :diminished-7)"
        "In C major:")
  (check (chord "F#" :diminished-7 "A")
         (make-chord-symbol :root (parse-pitch-class "F#") :quality :diminished-7
                            :bass (parse-pitch-class "A"))
         "the CHORD shorthand")
  (check (roman-for (chord "F#" :diminished-7) (parse-key "C"))
         (make-roman-numeral :degree 4 :alteration 1 :quality :diminished-7)
         "F# diminished-7 in C major")
  (show (roman-name (roman-for (chord "F#" :diminished-7) (parse-key "C"))) "#ivo7")
  (show (roman-name (roman-for (chord "F#" :diminished-7) (parse-key "C")
                               :applied-to (parse-roman "V")))
        "viio7/V")
  (terpri)
  (para "More chords in C major, read literally and as applied chords:")
  (format t "  ~18a~15a~9a~12a~a~%" "chord" "tones" "literal" "applied to" "reading")
  (let ((c (parse-key "C")))
    (loop for (root quality bass tones literal target applied) in *reading-rows*
          for chord = (chord root quality bass)
          for plain = (roman-for chord c)
          for reading = (roman-for chord c :applied-to (parse-roman target))
          for row = (list (chord-text plain c) (roman-name plain) (roman-name reading))
          do (out "  ~18a~15a~9a~12a~a~%" (format nil "~a ~(~a~)~@[ /~a~]" root quality bass)
                  (first row) (second row) target (third row))
             (check row (list tones literal applied) (format nil "~a ~a" root quality))))
  (terpri)
  (para "A chord that no numeral names signals MUSIC-THEORY-ERROR:")
  (show-signal (roman-for (chord "C" :major "D") (parse-key "C")))
  (show-signal (roman-for (chord "C" :major-6) (parse-key "C")))
  (show-signal (roman-for (make-chord-symbol :no-chord-p t) (parse-key "C")))
  (show-signal (roman-for (chord "D" :dominant-7) (parse-key "C") :applied-to (parse-roman "V7")))
  (terpri)
  (let* ((plain (all-numerals))
         (targets (all-targets))
         (numerals (append plain
                           (loop for r in plain
                                 nconc (mapcar (lambda (target) (revised r :applied-to target))
                                               targets))))
         (ok (count-if (lambda (r)
                         (and (equalp r (parse-roman (roman-name r)))
                              (equalp r (parse-roman (roman-name r :accidentals :unicode)))))
                       numerals)))
    (format t "Every well-formed numeral: 7 degrees x 5 alterations x 44 qualities and~%~
               inversions (~d), alone or applied to one of ~d targets (~d in all). Each~%~
               parses back from its ASCII and its Unicode name (~d of ~d).~%~%"
            (length plain) (length targets) (length numerals) ok (length numerals))
    (check (list (length plain) (length targets) ok) (list 1540 70 (length numerals))
           "numeral text round trips")
    (let ((total 0) (back 0) (unspellable 0))
      (dolist (key (thirty-keys))
        (dolist (r plain)
          (incf total)
          (handler-case (let ((chord (chord-for r key)))
                          (when (equalp r (ignore-errors (roman-for chord key)))
                            (incf back)))
            (invalid-pitch () (incf unspellable)))))
      (format t "In all 30 keys, ROMAN-FOR of CHORD-FOR gives each of those ~d numerals back~%~
                 (~d of ~d), except where a chord tone would need three accidentals and~%~
                 CHORD-FOR signals INVALID-PITCH (~d, such as #III in C# major: E## G### B##).~%~%"
              (length plain) back total unspellable)
      (check (+ back unspellable) total "chord round trips"))
    (let ((total 0) (back 0) (unspellable 0))
      (dolist (key (list (parse-key "C") (parse-key "a")))
        (dolist (r plain)
          (dolist (target targets)
            (let ((applied (revised r :applied-to target)))
              (incf total)
              (handler-case (let ((chord (chord-for applied key)))
                              (when (equalp applied
                                            (ignore-errors (roman-for chord key :applied-to target)))
                                (incf back)))
                (invalid-pitch () (incf unspellable)))))))
      (format t "Applied chords, in C major and A minor: each of the ~d numerals on each of~%~
                 the ~d targets (~d) comes back from ROMAN-FOR with :APPLIED-TO (~d), or~%~
                 cannot be spelled (~d).~%" (length plain) (length targets) total back unspellable)
      (check (+ back unspellable) total "applied chord round trips"))))

;;; 15. Mixture -------------------------------------------------------------------

(defun demo-mixture ()
  (para "Mixture is not a slot of the numeral: iv is borrowed in C major but diatonic in"
        "C minor. ROMAN-MIXTURE-P is true when the numeral's chord is not diatonic to the"
        "key but is diatonic to the parallel key, the same tonic in the other mode. The"
        "diatonic chords of minor include the harmonic minor's V, V7, vii° and vii°7.")
  (show (roman-mixture-p (parse-roman "bVI") (parse-key "C")) t "Ab C Eb, VI of C minor")
  (show (roman-mixture-p (parse-roman "iv") (parse-key "C")) t "F Ab C, iv of C minor")
  (show (roman-mixture-p (parse-roman "bVII") (parse-key "C")) t "Bb D F, VII of C minor")
  (show (roman-mixture-p (parse-roman "IV") (parse-key "C")) nil "diatonic")
  (show (roman-mixture-p (parse-roman "N6") (parse-key "C")) nil "Db: not in C minor either")
  (show (roman-mixture-p (parse-roman "V/V") (parse-key "C")) nil "V of G major")
  (show (roman-mixture-p (parse-roman "iv/V") (parse-key "C")) t "C minor, borrowed in G major")
  (terpri)
  (para "bVI, iv and bVII in each major key, spelled by CHORD-FOR. Each one is VI, iv or"
        "VII of the parallel minor, which is what makes it mixture:")
  (format t "  ~10a~13a~13a~13a~a~%" "key" "bVI" "iv" "bVII" "mixture")
  (let ((borrowed (mapcar #'parse-roman '("bVI" "iv" "bVII")))
        (sources (mapcar #'parse-roman '("VI" "iv" "VII")))
        (ok 0))
    (dolist (key (remove :minor (thirty-keys) :key #'key-mode))
      (let ((minor (make-key :tonic (key-tonic key) :mode :minor))
            (texts (mapcar (lambda (r) (chord-text r key)) borrowed))
            (flags (mapcar (lambda (r) (roman-mixture-p r key)) borrowed)))
        (out "  ~10a~{~13a~}~a~%" (key-name key) texts (if (every #'identity flags) "yes" "no"))
        (when (and (every #'identity flags)
                   (equal texts (mapcar (lambda (r) (chord-text r minor)) sources)))
          (incf ok))))
    (check ok 15 "mixture in major keys")
    (check (mapcar (lambda (r) (chord-text r (parse-key "C"))) borrowed)
           '("Ab C Eb" "F Ab C" "Bb D F") "borrowed in C major")
    (check (mapcar (lambda (r) (chord-text r (parse-key "Eb"))) borrowed)
           '("Cb Eb Gb" "Ab Cb Eb" "Db F Ab") "borrowed in Eb major"))
  (terpri)
  (para "In a minor key the borrowing runs the other way. In C minor, I, ii, IV, vi and"
        "viiø7 come from C major; V and vii° belong to the harmonic minor, so they are"
        "not mixture:")
  (let* ((c-minor (parse-key "c"))
         (texts '("i" "iio" "III" "iv" "v" "V" "VI" "VII" "viio" "viio7"
                  "I" "ii" "IV" "vi" "vii/o7"))
         (mixture (remove-if-not (lambda (s) (roman-mixture-p (parse-roman s) c-minor)) texts))
         (diatonic (remove-if (lambda (s) (roman-mixture-p (parse-roman s) c-minor)) texts)))
    (out "  ~13a~{~a~^ ~}~%  ~13a~{~a~^ ~}~%" "not mixture" diatonic "mixture" mixture)
    (check (list diatonic mixture)
           '(("i" "iio" "III" "iv" "v" "V" "VI" "VII" "viio" "viio7")
             ("I" "ii" "IV" "vi" "vii/o7"))
           "mixture in C minor")))

;;; 16. A progression, to chords and back -------------------------------------------

(defun progression-line (texts key)
  "The chords of the numerals TEXTS in KEY, separated by |."
  (format nil "~{~a~^ | ~}" (mapcar (lambda (text) (chord-text (parse-roman text) key)) texts)))

(defun demo-progression ()
  (para "A small round trip: I-vi-ii6-V7-I in G major. CHORD-FOR turns each numeral into"
        "a chord, and ROMAN-FOR reads each chord back. ii6 puts C, the third of A minor,"
        "in the bass, so the bass line is G E C D G, degrees 1 6 4 5 1.")
  (let* ((key (parse-key "G"))
         (numerals (mapcar #'parse-roman '("I" "vi" "ii6" "V7" "I")))
         (chords (mapcar (lambda (r) (chord-for r key)) numerals))
         (back (mapcar (lambda (chord) (roman-for chord key)) chords))
         (basses (mapcar (lambda (chord) (or (chord-symbol-bass chord) (chord-symbol-root chord)))
                         chords))
         (degrees (mapcar (lambda (bass) (degree-label (degree-of bass key) 0)) basses)))
    (format t "  ~9a~6a~12a~6a~11a~a~%" "numeral" "root" "quality" "bass" "tones" "ROMAN-FOR")
    (loop for r in numerals
          for chord in chords
          for b in back
          do (out "  ~9a~6a~12a~6a~11a~a~%" (roman-name r)
                  (pitch-class-string (chord-symbol-root chord))
                  (string-downcase (symbol-name (chord-symbol-quality chord)))
                  (if (chord-symbol-bass chord) (pitch-class-string (chord-symbol-bass chord)) "-")
                  (format nil "~{~a~^ ~}" (roman-tones r key))
                  (roman-name b)))
    (format t "~%  bass line  ~{~a~^ ~}   degrees ~{~a~^ ~}~%~%" (class-names basses) degrees)
    (check (mapcar (lambda (r) (roman-tones r key)) numerals)
           '(("G" "B" "D") ("E" "G" "B") ("A" "C" "E") ("D" "F#" "A" "C") ("G" "B" "D"))
           "progression tones")
    (check (list (class-names basses) degrees)
           '(("G" "E" "C" "D" "G") ("1" "6" "4" "5" "1")) "bass line")
    (check back numerals "progression read back"))
  (para "The numerals hold no key, so the same list is the same progression in any major"
        "key. In minor the same functions are i-VI-iio6-V7-i:")
  (let ((rows (list (list "Eb major" '("I" "vi" "ii6" "V7" "I")
                          "Eb G Bb | C Eb G | F Ab C /Ab | Bb D F Ab | Eb G Bb")
                    (list "F# major" '("I" "vi" "ii6" "V7" "I")
                          "F# A# C# | D# F# A# | G# B D# /B | C# E# G# B | F# A# C#")
                    (list "E minor" '("i" "VI" "iio6" "V7" "i")
                          "E G B | C E G | F# A C /A | B D# F# A | E G B"))))
    (loop for (name texts expected) in rows
          for line = (progression-line texts (parse-key name))
          do (out "  ~10a~{~a~^-~}~%  ~10a~a~%" name texts "" line)
             (check line expected (format nil "progression in ~a" name)))))

;;; 17. A lead-sheet chord chart (MT3) ------------------------------------------

(defparameter *degree-fifths*
  '((2 . 2) (4 . -1) (5 . 1) (6 . 3) (9 . 2) (11 . -1) (13 . 3))
  "The line-of-fifths offset above the root of the natural interval of each degree
that an alteration or an added tone can name: M2 +2, P4 -1, P5 +1, M6 +3. A sharp
adds 7 to the offset, a flat takes 7 away.")

(defun tone-label (chord pair)
  "The degree of PAIR, one of CHORD's tones, with b or # when CHORD alters that
tone or adds it altered: 9 for the D of C9, b9 for the Db of C7(b9), #4 for the F#
of C5(add#4)."
  (destructuring-bind (degree . pitch-class) pair
    (let* ((natural (cdr (assoc degree *degree-fifths*)))
           (alter (and natural
                       (/ (- (pitch-fifths pitch-class)
                             (pitch-fifths (chord-symbol-root chord))
                             natural)
                          7)))
           (sign (and alter
                      (member (cons degree alter)
                              (append (chord-symbol-alterations chord)
                                      (chord-symbol-added chord))
                              :test #'equal)
                      (case alter (-1 "b") (1 "#")))))
      (format nil "~@[~a~]~d" sign degree))))

(defun tone-names (pairs &optional (accidentals :ascii))
  "The pitch classes of the (degree . pitch class) PAIRS, as one string."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (pair) (pitch-class-string (cdr pair) :accidentals accidentals))
                  pairs)))

(defun tone-degrees (chord pairs)
  "The degrees of PAIRS, tones of CHORD, as one string (see TONE-LABEL)."
  (format nil "~{~a~^ ~}" (mapcar (lambda (pair) (tone-label chord pair)) pairs)))

(defun keyword-text (keyword)
  (if keyword (string-downcase (symbol-name keyword)) "-"))

(defun list-text (items)
  "ITEMS joined by spaces, or - when there are none."
  (if items (format nil "~{~a~^ ~}" items) "-"))

(defun alter-text (pair)
  "A (degree . alter) pair as b9, #11 or 9."
  (format nil "~a~d" (case (cdr pair) (-1 "b") (1 "#") (t "")) (car pair)))

(defun dash (text)
  (if (string= text "") "-" text))

(defun print-wrapped (label items &key (width 84))
  "Print LABEL and then ITEMS separated by spaces, wrapped at WIDTH columns, the
continuation lines indented under the first item."
  (let ((column 11))
    (format t "  ~9a" label)
    (loop for item in items
          for first = t then nil
          for length = (length item)
          do (cond (first)
                   ((> (+ column 1 length) width)
                    (format t "~%~11@a" "")
                    (setf column 11))
                   (t (write-char #\Space) (incf column)))
             (write-string item)
             (incf column length))
    (terpri)))

(defun quoted-empty (text)
  (if (string= text "") "\"\"" text))

(defun chord-slots (chord)
  "The slots of CHORD that a print and parse round trip keeps: all but TEXT and DIALECT."
  (list (chord-symbol-root chord) (chord-symbol-quality chord)
        (chord-symbol-extensions chord) (chord-symbol-alterations chord)
        (chord-symbol-suspension chord) (chord-symbol-added chord)
        (chord-symbol-omitted chord) (chord-symbol-bass chord)
        (chord-symbol-no-chord-p chord)))

(defun same-chord-p (a b)
  "True when the chord symbols A and B are equal in every slot but TEXT and DIALECT."
  (equalp (chord-slots a) (chord-slots b)))

(defun pretty-text (object)
  "OBJECT pretty-printed as it reads in this package, at most 78 columns wide."
  (let ((*package* (find-package '#:harness-demos-music-theory))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 78))
    (prin1-to-string object)))

(defparameter *chart* "| C  Am7 | Dm7  G7 | Cmaj7  A7b9 | D9sus4  G13 | C6/9  N.C. |")

(defun chart-bars (chart)
  "The bars of a one-line CHART, each a list of the symbol texts in it: the text
between bar lines, split at spaces."
  (loop for bar in (uiop:split-string chart :separator "|")
        for texts = (remove "" (uiop:split-string bar :separator " ") :test #'string=)
        when texts collect texts))

(defparameter *chart-rows*
  ;; (text degrees tones printed)
  '(("C" "1 3 5" "C E G" "C")
    ("Am7" "1 3 5 7" "A C E G" "Am7")
    ("Dm7" "1 3 5 7" "D F A C" "Dm7")
    ("G7" "1 3 5 7" "G B D F" "G7")
    ("Cmaj7" "1 3 5 7" "C E G B" "Cmaj7")
    ("A7b9" "1 3 5 7 b9" "A C# E G Bb" "A7(b9)")
    ("D9sus4" "1 4 5 7 9" "D G A C E" "D9sus4")
    ("G13" "1 3 5 7 9 11 13" "G B D F A C E" "G13")
    ("C6/9" "1 3 5 6 9" "C E G A D" "C6/9")
    ("N.C." "" "" "N.C.")))

(defun demo-chart ()
  (para "A CHORD-SYMBOL is the value behind lead-sheet text such as Cmaj7 or G7/B: ROOT,"
        "QUALITY (one of 15 keywords), EXTENSIONS (9, 11, 13), ALTERATIONS and ADDED as"
        "(degree . alter) pairs, SUSPENSION 2 or 4, OMITTED degrees, BASS, and NO-CHORD-P"
        "for N.C. PARSE-CHORD-SYMBOL reads text with a closed grammar (the EBNF is in the"
        "header of src/chord-symbol.lisp) by longest match, without backtracking or"
        "regular expressions. PRINT-CHORD-SYMBOL writes the canonical text. TEXT keeps"
        "the input as written and DIALECT the dialect it was read in, :POP by default.")
  (let ((chord (parse-chord-symbol "A7b9")))
    (format t "  (parse-chord-symbol \"A7b9\")~%    => ~a~%" (indent (pretty-text chord) 7))
    (check (chord-slots chord)
           (list (parse-pitch-class "A") :dominant-7 '() '((9 . -1)) nil '() '() nil nil)
           "A7b9 slots"))
  (show (print-chord-symbol (parse-chord-symbol "A7b9")) "A7(b9)")
  (show (chord-symbol-text (parse-chord-symbol "A7b9")) "A7b9")
  (terpri)
  (para "A chart on one line, split at the bar lines and the spaces, with every symbol"
        "parsed. A seventh word gives its quality, and its number the extensions: 9 is"
        "(9), 13 is (9 11 13).")
  (format t "  ~a~%~%" *chart*)
  (let ((bars (chart-bars *chart*)))
    (format t "  ~5a~9a~6a~13a~12a~13a~12a~a~%"
            "bar" "text" "root" "quality" "extensions" "alterations" "suspension" "added")
    (loop for texts in bars
          for bar from 1
          do (dolist (text texts)
               (let ((c (parse-chord-symbol text)))
                 (if (chord-symbol-no-chord-p c)
                     (out "  ~5a~9a~a~%" bar text "no chord: NO-CHORD-P is T, the other slots empty")
                     (out "  ~5a~9a~6a~13a~12a~13a~12a~a~%" bar text
                          (pitch-class-string (chord-symbol-root c))
                          (keyword-text (chord-symbol-quality c))
                          (list-text (chord-symbol-extensions c))
                          (list-text (mapcar #'alter-text (chord-symbol-alterations c)))
                          (or (chord-symbol-suspension c) "-")
                          (list-text (mapcar #'alter-text (chord-symbol-added c))))))))
    (check bars '(("C" "Am7") ("Dm7" "G7") ("Cmaj7" "A7b9") ("D9sus4" "G13") ("C6/9" "N.C."))
           "chart bars")
    (terpri)
    (para "CHORD-TONES realises a chord: its tones as (degree . PITCH-CLASS-NAME) pairs,"
          "spelled exactly, ascending by degree. Below, each list is written as its degrees"
          "(b9 for a flat ninth) and its names. A suspension replaces the 3 (D9sus4 has G,"
          "not F#), the 6/9 chord is the sixth chord with an added 9, and G13 lists its 11"
          "as MusicXML's dominant 13th does (section 20 shows which tones to keep). N.C."
          "has no tones.")
    (format t "  ~9a~18a~16a~a~%" "text" "degrees" "tones" "PRINT-CHORD-SYMBOL")
    (let ((rows (loop for text in (reduce #'append bars)
                      for c = (parse-chord-symbol text)
                      for tones = (chord-tones c)
                      collect (list text (tone-degrees c tones) (tone-names tones)
                                    (print-chord-symbol c)))))
      (loop for (text degrees tones printed) in rows
            do (out "  ~9a~18a~16a~a~%" text (dash degrees) (dash tones) printed))
      (check rows *chart-rows* "chart tones"))))

;;; 18. Dialects ------------------------------------------------------------------

(defparameter *dialects* '(:classical :pop :jazz))

(defparameter *dialect-readings*
  ;; (text quality-in-classical quality-in-pop quality-in-jazz)
  '(("Cdim" :diminished :diminished-7 :diminished-7)
    ("C°" :diminished :diminished-7 :diminished-7)
    ("C-7" :minor-7 :minor-7 :minor-7)
    ("CΔ7" :major-7 :major-7 :major-7)
    ("Cø" :half-diminished-7 :half-diminished-7 :half-diminished-7)
    ("C7alt" :dominant-7 :dominant-7 :dominant-7)
    ("C+" :augmented :augmented :augmented)))

(defparameter *dialect-prints*
  ;; (tones classical pop jazz), one row per distinct value read above
  '(("C Eb Gb" "C°" "Cm(b5)" "C-(b5)")
    ("C Eb Gb Bbb" "C°7" "Cdim7" "C°7")
    ("C Eb G Bb" "Cm7" "Cm7" "C-7")
    ("C E G B" "CM7" "Cmaj7" "CΔ7")
    ("C Eb Gb Bb" "Cø7" "Cm7(b5)" "Cø7")
    ("C E Bb Db D# F# Ab" "C7(b9,#9,#11,b13,omit5)" "C7(b9,#9,#11,b13,omit5)" "C7alt")
    ("C E G#" "C+" "C+" "C+")))

(defparameter *sweep-roots* '("C" "F#" "Bb"))

(defparameter *sweep-words*
  '("" "m" "-" "dim" "o" "°" "aug" "+" "5" "6" "m6" "-6" "6/9" "m6/9" "-6/9"
    "7" "maj7" "M7" "Δ7" "Δ" "m7" "-7" "mM7" "mmaj7" "-Δ7" "m(maj7)" "ø" "ø7"
    "dim7" "°7" "+7" "+M7" "+Δ" "9" "maj9" "m9" "-9" "mmaj9" "ø9" "°9" "+9"
    "11" "maj11" "m11" "ø11" "13" "maj13" "m13" "-13" "ø13" "+13")
  "Quality words for the round-trip sweep, some from each kind of row of the grammar.")

(defparameter *sweep-endings*
  '("" "(b5)" "(#5)" "(b9)" "(#9)" "(#11)" "(b13)" "(b9,#9)" "(b5,#5)" "alt"
    "(add9)" "(add2)" "(add4)" "(add6)" "(addb6)" "(add#4)" "(add11)" "(add13)"
    "(omit3)" "(omit5)" "(omit9)" "(omit11)" "sus4" "sus2" "sus4(b9)" "sus2(add6)"
    "(b9,omit5)" "(#11,omit3)" "(#5,add9)" "(add9,omit5)" "/E" "/F#" "(b9)/Ab")
  "What follows the quality word in the round-trip sweep.")

(defun issue-10-p (chord printed)
  "True for the shape of egao1980/music-theory#10: CHORD omits its 5, and PRINTED
writes a b5 next to the omit5."
  (and (member 5 (chord-symbol-omitted chord))
       (or (search "b5" printed) (search "♭5" printed))
       t))

(defun round-trip-sweep ()
  "Parse root + word + ending for every entry of the sweep tables in each dialect,
then print every distinct value in each dialect and style and read it back. Returns
(values texts rejected distinct trips held known others): KNOWN and OTHERS are the
printed texts that failed, of the shape of music-theory#10 or not."
  (let ((values (make-hash-table :test 'equalp))
        (texts 0) (rejected 0) (trips 0) (held 0) (known '()) (others '()))
    (dolist (root *sweep-roots*)
      (dolist (word *sweep-words*)
        (dolist (ending *sweep-endings*)
          (dolist (dialect *dialects*)
            (let ((text (concatenate 'string root word ending)))
              (incf texts)
              (handler-case (let ((c (parse-chord-symbol text :dialect dialect)))
                              (setf (gethash (chord-slots c) values) c))
                ((or chord-symbol-parse-error unknown-chord-quality) () (incf rejected))))))))
    (loop for c being the hash-values of values
          do (dolist (dialect *dialects*)
               (dolist (style '(:ascii :unicode))
                 (incf trips)
                 (let* ((printed (print-chord-symbol c :dialect dialect :accidentals style))
                        (back (handler-case (parse-chord-symbol printed :dialect dialect)
                                (chord-symbol-parse-error () nil))))
                   (cond ((and back (same-chord-p back c)
                               (string= printed (print-chord-symbol back :dialect dialect
                                                                         :accidentals style)))
                          (incf held))
                         ((issue-10-p c printed) (push printed known))
                         (t (push printed others)))))))
    (values texts rejected (hash-table-count values) trips held
            (sort known #'string<) (sort others #'string<))))

(defun demo-dialects ()
  (para "PARSE-CHORD-SYMBOL takes :DIALECT :CLASSICAL, :POP (the default) or :JAZZ. All"
        "three read the same grammar, and differ in one place only: dim, o and ° alone"
        "are the diminished triad in :CLASSICAL and the diminished seventh in :POP and"
        ":JAZZ. Cdim7, Co7 and C°7 are the seventh in all three. The other signs mean the"
        "same everywhere: - minor, Δ major seventh, ø half-diminished seventh (m7b5),"
        "+ augmented, and 7alt the altered dominant (b9 #9 #11 b13, no 5). The quality"
        "each text reads as:")
  (format t "  ~8a~20a~20a~a~%" "text" ":classical" ":pop" ":jazz")
  (let ((values '()))
    (let ((rows (loop for (text) in *dialect-readings*
                      collect (cons text
                                    (loop for dialect in *dialects*
                                          for c = (parse-chord-symbol text :dialect dialect)
                                          do (pushnew c values :test #'same-chord-p)
                                          collect (chord-symbol-quality c))))))
      (dolist (row rows)
        (out "  ~8a~{~20a~}~%" (first row) (mapcar #'keyword-text (rest row))))
      (check rows *dialect-readings* "qualities read in each dialect"))
    (terpri)
    (para "Printing is where the dialects differ most. PRINT-CHORD-SYMBOL writes each value"
          "read above in each dialect's words: :CLASSICAL ° ø + and M, :POP plain text"
          "(m(b5), m7(b5), dim7, maj), :JAZZ - Δ ø °, with 7alt for the altered dominant."
          "Only a dialect with a word for a chord prints it; otherwise the tones go in the"
          "group:")
    (format t "  ~20a~25a~25a~a~%" "tones" ":classical" ":pop" ":jazz")
    (let ((rows (loop for c in (reverse values)
                      collect (cons (tone-names (chord-tones c))
                                    (mapcar (lambda (dialect)
                                              (print-chord-symbol c :dialect dialect))
                                            *dialects*)))))
      (dolist (row rows)
        (out "  ~20a~{~25a~}~%" (first row) (rest row)))
      (check rows *dialect-prints* "printed in each dialect")))
  (terpri)
  (para "Printed text reads back. The library promises that for every well-formed chord,"
        "dialect and accidental style, parsing the printed text in that dialect gives the"
        "same value, and printing that again the same text. A sweep: every root, quality"
        "word and ending below, read in each dialect, then each distinct value printed and"
        "read back in each dialect and style.")
  (print-wrapped "roots" *sweep-roots*)
  (print-wrapped "words" (mapcar #'quoted-empty *sweep-words*))
  (print-wrapped "endings" (mapcar #'quoted-empty *sweep-endings*))
  (terpri)
  (multiple-value-bind (texts rejected distinct trips held known others) (round-trip-sweep)
    (format t "  ~:d texts, ~:d of them rejected (outside the grammar, or against R1-R6)~%" texts rejected)
    (format t "  ~:d distinct values, ~:d round trips (3 dialects x 2 styles each):~%" distinct trips)
    (format t "    ~:d give the same value back, and the same text when printed again~%" held)
    (format t "    ~:d fail, all of the shape of music-theory#10 (below)~%" (length known))
    (format t "    ~:d fail in any other way~%~%" (length others))
    (when others
      (format t "  !! other failures: ~{~a~^ ~}~%" (subseq others 0 (min 10 (length others)))))
    (check others '() "round trips not of the shape of #10")
    (check (+ held (length known)) trips "round trips counted")
    (para "Known issue, egao1980/music-theory#10 (open): a diminished triad or a"
          "half-diminished seventh without its 5 prints in :POP (the triad in :JAZZ too)"
          "as the minor chord with b5 and omit5, and rule R5 then rejects an omitted 5 that"
          "is also altered. Half of the failures are ASCII texts and half the same texts"
          "in Unicode. The ASCII ones with the root C, then the example from the issue:")
    (let ((ascii (remove-duplicates (remove-if (lambda (s) (find #\♭ s)) known) :test #'string=)))
      (format t "  ~d ASCII, ~d Unicode~%" (length ascii) (- (length known) (length ascii)))
      (print-wrapped "root C" (remove-if-not (lambda (s) (member (char s 1) '(#\m #\-))) ascii))
      (terpri)))
  (show (print-chord-symbol (parse-chord-symbol "C°(omit5)" :dialect :classical) :dialect :pop)
        "Cm(b5,omit5)")
  (show-signal (parse-chord-symbol "Cm(b5,omit5)") :checked nil))

;;; 19. Unicode and ASCII, slash chords ---------------------------------------------

(defparameter *style-rows*
  ;; (ascii unicode printed-ascii printed-unicode)
  '(("F#m7b5" "F♯m7♭5" "F#m7(b5)" "F♯m7(♭5)")
    ("Bb7#9" "B♭7♯9" "Bb7(#9)" "B♭7(♯9)")
    ("Gb13#11" "G♭13♯11" "Gb13(#11)" "G♭13(♯11)")
    ("Ebbmaj7" "E𝄫maj7" "Ebbmaj7" "E𝄫maj7")
    ("Fxm" "F𝄪m" "F##m" "F𝄪m")
    ("Db/Ab" "D♭/A♭" "Db/Ab" "D♭/A♭")))

(defparameter *slash-rows*
  ;; (text bass bass-degree tones printed)
  '(("G7/B" "B" "3" "G B D F" "G7/B")
    ("Dm7/C" "C" "7" "D F A C" "Dm7/C")
    ("C6/9/E" "E" "3" "C E G A D" "C6/9/E")
    ("G7(b9)/F" "F" "7" "G B D F Ab" "G7(b9)/F")
    ("Cmaj7/B" "B" "7" "C E G B" "Cmaj7/B")
    ("C/F#" "F#" "-" "C E G" "C/F#")
    ("C/F♯" "F#" "-" "C E G" "C/F#")
    ("Ab/Bb" "Bb" "-" "Ab C Eb" "Ab/Bb")
    ("D/C" "C" "-" "D F# A" "D/C")))

(defun demo-styles ()
  (para "Accidentals are ASCII (b # bb ## x) or Unicode (♭ ♯ 𝄫 𝄪), in the root, the bass"
        "and the modifiers alike, and both spellings read as the same value. In a"
        "modifier - is a flat and + a sharp (C7-9, C7+5); right after the root letter -"
        "is minor. PRINT-CHORD-SYMBOL prints :ASCII by default or :UNICODE; the style"
        "spells the root, the bass and the group, while the dialect's words stay fixed,"
        "so :JAZZ prints ø either way. SAME-CHORD-P (this demo) compares every slot but"
        "TEXT and DIALECT.")
  (show (same-chord-p (parse-chord-symbol "F♯m7♭5") (parse-chord-symbol "F#m7b5")) t)
  (show (chord-symbol-quality (parse-chord-symbol "F♯m7♭5")) :half-diminished-7
        "m7 with b5 folds into ø7")
  (show (print-chord-symbol (parse-chord-symbol "F♯m7♭5")) "F#m7(b5)")
  (show (print-chord-symbol (parse-chord-symbol "F#m7b5") :accidentals :unicode) "F♯m7(♭5)")
  (show (print-chord-symbol (parse-chord-symbol "F#m7b5") :dialect :jazz) "F#ø7")
  (show (print-chord-symbol (parse-chord-symbol "F#m7b5") :dialect :jazz :accidentals :unicode)
        "F♯ø7")
  (show (print-chord-symbol (parse-chord-symbol "C7-9")) "C7(b9)")
  (show (print-chord-symbol (parse-chord-symbol "C7+5")) "C+7" "C7 with #5 folds into C+7")
  (show (chord-symbol-quality (parse-chord-symbol "C-7")) :minor-7)
  (terpri)
  (format t "  ~10a~12a~7a~12a~a~%" "ASCII" "Unicode" "same" ":ascii" ":unicode")
  (let* ((same 0)
         (rows (loop for (ascii unicode) in *style-rows*
                     for a = (parse-chord-symbol ascii)
                     for u = (parse-chord-symbol unicode)
                     for row = (list ascii unicode (print-chord-symbol u)
                                     (print-chord-symbol a :accidentals :unicode))
                     do (when (same-chord-p a u) (incf same))
                        (out "  ~10a~12a~7a~12a~a~%" ascii unicode
                             (if (same-chord-p a u) "yes" "no") (third row) (fourth row))
                     collect row)))
    (check rows *style-rows* "ASCII and Unicode printing")
    (check same (length *style-rows*) "ASCII and Unicode read as one value"))
  (terpri)
  (para "A slash gives the bass. It can be a chord tone (G7/B has the 3 in the bass, the"
        "first inversion) or not (C/F#, a C triad over F#). CHORD-TONES lists the chord's"
        "own tones, so the bass is among them only when it is a chord tone anyway; it is"
        "always (CHORD-SYMBOL-BASS c). 6/9 is one word, so C6/9/E is the 6/9 chord over E.")
  (format t "  ~11a~6a~10a~14a~a~%" "text" "bass" "in chord" "tones" "printed")
  (let ((rows (loop for (text) in *slash-rows*
                    for c = (parse-chord-symbol text)
                    for tones = (chord-tones c)
                    for bass = (chord-symbol-bass c)
                    for tone = (find bass tones :key #'cdr :test #'equalp)
                    collect (list text (pitch-class-string bass)
                                  (if tone (tone-label c tone) "-")
                                  (tone-names tones) (print-chord-symbol c)))))
    (loop for (text bass degree tones printed) in rows
          do (out "  ~11a~6a~10a~14a~a~%" text bass degree tones printed))
    (check rows *slash-rows* "slash chords"))
  (show (print-chord-symbol (parse-chord-symbol "C/F#") :accidentals :unicode) "C/F♯"))

;;; 20. Essential tones -------------------------------------------------------------

(defparameter *essential-rows*
  ;; (text all-degrees all-tones n3-degrees n3-tones n4-degrees n4-tones)
  '(("G13" "1 3 5 7 9 11 13" "G B D F A C E" "3 7 13" "B F E" "3 7 13 1" "B F E G")
    ("C7#9" "1 3 5 7 #9" "C E G Bb D#" "3 7 #9" "E Bb D#" "3 7 #9 1" "E Bb D# C")
    ("Fm11" "1 3 5 7 9 11" "F Ab C Eb G Bb" "3 7 11" "Ab Eb Bb" "3 7 11 1" "Ab Eb Bb F")
    ("Bø7" "1 3 5 7" "B D F A" "3 7 5" "D A F" "3 7 5 1" "D A F B")))

(defparameter *priority-rows*
  ;; (text priority-order left-out)
  '(("C" "1 3 5" "-")
    ("Cadd9" "1 3 9 5" "-")
    ("C6" "3 6 1 5" "-")
    ("C7" "3 7 1 5" "-")
    ("C7sus4" "4 7 1 5" "-")
    ("C7(omit3)" "7 1 5" "-")
    ("C9" "3 7 9 1 5" "-")
    ("C11" "11 7 1 9 5" "3")
    ("C13" "3 7 13 1 9 5" "11")
    ("C13(#11)" "3 7 13 #11 1 9 5" "-")
    ("C13(b9)" "3 7 13 b9 1 5" "11")
    ("C7alt" "3 7 b9 #9 #11 b13 1" "-")
    ("Cm11" "3 7 11 1 9 5" "-")
    ("C°7" "3 7 5 1" "-")
    ("C+7" "3 7 5 1" "-")))

(defun demo-essential ()
  (para "Fewer voices than chord tones: CHORD-ESSENTIAL-TONES chord n returns at most N of"
        "the chord's tones, the most important first, which is what a three-voice choir or"
        "a guitar trio would keep. The third and the seventh come first (they give the"
        "quality), then the top extension, a fifth the quality alters (the b5 of Bø7),"
        "the altered and added tones, the root, the lower extensions, and a perfect fifth"
        "last. Each chord below as all its tones, then n = 3 and n = 4, as degrees and"
        "names:")
  (format t "  ~8a~20a~13a~a~%" "chord" "all tones" "n = 3" "n = 4")
  (let ((rows (loop for (text) in *essential-rows*
                    for c = (parse-chord-symbol text)
                    for all = (chord-tones c)
                    for three = (chord-essential-tones c 3)
                    for four = (chord-essential-tones c 4)
                    collect (list text (tone-degrees c all) (tone-names all)
                                  (tone-degrees c three) (tone-names three)
                                  (tone-degrees c four) (tone-names four)))))
    (loop for (text d-all n-all d3 n3 d4 n4) in rows
          do (out "  ~8a~20a~13a~a~%  ~8a~20a~13a~a~%" text d-all d3 d4 "" n-all n3 n4))
    (check rows *essential-rows* "essential tones, n = 3 and 4"))
  (terpri)
  (para "G13 keeps B F E: the 13 outranks the root, and the 5, 9 and 11 go first. Bø7"
        "keeps its b5 (F) over its root, because the b5 is what makes it half-diminished."
        "The whole order comes from a table keyed by quality and extensions, changed by"
        "the chord: a suspension takes the 3's place, omitted tones leave, and the"
        "altered and added tones go where the table says. The order with N large, and"
        "the tones it never lists (the 11 of a dominant 13th, the 3 of an eleventh"
        "chord with a major third):")
  (format t "  ~11a~22a~a~%" "chord" "priority order" "left out")
  (let ((rows (loop for (text) in *priority-rows*
                    for c = (parse-chord-symbol text)
                    for all = (chord-tones c)
                    for order = (chord-essential-tones c 99)
                    collect (list text (tone-degrees c order)
                                  (dash (tone-degrees c (remove-if (lambda (pair)
                                                                     (member pair order :test #'equalp))
                                                                   all)))))))
    (loop for (text order left-out) in rows
          do (out "  ~11a~22a~a~%" text order left-out))
    (check rows *priority-rows* "priority orders"))
  (show (tone-names (chord-essential-tones (parse-chord-symbol "C7alt") 4)) "E Bb Db D#")
  (show (chord-essential-tones (parse-chord-symbol "C7") 0) nil)
  (show (chord-essential-tones (parse-chord-symbol "N.C.") 3) nil))

;;; 21. Transposing chord symbols ---------------------------------------------------

(defparameter *eb-progression*
  '("Ebmaj7" "Cm7" "Fm7" "Bb7/D" "Gm7" "C7b9" "Fm7/Ab" "Bb13" "Eb6/9"))

(defparameter *transposed-rows*
  ;; (Eb major, up M2, from Eb major to E major, up m2)
  '(("Ebmaj7" "Fmaj7" "Emaj7" "Fbmaj7")
    ("Cm7" "Dm7" "C#m7" "Dbm7")
    ("Fm7" "Gm7" "F#m7" "Gbm7")
    ("Bb7/D" "C7/E" "B7/D#" "Cb7/Eb")
    ("Gm7" "Am7" "G#m7" "Abm7")
    ("C7(b9)" "D7(b9)" "C#7(b9)" "Db7(b9)")
    ("Fm7/Ab" "Gm7/Bb" "F#m7/A" "Gbm7/Bbb")
    ("Bb13" "C13" "B13" "Cb13")
    ("Eb6/9" "F6/9" "E6/9" "Fb6/9")))

(defun demo-transposition ()
  (para "TRANSPOSE-CHORD-SYMBOL moves ROOT and BASS and keeps every other slot: quality,"
        "extensions and alterations go along unchanged, and TEXT becomes NIL. The second"
        "argument is an INTERVAL, or a list of two KEYs. An interval moves a pitch a fixed"
        "distance on the line of fifths (M2 +2, m2 -5, A1 +7); a pair of keys moves it"
        "from the first tonic to the second, so the spelling follows the target key. A"
        "progression in Eb major, up a M2, from Eb major to E major, and up a m2 for"
        "comparison (the same sounds as E major, spelled from Fb):")
  (let* ((chords (mapcar #'parse-chord-symbol *eb-progression*))
         (up (parse-interval "M2"))
         (keys (list (parse-key "Eb major") (parse-key "E major")))
         (rows (loop for c in chords
                     collect (list (print-chord-symbol c)
                                   (print-chord-symbol (transpose-chord-symbol c up))
                                   (print-chord-symbol (transpose-chord-symbol c keys))
                                   (print-chord-symbol
                                    (transpose-chord-symbol c (parse-interval "m2")))))))
    (format t "  ~12a~12a~16a~a~%" "Eb major" "up M2" "Eb to E major" "up m2")
    (loop for (from by-m2 by-key by-minor) in rows
          do (out "  ~12a~12a~16a~a~%" from by-m2 by-key by-minor))
    (check rows *transposed-rows* "progression transposed")
    (terpri)
    (para "The tones follow the root, because they are spelled from it:")
    (show (tone-names (chord-tones (parse-chord-symbol "C7b9"))) "C E G Bb Db")
    (show (tone-names (chord-tones (transpose-chord-symbol (parse-chord-symbol "C7b9")
                                                           (list (parse-key "Eb major")
                                                                 (parse-key "E major")))))
          "C# E# G# B D")
    (show (chord-symbol-text (transpose-chord-symbol (parse-chord-symbol "Bb7/D")
                                                     (parse-interval "M2")))
          nil)
    (show (print-chord-symbol (transpose-chord-symbol (parse-chord-symbol "G7/B")
                                                      (reverse-interval (parse-interval "M2"))))
          "F7/A")
    (show (print-chord-symbol (transpose-chord-symbol (parse-chord-symbol "G7/B")
                                                      (parse-interval "A1")))
          "G#7/B#")
    (show (print-chord-symbol (transpose-chord-symbol (parse-chord-symbol "G7/B")
                                                      (list (parse-key "C") (parse-key "Db"))))
          "Ab7/C")
    (show (chord-symbol-no-chord-p (transpose-chord-symbol (parse-chord-symbol "N.C.")
                                                           (parse-interval "M2")))
          t)
    (terpri)
    (para "The progression in every major key, by key. Transposing back to Eb major gives"
          "each chord back, and every tone of each chord moves by the same distance on"
          "the line of fifths as the tonic:")
    (let ((back 0) (moved 0) (total 0)
          (home (first keys)))
      (dolist (key (remove :minor (thirty-keys) :key #'key-mode))
        (let* ((pair (list home key))
               (shift (- (pitch-fifths (key-tonic key)) (pitch-fifths (key-tonic home))))
               (there (mapcar (lambda (c) (transpose-chord-symbol c pair)) chords)))
          (out "  ~10a~{~a~^ ~}~%" (key-name key) (mapcar #'print-chord-symbol there))
          (loop for c in chords
                for moved-chord in there
                do (incf total)
                   (when (same-chord-p c (transpose-chord-symbol moved-chord (list key home)))
                     (incf back))
                   (when (equal (mapcar (lambda (a b)
                                          (cons (car b) (- (pitch-fifths (cdr b))
                                                           (pitch-fifths (cdr a)))))
                                        (chord-tones c) (chord-tones moved-chord))
                                (mapcar (lambda (a) (cons (car a) shift)) (chord-tones c)))
                     (incf moved)))))
      (format t "~%  ~d chords: ~d come back to Eb major, ~d have every tone moved by the shift~%"
              total back moved)
      (check (list back moved) (list total total) "progression in 15 keys")
      (check total 135 "chords in 15 keys"))
    (let ((down (reverse-interval up)))
      (check (every (lambda (c) (same-chord-p c (transpose-chord-symbol
                                                  (transpose-chord-symbol c up) down)))
                    chords)
             t "up M2 and down again"))))

;;; 22. Errors and restarts -----------------------------------------------------------

(defparameter *error-rows*
  ;; (text position)
  '(("Cmaj8" 4) ("Hm7" 0) ("c7" 0) ("C7b95" 2) ("C7 b9" 3) ("C7(b9" 5) ("C/E7" 3)
    ("Cm7sus4" 3) ("C+(b5)" 3) ("C7(b9,b9)" 6) ("C9(add9)" 3) ("C7(b5,omit5)" 6)
    ("C7b9alt" 4)))

(defparameter *bad-chart* "| C  Cmaj8 | Am7  Hm7 | Dm7  G7(b9 | Cmaj7  H7 |")

(defun parse-chart (chart)
  "Every symbol of CHART parsed, bar by bar; NIL for a symbol a handler skipped."
  (mapcar (lambda (texts) (mapcar #'parse-chord-symbol texts)) (chart-bars chart)))

(defun chart-text (bars)
  "BARS of parsed symbols printed as a chart, with ? for a skipped symbol."
  (format nil "| ~{~{~a~^  ~}~^ | ~} |"
          (mapcar (lambda (bar) (mapcar (lambda (c) (if c (print-chord-symbol c) "?")) bar))
                  bars)))

(defun demo-chord-errors ()
  (para "Text outside the grammar, or an item that breaks one of the rules R1-R6 (one"
        "suspension, on a major third; a 5 altered only where the fifth is perfect; no"
        "repeated alteration; no added tone already in the chord; an omitted tone must be"
        "a natural chord tone; alt only on a plain dominant seventh), signals"
        "CHORD-SYMBOL-PARSE-ERROR, a MUSIC-THEORY-ERROR. POSITION is the index of the"
        "first character the parser could not accept and EXPECTED names what it could"
        "have taken there:")
  (show-signal (parse-chord-symbol "Cmaj8"))
  (show-signal (parse-chord-symbol "Hm7"))
  (show (handler-case (parse-chord-symbol "Cmaj8")
          (chord-symbol-parse-error (c)
            (list (chord-symbol-parse-error-position c) (chord-symbol-parse-error-expected c))))
        '(4 "a modifier, \"(\", \"/\" or the end of the symbol"))
  (terpri)
  (para "Cmaj8 reads C and the word maj, and fails at the 8: no seventh word ends in 8."
        "H, the German B, is not a root letter, and neither is a lower-case c. Others,"
        "with a caret under the position:")
  (let ((positions
          (loop for (text) in *error-rows*
                collect (handler-case (progn (parse-chord-symbol text) :returned)
                          (chord-symbol-parse-error (c)
                            (let ((position (chord-symbol-parse-error-position c)))
                              (out "  ~a~%  ~a^ ~d: ~a~@[ (~a)~]~%" text
                                   (make-string position :initial-element #\Space) position
                                   (chord-symbol-parse-error-expected c)
                                   (music-theory-error-reason c))
                              position))))))
    (check (mapcar #'list (mapcar #'first *error-rows*) positions) *error-rows* "error positions"))
  (terpri)
  (para "The error is signalled inside two restarts: USE-VALUE, whose value"
        "PARSE-CHORD-SYMBOL returns, and SKIP-CHORD-SYMBOL, which makes it return NIL:")
  (block restarts
    (handler-bind ((chord-symbol-parse-error
                     (lambda (c)
                       (format t "  the restarts of ~s: ~{~(~a~)~^, ~}~%~%"
                               (music-theory-error-text c)
                               (mapcar #'restart-name (subseq (compute-restarts c) 0 2)))
                       (return-from restarts))))
      (parse-chord-symbol "Hm7")))
  (para "A chart with bad symbols, read by one handler that logs each error and skips the"
        "symbol, so the rest of the chart still comes through:")
  (format t "  ~a~%" *bad-chart*)
  (let* ((log '())
         (bars (handler-bind ((chord-symbol-parse-error
                                (lambda (c)
                                  (push (format nil "~s at ~d" (music-theory-error-text c)
                                                (chord-symbol-parse-error-position c))
                                        log)
                                  (skip-chord-symbol c))))
                 (parse-chart *bad-chart*))))
    (format t "  skipped ~{~a~^, ~}~%  => ~a~%~%" (reverse log) (chart-text bars))
    (check (chart-text bars) "| C  ? | Am7  ? | Dm7  ? | Cmaj7  ? |" "chart with skipped symbols")
    (check (reverse log) '("\"Cmaj8\" at 4" "\"Hm7\" at 0" "\"G7(b9\" at 5" "\"H7\" at 0")
           "skip log"))
  (para "The same chart with a handler for German charts: when the error is an H in the"
        "root's place, it reads the text again with B there and returns that value with"
        "USE-VALUE; anything else it skips:")
  (let ((bars (handler-bind ((chord-symbol-parse-error
                               (lambda (c)
                                 (let ((text (music-theory-error-text c)))
                                   (if (and (eql (chord-symbol-parse-error-position c) 0)
                                            (plusp (length text))
                                            (char= (char text 0) #\H))
                                       (use-value (parse-chord-symbol
                                                   (concatenate 'string "B" (subseq text 1)))
                                                  c)
                                       (skip-chord-symbol c))))))
                (parse-chart *bad-chart*))))
    (format t "  => ~a~%~%" (chart-text bars))
    (check (chart-text bars) "| C  ? | Am7  Bm7 | Dm7  ? | Cmaj7  B7 |" "chart with H as B"))
  (para "An unknown word in the quality's place is a warning, not an error:"
        "UNKNOWN-CHORD-QUALITY with the word as TEXT, inside a USE-VALUE restart that takes"
        "a quality keyword. Unhandled or muffled, the quality is :MAJOR, and the parser"
        "goes on after the word. (Cmajor is not unknown: maj matches, and or is an error.)")
  (show (handler-case (parse-chord-symbol "Cdom7(b9)")
          (unknown-chord-quality (w) (list (unknown-chord-quality-text w) (princ-to-string w))))
        '("dom7" "unknown chord quality \"dom7\""))
  (show (handler-bind ((unknown-chord-quality (lambda (w) (use-value :dominant-7 w))))
          (print-chord-symbol (parse-chord-symbol "Cdom7(b9)")))
        "C7(b9)")
  (show (handler-bind ((warning #'muffle-warning))
          (print-chord-symbol (parse-chord-symbol "Cdom7(b9)")))
        "C(b9)" "muffled: a C major triad with a b9")
  (terpri)
  (para "One handler on MUSIC-THEORY-ERROR catches these and the other chord errors: a"
        "value the printer cannot print (a triad with EXTENSIONS breaks rule W3), and"
        "INVALID-PITCH for a tone or a transposed root beyond two accidentals:")
  (show-signal (parse-chord-symbol "Hm7"))
  (show-signal (print-chord-symbol (make-chord-symbol :root (parse-pitch-class "C")
                                                      :quality :major :extensions '(9))))
  (show-signal (chord-tones (parse-chord-symbol "E##")))
  (show-signal (transpose-chord-symbol (parse-chord-symbol "F##7") (parse-interval "A1"))))

;;; Entry point -----------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()))
    (format t "~&music-theory ~a: spelled pitches, the line of fifths and intervals (slice MT0,~%~
               sections 1-6); meters, rhythm and tempo (slice MT1, sections 7-11); keys, scale~%~
               degrees and Roman numerals (slice MT2, sections 12-16); chord symbols: the~%~
               grammar, dialects, realisation, essential tones, transposition (slice MT3,~%~
               sections 17-22)~%"
            (asdf:component-version (asdf:find-system "music-theory")))
    (section "1. Spelled pitches: letter, accidental, octave" (demo-pitches))
    (section "2. MIDI numbers and the scientific octave" (demo-midi))
    (section "3. The line of fifths" (demo-fifths))
    (section "4. Intervals: quality, number, compound, inversion" (demo-intervals))
    (section "5. Transposition keeps the spelling" (demo-transpose))
    (section "6. Conditions and restarts" (demo-conditions))
    (section "7. Meters: beats, beat unit, compound" (demo-meters))
    (section "8. The metric hierarchy" (demo-metric))
    (section "9. Dots and tuplets" (demo-dots))
    (section "10. Tie splitting" (demo-ties))
    (section "11. Tempo and phrase windows" (demo-tempo))
    (section "12. Keys, key signatures and scales" (demo-keys))
    (section "13. Scale degrees" (demo-degrees))
    (section "14. Roman numerals" (demo-roman))
    (section "15. Mixture: chords borrowed from the parallel key" (demo-mixture))
    (section "16. A progression, to chords and back" (demo-progression))
    (section "17. A lead-sheet chord chart parsed" (demo-chart))
    (section "18. Dialects: classical, pop and jazz" (demo-dialects))
    (section "19. Unicode and ASCII, slash chords" (demo-styles))
    (section "20. Essential tones" (demo-essential))
    (section "21. Transposing chord symbols" (demo-transposition))
    (section "22. Chord-symbol errors and restarts" (demo-chord-errors))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
