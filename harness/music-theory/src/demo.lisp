(in-package #:harness-demos-music-theory)

;;;; A short tour of music-theory 0.1.0 (slice MT0): spelled pitches, the line
;;;; of fifths and intervals. Every result with a known answer is also checked:
;;;; RUN-DEMOS returns NIL when one differs or a section stops on an error.

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
  "VALUE as printed, with the name of a pitch, pitch class or interval after it."
  (let ((name (typecase value
                (spelled-pitch (pitch-name value))
                (pitch-class-name (pitch-class-string value))
                (interval (ignore-errors (interval-name value))))))
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

(defun print-result (form value)
  (let ((ftext (indent (code-text form) 2))
        (vtext (value-text value)))
    (if (and (<= (+ 2 (length ftext) 4 (length vtext)) 84)
             (not (find #\Newline ftext)))
        (format t "~&  ~a => ~a~%" ftext vtext)
        (format t "~&  ~a~%    => ~a~%" ftext vtext))))

(defmacro show (form &optional (expected nil expected-p))
  "Print FORM and its value like a REPL transcript; check the value against
EXPECTED when one is given."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (print-result ',form ,value)
       ,@(when expected-p `((check ,value ,expected (code-text ',form))))
       ,value)))

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
  (let* ((inputs '("C4" "H4" "Eb" "G#10" "Bb3" "F#4x" "" "Cbbb4" "C♯4"))
         (parsed (handler-bind ((invalid-pitch
                                  (lambda (c)
                                    (format t "  ~8s ~a~%" (music-theory-error-text c)
                                            (music-theory-error-reason c))
                                    (use-value nil c))))
                   (mapcar #'parse-pitch inputs))))
    (format t "  => ~{~a~^ ~}~%~%" (mapcar (lambda (p) (if p (pitch-name p) "nil")) parsed))
    (check (mapcar (lambda (p) (and p (pitch-name p))) parsed)
           '("C4" nil nil nil "Bb3" nil nil nil "C#4") "parsed with use-value"))
  (para "\"Cbbb4\" has an octave; its fault is the third flat. The reason should say so"
        "(egao1980/music-theory#4).")
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

;;; Entry point -----------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()))
    (format t "~&music-theory ~a: spelled pitches, the line of fifths and intervals (slice MT0)~%"
            (asdf:component-version (asdf:find-system "music-theory")))
    (section "1. Spelled pitches: letter, accidental, octave" (demo-pitches))
    (section "2. MIDI numbers and the scientific octave" (demo-midi))
    (section "3. The line of fifths" (demo-fifths))
    (section "4. Intervals: quality, number, compound, inversion" (demo-intervals))
    (section "5. Transposition keeps the spelling" (demo-transpose))
    (section "6. Conditions and restarts" (demo-conditions))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
