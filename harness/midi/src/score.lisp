;;;; Part 2: score-backend-midi 0.1.1 (slice SC4), notated scores to and from
;;;; Standard MIDI Files. It runs on midi-protocol (part 1), score-protocol
;;;; (0.3.0 in the last run), music-theory 0.5.2 and sat-protocol 0.2.0 with
;;;; sat-backend-cdcl 0.1.0.
;;;; A four-part OLD HUNDREDTH is built in code and exported as written and with
;;;; its repeat played out (section 9), as a rehearsal set (10), and read back
;;;; (11). A humanised performance of a melody is then imported, with the grid
;;;; chosen per beat, the spelling and the uncertainty report (12), and mido
;;;; reads the exported file (13). Section 14 shows the two 0.1.1 fixes: the
;;;; spelling confidence from the deciding rule and the late last note. The
;;;; checks and sections are part 1's.

(in-package #:harness-demos-midi)

;;; Reading scores ------------------------------------------------------------

(defun seq-list (seq)
  (fset:convert 'list seq))

(defun score-voices (score)
  "Every voice of SCORE, in document order."
  (loop for part in (seq-list (sp:score-parts score)) append (sp:part-voices part)))

(defun voice-events (score voice)
  "(onset event bar offset) for each event of VOICE: onsets in whole notes from
the start of the score, offsets from the start of the bar."
  (loop for measure in (seq-list (sp:voice-measures voice))
        for bar from (sp:score-first-bar score)
        append (loop with offset = 0
                     for event in (seq-list measure)
                     collect (list (+ (sp:bar-start score bar) offset) event bar offset)
                     unless (and (typep event 'sp:sounding-event) (sp:event-grace event))
                       do (incf offset (sp:event-duration event)))))

(defun event-notes (event)
  (typecase event
    (sp:note-event (list event))
    (sp:chord-event (sp:chord-event-notes event))
    (t '())))

(defun voice-notes (score voice)
  "((onset end pitch-name) ...) for the sounding notes of VOICE, with the
pieces of a tied note joined into one, in score order."
  (let ((pieces '())
        (by-id (make-hash-table :test #'equal))
        (continued (make-hash-table :test #'equal)))
    (loop for (onset event) in (voice-events score voice)
          do (dolist (note (event-notes event))
               (let ((piece (list note onset (+ onset (sp:event-duration event)))))
                 (push piece pieces)
                 (setf (gethash (sp:element-id note) by-id) piece)
                 (when (sp:note-tie note)
                   (setf (gethash (sp:tie-end (sp:note-tie note)) continued) t)))))
    (loop for (note onset end) in (nreverse pieces)
          unless (gethash (sp:element-id note) continued)
            collect (let ((current note) (last-end end))
                      (loop for tie = (sp:note-tie current)
                            for next = (and tie (gethash (sp:tie-end tie) by-id))
                            while next
                            do (setf current (first next) last-end (third next)))
                      (list onset last-end (mt:pitch-name (sp:note-pitch note)))))))

(defun voice-lyrics (score voice &key (verse 1))
  "((onset text syllabic) ...) for the lyrics of VERSE in VOICE."
  (loop for (onset event) in (voice-events score voice)
        append (loop for lyric in (sp:event-lyrics event)
                     when (= verse (sp:lyric-verse lyric))
                       collect (list onset (sp:lyric-text lyric) (sp:lyric-syllabic lyric)))))

(defun anchor-rows (score)
  "((bar offset text) ...) for the chord symbols of SCORE, printed in the :POP dialect."
  (loop for anchor in (seq-list (sp:score-chord-symbols score))
        collect (list (car (sp:anchor-position anchor)) (cdr (sp:anchor-position anchor))
                      (mt:print-chord-symbol (sp:anchor-symbol anchor) :dialect :pop))))

(defun duration-name (duration)
  (case duration
    (1 "w") (3/4 "h.") (1/2 "h") (3/8 "q.") (1/4 "q") (3/16 "e.") (1/8 "e") (1/16 "s")
    (t (princ-to-string duration))))

(defun event-label (event &key (quarter-implied t))
  "EVENT as PITCH:DURATION (the duration left out for a quarter when QUARTER-IMPLIED),
~ for a tie, [p] for a dynamic. A tuplet note shows its written value and 3."
  (let ((duration (* (sp:event-duration event) (or (sp:event-tuplet event) 1))))
    (format nil "~a~:[:~a~;~*~]~@[~a~]~:[~;~~~]~@[[~(~a~)]~]"
            (typecase event
              (sp:rest-event "r")
              (sp:note-event (mt:pitch-name (sp:note-pitch event)))
              (sp:chord-event (format nil "<~{~a~^ ~}>" (mapcar (lambda (n) (mt:pitch-name (sp:note-pitch n)))
                                                               (sp:chord-event-notes event)))))
            (and quarter-implied (= duration 1/4)) (duration-name duration)
            (and (sp:event-tuplet event) "3")
            (and (typep event 'sp:note-event) (sp:note-tie event))
            (and (typep event '(or sp:note-event sp:chord-event sp:rest-event)) (sp:event-dynamic event)
                 (sp:dynamic-mark-value (sp:event-dynamic event))))))

(defun voice-text (voice &key (from 0) (to nil) (quarter-implied t))
  "The voice measures FROM..TO (indices) of VOICE, | between bars."
  (let* ((measures (seq-list (sp:voice-measures voice)))
         (shown (subseq measures from (and to (min (1+ to) (length measures))))))
    (format nil "~{~a~^ | ~}"
            (mapcar (lambda (events)
                      (format nil "~{~a~^ ~}" (mapcar (lambda (e) (event-label e :quarter-implied quarter-implied))
                                                      (seq-list events))))
                    shown))))

(defun position-text (position)
  "A (bar . offset) position as bar:offset."
  (format nil "~d:~a" (car position) (cdr position)))

;;; Reading SMFs ---------------------------------------------------------------

(defun timed (track &optional (predicate (constantly t)))
  "(tick . event) for the events of TRACK that satisfy PREDICATE."
  (remove-if-not predicate (midi:track-to-absolute track) :key #'cdr))

(defun sounding-on-p (event)
  (and (midi:note-on-event-p event) (plusp (midi:note-on-event-velocity event))))

(defun sounding-off-p (event)
  (or (midi:note-off-event-p event)
      (and (midi:note-on-event-p event) (zerop (midi:note-on-event-velocity event)))))

(defun track-notes (track)
  "((on off key velocity channel) ...) for the notes of TRACK, in note-on
order: each note-on paired with the first later note-off of its channel and key."
  (let ((open (make-hash-table :test #'equal)) (notes '()))
    (loop for (tick . event) in (midi:track-to-absolute track)
          do (cond ((sounding-on-p event)
                    (let ((note (list tick nil (midi:note-on-event-key event)
                                      (midi:note-on-event-velocity event)
                                      (midi:channel-event-channel event))))
                      (push note notes)
                      (setf (gethash (list (fifth note) (third note)) open)
                            (append (gethash (list (fifth note) (third note)) open) (list note)))))
                   ((sounding-off-p event)
                    (let* ((key (list (midi:channel-event-channel event)
                                      (if (midi:note-on-event-p event)
                                          (midi:note-on-event-key event)
                                          (midi:note-off-event-key event))))
                           (note (pop (gethash key open))))
                      (when note (setf (second note) tick))))))
    (nreverse notes)))

(defun first-text (track predicate)
  (let ((event (find-if predicate track)))
    (and event (midi:text-meta-event-text event))))

(defun track-setup (track)
  "(channel program volume pan) from the channel events at tick 0 of TRACK."
  (let ((setup (list nil nil nil nil)))
    (loop for (tick . event) in (midi:track-to-absolute track)
          while (zerop tick)
          do (typecase event
               (midi:program-change-event
                (setf (first setup) (midi:channel-event-channel event)
                      (second setup) (midi:program-change-event-program event)))
               (midi:control-change-event
                (case (midi:control-change-event-controller event)
                  (7 (setf (third setup) (midi:control-change-event-value event)))
                  (10 (setf (fourth setup) (midi:control-change-event-value event)))))))
    setup))

(defun track-end-tick (track)
  (car (first (last (midi:track-to-absolute track)))))

(defparameter +gm-names+ '((52 . "Choir Aahs") (0 . "Acoustic Grand Piano"))
  "General MIDI names of the programs used in part 2 (numbers count from 0).")

(defun smf-event-text (event)
  "One line for EVENT: the text events part 1 has no use for, then DESCRIBE-EVENT."
  (let ((*print-pretty* nil))
    (typecase event
      (midi:instrument-name-event (format nil "instrument-name ~s" (midi:text-meta-event-text event)))
      (midi:copyright-event (format nil "copyright       ~s" (midi:text-meta-event-text event)))
      (midi:text-event (format nil "text            ~s" (midi:text-meta-event-text event)))
      (midi:program-change-event
       (let ((program (midi:program-change-event-program event)))
         (format nil "program-change  ch ~d  ~d (~a)" (midi:channel-event-channel event) program
                 (or (cdr (assoc program +gm-names+)) "?"))))
      (t (describe-event event)))))

(defun ticks-whole (tick &optional (division +ppq+))
  "TICK as a position in whole notes."
  (/ tick (* 4 division)))

;;; Conditions ------------------------------------------------------------------

(defun call-collecting-losses (thunk)
  "(values result losses): THUNK's value, every LOSSY-CONVERSION it warns
muffled, and their WHAT strings in order."
  (let ((losses '()))
    (values (handler-bind ((sp:lossy-conversion
                             (lambda (w)
                               (push (sp:lossy-conversion-what w) losses)
                               (muffle-warning w))))
              (funcall thunk))
            (nreverse losses))))

(defun export-smf (score &rest keys)
  "(values smf losses): SCORE-TO-SMF with its LOSSY-CONVERSION warnings collected."
  (call-collecting-losses (lambda () (apply #'smidi:score-to-smf score keys))))

;;; OLD HUNDREDTH ---------------------------------------------------------------
;;; The melody of Psalm 100 in the Genevan Psalter, by Louis Bourgeois (1551), in
;;; G major and the even rhythm of modern hymnals, with the words of William
;;; Kethe (1561), verses 1 and 2. The alto, tenor and bass are a plain chordal
;;; setting written for this demo; the alto holds its D4 across the barline
;;; into bar 3, a tie. Line 3 is p and line 4 f. A repeat sign at the end plays
;;; the hymn again for verse 2.

(defparameter *hundredth-voices*
  '(("Soprano" 1 :up
     "G4/2 | G4 F#4 E4 D4 | G4 A4 B4/2 | B4/2 B4 B4 | A4 G4 C5 B4 | A4/2 G4/2:p | A4 B4 A4 G4 | E4 F#4 G4/2 | D5/2:f B4 G4 | A4 C5 B4 A4 | G4/2")
    ("Alto" 1 :down
     "D4/2 | D4 D4 B3 B3 | D4 D4 D4/2~ | D4/2 E4 D4 | D4 E4 E4 D4 | D4/2 D4/2:p | D4 D4 D4 E4 | C4 D4 D4/2 | G4/2:f G4 E4 | F#4 E4 D4 C4 | B3/2")
    ("Tenor" 2 :up
     "B3/2 | B3 A3 G3 G3 | G3 F#3 G3/2 | G3/2 G3 F#3 | F#3 B3 G3 G3 | F#3/2 B3/2:p | A3 G3 F#3 B3 | G3 A3 B3/2 | B3/2:f B3 C4 | A3 A3 G3 F#3 | G3/2")
    ("Bass" 2 :down
     "G2/2 | B2 D3 E3 G2 | B2 D3 G2/2 | B2/2 E3 B2 | D3 E3 C3 G2 | D3/2 G2/2:p | F#2 G2 D3 E3 | C3 D3 G2/2 | G3/2:f E3 C3 | D3 A2 D3 D3 | G2/2"))
  "Name, staff, stem and music of each voice, bar by bar from the half-note
pickup. A note is a pitch, then /2 for a half note (else a quarter), ~ for a
tie to the next note, :p or :f for a dynamic.")

(defparameter *hundredth-chords*
  '((0 0 "G")
    (1 0 "G/B") (1 1/4 "D") (1 1/2 "Em") (1 3/4 "G")
    (2 0 "G/B") (2 1/4 "D") (2 1/2 "G")
    (3 0 "G/B") (3 1/2 "Em") (3 3/4 "Bm")
    (4 0 "D") (4 1/4 "Em") (4 1/2 "C") (4 3/4 "G")
    (5 0 "D") (5 1/2 "G")
    (6 0 "D/F#") (6 1/4 "G") (6 1/2 "D") (6 3/4 "Em")
    (7 0 "C") (7 1/4 "D") (7 1/2 "G")
    (8 0 "G") (8 1/2 "Em") (8 3/4 "C")
    (9 0 "D") (9 1/4 "Am") (9 1/2 "G/D") (9 3/4 "D7")
    (10 0 "G"))
  "Chord symbols as (bar offset text): the harmony of the setting.")

(defparameter *hundredth-verses*
  '("All peo- ple that on earth do dwell, Sing to the Lord with cheer- ful voice. Him serve with fear, his praise forth tell; Come ye be- fore him and re- joice."
    "The Lord, ye know, is God in- deed; With- out our aid he did us make; We are his folk, he doth us feed, And for his sheep he doth us take.")
  "William Kethe's verses 1 and 2, hymnal style: a trailing hyphen continues a word.")

(defparameter *hundredth-metadata*
  '(:title "All people that on earth do dwell" :subtitle "OLD HUNDREDTH"
    :composer "Louis Bourgeois, 1551" :lyricist "William Kethe, 1561"
    :copyright "Public domain"))

(defun words (string)
  (remove "" (uiop:split-string string :separator " ") :test #'string=))

(defun verse-syllables (text)
  "TEXT as (SYLLABLE SYLLABIC) pairs: a token that ends in a hyphen begins or
continues a word, and the token after the last one ends it."
  (loop with continued = nil
        for token in (words text)
        for hyphen = (char= #\- (char token (1- (length token))))
        collect (list (string-right-trim "-" token)
                      (cond ((and continued hyphen) :middle)
                            (hyphen :begin)
                            (continued :end)
                            (t :single)))
        do (setf continued hyphen)))

(defun parse-note-token (token)
  "(pitch duration tie dynamic) for a token such as \"D4/2~\" or \"G4/2:p\"."
  (let* ((colon (position #\: token))
         (dynamic (and colon (intern (string-upcase (subseq token (1+ colon))) '#:keyword)))
         (body (subseq token 0 colon))
         (tie (char= #\~ (char body (1- (length body)))))
         (body (string-right-trim "~" body))
         (slash (position #\/ body)))
    (list (subseq body 0 slash)
          (if slash (/ 1 (parse-integer body :start (1+ slash))) 1/4)
          tie dynamic)))

(defun hymn-voice-measures (name music verses)
  "The voice measures of MUSIC. VERSES, a list of (syllable syllabic) lists, one
per verse, give the lyrics of the notes in order. A tied note's TIE names the
next note, which gets the id <name>-held."
  (let ((held nil) (k -1))
    (loop for bar in (uiop:split-string music :separator "|")
          collect (loop for (pitch duration tie dynamic) in (mapcar #'parse-note-token (words bar))
                        collect (let* ((id (shiftf held nil))
                                       (tie-end (and tie (setf held (format nil "~(~a~)-held" name)))))
                                  (incf k)
                                  (sp:make-note-event
                                   :id id :pitch (mt:parse-pitch pitch) :duration duration
                                   :tie (and tie-end (sp:make-tie :end tie-end))
                                   :dynamic (and dynamic (sp:make-dynamic-mark :value dynamic))
                                   :lyrics (loop for verse in verses
                                                 for v from 1
                                                 for (text syllabic) = (nth k verse)
                                                 when text
                                                   collect (sp:make-lyric :verse v :text text
                                                                          :syllabic syllabic))))))))

(defun build-hundredth ()
  "OLD HUNDREDTH as a score: one part, Choir, with four voices on two staves."
  (let ((verses (mapcar #'verse-syllables *hundredth-verses*)))
    (sp:make-score
     :metadata *hundredth-metadata*
     :measures (loop for bar from 0 to 10
                     collect (case bar
                               (0 (sp:make-measure-info
                                   :time (mt:parse-meter "4/4") :key (mt:parse-key "G major")
                                   :pickup-p t :length 1/2
                                   :tempo (list (sp:make-tempo-mark
                                                 :tempo (mt:make-tempo :bpm 80 :beat-unit 1/4)))))
                               (10 (sp:make-measure-info :length 1/2 :barline :final :repeat-end 2))
                               (t (sp:make-measure-info))))
     :parts (list (sp:make-part
                   :name "Choir"
                   :staves (list (sp:make-staff :clef :treble) (sp:make-staff :clef :bass))
                   :voices (loop for (name staff stem music) in *hundredth-voices*
                                 collect (sp:make-voice
                                          :name name :staff staff :stem stem
                                          :measures (hymn-voice-measures
                                                     name music (and (string= name "Soprano") verses))))))
     :chord-symbols (loop for (bar offset text) in *hundredth-chords*
                          collect (sp:make-chord-symbol-anchor :position (cons bar offset)
                                                               :symbol (mt:parse-chord-symbol text))))))

(defvar *hundredth* nil)

(defun hundredth ()
  (or *hundredth* (setf *hundredth* (build-hundredth))))

(defun hundredth-path ()
  (asdf:system-relative-pathname "harness-demos-midi" "old-hundredth.mid"))

(defun hundredth-file-smf ()
  "The SMF that old-hundredth.mid holds: the hymn with its repeat played out, a
verse per pass."
  (values (export-smf (hundredth) :verse :pass)))

(defun voice-by-name (score name)
  (find name (score-voices score) :key #'sp:voice-name :test #'equal))

;;; 9. Export ------------------------------------------------------------------------

(defun print-track-table (smf)
  (out "  track  name                                 instrument  ch  program          CC7  CC10  notes  lyrics~%")
  (loop for track in (midi:smf-tracks smf)
        for i from 0
        for (channel program volume pan) = (track-setup track)
        do (out "  ~5d  ~36a ~11a ~:[  -~;~:*~2d~]  ~16a ~:[  -~;~:*~3d~]  ~:[   -~;~:*~4d~]  ~5d  ~6d~%"
                i (format nil "~s" (first-text track #'midi:track-name-event-p))
                (or (first-text track #'midi:instrument-name-event-p) "-")
                channel
                (if program (format nil "~d ~a" program (cdr (assoc program +gm-names+))) "-")
                volume pan
                (length (track-notes track))
                (count-if #'midi:lyric-event-p track))))

(defun chord-text-p (event)
  (and (midi:text-event-p event)
       (let ((text (midi:text-meta-event-text event)))
         (and (>= (length text) 6) (string= "chord:" text :end2 6)))))

(defun demo-score-export ()
  (let* ((hymn (hundredth))
         (voices (score-voices hymn)))
    (out "score-backend-midi turns a score-protocol score into a Standard MIDI File and back.~%~
          The score here is a four-part OLD HUNDREDTH (melody by Louis Bourgeois, 1551; words~%~
          by William Kethe, 1561), built in code like the hymn of harness/score: one part,~%~
          \"Choir\", with four voices on two staves, G major, 4/4, quarter = 80, a half-note~%~
          pickup and a half bar at the end. ~d chord symbols follow the harmony, line 3 is p~%~
          and line 4 f, the alto holds its D4 across the barline into bar 3 (a tie), and a~%~
          repeat sign at the end plays the hymn twice. Verses 1 and 2 are on the soprano.~%~%"
         (length *hundredth-chords*))
    (dolist (voice voices)
      (out "  ~8a ~a~%" (sp:voice-name voice) (voice-text voice)))
    (out "  (quarter notes unmarked, h a half, ~~ a tie, [p] a dynamic)~%")
    (let ((issues (sp:check-score hymn)))
      (out "~%CHECK-SCORE finds ~d issues. ~d events, ~d lyrics, ~d bars (0 to ~d), ~a whole notes.~%"
           (length issues)
           (loop for v in voices sum (length (voice-events hymn v)))
           (loop for v in voices sum (loop for row in (voice-events hymn v)
                                           sum (length (sp:event-lyrics (second row)))))
           (1+ (- (sp:score-last-bar hymn) (sp:score-first-bar hymn))) (sp:score-last-bar hymn)
           (sp:score-duration hymn))
      (check issues '() "the hymn is valid"))
    ;; As written.
    (multiple-value-bind (smf losses) (export-smf hymn :unfold nil)
      (let* ((tracks (midi:smf-tracks smf))
             (conductor (first tracks)))
        (out "~%(score-to-smf hymn :unfold nil) plays every bar once, as written. It returns an~%~
              SMF of format ~d at ~d ticks per quarter note, and warns LOSSY-CONVERSION ~s~%~
              (ids, provenance and confidence have no place in a MIDI file). Track 0 is the~%~
              conductor track, then one track and one channel per voice:~%~%"
             (midi:smf-format smf) (midi:smf-division smf) losses)
        (check (list (midi:smf-format smf) (midi:smf-division smf) (length tracks)) '(1 480 5) "format 1, 480, 5 tracks")
        (check losses '("ids") "export losses")
        (print-track-table smf)
        (check (mapcar #'track-setup (rest tracks)) '((0 52 100 64) (1 52 100 64) (2 52 100 64) (3 52 100 64))
               "channel, program, CC7, CC10 per voice")
        (check (mapcar (lambda (track) (length (track-notes track))) (rest tracks)) '(32 31 32 32) "notes per track")
        (out "~%Voices get channels 0, 1, 2, ... (9, the GM percussion channel, is skipped), the~%~
              program 52 (Choir Aahs, +DEFAULT-PROGRAM+) unless :PROGRAMS names another, CC7 100~%~
              and CC10 64 unless :MIX does. The track name is the voice's, the instrument name~%~
              its part's.~%")
        (out "~%The conductor track: title, copyright, then the time signature of each bar that~%~
              changes it, the key, the tempo, and a \"chord:\" text event per chord symbol. Ticks~%~
              are T(x) = 4 * 480 * x, x in whole notes; the position is bar:offset.~%~%")
        (out "  tick   position  event~%")
        (loop for (tick . event) in (midi:track-to-absolute conductor)
              unless (chord-text-p event)
                do (out "  ~5d  ~8a  ~a~%" tick (position-text (sp:offset-position hymn (ticks-whole tick)))
                        (smf-event-text event)))
        (let ((chords (timed conductor #'chord-text-p)))
          (out "~%and ~d chord text events, the first six:~%" (length chords))
          (loop for (tick . event) in (subseq chords 0 6)
                do (out "  ~5d  ~8a  ~a~%" tick (position-text (sp:offset-position hymn (ticks-whole tick)))
                        (smf-event-text event)))
          (check (length chords) (length *hundredth-chords*) "one text event per chord symbol")
          (check (mapcar (lambda (te) (midi:text-meta-event-text (cdr te))) (subseq chords 0 6))
                 '("chord:G" "chord:G/B" "chord:D" "chord:Em" "chord:G" "chord:G/B") "chord texts"))
        (check (mapcar (lambda (te) (let ((e (cdr te)))
                                      (list (car te) (midi:time-signature-event-numerator e)
                                            (midi:time-signature-event-denominator e)
                                            (midi:time-signature-event-clocks-per-click e))))
                       (timed conductor #'midi:time-signature-event-p))
               '((0 2 4 24) (960 4 4 24) (18240 2 4 24)) "time signatures as written")
        (check (mapcar (lambda (te) (midi:tempo-event-tempo (cdr te))) (timed conductor #'midi:tempo-event-p))
               '(750000) "tempo 80 = 750000 us")
        (out "~%The pickup and the last bar are half bars, so they get 2/4: a bar's signature is~%~
              its actual length. 80 quarters a minute is 750000 microseconds per quarter.~%")
        ;; Lyrics.
        (let* ((soprano (second tracks))
               (lyrics (timed soprano #'midi:lyric-event-p))
               (ons (timed soprano #'sounding-on-p)))
          (out "~%The soprano track: a LYRIC-EVENT (FF 05) at each syllable's note, before its~%~
                note-on, \"-\" after a syllable that continues its word. The first eight:~%~%")
          (out "  tick   position  lyric     note-on~%")
          (loop for (tick . event) in (subseq lyrics 0 8)
                for on = (cdr (find tick ons :key #'car))
                do (out "  ~5d  ~8a  ~8s  ~a vel ~d~%" tick
                        (position-text (sp:offset-position hymn (ticks-whole tick)))
                        (midi:text-meta-event-text event) (note-name (midi:note-on-event-key on))
                        (midi:note-on-event-velocity on)))
          (check (mapcar (lambda (te) (midi:text-meta-event-text (cdr te))) (subseq lyrics 0 8))
                 '("All" "peo-" "ple" "that" "on" "earth" "do" "dwell,") "first lyrics")
          (check (length lyrics) 32 "one lyric per soprano note (verse 1)")
          (out "  ... ~d lyric events in all: :VERSE 1 is the default, so verse 2 is not written.~%"
               (length lyrics)))
        ;; Velocities.
        (out "~%Velocities come from the dynamics: mf (80, :DEFAULT-DYNAMIC) until the p of~%~
              line 3 (49), then the f of line 4 (96), in every voice. Note-on velocities by line:~%~%")
        (let ((lines '(("line 1" 0 3840) ("line 2" 4800 8640) ("line 3 (p)" 9600 13440) ("line 4 (f)" 14400 18240))))
          (out "  voice    ~{~12a~}~%" (mapcar #'first lines))
          (loop for voice in voices
                for track in (rest tracks)
                for notes = (track-notes track)
                for row = (loop for (nil from to) in lines
                                collect (remove-duplicates
                                         (loop for (on nil nil velocity) in notes
                                               when (<= from on to) collect velocity)))
                do (out "  ~8a ~{~12a~}~%" (sp:voice-name voice) (mapcar (lambda (cell) (format nil "~{~d~^ ~}" cell)) row))
                   (check row '((80) (80) (49) (96)) (format nil "~a velocities" (sp:voice-name voice)))))
        ;; The tie.
        (let* ((alto (find "Alto" voices :key #'sp:voice-name :test #'equal))
               (held (sp:find-element hymn "alto-held"))
               (tied (find-if (lambda (row) (and (typep (second row) 'sp:note-event) (sp:note-tie (second row))))
                              (voice-events hymn alto)))
               (note (find 3840 (track-notes (third tracks)) :key #'first)))
          (out "~%The tie. In the score the alto has two half notes: ~a at ~a, whose TIE names~%~
                ~s, and that note, ~a at ~a. The export sounds them as one note: a~%~
                note-on at tick ~d and one note-off at ~d, 1920 ticks (a whole note), with the~%~
                first note's velocity ~d. So the alto has 32 note events in the score and 31~%~
                notes in its track.~%"
               (event-label (second tied)) (position-text (cons (third tied) (fourth tied)))
               (sp:tie-end (sp:note-tie (second tied)))
               (event-label held) (position-text (sp:element-position hymn "alto-held"))
               (first note) (second note) (fourth note))
          (check (subseq note 0 4) (list 3840 5760 62 80) "the tied D4 is one note")
          (check (length (voice-notes hymn alto)) 31 "31 alto notes once ties are joined"))))
    ;; Unfolded.
    (multiple-value-bind (view id-map passes) (sp:unfold hymn)
      (let* ((source-ids (mapcar #'sp:element-id (seq-list (sp:score-measure-map hymn))))
             (order (loop for m in (seq-list (sp:score-measure-map view))
                          collect (cons (position (fset:lookup id-map (sp:element-id m)) source-ids :test #'equal)
                                        (fset:lookup passes (sp:element-id m))))))
        (multiple-value-bind (smf losses) (export-smf hymn :verse :pass)
          (let* ((tracks (midi:smf-tracks smf))
                 (conductor (first tracks))
                 (soprano (second tracks))
                 (lyrics (timed soprano #'midi:lyric-event-p))
                 (turn (position 19200 lyrics :key #'car)))
            (out "~%(score-to-smf hymn :verse :pass) uses the default :UNFOLD T: the bars are played~%~
                  in the order of score-protocol's UNFOLD view, ~d bars as (bar . pass),~%~
                  ~a ... ~a, then ~a ... ~a.~%~
                  :VERSE :PASS sings verse n on pass n. Warnings: ~s.~%"
                 (length order) (first order) (nth 10 order) (nth 11 order) (car (last order)) losses)
            (check order (append (loop for b from 0 to 10 collect (cons b 1))
                                 (loop for b from 0 to 10 collect (cons b 2)))
                   "unfold order")
            (out "~%  as written: ~d ticks, ~{~d~^, ~} notes per voice track, ~d lyrics~%"
                 19200 '(32 31 32 32) 32)
            (out "  unfolded:   ~d ticks, ~{~d~^, ~} notes per voice track, ~d lyrics~%"
                 (track-end-tick conductor)
                 (mapcar (lambda (track) (length (track-notes track))) (rest tracks))
                 (length lyrics))
            (check (list (track-end-tick conductor) (mapcar (lambda (track) (length (track-notes track))) (rest tracks))
                         (length lyrics))
                   '(38400 (64 62 64 64) 64)
                   "the unfolded export plays everything twice")
            (out "~%The conductor track now has these time signature and tempo events:~%~%")
            (loop for (tick . event) in (midi:track-to-absolute conductor)
                  when (or (midi:time-signature-event-p event) (midi:tempo-event-p event))
                    do (out "  ~5d  ~8a  ~a~%" tick (position-text (sp:offset-position view (ticks-whole tick)))
                            (smf-event-text event)))
            (check (mapcar #'car (timed conductor #'midi:time-signature-event-p)) '(0 960 18240 20160 37440)
                   "unfolded time signatures")
            (out "~%The half bar 10 and the pickup of pass 2 are both 2/4, so there is no event at~%~
                  19200 (view bar 11); 4/4 comes back with bar 1 of pass 2. Bar 0's tempo mark~%~
                  is played again on pass 2, as a second tempo event. Around the turn the soprano~%~
                  sings the end of verse 1, then verse 2:~%~%")
            (out "  tick   view bar  lyric~%")
            (loop for (tick . event) in (subseq lyrics (- turn 3) (+ turn 5))
                  do (out "  ~5d  ~8a  ~s~%" tick (position-text (sp:offset-position view (ticks-whole tick)))
                          (midi:text-meta-event-text event)))
            (check (mapcar (lambda (te) (midi:text-meta-event-text (cdr te))) (subseq lyrics (- turn 3) (+ turn 5)))
                   '("and" "re-" "joice." "The" "Lord," "ye" "know," "is")
                   "verse 2 on pass 2")
            (let* ((path (hundredth-path))
                   (committed (and (probe-file path) (file-octets path))))
              (midi:write-smf smf path)
              (let ((written (file-octets path)))
                (out "~%(write-smf smf path) wrote harness/midi/old-hundredth.mid: ~d bytes, the hymn~%~
                      with both verses, for any MIDI player.~@[ It is byte-identical to the~%~
                      committed copy: ~a.~]~%"
                     (length written) (and committed (equalp committed written)))
                (when committed
                  (check (equalp committed written) t "old-hundredth.mid as committed"))))))))))

;;; 10. Rehearsal set ------------------------------------------------------------------

(defun demo-rehearsal ()
  (let* ((hymn (hundredth))
         (voices (score-voices hymn))
         (ids (mapcar #'sp:element-id voices))
         (alto-id (sp:element-id (voice-by-name hymn "Alto"))))
    (out "A rehearsal set is one file per voice with that voice brought forward: louder,~%~
          centred, the others quieter and to one side. REHEARSAL-MIX gives the :MIX of~%~
          SCORE-TO-SMF for one voice. For the alto (voice ~a):~%~%" alto-id)
    (let ((*print-pretty* nil))
      (dolist (entry (smidi:rehearsal-mix hymn alto-id))
        (out "  ~s~%" entry)))
    (check (smidi:rehearsal-mix hymn alto-id)
           (mapcar (lambda (id) (if (equal id alto-id)
                                    (list id :velocity 20 :volume 127 :pan 64)
                                    (list id :velocity 0 :volume 80 :pan 16)))
                   ids)
           "rehearsal-mix for the alto")
    (let ((set (call-collecting-losses (lambda () (smidi:rehearsal-set hymn :verse :pass)))))
      (out "~%(rehearsal-set hymn :verse :pass) makes ~d SMFs, each with the score's other~%~
            options (here the repeat played out and both verses). In each file, every voice~%~
            track's note-on velocity on line 1 (mf), line 3 (p) and line 4 (f), its CC7 and~%~
            its CC10:~%~%" (length set))
      (out "  file           ~{~22a~}~%" (mapcar #'sp:voice-name voices))
      (out "                 ~{~22a~}~%" (loop repeat (length voices) collect "velocity   CC7 CC10"))
      (loop for (id . smf) in set
            for voice = (find id voices :key #'sp:element-id :test #'equal)
            for cells = (loop for track in (rest (midi:smf-tracks smf))
                              for notes = (track-notes track)
                              for (nil nil volume pan) = (track-setup track)
                              collect (list (loop for tick in '(0 9600 14400)
                                                  collect (fourth (find tick notes :key #'first)))
                                            volume pan))
            do (out "  ~13a  ~{~22a~}~%" (format nil "~a file" (sp:voice-name voice))
                    (mapcar (lambda (cell) (format nil "~10a ~3d ~4d" (format nil "~{~d~^/~}" (first cell)) (second cell) (third cell)))
                            cells))
               (check cells
                      (loop for other in voices
                            collect (if (eq other voice)
                                        '((100 69 116) 127 64)
                                        '((80 49 96) 80 16)))
                      (format nil "~a rehearsal file" (sp:voice-name voice)))
               (check (length (midi:write-smf smf nil)) (length (midi:write-smf (hundredth-file-smf) nil))
                      "same size as the plain file"))
      (check (mapcar #'car set) ids "one file per voice, in order")
      (out "~%The voice brought forward plays 20 louder (+BOOST+ over 80, 49 and 96), at CC7~%~
            127 and centred (64); the others keep their velocities at CC7 80, panned to 16.~%~
            Only bytes in the mix differ, so each file has the size of the plain export.~%"))
    (out "~%A voice id that isn't in the score signals UNKNOWN-ELEMENT inside a USE-VALUE~%~
          restart. A handler that answers with the tenor's id:~%~%")
    (let* ((tenor-id (sp:element-id (voice-by-name hymn "Tenor")))
           (seen nil)
           (mix (handler-bind ((sp:unknown-element
                                 (lambda (c)
                                   (setf seen (sp:unknown-element-id c))
                                   (out "  UNKNOWN-ELEMENT: ~a~%  innermost restart: ~a, \"~a\"~%" c
                                        (restart-name (first (compute-restarts c))) (first (compute-restarts c)))
                                   (invoke-restart 'use-value tenor-id))))
                  (smidi:rehearsal-mix hymn "tenor"))))
      (out "  (rehearsal-mix hymn \"tenor\") then brings forward ~a: ~s~%"
           (first (find-if (lambda (entry) (eql 20 (getf (rest entry) :velocity))) mix))
           (let ((*print-pretty* nil)) (find tenor-id mix :key #'first :test #'equal)))
      (check (list seen (first (find-if (lambda (entry) (eql 20 (getf (rest entry) :velocity))) mix)))
             (list "tenor" tenor-id) "unknown voice id and USE-VALUE")))
  (values))

;;; 11. Round trip -----------------------------------------------------------------------

(defun demo-round-trip ()
  (let* ((hymn (hundredth))
         (bytes (midi:write-smf (export-smf hymn :unfold nil) nil))
         (guesses 0)
         (losses '())
         (back (handler-bind ((sp:lossy-conversion (lambda (w) (push (sp:lossy-conversion-what w) losses)
                                                      (muffle-warning w)))
                              (sp:low-confidence-guess (lambda (g) (declare (ignore g)) (incf guesses))))
                 (smidi:import-midi bytes)))
         (part (fset:first (sp:score-parts back))))
    (out "(import-midi bytes) reads the as-written export (~d bytes) back into a score.~%~
          Import warnings: ~s; LOW-CONFIDENCE-GUESS signals: ~d.~%~%"
         (length bytes) (reverse losses) guesses)
    (check (list (reverse losses) guesses) '(() 0) "a clean import")
    (out "  metadata  ~s~%" (sp:score-metadata back))
    (out "  bars      ~d to ~d, pickup ~a, ~a, ~a, ~a whole notes~%"
         (sp:score-first-bar back) (sp:score-last-bar back)
         (sp:measure-info-pickup-p (fset:first (sp:score-measure-map back)))
         (mt:meter-string (sp:bar-time back 1)) (mt:key-name (sp:bar-key back 1)) (sp:score-duration back))
    (out "  tempo     ~{~a~}~%"
         (loop for tm in (sp:measure-info-tempo (fset:first (sp:score-measure-map back)))
               collect (format nil "quarter = ~a at ~a" (mt:tempo-bpm (sp:tempo-mark-tempo tm)) (sp:tempo-mark-offset tm))))
    (out "  parts     ~d: ~s with voices ~{~s~^ ~} on ~d staves (~{~(~a~)~^ ~})~%"
         (fset:size (sp:score-parts back)) (sp:part-name part) (mapcar #'sp:voice-name (sp:part-voices part))
         (length (sp:part-staves part)) (mapcar #'sp:staff-clef (sp:part-staves part)))
    (check (list (sp:score-first-bar back) (sp:score-last-bar back) (sp:score-duration back)
                 (mt:meter-string (sp:bar-time back 1)) (mt:key-name (sp:bar-key back 1))
                 (sp:part-name part) (mapcar #'sp:voice-name (sp:part-voices part)))
           (list 0 10 10 "4/4" "G major" "Choir" '("Soprano" "Alto" "Tenor" "Bass"))
           "imported structure")
    (out "~%The four voice tracks share the instrument name \"Choir\", so they come back as one~%~
          part, each voice named by its track. Voice by voice, the sounding notes (onset,~%~
          end and spelled pitch, tied pieces joined) and the lyrics (onset, text, syllabic),~%~
          then the chord symbols, against the original:~%~%")
    (out "  compared        rows  differences~%")
    (let ((total 0))
      (flet ((row (label original imported)
               (let ((diff (+ (abs (- (length original) (length imported)))
                              (count nil (mapcar #'equal original imported)))))
                 (incf total diff)
                 (out "  ~14a  ~4d  ~d~%" label (length original) diff))))
        (loop for voice in (score-voices hymn)
              for other in (score-voices back)
              do (row (format nil "~a notes" (sp:voice-name voice)) (voice-notes hymn voice) (voice-notes back other)))
        (row "Soprano lyrics" (voice-lyrics hymn (first (score-voices hymn)))
             (voice-lyrics back (first (score-voices back))))
        (row "chord symbols" (anchor-rows hymn) (anchor-rows back)))
      (out "  differences in all: ~d~%" total)
      (check total 0 "round trip differences"))
    (let ((alto (second (score-voices back))))
      (out "~%The imported alto, bars 2 and 3: the one note across the barline is cut there into~%~
            two tied pieces again, as in the original (the ids are minted anew):~%  ~a~%  ~a~%"
           (voice-text alto :from 2 :to 3)
           (let ((tied (find-if (lambda (row) (and (typep (second row) 'sp:note-event) (sp:note-tie (second row))))
                                (voice-events back alto))))
             (format nil "~a at ~a has a TIE to ~a" (sp:element-id (second tied))
                     (position-text (cons (third tied) (fourth tied))) (sp:tie-end (sp:note-tie (second tied))))))
      (check (voice-text alto :from 2 :to 3) "D4 D4 D4:h~ | D4:h E4 D4" "imported alto bars 2-3"))
    (let ((elements (sp:score-elements back)))
      (out "~%All ~d imported elements have provenance :MIDI from ~s, and (uncertain-elements~%~
            score) is ~s: every position is on the grid and every pitch is in G major, so~%~
            nothing was guessed.~%"
           (length elements)
           (sp:provenance-backend (sp:element-provenance (first elements)))
           (sp:uncertain-elements back))
      (check (every (lambda (e) (eq :midi (sp:provenance-source (sp:element-provenance e)))) elements) t
             "provenance :midi")
      (check (sp:uncertain-elements back) '() "no uncertain elements"))
    (out "~%What a MIDI file can't carry stays behind: the ids, the dynamics (only their~%~
          velocities are in the file), the subtitle, composer and lyricist, stems and the~%~
          two-staff layout (the import gives each voice its own staff, its clef from the mean~%~
          pitch), and the repeat sign, which this file plays once.~%")))

;;; 12. A performance ----------------------------------------------------------------------
;;; The "Ode to Joy" melody (Beethoven, Symphony No. 9, 1824), bars 1-4 and
;;; 9-12 of the theme in D major, played at quarter = 100 on a keyboard. Bar 7
;;; is varied: a triplet run F#4 G4 G#4 up to A4 in place of the eighths F#4 G4.

(defparameter *ode*
  '("F#4 F#4 G4 A4" "A4 G4 F#4 E4" "D4 D4 E4 F#4" "F#4/3:8 E4/1:8 E4/2"
    "E4 E4 F#4 D4" "E4 F#4/1:8 G4/1:8 F#4 D4" "E4 F#4/1:12 G4/1:12 G#4/1:12 A4 F#4" "D4 E4 A3/2")
  "Bar by bar: a pitch, then /n:d for n/d of a whole note (else a quarter).")

(defun ode-notes ()
  "((onset duration midi) ...) of the written melody: whole notes from 0."
  (let ((onset 0) (notes '()))
    (dolist (bar *ode*)
      (dolist (token (words bar))
        (let* ((slash (position #\/ token))
               (pitch (mt:parse-pitch (subseq token 0 slash)))
               (colon (position #\: token))
               (duration (cond (colon (/ (parse-integer token :start (1+ slash) :end colon)
                                         (parse-integer token :start (1+ colon))))
                               (slash (/ 1 (parse-integer token :start (1+ slash))))
                               (t 1/4))))
          (push (list onset duration (mt:pitch-midi pitch)) notes)
          (incf onset duration))))
    (nreverse notes)))

(defconstant +ode-tempo+ 600000 "Quarter = 100: 600 ms per quarter.")

(defun lcg-stream (seed)
  "A function returning the next value of a 31-bit linear congruential generator."
  (let ((x seed))
    (lambda () (setf x (mod (+ (* 1103515245 x) 12345) (expt 2 31))))))

(defun ode-performance ()
  "((written-onset midi on-tick off-tick velocity jitter-ms) ...): each note
played up to 20 ms early or late, held for 90% of its length, its velocity
between 70 and 90, from a seeded generator."
  (let ((next (lcg-stream 1824)))
    (loop for (onset duration midi) in (ode-notes)
          for jitter = (- (mod (ash (funcall next) -8) 41) 20)
          for jitter* = (if (zerop onset) (abs jitter) jitter)
          for ms = (+ (* onset 4 600) jitter*)
          for on = (round (* ms 480) 600)
          for off = (+ on (round (* 9/10 duration 4 480)))
          collect (list onset midi on off (+ 70 (mod (ash (funcall next) -8) 21)) jitter*))))

(defun ode-smf ()
  "The performance as a one-track SMF of format 0: a tempo, a program and the
notes. No time or key signature: the keyboard didn't know them."
  (let ((timed (list (cons 0 (midi:make-tempo-event :tempo +ode-tempo+))
                     (cons 0 (midi:make-program-change-event :channel 0 :program 0)))))
    (loop for (nil midi on off velocity) in (ode-performance)
          do (push (cons on (midi:make-note-on-event :channel 0 :key midi :velocity velocity)) timed)
             (push (cons off (midi:make-note-off-event :channel 0 :key midi :velocity 64)) timed))
    (midi:make-smf :format 0 :division +ppq+
                   :tracks (list (midi:track-from-absolute
                                  (stable-sort (nreverse timed) #'< :key #'car))))))

(defun ode-beats ()
  "The quarter-note beats of the 8 bars of 4/4, as QUANTIZE-ONSETS takes them."
  (loop for k below 32 collect (list (/ k 4) (/ (1+ k) 4) 1/4)))

(defun beat-onsets (beat onsets)
  "The indices of the ONSETS that lie in BEAT, a (START END UNIT) list."
  (loop for o in onsets
        for i from 0
        when (and (<= (first beat) o) (< o (second beat))) collect i))

(defun demo-performance ()
  (let* ((performance (ode-performance))
         (smf (ode-smf))
         (bytes (midi:write-smf smf nil))
         (onsets (mapcar (lambda (row) (ticks-whole (third row))) performance))
         (written (mapcar #'first performance)))
    (out "A performance has no notation in it: the notes start wherever the player put them.~%~
          Here the \"Ode to Joy\" melody (Beethoven, 1824; bars 1-4 and 9-12 of the theme, D~%~
          major), played at quarter = 100 with every onset up to 20 ms early or late, each~%~
          note held for 90% of its value, velocities 70 to 90 (a seeded generator). Bar 7 is~%~
          varied: a triplet F#4 G4 G#4 runs up to A4 where the theme has the eighths F#4 G4.~%~
          The SMF (format 0, ~d bytes) has a tempo, a program change and the notes; like a~%~
          keyboard recording it has no time or key signature.~%~%" (length bytes))
    (out "  bar  written notes, then each note's jitter in ms~%")
    (loop for bar from 1 to 8
          for rows = (remove-if-not (lambda (row) (= bar (1+ (floor (first row) 1)))) performance)
          do (out "  ~3d  ~36a ~{~@d~^ ~}~%" bar (nth (1- bar) *ode*) (mapcar #'sixth rows)))
    (check (length performance) 33 "33 notes")
    (check (every (lambda (row) (<= -20 (sixth row) 20)) performance) t "jitter within 20 ms")
    ;; The grid, through sat-protocol.
    (out "~%QUANTIZE-ONSETS chooses a subdivision per beat (1, 2, 3, 4, 6 or 8) by cost: onset~%~
          deviation plus notation complexity, a triplet costing more than a straight grid.~%~
          The choice is a selection problem, solved with SAT-PROTOCOL:MINIMIZE. With no~%~
          default backend, each MINIMIZE call signals SAT-NO-BACKEND, and a handler gives it~%~
          a CDCL backend through the USE-BACKEND restart, which also counts the calls:~%~%")
    (let* ((calls 0)
           (beats (ode-beats)))
      (multiple-value-bind (choices positions confidences)
          (let ((sat:*sat-backend* nil))
            (handler-bind ((sat:sat-no-backend
                             (lambda (c)
                               (incf calls)
                               (invoke-restart (find-restart 'sat:use-backend c)
                                               (make-instance 'cdcl:cdcl-backend)))))
              (smidi:quantize-onsets onsets beats)))
        (out "  beat   onsets (ms from the beat)    grid           snapped to (whole)  confidence~%")
        (loop for (start) in beats
              for k from 0
              for s in choices
              for idx = (beat-onsets (nth k beats) onsets)
              when idx
                do (out "  ~d.~d    ~28a ~14a ~19a ~{~,2f~^ ~}~%"
                        (1+ (floor k 4)) (1+ (mod k 4))
                        (format nil "~{~d~^ ~}" (mapcar (lambda (i) (round (* (- (nth i onsets) start) 4 600))) idx))
                        (format nil "~d (~a)" s (case s (1 "quarter") (2 "eighths") (3 "triplet") (4 "16ths") (t "")))
                        (format nil "~{~a~^ ~}" (mapcar (lambda (i) (- (nth i positions) start)) idx))
                        (mapcar (lambda (i) (float (nth i confidences))) idx)))
        (out "~%An onset that comes early lies in the beat before its own, near the end, and~%~
              snaps to that beat's end, the next beat's start: in 1.2 the G4 played 7 ms early~%~
              is at 592 ms. Every onset snaps to its written position: ~a. Beat 4.2 (the eighth~%~
              after the dotted quarter) and beat 6.2 (two eighths) choose 2, beat 7.2 chooses~%~
              3, the triplet, and the rest stay on the quarter. The ~d beats with onsets took ~d~%~
              MINIMIZE calls: beats whose cost tables are equal share one answer, and the ~d~%~
              beats inside held notes need none.~%"
             (if (equal positions written) "yes" "no")
             (count-if (lambda (beat) (beat-onsets beat onsets)) beats)
             calls
             (count-if-not (lambda (beat) (beat-onsets beat onsets)) beats))
        (check (equal positions written) t "the snapped onsets are the written ones")
        (check (list (nth 13 choices) (nth 21 choices) (nth 25 choices)) '(2 2 3) "eighths, eighths, triplet")
        (check (count 1 choices) 29 "the other beats on the quarter")
        (check (plusp calls) t "MINIMIZE was called")
        (let ((k 25))
          (multiple-value-bind (straight-choices straight-positions straight-confidences)
              (smidi:quantize-onsets onsets beats :subdivisions '(1 2 4 8))
            (let ((idx (beat-onsets (nth k beats) onsets)))
              (out "~%With straight grids only (:subdivisions '(1 2 4 8)), beat 7.2 takes ~d (16ths)~%~
                    and snaps the triplet's onsets to ~{~a~^ ~} of the beat (written ~{~a~^ ~}), with~%~
                    confidences ~{~,2f~^ ~}: the import would flag them.~%"
                   (nth k straight-choices)
                   (mapcar (lambda (i) (- (nth i straight-positions) (first (nth k beats)))) idx)
                   (mapcar (lambda (i) (- (nth i written) (first (nth k beats)))) idx)
                   (mapcar (lambda (i) (float (nth i straight-confidences))) idx))
              (check (nth k straight-choices) 4 "straight-only grid for the triplet beat")
              (check (every (lambda (i) (< (nth i straight-confidences) smidi:+flag-below+)) idx) t
                     "straight-only confidences flagged"))))))
    ;; The import.
    (out "~%(import-midi bytes) does the same per voice, then spells the pitches, finds the~%~
          bars and builds the score. It SIGNALs LOW-CONFIDENCE-GUESS for every element whose~%~
          confidence is below +FLAG-BELOW+ (~a). The signal is silent unless handled; here a~%~
          HANDLER-BIND logs each one:~%~%"
         smidi:+flag-below+)
    (let* ((log '())
           (score (handler-bind ((sp:low-confidence-guess
                                   (lambda (g)
                                     (push (list (sp:low-confidence-guess-element-id g)
                                                 (sp:low-confidence-guess-what g)
                                                 (sp:low-confidence-guess-confidence g))
                                           log)
                                     (out "  LOW-CONFIDENCE-GUESS ~a~%" g)))
                                 (sp:lossy-conversion
                                   (lambda (w) (out "  LOSSY-CONVERSION ~a~%" w) (muffle-warning w))))
                    (smidi:import-midi bytes)))
           (log (nreverse log))
           (voice (first (score-voices score)))
           (first-measure (fset:first (sp:score-measure-map score)))
           (notes (voice-notes score voice)))
      (out "~%The imported melody:~%")
      (loop for from from 0 below 8 by 4
            do (out "  ~a~%" (voice-text voice :from from :to (+ from 3) :quarter-implied nil)))
      (out "  (q a quarter, e an eighth, h a half, 3 a triplet note, ~~ a tie)~%")
      (check (mapcar #'first notes) written "imported onsets as written")
      (check (mapcar #'third notes)
             '("F#4" "F#4" "G4" "A4" "A4" "G4" "F#4" "E4" "D4" "D4" "E4" "F#4" "F#4" "E4" "E4"
               "E4" "E4" "F#4" "D4" "E4" "F#4" "G4" "F#4" "D4" "E4" "F#4" "G4" "G#4" "A4" "F#4"
               "D4" "E4" "A3")
             "spelled pitches")
      (check (count-if (lambda (row) (sp:event-tuplet (second row))) (voice-events score voice)) 3
             "three triplet notes")
      (let ((ranking (mt:detect-key (mt:pitch-class-histogram (mapcar #'third (ode-notes)) :weight :count)))
            (sharp (second (find-if (lambda (row) (equal "G#4" (mt:pitch-name (sp:note-pitch (second row)))))
                                    (voice-events score voice)))))
        (out "~%The meter: no time signature, so 4/4 was assumed, confidence ~a (+GUESS-CONFIDENCE+).~%~
              The key: no key signature, so the import ranks the keys by correlation with the~%~
              pitch-class histogram of the notes, music-theory's DETECT-KEY; the first four:~%~
              ~{  ~{~a ~,3f~}~^,~}.~%~
              ~a is the key, and its correlation would be the key's confidence; the first~%~
              measure takes the lesser of the two, ~a, and is reported as \"meter\".~%~
              Spelling follows the key: MIDI 66 is F#4, not Gb4, in D major, and the chromatic~%~
              68 of the triplet is G#4, the raised fourth rising to A4. It is not in the key, but~%~
              rule S4 (a semitone resolution) decides it. Since 0.1.1 its spelling confidence is~%~
              1, so the note keeps its onset's quantisation confidence, ~a (~,3f), and is not~%~
              flagged (0.1.0 gave it 14/45; section 14).~%"
             smidi:+guess-confidence+
             (mapcar (lambda (cell) (list (mt:key-name (car cell)) (cdr cell))) (subseq ranking 0 4))
             (mt:key-name (car (first ranking))) (sp:element-confidence first-measure)
             (sp:element-confidence sharp) (float (sp:element-confidence sharp)))
        (check (mt:key-name (car (first ranking))) "D major" "first of the ranking")
        (check (sp:element-confidence sharp) 99/100 "G#4 confidence"))
      (check (mt:key-name (sp:bar-key score (sp:score-first-bar score))) "D major" "detected key")
      (out "~%The uncertainty report, (uncertain-elements score) in document order:~%~%")
      (out "  id      kind          position  confidence  what~%")
      (dolist (element (sp:uncertain-elements score))
        (let ((entry (find (sp:element-id element) log :key #'first :test #'equal)))
          (out "  ~6a  ~12a  ~8a  ~10,3f  ~a~@[  ~a~]~%"
               (sp:element-id element) (string-downcase (type-of element))
               (position-text (sp:element-position score (sp:element-id element)))
               (float (sp:element-confidence element)) (second entry)
               (typecase element
                 (sp:note-event (event-label element :quarter-implied nil))
                 (sp:measure-info (format nil "~a, ~a" (mt:meter-string (sp:bar-time score 1))
                                          (mt:key-name (sp:bar-key score 1))))))))
      (check (mapcar #'sp:element-id (sp:uncertain-elements score)) (mapcar #'first log)
             "the logged guesses are the uncertainty report")
      (check (sp:element-confidence first-measure) smidi:+guess-confidence+ "the meter guess")
      (out "~%The HANDLER-BIND saw exactly these ids, in this order. No onset is in doubt: a~%~
            20 ms slip is small against a 300 ms eighth or a 200 ms triplet eighth, and the~%~
            least note confidence from quantisation is ~,2f. CHECK-SCORE finds ~d issues.~%"
           (reduce #'min (loop for (nil event) in (voice-events score voice)
                               when (and (typep event 'sp:note-event)
                                         (not (equal "G#4" (mt:pitch-name (sp:note-pitch event)))))
                                 collect (or (sp:element-confidence event) 1)))
           (length (sp:check-score score)))
      (check (sp:check-score score) '() "the imported performance is valid"))))

;;; 13. mido ------------------------------------------------------------------------------------

(defparameter *mido-score-script*
  "import json, sys, importlib.metadata, mido
f = mido.MidiFile(sys.argv[1])
q = json.dumps
out = ['(:version', q(importlib.metadata.version('mido')), ':type', str(f.type),
       ':ticks-per-beat', str(f.ticks_per_beat), ':seconds', repr(f.length), ':tracks (']
for t in f.tracks:
    ons = [m for m in t if m.type == 'note_on' and m.velocity > 0]
    lyrics = [m.text for m in t if m.type == 'lyrics']
    out += ['(:name', q(t.name),
            ':channels (', ' '.join(str(c) for c in sorted({m.channel for m in ons})), ')',
            ':programs (', ' '.join(str(m.program) for m in t if m.type == 'program_change'), ')',
            ':note-ons', str(len(ons)),
            ':velocities (', ' '.join(str(v) for v in sorted({m.velocity for m in ons})), ')',
            ':lyrics', str(len(lyrics)),
            ':first-lyrics (', ' '.join(q(s) for s in lyrics[:4]), ')',
            ':texts', str(sum(1 for m in t if m.type == 'text')),
            ':tempos (', ' '.join(str(m.tempo) for m in t if m.type == 'set_tempo'), ')',
            ':time-signatures (', ' '.join(q('%d/%d' % (m.numerator, m.denominator)) for m in t if m.type == 'time_signature'), ')',
            ':keys (', ' '.join(q(m.key) for m in t if m.type == 'key_signature'), '))']
out.append('))')
print(' '.join(out))
"
  "Python for `python -I -c`: mido's view of the file named by argv[1], printed as a Lisp plist.")

(defun our-track-view (track)
  "What *MIDO-SCORE-SCRIPT* prints for one track, from midi-protocol's reading."
  (let ((notes (track-notes track)))
    (list :name (or (first-text track #'midi:track-name-event-p) "")
          :channels (sort (remove-duplicates (mapcar #'fifth notes)) #'<)
          :programs (mapcar #'midi:program-change-event-program (remove-if-not #'midi:program-change-event-p track))
          :note-ons (length notes)
          :velocities (sort (remove-duplicates (mapcar #'fourth notes)) #'<)
          :lyrics (count-if #'midi:lyric-event-p track)
          :first-lyrics (let ((lyrics (mapcar #'midi:text-meta-event-text (remove-if-not #'midi:lyric-event-p track))))
                          (subseq lyrics 0 (min 4 (length lyrics))))
          :texts (count-if #'midi:text-event-p track)
          :tempos (mapcar #'midi:tempo-event-tempo (remove-if-not #'midi:tempo-event-p track))
          :time-signatures (mapcar (lambda (e) (format nil "~d/~d" (midi:time-signature-event-numerator e)
                                                       (midi:time-signature-event-denominator e)))
                                   (remove-if-not #'midi:time-signature-event-p track))
          :keys (mapcar (lambda (e)
                          (let ((sharps (midi:key-signature-event-sharps e)))
                            (if (midi:key-signature-event-minor-p e)
                                (format nil "~am" (aref +minor-keys+ (+ sharps 7)))
                                (aref +major-keys+ (+ sharps 7)))))
                        (remove-if-not #'midi:key-signature-event-p track)))))

(defun demo-score-mido ()
  (let ((path (hundredth-path))
        (smf (hundredth-file-smf)))
    (unless (probe-file path)
      (midi:write-smf smf path))
    (if (not (uv-available-p))
        (out "uv is not on PATH, so the cross-read of old-hundredth.mid with mido is skipped~%~
              (score-backend-midi's own tests skip it the same way).~%")
        (multiple-value-bind (code stdout stderr)
            (proc:run (list "uv" "run" "--no-project" "--with" "mido==1.3.3" "python" "-I" "-c"
                            *mido-score-script* (namestring path))
                      :output :string :error-output :string :timeout 600
                      :env '(("PYTHONIOENCODING" . "utf-8")))
          (out "uv run --no-project --with mido==1.3.3 python -I -c '<script>' old-hundredth.mid~%~
                reads the exported hymn with mido (exit ~d). Per track, what mido sees:~%~%" code)
          (check code 0 "uv run exit code")
          (if (/= code 0)
              (out "~a~%" stderr)
              (let* ((mido (let ((*read-eval* nil)
                                 (*read-default-float-format* 'double-float)
                                 (*package* (find-package '#:keyword)))
                             (read-from-string stdout)))
                     (ours (mapcar #'our-track-view (midi:smf-tracks smf)))
                     (*print-pretty* nil))
                (loop for track in (getf mido :tracks)
                      for i from 0
                      do (out "  track ~d ~s~%" i (getf track :name))
                         (loop for (key value) on (rest (rest track)) by #'cddr
                               unless (or (null value) (eql value 0))
                                 do (out "    ~16a ~s~%" (string-downcase key) value)))
                (check (length (getf mido :tracks)) (length ours) "mido track count")
                (loop for track in (getf mido :tracks)
                      for view in ours
                      for i from 0
                      do (loop for (key value) on view by #'cddr
                               do (check (getf track key) value (format nil "mido track ~d ~(~a~)" i key))))
                (let ((seconds (midi:ticks-to-seconds (track-end-tick (first (midi:smf-tracks smf))) +ppq+
                                                      (midi:tempo-map smf))))
                  (out "~%  type ~d, ~d ticks per beat, ~,3f s long (TICKS-TO-SECONDS: ~a = ~,3f s)~%"
                       (getf mido :type) (getf mido :ticks-per-beat) (getf mido :seconds) seconds (float seconds 1d0))
                  (check (list (getf mido :type) (getf mido :ticks-per-beat)) '(1 480) "mido header")
                  (check (< (abs (- (getf mido :seconds) seconds)) 1d-9) t "mido length"))
                (out "~%mido agrees with midi-protocol on every track's name, channel, program, note and~%~
                      velocity counts, lyrics, chord text events, tempi, time and key signatures.~%")))))))

;;; 14. The 0.1.1 fixes --------------------------------------------------------------------------
;;; score-backend-midi 0.1.1 changes two import rules: a note's spelling
;;; confidence follows the music-theory rule that decided its spelling
;;; (score-backend-midi#4), and a start snapped to the end of the last bar moves
;;; back into it, flagged (#3, item 1).

(defun quarter-notes-smf (key-name midis)
  "Format 1, 480 ticks a quarter: track 0 holds 4/4 and the signature of the
key KEY-NAME, track 1 the MIDIS as quarter notes from tick 0."
  (let ((key (mt:parse-key key-name)))
    (midi:make-smf
     :format 1 :division +ppq+
     :tracks (list (midi:track-from-absolute
                    (list (cons 0 (midi:make-time-signature-event :numerator 4 :denominator 4))
                          (cons 0 (midi:make-key-signature-event
                                   :sharps (mt:key-signature key) :minor-p (eq :minor (mt:key-mode key))))))
                   (midi:track-from-absolute
                    (loop for midi in midis
                          for tick from 0 by +ppq+
                          collect (cons tick (midi:make-note-on-event :key midi :velocity 80))
                          collect (cons (+ tick +ppq+) (midi:make-note-off-event :key midi))))))))

(defparameter *spelling-rows*
  '(("D major" (62 66 67 68 69)    "a semitone resolution: G#4 rises to A4")
    ("C major" (64 68 71)          "an interval from the note before")
    ("C major" (60 66 60)          "the key's center decides F#4 / Gb4")
    ("C major" (60 66 60 66 60)    "the second F#4 copies the first")
    ("C major" (68)                "a single G#4 / Ab4: a tie between the spellings"))
  "Short note runs, one for each kind of deciding rule: key, MIDI numbers, what decides.")

(defun spelling-table (key-name midis)
  "((pitch rule ambiguity confidence) ...) for MIDIS imported as quarter notes in KEY-NAME."
  (multiple-value-bind (pitches ambiguities rules)
      (mt:spell-sequence midis :key (mt:parse-key key-name))
    (let* ((score (smidi:import-midi (midi:write-smf (quarter-notes-smf key-name midis) nil)))
           (events (mapcar #'second (voice-events score (first (score-voices score))))))
      (loop for pitch in pitches
            for a in ambiguities
            for rule in rules
            for event in events
            do (check (mt:pitch-name (sp:note-pitch event)) (mt:pitch-name pitch) "import spells as SPELL-SEQUENCE")
            collect (list (mt:pitch-name pitch) rule a (or (sp:element-confidence event) 1))))))

(defun joyful-smf ()
  "One bar of 4/4 in D major, the first bar of the \"Ode to Joy\" melody with the
words \"Joy-ful, joy-ful\" (Henry van Dyke, 1907) as lyric events. The first
three notes are on the beat and held for 90% of a quarter. The last, A4,
belongs on beat 4 (tick 1440) but is played at tick 1882 and released at 1910,
10 ticks before the barline. Nothing follows it: the file ends at 1910."
  (flet ((note (tick off key text)
           (list (cons tick (midi:make-note-on-event :key key :velocity 80))
                 (cons tick (midi:make-lyric-event :text text))
                 (cons off (midi:make-note-off-event :key key)))))
    (midi:make-smf
     :format 1 :division +ppq+
     :tracks (list (midi:track-from-absolute
                    (list (cons 0 (midi:make-time-signature-event :numerator 4 :denominator 4))
                          (cons 0 (midi:make-key-signature-event :sharps 2))))
                   (midi:track-from-absolute
                    (append (note 0 432 66 "Joy-") (note 480 912 66 "ful,")
                            (note 960 1392 67 "joy-") (note 1882 1910 69 "ful,")))))))

(defun demo-fixes ()
  (out "score-backend-midi 0.1.1 changes two import rules. For each, the case below shows~%~
        what 0.1.1 imports and what 0.1.0 did (from the issue, the 0.1.1 contract, or the~%~
        0.1.0 run of section 12 in this demo's history).~%~%")
  (out "(a) Spelling confidence from the deciding rule (score-backend-midi#4). The import~%~
        spells pitches with music-theory's SPELL-SEQUENCE, which since music-theory 0.5.2~%~
        also returns the rule that decided each note (:S0 .. :S7). 0.1.0 gave every note~%~
        outside the key the confidence 1 - its ambiguity, whatever decided it, so a G#4~%~
        that resolves up to A4, which no musician would spell Ab4, was flagged. 0.1.1 maps~%~
        the rule to a confidence through +SPELLING-RULE-CONFIDENCE+:~%~%")
  (dolist (row smidi:+spelling-rule-confidence+)
    (out "  ~(~s~)  ~a~%" (car row)
         (let ((source (cdr row)))
           (case source
             (:copied "the confidence of the note whose spelling it copies")
             (:before "the confidence of the note before (the interval's start)")
             (:ambiguity "1 - ambiguity")
             (t (format nil "~a" source))))))
  (check (mapcar #'car smidi:+spelling-rule-confidence+) '(:s0 :s1 :s2 :s3 :s4 :s5 :s6 :s7) "the rules")
  (out "~%Short runs imported as quarter notes, each in its key (QUARTER-NOTES-SMF). Every~%~
        onset is exact, so a note's confidence is its spelling's; +FLAG-BELOW+ is ~a:~%~%"
       smidi:+flag-below+)
  (out "  key      MIDI              spelled (rule, confidence)~%")
  (let ((results '()))
    (loop for (key-name midis what) in *spelling-rows*
          for table = (spelling-table key-name midis)
          do (push table results)
             (out "  ~8a ~17a ~{~a~^  ~}~%           ~a~%" key-name (format nil "~{~d~^ ~}" midis)
                  (mapcar (lambda (r) (format nil "~a (~(~s~), ~a)~:[~; flagged~]" (first r) (second r) (fourth r)
                                              (< (fourth r) smidi:+flag-below+)))
                          table)
                  what))
    (setf results (nreverse results))
    (check (mapcar (lambda (table) (mapcar #'second table)) results)
           '((:s1 :s1 :s1 :s4 :s1) (:s1 :s5 :s1) (:s1 :s6 :s1) (:s1 :s6 :s1 :s3 :s1) (:s7))
           "deciding rules")
    (check (mapcar (lambda (table) (mapcar #'fourth table)) (subseq results 0 4))
           '((1 1 1 1 1) (1 1 1) (1 7/24 1) (1 7/24 1 7/24 1))
           "confidences")
    (let ((tie (first (fifth results))))
      (check (fourth tie) (- 1 (third tie)) "a tie keeps 1 - ambiguity")))
  (out "~%Settled notes (key notes, a resolution, an interval) are no longer flagged. The~%~
        notes only the center or a tie decides still are, and so is a note that copies~%~
        one of them.~%~%")
  (out "The performance of section 12, imported again. Its triplet G#4 is decided by :S4,~%~
        the resolution to A4:~%~%")
  (let* ((midis (mapcar #'third (ode-notes)))
         (k (position 68 midis))
         (rule (nth k (nth-value 2 (mt:spell-sequence midis :key (mt:parse-key "D major")))))
         (score (handler-bind ((sp:lossy-conversion #'muffle-warning))
                  (smidi:import-midi (midi:write-smf (ode-smf) nil))))
         (sharp (second (find-if (lambda (row) (and (typep (second row) 'sp:note-event)
                                                    (equal "G#4" (mt:pitch-name (sp:note-pitch (second row))))))
                                 (voice-events score (first (score-voices score))))))
         (report (mapcar (lambda (e) (list (sp:element-id e) (type-of e) (sp:element-confidence e)))
                         (sp:uncertain-elements score))))
    (out "  rule ~(~s~), confidence ~a (its onset's quantisation confidence), uncertain: ~a~%"
         rule (sp:element-confidence sharp) (if (< (sp:element-confidence sharp) smidi:+flag-below+) "yes" "no"))
    (out "  0.1.0: confidence 14/45 (0.311), and the uncertainty report listed it:~%")
    (out "    e-14    note-event    7:5/12         0.311  spelling  G#4:e3~%")
    (out "  0.1.1 report: ~{~{~a ~(~a~) ~a~}~^, ~}~%" report)
    (check rule :s4 "the triplet G#4's rule")
    (check (sp:element-confidence sharp) 99/100 "the triplet G#4's confidence")
    (check (length report) 1 "the report: the assumed meter alone")
    (check (second (first report)) 'sp:measure-info "the meter guess"))
  (out "~%(b) A late last note stays in the last bar (score-backend-midi#3, item 1).~%~
        JOYFUL-SMF is one bar of the \"Ode to Joy\" melody in D major with the words~%~
        \"Joy-ful, joy-ful\" (Henry van Dyke, 1907). The last note, A4, belongs on beat 4~%~
        but is played 442 ticks late, at 1882, and released at 1910, just before the~%~
        barline at 1920. The file ends there, so the import finds one bar. On the beat's~%~
        grid the start snaps to 1920, the end of the bar, which no bar holds.~%~%")
  (let* ((bytes (midi:write-smf (joyful-smf) nil))
         (log '())
         (score (handler-bind ((sp:low-confidence-guess
                                 (lambda (g)
                                   (push (list (sp:low-confidence-guess-element-id g)
                                               (sp:low-confidence-guess-what g)
                                               (sp:low-confidence-guess-confidence g))
                                         log)))
                               (sp:lossy-conversion #'muffle-warning))
                  (smidi:import-midi bytes)))
         (voice (first (score-voices score)))
         (rows (voice-events score voice))
         (late (second (car (last rows)))))
    (setf log (nreverse log))
    (out "  (import-midi bytes)~%  bars ~d, the melody ~a~%  lyrics ~{~{~a ~s ~(~a~)~}~^, ~}~%"
         (fset:size (sp:score-measure-map score)) (voice-text voice) (voice-lyrics score voice))
    (out "  guesses ~{~{~a ~a ~a~}~^, ~}~%" log)
    (check (fset:size (sp:score-measure-map score)) 1 "no bar is added")
    (check (mapcar #'first (voice-notes score voice)) '(0 1/4 1/2 3/4) "A4 on beat 4")
    (check (mt:pitch-name (sp:note-pitch late)) "A4" "the last note")
    (check (sp:element-confidence late) smidi:+guess-confidence+ "flagged at +GUESS-CONFIDENCE+")
    (check (assoc (sp:element-id late) log :test #'equal) (list (sp:element-id late) "quantization" 1/2)
           "its guess")
    (check (mapcar #'second (voice-lyrics score voice)) '("Joy" "ful," "joy" "ful,") "every syllable on its note")
    (check (sp:check-score score) '() "a valid score")
    (out "~%The start moved back to the last point of beat 4's own grid below the barline,~%~
          the beat itself, at confidence ~a (+GUESS-CONFIDENCE+), and it is reported as a~%~
          quantisation guess. Its lyric stays on it. 0.1.0 made no event for the A4, and~%~
          placing its lyric then failed with an error. With :QUANTIZE NIL the ticks are~%~
          kept, and the A4 starts inside the bar:~%~%" smidi:+guess-confidence+)
    (let* ((exact (handler-bind ((sp:lossy-conversion #'muffle-warning))
                    (smidi:import-midi bytes :quantize nil)))
           (starts (mapcar #'first (voice-notes exact (first (score-voices exact))))))
      (out "  (import-midi bytes :quantize nil) starts ~{~a~^ ~}~%" starts)
      (check starts '(0 1/4 1/2 941/960) "unquantised starts"))))
