;;;; A tour of midi-protocol 0.1.0: the MIDI 1.0 event model and the Standard
;;;; MIDI File (SMF) codec for types 0, 1 and 2, files only. One short piece,
;;;; "Twinkle, Twinkle, Little Star" (tune and words in the public domain), is
;;;; built in code, written, read back, timed, broken and repaired. Every result
;;;; with a known answer is checked: RUN-DEMOS returns NIL when one differs or a
;;;; section stops on an error.

(in-package #:harness-demos-midi)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args))

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (actual expected label &key (test #'equal))
  "Count one self-check; print it only when it fails. Returns ACTUAL."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (let ((*print-pretty* nil))
      (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected)))
  actual)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

;;; Display helpers ------------------------------------------------------------

(defun hex (octets &optional (start 0) end)
  "OCTETS[START, END) as upper-case hex pairs separated by spaces."
  (format nil "~{~2,'0X~^ ~}" (coerce (subseq octets start end) 'list)))

(defun hex-rows (octets start end &optional (width 16))
  (loop for i from start below end by width
        collect (hex octets i (min end (+ i width)))))

(defparameter +pitch-classes+ #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(defun note-name (key)
  "A MIDI key number as a pitch name in the scientific octave (60 is C4)."
  (format nil "~a~d" (aref +pitch-classes+ (mod key 12)) (1- (floor key 12))))

(defparameter +major-keys+ #("Cb" "Gb" "Db" "Ab" "Eb" "Bb" "F" "C" "G" "D" "A" "E" "B" "F#" "C#")
  "Major key by key-signature SHARPS + 7.")
(defparameter +minor-keys+ #("Ab" "Eb" "Bb" "F" "C" "G" "D" "A" "E" "B" "F#" "C#" "G#" "D#" "A#")
  "Minor key by key-signature SHARPS + 7.")
(defparameter +program-names+ '((73 . "Flute") (32 . "Acoustic Bass"))
  "General MIDI names of the programs used here (program numbers count from 0).")
(defparameter +controller-names+ '((7 . "volume") (10 . "pan")))

(defun bpm (tempo)
  "Quarter notes per minute for TEMPO microseconds per quarter note."
  (/ 60000000 tempo))

(defun decimal (x &optional (digits 3))
  (format nil "~,vF" digits (float x 1d0)))

(defun describe-event (event)
  "One line for EVENT, without its delta."
  (let ((*print-pretty* nil))
    (typecase event
      (midi:note-on-event
       (format nil "note-on         ch ~d  ~4a vel ~d" (midi:channel-event-channel event)
               (note-name (midi:note-on-event-key event)) (midi:note-on-event-velocity event)))
      (midi:note-off-event
       (format nil "note-off        ch ~d  ~4a vel ~d" (midi:channel-event-channel event)
               (note-name (midi:note-off-event-key event)) (midi:note-off-event-velocity event)))
      (midi:program-change-event
       (let ((program (midi:program-change-event-program event)))
         (format nil "program-change  ch ~d  ~d (~a)" (midi:channel-event-channel event) program
                 (or (cdr (assoc program +program-names+)) "?"))))
      (midi:control-change-event
       (let ((controller (midi:control-change-event-controller event)))
         (format nil "control-change  ch ~d  cc ~d (~a) = ~d" (midi:channel-event-channel event)
                 controller (or (cdr (assoc controller +controller-names+)) "?")
                 (midi:control-change-event-value event))))
      (midi:track-name-event (format nil "track-name      ~s" (midi:text-meta-event-text event)))
      (midi:lyric-event (format nil "lyric           ~s" (midi:text-meta-event-text event)))
      (midi:marker-event (format nil "marker          ~s" (midi:text-meta-event-text event)))
      (midi:tempo-event
       (let ((tempo (midi:tempo-event-tempo event)))
         (format nil "tempo           ~d us per quarter (~a bpm)" tempo (bpm tempo))))
      (midi:time-signature-event
       (format nil "time-signature  ~d/~d, ~d clocks per click, ~d 32nds per quarter"
               (midi:time-signature-event-numerator event)
               (midi:time-signature-event-denominator event)
               (midi:time-signature-event-clocks-per-click event)
               (midi:time-signature-event-thirty-seconds-per-quarter event)))
      (midi:key-signature-event
       (let* ((sharps (midi:key-signature-event-sharps event))
              (minor (midi:key-signature-event-minor-p event))
              (mode (if minor "minor" "major")))
         (format nil "key-signature   ~d sharps, ~a (~a ~a)"
                 sharps mode (aref (if minor +minor-keys+ +major-keys+) (+ sharps 7)) mode)))
      (midi:sysex-event (format nil "sysex           ~a" (hex (midi:sysex-event-data event))))
      (midi:end-of-track-event "end-of-track")
      (t (prin1-to-string event)))))

(defun lyric-line (syllables)
  "SYLLABLES as text: a syllable ending in a hyphen joins the next one."
  (with-output-to-string (s)
    (loop for (syllable . more) on syllables
          for joined = (and (plusp (length syllable))
                            (char= #\- (char syllable (1- (length syllable)))))
          do (write-string (if joined (subseq syllable 0 (1- (length syllable))) syllable) s)
             (when (and more (not joined)) (write-char #\Space s)))))

;;; The piece ------------------------------------------------------------------

(defconstant +ppq+ 480 "Ticks per quarter note: the SMF division.")
(defconstant +tempo-1+ 600000 "Microseconds per quarter note in phrase 1: 100 bpm.")
(defconstant +tempo-2+ 750000 "Phrase 2 is a little broader: 80 bpm.")
(defconstant +phrase-2+ (* 8 480) "Tick of bar 3, where phrase 2 starts.")
(defconstant +end+ (* 16 480) "Tick of the end of bar 4.")

(defparameter *title* "Twinkle, Twinkle, Little Star")

(defparameter *melody*
  '((60 1 "Twin-") (60 1 "kle") (67 1 "twin-") (67 1 "kle") (69 1 "lit-") (69 1 "tle") (67 2 "star,")
    (65 1 "how") (65 1 "I") (64 1 "won-") (64 1 "der") (62 1 "what") (62 1 "you") (60 2 "are."))
  "The tune, one (key quarter-notes syllable) per note: bars 1-2 are phrase 1, bars 3-4 phrase 2.")

(defparameter *bass*
  '((36 43) (36 43) (41 48) (36 43) (41 48) (36 43) (43 50) (36 43))
  "Root and fifth, a half note each: C C | F C | F C | G C.")

(defun tempo-track ()
  "The conductor track: title, meter, key, tempo and phrase markers."
  (midi:track-from-absolute
   (list (cons 0 (midi:make-track-name-event :text *title*))
         (cons 0 (midi:make-time-signature-event :numerator 4 :denominator 4 :clocks-per-click 24
                                                 :thirty-seconds-per-quarter 8))
         (cons 0 (midi:make-key-signature-event :sharps 0 :minor-p nil))
         (cons 0 (midi:make-tempo-event :tempo +tempo-1+))
         (cons 0 (midi:make-marker-event :text "Phrase 1"))
         (cons +phrase-2+ (midi:make-tempo-event :tempo +tempo-2+))
         (cons +phrase-2+ (midi:make-marker-event :text "Phrase 2"))
         (cons +end+ (midi:make-end-of-track-event)))))

(defun melody-track ()
  "Channel 0, a flute: a lyric event and a note for each syllable."
  (let ((tick 0) (timed '()))
    (flet ((at (tk event) (push (cons tk event) timed)))
      (at 0 (midi:make-track-name-event :text "Melody"))
      (at 0 (midi:make-program-change-event :channel 0 :program 73))
      (at 0 (midi:make-control-change-event :channel 0 :controller 7 :value 100))
      (at 0 (midi:make-control-change-event :channel 0 :controller 10 :value 40))
      (loop for (key quarters syllable) in *melody*
            for end = (+ tick (* quarters +ppq+))
            do (at tick (midi:make-lyric-event :text syllable))
               (at tick (midi:make-note-on-event :channel 0 :key key :velocity 80))
               (at end (midi:make-note-off-event :channel 0 :key key :velocity 64))
               (setf tick end)))
    (midi:track-from-absolute (nreverse timed))))

(defun bass-track ()
  "Channel 1, an acoustic bass: root and fifth together, a half note each."
  (let ((timed '()))
    (flet ((at (tk event) (push (cons tk event) timed)))
      (at 0 (midi:make-track-name-event :text "Bass"))
      (at 0 (midi:make-program-change-event :channel 1 :program 32))
      (at 0 (midi:make-control-change-event :channel 1 :controller 7 :value 90))
      (at 0 (midi:make-control-change-event :channel 1 :controller 10 :value 80))
      (loop for (root fifth) in *bass*
            for start from 0 by (* 2 +ppq+)
            for end = (+ start (* 2 +ppq+))
            do (dolist (key (list root fifth))
                 (at start (midi:make-note-on-event :channel 1 :key key :velocity 70)))
               (dolist (key (list root fifth))
                 (at end (midi:make-note-off-event :channel 1 :key key :velocity 64)))))
    (midi:track-from-absolute (nreverse timed))))

(defun song ()
  "The piece as a fresh type-1 SMF: tempo track, melody, bass."
  (midi:make-smf :format 1 :division +ppq+
                 :tracks (list (tempo-track) (melody-track) (bass-track))))

(defun song-bytes (&key (running-status t))
  (midi:write-smf (song) nil :running-status running-status))

(defun expected-seconds (tick)
  "Seconds at TICK worked out by hand: 100 bpm up to bar 3, 80 bpm after it."
  (let ((per-tick-1 (/ +tempo-1+ (* 1000000 +ppq+)))
        (per-tick-2 (/ +tempo-2+ (* 1000000 +ppq+))))
    (if (<= tick +phrase-2+)
        (* tick per-tick-1)
        (+ (* +phrase-2+ per-tick-1) (* (- tick +phrase-2+) per-tick-2)))))

;;; Where each event's bytes are -----------------------------------------------

(defun chunk-data (track &key (running-status t))
  "The data of TRACK's MTrk chunk as WRITE-SMF writes it: what follows the
14-byte header and the 8-byte chunk header of a one-track file."
  (subseq (midi:write-smf (midi:make-smf :format 0 :division +ppq+ :tracks (list track)) nil
                          :running-status running-status)
          22))

(defun event-ranges (track &key (running-status t))
  "(start . end) of each event of TRACK in its chunk data. Every prefix of TRACK
is written with WRITE-SMF; a prefix that doesn't end with an END-OF-TRACK-EVENT
gets 00 FF 2F 00 appended, and those 4 bytes are not counted."
  (let ((start 0))
    (loop for k from 1 to (length track)
          for prefix = (subseq track 0 k)
          for end = (- (length (chunk-data prefix :running-status running-status))
                       (if (midi:end-of-track-event-p (car (last prefix))) 0 4))
          collect (cons start end)
          do (setf start end))))

(defun data-offsets (smf &key (running-status t))
  "File offset of the data of each track chunk of SMF."
  (let ((pos 14))
    (loop for track in (midi:smf-tracks smf)
          collect (+ pos 8)
          do (incf pos (+ 8 (length (chunk-data track :running-status running-status)))))))

(defun u32-hex (n)
  (format nil "~{~2,'0X~^ ~}" (list (ldb (byte 8 24) n) (ldb (byte 8 16) n)
                                    (ldb (byte 8 8) n) (ldb (byte 8 0) n))))

(defun text-hex (string)
  (format nil "~{~2,'0X~^ ~}" (map 'list #'char-code string)))

(defun dump-track (bytes data-offset track)
  "Each event of TRACK: its file offset, delta, what it is and its bytes."
  (out "  offset  delta  event~%")
  (loop for event in track
        for (start . end) in (event-ranges track)
        do (out "  ~6d  ~5d  ~a~%" (+ data-offset start) (midi:midi-event-delta event)
                (describe-event event))
           (dolist (row (hex-rows bytes (+ data-offset start) (+ data-offset end)))
             (out "                 ~a~%" row))))

(defun list-track (track)
  (out "  delta   tick  event~%")
  (loop for (tick . event) in (midi:track-to-absolute track)
        do (out "  ~5d  ~5d  ~a~%" (midi:midi-event-delta event) tick (describe-event event))))

;;; 1. Build ---------------------------------------------------------------------

(defun demo-build ()
  (out "~a in C major, 4/4, two phrases of two bars, at 480 ticks per~%~
        quarter note. Each track is built as (tick . event) pairs at absolute ticks, and~%~
        TRACK-FROM-ABSOLUTE turns the list into delta times and appends the end of track~%~
        (section 4). Events are immutable structs made by MAKE-<TYPE> keyword constructors.~%~%"
          *title*)
  (let* ((smf (song))
         (tracks (midi:smf-tracks smf))
         (bytes (midi:write-smf smf nil)))
    (out "(make-smf :format 1 :division 480 :tracks (list tempo melody bass)):~%")
    (loop for track in tracks
          for i from 0
          for what in '("tempo, meter, key, title and markers"
                        "flute on channel 0: track name, program, volume, pan, one lyric per note"
                        "bass on channel 1: track name, program, volume, pan, root and fifth")
          do (out "  track ~d  ~2d events  ~a~%" i (length track) what))
    (check (mapcar #'length tracks) '(8 47 37) "event counts")
    (out "~%(write-smf smf nil) returns ~d bytes as a ~s.~%" (length bytes) (type-of bytes))
    (check (typep bytes '(simple-array (unsigned-byte 8) (*))) t "octet vector")
    (out "~%The header chunk, 14 bytes, big-endian:~%")
    (out "  offset  bytes         field~%")
    (loop for (start end label)
            in `((0 4 "\"MThd\"")
                 (4 8 "header length 6")
                 (8 10 ,(format nil "format ~d" (midi:smf-format smf)))
                 (10 12 ,(format nil "~d track chunks follow" (length tracks)))
                 (12 14 ,(format nil "division: ~d ticks per quarter note" (midi:smf-division smf))))
          do (out "  ~6d  ~12a  ~a~%" start (hex bytes start end) label))
    (check (hex bytes 0 14) "4D 54 68 64 00 00 00 06 00 01 00 03 01 E0" "header bytes")
    (let* ((tempo (first tracks))
           (data (chunk-data tempo))
           (ranges (event-ranges tempo)))
      (out "~%The first track chunk, the tempo track: \"MTrk\", its length, then each event as~%~
            a delta time (a variable-length quantity) and the event's bytes. Meta events are~%~
            FF <type> <length> <data>.~%")
      (out "  offset  bytes~%  ~6d  ~12a  \"MTrk\"~%  ~6d  ~12a  chunk length ~d~%"
           14 (hex bytes 14 18) 18 (hex bytes 18 22) (length data))
      (dump-track bytes 22 tempo)
      (check (hex bytes 14 18) "4D 54 72 6B" "MTrk")
      (check (hex bytes 18 22) (u32-hex (length data)) "chunk length field")
      (check (equalp (subseq bytes 22 (+ 22 (length data))) data) t "chunk data in the file")
      (check (loop for (start . end) in ranges collect (hex data start end))
             (list (format nil "00 FF 03 1D ~a" (text-hex *title*))
                   "00 FF 58 04 04 02 18 08"
                   "00 FF 59 02 00 00"
                   "00 FF 51 03 09 27 C0"
                   (format nil "00 FF 06 08 ~a" (text-hex "Phrase 1"))
                   "9E 00 FF 51 03 0B 71 B0"
                   (format nil "00 FF 06 08 ~a" (text-hex "Phrase 2"))
                   "9E 00 FF 2F 00")
             "tempo track event bytes")
      (out "~%The delta 3840 is 9E 00: 7 bits per byte, bit 7 set on all but the last~%~
            (30 * 128 + 0). 600000 us is 09 27 C0 and 750000 us is 0B 71 B0 (u24). The time~%~
            signature stores the denominator 4 as its base-2 logarithm, 02.~%"))))

;;; 2. Read ------------------------------------------------------------------------

(defun demo-read ()
  (let* ((smf (song))
         (bytes (midi:write-smf smf nil))
         (read (midi:read-smf bytes))
         (tracks (midi:smf-tracks read)))
    (out "(read-smf bytes) gives format ~d, division ~d, ~d tracks of ~{~d~^, ~} events.~%"
         (midi:smf-format read) (midi:smf-division read) (length tracks) (mapcar #'length tracks))
    (check (equalp read smf) t "read-smf gives the SMF back (EQUALP)")
    (check (equalp (midi:write-smf read nil) bytes) t "write-smf of it gives the same bytes")
    (out "It is EQUALP to the SMF that was written, and writing it again gives the same~%~
          ~d bytes: a byte-for-byte round trip.~%" (length bytes))
    (loop for track in tracks
          for i from 0
          for name = (midi:text-meta-event-text (find-if #'midi:track-name-event-p track))
          do (out "~%Track ~d, ~s (delta, absolute tick from TRACK-TO-ABSOLUTE, event):~%" i name)
             (list-track track))
    (let* ((note (find-if #'midi:note-on-event-p (second tracks)))
           (cl-user (find-package '#:cl-user))
           (text (let ((*print-pretty* nil) (*package* cl-user))
                   (prin1-to-string note)))
           (up (midi:copy-event note :key (+ 7 (midi:note-on-event-key note)) :channel 2)))
      (out "~%Events print readably, and what READ makes of the text is EQUALP to the event:~%  ~a~%"
           text)
      (check (equalp (let ((*read-eval* nil) (*package* cl-user)) (read-from-string text)) note) t
             "#S read back")
      (out "COPY-EVENT makes a changed copy (slots are read-only):~%  (copy-event note :key 67 :channel 2) -> ~a, delta ~d~%  the note itself is still ~a~%"
           (describe-event up) (midi:midi-event-delta up) (describe-event note))
      (check (list (midi:note-on-event-key note) (midi:note-on-event-key up)
                   (midi:channel-event-channel up) (midi:note-on-event-velocity up))
             '(60 67 2 80) "copy-event"))))

;;; 3. Running status ----------------------------------------------------------------

(defun repeated-status-count (track)
  "Channel events whose status byte (type and channel) is that of the event just
before them, itself a channel event: the ones running status writes without one."
  (loop for (previous event) on track
        while event
        count (and (midi:channel-event-p previous) (midi:channel-event-p event)
                   (eq (type-of previous) (type-of event))
                   (= (midi:channel-event-channel previous) (midi:channel-event-channel event)))))

(defun demo-running-status ()
  (let* ((smf (song))
         (tracks (midi:smf-tracks smf))
         (with (midi:write-smf smf nil))
         (without (midi:write-smf smf nil :running-status nil)))
    (out "With running status (the default, as mido writes) a channel event leaves out its~%~
          status byte when the event before it in the track is a channel event with the same~%~
          status. A meta, sysex or escape event in between resets it. :RUNNING-STATUS NIL~%~
          writes every status byte.~%~%")
    (out "  track    :running-status t   nil   saved   repeated statuses~%")
    (loop for track in tracks
          for i from 0
          for on = (length (chunk-data track))
          for off = (length (chunk-data track :running-status nil))
          do (out "  ~5d    ~17d ~5d ~7d ~11d~%" i on off (- off on) (repeated-status-count track))
             (check (- off on) (repeated-status-count track) (format nil "track ~d saving" i)))
    (out "  file     ~17d ~5d ~7d~%" (length with) (length without) (- (length without) (length with)))
    (check (- (length without) (length with)) (reduce #'+ (mapcar #'repeated-status-count tracks))
           "file saving")
    (out "~%The melody saves one byte (pan after volume): every note-on follows a lyric, a meta~%~
          event, and every note-off follows a note-on. The bass plays two notes at once, so~%~
          the second note-on and the second note-off of each pair go without a status byte.~%~
          Its first events, encoded both ways:~%~%")
    (let* ((bass (third tracks))
           (data-on (chunk-data bass))
           (data-off (chunk-data bass :running-status nil))
           (on (event-ranges bass))
           (off (event-ranges bass :running-status nil)))
      (out "  ~44a ~18a ~a~%" "event" "running status t" "nil")
      (loop for i from 2 to 9
            for (s1 . e1) = (nth i on)
            for (s2 . e2) = (nth i off)
            do (out "  ~44a ~18a ~a~%" (describe-event (nth i bass))
                    (hex data-on s1 e1) (hex data-off s2 e2)))
      (check (list (hex data-on (car (nth 5 on)) (cdr (nth 5 on)))
                   (hex data-off (car (nth 5 off)) (cdr (nth 5 off))))
             '("00 2B 46" "00 91 2B 46") "second note-on, with and without"))
    (let ((a (midi:read-smf with))
          (b (midi:read-smf without)))
      (out "~%Both files read back to the same events, and the one without running status,~%~
            written again with the default, gives the shorter file byte for byte.~%")
      (check (equalp a b) t "both read back equal")
      (check (equalp a smf) t "and equal to the SMF")
      (check (equalp (midi:write-smf b nil) with) t "rewritten with running status"))))

;;; 4. Absolute time ------------------------------------------------------------------

(defun events-at (tick timed)
  (loop for (tk . event) in timed when (= tk tick) collect event))

(defun timed-signature (timed)
  "TIMED events as sorted (tick . printed event with delta 0) pairs, END-OF-TRACK
events left out: equal for the same events at the same ticks in any order."
  (let ((*print-pretty* nil))
    (sort (loop for (tick . event) in timed
                unless (midi:end-of-track-event-p event)
                  collect (cons tick (prin1-to-string (midi:copy-event event :delta 0))))
          (lambda (a b) (or (< (car a) (car b))
                            (and (= (car a) (car b)) (string< (cdr a) (cdr b))))))))

(defun demo-time ()
  (let* ((smf (song))
         (tracks (midi:smf-tracks smf))
         (melody (second tracks))
         (timed (midi:track-to-absolute melody)))
    ;; Ticks from deltas and back.
    (out "TRACK-TO-ABSOLUTE pairs each event with its absolute tick, the sum of the deltas~%~
          so far. The first ten of the melody:~%")
    (loop for (tick . event) in (subseq timed 0 10)
          do (out "  (~4d . ~a)  delta ~d~%" tick (describe-event event) (midi:midi-event-delta event)))
    (check (loop for (tick . event) in timed when (midi:note-on-event-p event) collect tick)
           (loop with tick = 0
                 for (nil quarters) in *melody*
                 collect tick
                 do (incf tick (* quarters +ppq+)))
           "note-on ticks are the running sum of the note values")
    (check (equalp (midi:track-from-absolute timed) melody) t "track-from-absolute inverts it")
    (out "TRACK-FROM-ABSOLUTE turns them back into the same track (EQUALP). It sorts by tick~%~
          with a stable sort, so events at the same tick keep their input order. Given the~%~
          pairs in reverse, the events at tick 480 come out reversed too:~%")
    (let ((reversed (midi:track-to-absolute (midi:track-from-absolute (reverse timed)))))
      (out "  in order:   ~{~a~^~%              ~}~%" (mapcar #'describe-event (events-at 480 timed)))
      (out "  reversed:   ~{~a~^~%              ~}~%" (mapcar #'describe-event (events-at 480 reversed)))
      (out "The second C4 would be switched off at the tick it starts. Give note-offs before~%~
            the note-ons at the same tick, as the builders in section 1 do.~%")
      (check (mapcar #'type-of (events-at 480 reversed))
             '(midi:note-on-event midi:lyric-event midi:note-off-event) "reversed order at 480")
      (check (timed-signature reversed) (timed-signature timed) "same events at the same ticks"))
    ;; Type 1 to type 0.
    (let* ((merged (midi:merge-tracks tracks))
           (merged-timed (midi:track-to-absolute merged))
           (type-0 (midi:make-smf :format 0 :division +ppq+ :tracks (list merged)))
           (bytes-0 (midi:write-smf type-0 nil)))
      (out "~%MERGE-TRACKS puts every event of the three tracks into one track at its absolute~%~
            tick (ties keep track order, then order in the track) with one end of track: the~%~
            type-0 view of the piece. ~d + ~d + ~d events become ~d. The ~d events at tick 0:~%"
           (length (first tracks)) (length (second tracks)) (length (third tracks))
           (length merged) (length (events-at 0 merged-timed)))
      (dolist (event (events-at 0 merged-timed))
        (out "  ~a~%" (describe-event event)))
      (out "As a type-0 file it is ~d bytes (type 1: ~d). Running status saves less now that~%~
            the channels interleave.~%" (length bytes-0) (length (song-bytes)))
      (check (length merged) 90 "merged event count")
      (check (length (events-at 0 merged-timed)) 17 "events at tick 0")
      (check (midi:text-meta-event-text (first merged)) *title* "the tempo track comes first")
      (check (timed-signature merged-timed)
             (timed-signature (loop for track in tracks append (midi:track-to-absolute track)))
             "every event at its tick")
      (check (car (car (last merged-timed))) +end+ "merged end of track")
      (check (equalp (midi:read-smf bytes-0) type-0) t "type 0 round trip")
      (check (midi:tempo-map type-0) (midi:tempo-map smf) "same tempo map"))
    ;; The tempo map.
    (let ((map (midi:tempo-map smf)))
      (out "~%TEMPO-MAP of the SMF (all tracks merged): ~s~%" map)
      (loop for (tick . tempo) in map
            do (out "  from tick ~4d: ~d us per quarter, ~a bpm~%" tick tempo (bpm tempo)))
      (check map (list (cons 0 +tempo-1+) (cons +phrase-2+ +tempo-2+)) "tempo map")
      (out "TEMPO-MAP of the melody track alone, which has no tempo event: ~s~%~
            (+default-tempo+, ~d us, 120 bpm).~%" (midi:tempo-map melody) midi:+default-tempo+)
      (check (midi:tempo-map melody) (list (cons 0 midi:+default-tempo+)) "default tempo")
      ;; Seconds.
      (out "~%TICKS-TO-SECONDS gives exact rationals. When each syllable is sung:~%")
      (out "  syllable  tick   bpm   seconds  exact~%")
      (let ((syllables '()))
        (loop for (tick . event) in timed
              when (midi:lyric-event-p event)
                do (let ((seconds (midi:ticks-to-seconds tick +ppq+ map))
                         (text (midi:text-meta-event-text event)))
                     (push text syllables)
                     (out "  ~8a ~5d  ~4d  ~7a  ~a~%" text tick
                          (bpm (if (< tick +phrase-2+) +tempo-1+ +tempo-2+)) (decimal seconds) seconds)
                     (check seconds (expected-seconds tick) (format nil "seconds at ~d" tick))))
        (let ((end (midi:ticks-to-seconds +end+ +ppq+ map)))
          (out "  end      ~5d        ~7a  ~a~%" +end+ (decimal end) end)
          (check end 54/5 "end of the piece"))
        (out "The lyric line: ~s~%" (lyric-line (reverse syllables)))
        (check (lyric-line (reverse syllables)) "Twinkle twinkle little star, how I wonder what you are."
               "lyric line"))
      (out "Without a tempo map (120 bpm throughout) the end is at ~a s. With an SMPTE~%~
            division the tempo map is ignored: at 25 fps and 40 ticks per frame, tick 7680~%~
            is at ~a s; at 29.97 drop frame (29) it is ~a s = ~a.~%"
           (midi:ticks-to-seconds +end+ +ppq+)
           (midi:ticks-to-seconds +end+ (midi:make-smpte-division :frames-per-second 25 :ticks-per-frame 40) map)
           (decimal (midi:ticks-to-seconds +end+ (midi:make-smpte-division :frames-per-second 29 :ticks-per-frame 40)) 4)
           (midi:ticks-to-seconds +end+ (midi:make-smpte-division :frames-per-second 29 :ticks-per-frame 40)))
      (check (midi:ticks-to-seconds +end+ +ppq+) 8 "default tempo seconds")
      (check (midi:ticks-to-seconds +end+ (midi:make-smpte-division :frames-per-second 25 :ticks-per-frame 40) map)
             (/ 7680 (* 25 40)) "smpte 25")
      (check (midi:ticks-to-seconds +end+ (midi:make-smpte-division :frames-per-second 29 :ticks-per-frame 40))
             (/ 7680 (* 30000/1001 40)) "smpte 29.97"))
    ;; Type 2.
    (let* ((own-tempo (lambda (track tempo)
                        (midi:track-from-absolute
                         (cons (cons 0 (midi:make-tempo-event :tempo tempo))
                               (midi:track-to-absolute track)))))
           (up-a-fourth (mapcar (lambda (event)
                                  (typecase event
                                    (midi:note-on-event
                                     (midi:copy-event event :key (+ 5 (midi:note-on-event-key event))))
                                    (midi:note-off-event
                                     (midi:copy-event event :key (+ 5 (midi:note-off-event-key event))))
                                    (t event)))
                                melody))
           (a (funcall own-tempo melody +tempo-1+))
           (b (funcall own-tempo up-a-fourth 400000))
           (type-2 (midi:make-smf :format 2 :division +ppq+ :tracks (list a b))))
      (out "~%Type 2 holds independent sequences, each with its own tempo. Here: the tune at~%~
            100 bpm, and a copy up a fourth (COPY-EVENT :KEY) at 150 bpm. Give TEMPO-MAP one~%~
            track at a time:~%")
      (loop for track in (list a b)
            for label in '("pattern 1" "pattern 2")
            for map = (midi:tempo-map track)
            do (out "  ~a  first note ~a  tempo map ~s  ends at ~a s~%" label
                    (note-name (midi:note-on-event-key (find-if #'midi:note-on-event-p track)))
                    map (midi:ticks-to-seconds +end+ +ppq+ map)))
      (out "Given the type-2 SMF, TEMPO-MAP merges the tracks and the last tempo at tick 0~%~
            wins: ~s.~%" (midi:tempo-map type-2))
      (check (list (midi:tempo-map a) (midi:tempo-map b))
             (list (list (cons 0 +tempo-1+)) (list (cons 0 400000))) "type 2 tempo maps")
      (check (list (midi:ticks-to-seconds +end+ +ppq+ (midi:tempo-map a))
                   (midi:ticks-to-seconds +end+ +ppq+ (midi:tempo-map b)))
             '(48/5 32/5) "type 2 lengths")
      (check (midi:tempo-map type-2) (list (cons 0 400000)) "merged type 2 map")
      (check (equalp (midi:read-smf (midi:write-smf type-2 nil)) type-2) t "type 2 round trip"))))

;;; 5. Parse errors -------------------------------------------------------------------

(defparameter +restart-names+ (list 'midi:skip-event 'use-value 'midi:skip-chunk 'midi:use-partial-smf))

(defun midi-restarts (condition)
  "Names of the READ-SMF restarts around CONDITION, innermost first."
  (loop for restart in (compute-restarts condition)
        for name = (restart-name restart)
        when (member name +restart-names+)
          collect (string-downcase name)))

(defun read-with (bytes &optional action)
  "READ-SMF BYTES. Each MIDI-PARSE-ERROR is recorded with the restarts around
it, then ACTION (a function of the condition) runs. ACTION may invoke a
restart; if it returns, the error unwinds and the SMF is NIL. Warnings are
recorded and muffled. Returns (values smf errors warnings); each error is
(condition restart-names)."
  (let ((errors '()) (warnings '()))
    (let ((smf (handler-case
                   (handler-bind ((midi:midi-parse-error
                                    (lambda (c)
                                      (push (list c (midi-restarts c)) errors)
                                      (when action (funcall action c))))
                                  (midi:midi-format-warning
                                    (lambda (w)
                                      (push w warnings)
                                      (muffle-warning w))))
                     (midi:read-smf bytes))
                 (midi:midi-parse-error () nil))))
      (values smf (nreverse errors) (nreverse warnings)))))

(defun show-parse-error (error)
  (destructuring-bind (c restarts) error
    (out "  ~a~%    offset ~d, track ~a, reason ~s~%    restarts: ~:[none~;~:*~{~a~^, ~}~]~%"
         c (midi:midi-parse-error-offset c) (midi:midi-parse-error-track c)
         (midi:midi-error-reason c) restarts)))

(defun patched (bytes &rest offsets-and-values)
  "A copy of BYTES with the byte at each OFFSET set to its VALUE."
  (let ((copy (copy-seq bytes)))
    (loop for (offset value) on offsets-and-values by #'cddr
          do (setf (aref copy offset) value))
    copy))

(defun prefix-of-p (part whole)
  (and (<= (length part) (length whole)) (equalp part (subseq whole 0 (length part)))))

(defun demo-parse-errors ()
  (let* ((smf (song))
         (tracks (midi:smf-tracks smf))
         (bytes (midi:write-smf smf nil))
         (n (length bytes))
         (offsets (data-offsets smf)))
    ;; Truncation.
    (out "A file cut 20 bytes short, inside the bass track:~%")
    (let ((cut (subseq bytes 0 (- n 20))))
      (multiple-value-bind (result errors) (read-with cut)
        (show-parse-error (first errors))
        (check (list result (midi:midi-parse-error-offset (first (first errors)))
                     (midi:midi-parse-error-track (first (first errors))) (second (first errors)))
               (list nil (- n 20) 2 '("skip-chunk" "use-partial-smf")) "truncated: offset, track, restarts"))
      (let ((partial (read-with cut #'midi:use-partial-smf)))
        (out "USE-PARTIAL-SMF returns what was read: ~d tracks of ~{~d~^, ~} events. The bass track~%~
              stops at tick ~d, after~%  ~a~%and no end of track is added.~%"
             (length (midi:smf-tracks partial)) (mapcar #'length (midi:smf-tracks partial))
             (car (car (last (midi:track-to-absolute (third (midi:smf-tracks partial))))))
             (describe-event (car (last (third (midi:smf-tracks partial))))))
        (check (equalp (subseq (midi:smf-tracks partial) 0 2) (subseq tracks 0 2)) t "whole tracks kept")
        (check (prefix-of-p (third (midi:smf-tracks partial)) (third tracks)) t "bass is a prefix"))
      (let ((skipped (read-with cut #'midi:skip-chunk)))
        (out "SKIP-CHUNK drops the bass track instead. It still counts toward the 3 tracks the~%~
              header announced, so the read ends there: ~d tracks.~%" (length (midi:smf-tracks skipped)))
        (check (equalp (midi:smf-tracks skipped) (subseq tracks 0 2)) t "skip-chunk")))
    ;; Every prefix.
    (let ((at-own-length 0) (partial-prefixes 0))
      (dotimes (len n)
        (let ((prefix (subseq bytes 0 len)))
          (multiple-value-bind (result errors) (read-with prefix)
            (when (and (null result) errors
                       (eql (midi:midi-parse-error-offset (first (first errors))) len))
              (incf at-own-length)))
          (when (>= len 14)
            (let ((partial (read-with prefix #'midi:use-partial-smf)))
              (when (and partial
                         (<= (length (midi:smf-tracks partial)) 3)
                         (every #'prefix-of-p (midi:smf-tracks partial) tracks))
                (incf partial-prefixes))))))
      (out "~%Every one of the ~d proper prefixes of the file (lengths 0 to ~d): ~d signal~%~
            MIDI-PARSE-ERROR at their own length. For the ~d that hold the whole header,~%~
            USE-PARTIAL-SMF gives whole tracks and a prefix of the cut one in ~d.~%"
           n (1- n) at-own-length (- n 14) partial-prefixes)
      (check at-own-length n "prefixes fail at their length")
      (check partial-prefixes (- n 14) "partial results are prefixes"))
    ;; Header errors.
    (out "~%Errors in the header chunk have no restarts:~%")
    (loop for (label expected-offset . patches)
            in '(("cut after 10 bytes" 10)
                 ("\"RThd\" instead of \"MThd\"" 0 0 #x52)
                 ("header length 5" 4 7 5)
                 ("format 3" 8 9 3)
                 ("division 0" 12 12 0 13 0)
                 ("SMPTE rate -26 (E6)" 12 12 #xE6))
          for bad = (if patches (apply #'patched bytes patches) (subseq bytes 0 10))
          do (multiple-value-bind (result errors) (read-with bad)
               (destructuring-bind (c restarts) (first errors)
                 (out "  ~26a byte ~2d  ~a  restarts: ~:[none~;~:*~{~a~^, ~}~]~%" label
                      (midi:midi-parse-error-offset c) (midi:midi-error-reason c) restarts)
                 (check (list result (midi:midi-parse-error-offset c) (midi:midi-parse-error-track c) restarts)
                        (list nil expected-offset nil nil) label))))
    (let (answer)
      (read-with (subseq bytes 0 10) (lambda (c) (setf answer (list (midi:use-partial-smf c)))))
      (out "Called there, the function USE-PARTIAL-SMF finds no restart and returns ~s.~%" (first answer))
      (check answer '(nil) "use-partial-smf without a restart"))
    ;; Corrupted bytes in a track.
    (let* ((melody (second tracks))
           (ranges (event-ranges melody))
           (lit (nth 4 (loop for event in melody for i from 0
                             when (midi:note-on-event-p event) collect i)))
           (status (+ (second offsets) (car (nth lit ranges)) 1))
           (bass-program (+ (third offsets) (car (nth 1 (event-ranges (third tracks)))) 1)))
      (out "~%Corrupted bytes inside track chunks. The note-on of \"lit-\" is ~a at byte ~d;~%~
            the bass program change starts with ~a at byte ~d.~%"
           (hex bytes status (+ status 3)) status (hex bytes bass-program (+ bass-program 2)) bass-program)
      (check (list (aref bytes status) (aref bytes bass-program)) '(#x90 #xC1) "status bytes found")
      (loop for (label bad expected-offset expected-track)
              in (list (list "status 90 -> F4 (system common)" (patched bytes status #xF4) status 1)
                       (list "key 45 -> 90 (a status byte)" (patched bytes (1+ status) #x90) (1+ status) 1)
                       (list "program change C1 -> 20" (patched bytes bass-program #x20) bass-program 2))
            do (multiple-value-bind (result errors) (read-with bad)
                 (out "~a:~%" label)
                 (show-parse-error (first errors))
                 (check (list result (midi:midi-parse-error-offset (first (first errors)))
                              (midi:midi-parse-error-track (first (first errors))))
                        (list nil expected-offset expected-track) label)))
      (let* ((bad (patched bytes status #xF4))
             (skipped (read-with bad #'midi:skip-chunk))
             (partial (read-with bad #'midi:use-partial-smf)))
        (out "With F4: SKIP-CHUNK reads on at the chunk's declared end and returns ~d tracks~%~
              (tempo and bass); USE-PARTIAL-SMF returns the tempo track and the first ~d~%~
              melody events, the ones before the bad note-on.~%"
             (length (midi:smf-tracks skipped)) (length (second (midi:smf-tracks partial))))
        (check (equalp (midi:smf-tracks skipped) (list (first tracks) (third tracks))) t "skip-chunk F4")
        (check (equalp (midi:smf-tracks partial) (list (first tracks) (subseq melody 0 lit))) t
               "use-partial-smf F4")))
    ;; A bad meta event.
    (let* ((tempo-track (first tracks))
           (start (+ (first offsets) (car (nth 5 (event-ranges tempo-track)))))
           (ff (position #xFF bytes :start start))
           (bad (patched bytes (1+ ff) #x59)))
      (out "~%A bad meta event. The tempo change at bar 3, at byte ~d, is ~a.~%~
            Its type byte 51 becomes 59, a key signature, which must have 2 data bytes, not 3:~%"
           start (hex bytes start (+ ff 6)))
      (check (hex bytes ff (+ ff 3)) "FF 51 03" "tempo change found")
      (multiple-value-bind (result errors) (read-with bad)
        (show-parse-error (first errors))
        (check (list result (midi:midi-parse-error-offset (first (first errors)))
                     (midi:midi-parse-error-track (first (first errors))) (second (first errors)))
               (list nil ff 0 '("skip-event" "use-value" "skip-chunk" "use-partial-smf"))
               "bad meta: offset at FF, restarts"))
      (let* ((skipped (read-with bad #'midi:skip-event))
             (track (first (midi:smf-tracks skipped)))
             (marker (find-if (lambda (e) (and (midi:marker-event-p e)
                                               (string= "Phrase 2" (midi:text-meta-event-text e))))
                              track))
             (map (midi:tempo-map skipped)))
        (out "SKIP-EVENT drops it and adds its delta (3840) to the next event, so the marker~%~
              \"Phrase 2\" keeps its tick ~d (its delta is now ~d). The tempo map loses the change,~%~
              ~s, and the piece ends at ~a s instead of ~a s.~%"
             (car (rassoc marker (midi:track-to-absolute track))) (midi:midi-event-delta marker) map
             (midi:ticks-to-seconds +end+ +ppq+ map) (midi:ticks-to-seconds +end+ +ppq+ (midi:tempo-map smf)))
        (check (list (length track) (car (rassoc marker (midi:track-to-absolute track))) map)
               (list 7 +phrase-2+ (list (cons 0 +tempo-1+))) "skip-event")
        (check (equalp (rest (midi:smf-tracks skipped)) (rest tracks)) t "other tracks untouched"))
      (let ((fixed (read-with bad (lambda (c) (use-value (midi:make-tempo-event :tempo +tempo-2+) c)))))
        (out "USE-VALUE with (make-tempo-event :tempo 750000) puts a tempo change in its place,~%~
              with the bad event's delta: the result is EQUALP to the original SMF: ~a.~%"
             (equalp fixed smf))
        (check (equalp fixed smf) t "use-value repairs it")))
    ;; A warning.
    (let* ((bass-data (length (chunk-data (third tracks))))
           (length-field (- (third offsets) 4))
           (new-length (- bass-data 4))
           (bad (apply #'patched (subseq bytes 0 (- n 4))
                       (loop for i from 0 below 4
                             append (list (+ length-field i) (ldb (byte 8 (* 8 (- 3 i))) new-length))))))
      (out "~%A last chunk without its end of track: the final 00 FF 2F 00 removed and the~%~
            chunk length set to ~d. That loses no events, so READ-SMF warns and goes on:~%" new-length)
      (multiple-value-bind (result errors warnings) (read-with bad)
        (out "  ~a  (~s)~%" (first warnings) (type-of (first warnings)))
        (out "The bass track has ~d events, no end of track added. WRITE-SMF appends one, as~%~
              mido does, so writing it gives the original file again: ~a.~%"
             (length (third (midi:smf-tracks result)))
             (equalp (midi:write-smf result nil) bytes))
        (check (list (length warnings) (midi:midi-format-warning-offset (first warnings)) errors
                     (length (third (midi:smf-tracks result))))
               (list 1 (- n 4) nil 36) "format warning")
        (check (equalp (midi:write-smf result nil) bytes) t "rewritten with the end of track")))))

;;; 6. Encode errors -------------------------------------------------------------------

(defparameter *out-of-range-key* 128 "A key number one past the MIDI range.")
(defparameter *out-of-range-delta* #x10000000 "A delta one past the 4-byte VLQ range.")

(defun encode-error (smf)
  (handler-case (progn (midi:write-smf smf nil) nil)
    (midi:midi-encode-error (c) c)))

(defun with-track (smf index function)
  "SMF with track INDEX replaced by (FUNCALL FUNCTION track)."
  (midi:make-smf :format (midi:smf-format smf) :division (midi:smf-division smf)
                 :tracks (loop for track in (midi:smf-tracks smf)
                               for i from 0
                               collect (if (= i index) (funcall function track) track))))

(defun insert-after (track position element)
  (append (subseq track 0 (1+ position)) (list element) (nthcdr (1+ position) track)))

(defun demo-encode-errors ()
  (out "At SBCL's default safety the constructors check the slot types themselves:~%")
  (dolist (case (list (list "(make-note-on-event :key 128)"
                            (lambda () (midi:make-note-on-event :key *out-of-range-key*)))
                      (list "(make-note-on-event :delta #x10000000)"
                            (lambda () (midi:make-note-on-event :delta *out-of-range-delta*)))))
    (let ((c (handler-case (progn (funcall (second case)) nil) (type-error (c) c))))
      (out "  ~40a -> ~s~%" (first case) (type-of c))
      (check (typep c 'type-error) t (first case))))
  (out "~%Values a slot type lets through are checked by WRITE-SMF, which checks the whole~%~
        SMF before it writes a byte and signals MIDI-ENCODE-ERROR with the track and the~%~
        element at fault:~%")
  (let* ((smf (song))
         (sysex (midi:make-sysex-event :data (vector #xF0 #x7E #x7F #x09 300 #xF7)))
         (lyric (midi:make-lyric-event :text (string (code-char #x266A))))
         (meter (midi:make-time-signature-event :numerator 4 :denominator 6))
         (eot (midi:make-end-of-track-event))
         (timed (first (midi:track-to-absolute (second (midi:smf-tracks smf)))))
         (unknown (midi:make-unknown-meta-event :type #x51 :data (vector #x07 #xA1 #x20)))
         (cases
           (list (list "a sysex data byte 300" (with-track smf 1 (lambda (tr) (insert-after tr 1 sysex))) 1 sysex)
                 (list "a lyric of U+266A (code above 255)" (with-track smf 1 (lambda (tr) (insert-after tr 3 lyric))) 1 lyric)
                 (list "a 4/6 time signature" (with-track smf 0 (lambda (tr) (insert-after tr 0 meter))) 0 meter)
                 (list "an end of track mid-track" (with-track smf 2 (lambda (tr) (insert-after tr 3 eot))) 2 eot)
                 (list "a (tick . event) pair as an element" (with-track smf 1 (lambda (tr) (cons timed (rest tr)))) 1 timed)
                 (list "an unknown meta event of type 51" (with-track smf 0 (lambda (tr) (insert-after tr 0 unknown))) 0 unknown)
                 (list "format 0 with three tracks"
                       (midi:make-smf :format 0 :division +ppq+ :tracks (midi:smf-tracks smf)) nil nil))))
    (loop for (label bad track element) in cases
          for c = (encode-error bad)
          do (out "  ~36a track ~3a ~22a ~a~%" label (or (midi:midi-encode-error-track c) "-")
                  (if (midi:midi-encode-error-event c)
                      (string-downcase (type-of (midi:midi-encode-error-event c)))
                      "-")
                  (midi:midi-error-reason c))
             (check (list (typep c 'midi:midi-encode-error) (midi:midi-encode-error-track c)
                          (eq (midi:midi-encode-error-event c) element))
                    (list t track t) label))
    (let ((c (encode-error (second (first cases)))))
      (out "The report of the first: ~a~%" (let ((*print-pretty* nil)) (princ-to-string c))))
    (let ((path (merge-pathnames "harness-demos-midi-never-written.mid" (uiop:temporary-directory))))
      (when (probe-file path) (delete-file path))
      (handler-case (midi:write-smf (second (first cases)) path)
        (midi:midi-encode-error () nil))
      (out "Writing that SMF to a file signals before the file is opened: it does not exist~%~
            afterwards (probe-file -> ~s).~%" (probe-file path))
      (check (probe-file path) nil "no file written"))))

;;; 7. Files and streams ----------------------------------------------------------------

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun song-path ()
  (asdf:system-relative-pathname "harness-demos-midi" "twinkle.mid"))

(defun demo-files ()
  (let* ((smf (song))
         (path (song-path))
         (committed (and (probe-file path) (file-octets path))))
    (midi:write-smf smf path)
    (let ((written (file-octets path)))
      (out "(write-smf smf path) wrote harness/midi/twinkle.mid, ~d bytes. (read-smf path)~%~
            reads it back: ~a.~%" (length written) (equalp (midi:read-smf path) smf))
      (check (equalp (midi:read-smf path) smf) t "read-smf of a pathname")
      (check (equalp written (midi:write-smf smf nil)) t "the file holds the bytes")
      (if committed
          (progn
            (out "It is byte-identical to the copy committed with this demo: ~a.~%" (equalp committed written))
            (check (equalp committed written) t "same as the committed file"))
          (out "There was no committed copy to compare it with.~%"))))
  (let* ((smf (song))
         (type-0 (midi:make-smf :format 0 :division +ppq+ :tracks (list (midi:merge-tracks (midi:smf-tracks smf)))))
         (size-0 (length (midi:write-smf type-0 nil)))
         (size-1 (length (midi:write-smf smf nil)))
         (path (merge-pathnames "harness-demos-midi-stream.mid" (uiop:temporary-directory))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (midi:write-smf type-0 s)
      (midi:write-smf smf s))
    (unwind-protect
         (with-open-file (s path :element-type '(unsigned-byte 8))
           (let* ((a (midi:read-smf s))
                  (after-a (file-position s))
                  (b (midi:read-smf s))
                  (after-b (file-position s)))
             (out "~%Streams: the type-0 and the type-1 file written one after the other to one~%~
                   octet stream (~d + ~d bytes). READ-SMF on a stream stops after the last track~%~
                   chunk it needs, so two calls read them back: format ~d at position ~d, then~%~
                   format ~d at position ~d, and then the stream is at its end (~s).~%"
                  size-0 size-1 (midi:smf-format a) after-a (midi:smf-format b) after-b
                  (read-byte s nil :eof))
             (check (list (equalp a type-0) after-a (equalp b smf) after-b)
                    (list t size-0 t (+ size-0 size-1)) "two SMFs on one stream")))
      (delete-file path))))

;;; 8. mido ------------------------------------------------------------------------

(defparameter *mido-script*
  "import json, sys, importlib.metadata, mido; f = mido.MidiFile(sys.argv[1]); print('(:version', json.dumps(importlib.metadata.version('mido')), ':type', f.type, ':ticks-per-beat', f.ticks_per_beat, ':names (', ' '.join(json.dumps(t.name) for t in f.tracks), ') :messages (', ' '.join(str(len(t)) for t in f.tracks), ') :note-ons', sum(1 for t in f.tracks for m in t if m.type == 'note_on'), ':lyrics (', ' '.join(json.dumps(m.text) for t in f.tracks for m in t if m.type == 'lyrics'), ') :seconds', repr(f.length), ')')"
  "Python for `python -I -c`: mido's own view of the file named by argv[1], printed
as a Lisp plist.")

(defun uv-available-p ()
  (handler-case (zerop (proc:run '("uv" "--version") :output nil :error-output nil :timeout 60))
    (error () nil)))

(defun demo-mido ()
  (let ((path (song-path))
        (smf (song)))
    (unless (probe-file path)
      (midi:write-smf smf path))
    (if (not (uv-available-p))
        (out "uv is not on PATH, so the cross-read with mido is skipped (midi-protocol's own~%~
              tests skip it the same way).~%")
        (multiple-value-bind (code stdout stderr)
            (proc:run (list "uv" "run" "--no-project" "--with" "mido==1.3.3" "python" "-I" "-c"
                            *mido-script* (namestring path))
                      :output :string :error-output :string :timeout 600
                      :env '(("PYTHONIOENCODING" . "utf-8")))
          (out "uv run --no-project --with mido==1.3.3 python -I -c '<script>' twinkle.mid~%~
                reads the file with mido and prints what mido sees (exit ~d).~%~%" code)
          (check code 0 "uv run exit code")
          (if (/= code 0)
              (out "~a~%" stderr)
              (let* ((mido (let ((*read-eval* nil)
                                 (*read-default-float-format* 'double-float)
                                 (*package* (find-package '#:keyword)))
                             (read-from-string stdout)))
                     (tracks (midi:smf-tracks smf))
                     (map (midi:tempo-map smf))
                     (seconds (midi:ticks-to-seconds +end+ +ppq+ map))
                     (ours (list :type (midi:smf-format smf)
                                 :ticks-per-beat (midi:smf-division smf)
                                 :names (loop for track in tracks
                                              collect (midi:text-meta-event-text
                                                       (find-if #'midi:track-name-event-p track)))
                                 :messages (mapcar #'length tracks)
                                 :note-ons (loop for track in tracks sum (count-if #'midi:note-on-event-p track))
                                 :lyrics (loop for track in tracks
                                               append (mapcar #'midi:text-meta-event-text
                                                              (remove-if-not #'midi:lyric-event-p track))))))
                (flet ((row (label ours-text mido-text)
                         (out "  ~16a ~14a ~a~%  ~16a ~14a ~a~%" label "midi-protocol" ours-text
                              "" (format nil "mido ~a" (getf mido :version)) mido-text)))
                  (loop for key in '(:type :ticks-per-beat :names :messages :note-ons)
                        do (let ((*print-pretty* nil))
                             (row (string-downcase key) (prin1-to-string (getf ours key))
                                  (prin1-to-string (getf mido key))))
                           (check (getf mido key) (getf ours key) (format nil "mido ~(~a~)" key)))
                  (row "lyric line" (lyric-line (getf ours :lyrics)) (lyric-line (getf mido :lyrics)))
                  (check (getf mido :lyrics) (getf ours :lyrics) "mido lyrics")
                  (row "length (s)" (format nil "~a = ~a" seconds (decimal seconds 6))
                       (let ((*read-default-float-format* 'double-float))
                         (princ-to-string (getf mido :seconds)))))
                (check (< (abs (- (getf mido :seconds) seconds)) 1d-9) t "mido length")
                (out "~%mido counts every event, the end of track included, as a message, and it agrees~%~
                      on every count, name and syllable. Its length (playback time through the tempo~%~
                      map, in floats) matches TICKS-TO-SECONDS.~%")))))))

;;; Entry point --------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 110))
    (format t "~&midi-protocol ~a (~a ~a): MIDI 1.0 events and Standard MIDI Files,~%~
               files only~%"
            (asdf:component-version (asdf:find-system "midi-protocol"))
            (lisp-implementation-type) (lisp-implementation-version))
    (section "1. Build a type-1 SMF in code and write it to bytes" (demo-build))
    (section "2. Read it back: the event listing and the round trip" (demo-read))
    (section "3. Running status" (demo-running-status))
    (section "4. Absolute time: ticks, merging, the tempo map, seconds" (demo-time))
    (section "5. Bad bytes: MIDI-PARSE-ERROR, its restarts, MIDI-FORMAT-WARNING" (demo-parse-errors))
    (section "6. Bad values: MIDI-ENCODE-ERROR" (demo-encode-errors))
    (section "7. Files and streams" (demo-files))
    (section "8. Cross-read with mido" (demo-mido))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
