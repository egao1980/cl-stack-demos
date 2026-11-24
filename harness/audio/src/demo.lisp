;;;; A tour of audio-protocol 0.1.0 (slice AU0, batch audio): PCM buffers built
;;;; in code, MIX with per-voice gain and pan, 16-bit WAV written and read back,
;;;; RESAMPLE from 44.1 kHz to 16 kHz, a hand-built Standard MIDI File
;;;; (midi-protocol 0.1.0) rendered by REFERENCE-ENGINE to a mix and to stems,
;;;; and the conditions and restarts of the WAV codec. Every result with a known
;;;; answer is checked: RUN-DEMOS returns NIL when one differs or a section
;;;; stops on an error.

(in-package #:harness-demos-audio)

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

(defun check-true (value label)
  (check (and value t) t label))

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

(defun seconds-since (start)
  (/ (float (- (get-internal-real-time) start) 1d0) internal-time-units-per-second))

(defmacro timed (&body body)
  "(values seconds . BODY's values)."
  (let ((start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (let ((values (multiple-value-list (progn ,@body))))
         (values-list (cons (seconds-since ,start) values))))))

;;; Helpers ----------------------------------------------------------------------

(defun version-of (system)
  (asdf:component-version (asdf:find-system system)))

(defun sha256-hex (octets)
  "SHA-256 of OCTETS through crypto-protocol, as lower-case Base16 through
encoding-protocol."
  (string-downcase (stack-encoding:encode (crypto:digest octets :algorithm :sha256)
                                          :encoding :base16)))

(defun peak (buffer &optional channel)
  "Largest |sample| of BUFFER, of one CHANNEL when given."
  (let ((samples (if channel
                     (audio:pcm-channel buffer channel)
                     (audio:pcm-buffer-samples buffer))))
    (reduce #'max samples :key #'abs :initial-value 0f0)))

(defun db (ratio)
  "20 log10 RATIO, or -infinity printed as a string for 0."
  (if (zerop ratio) "-inf" (format nil "~,2f" (* 20 (log (float ratio 1d0) 10)))))

(defun max-difference (a b)
  "Largest |a_i - b_i| over two sample vectors of one length."
  (loop for x across a for y across b maximize (abs (- x y))))

(defun last-sounding-second (buffer)
  "Time in seconds just after the last frame of BUFFER with a non-zero sample."
  (let* ((samples (audio:pcm-buffer-samples buffer))
         (channels (audio:pcm-buffer-channels buffer))
         (last (position-if-not #'zerop samples :from-end t)))
    (if last
        (/ (1+ (floor last channels)) (audio:pcm-buffer-sample-rate buffer))
        0)))

(defun chars (octets-or-string)
  "A 4-byte chunk id as a string (binary-protocol's 4s gives octets)."
  (if (stringp octets-or-string)
      octets-or-string
      (map 'string #'code-char octets-or-string)))

(defun pcm-header (octets)
  "The 44-byte PCM header WRITE-WAV writes, as a plist, unpacked with binary-protocol."
  (destructuring-bind (riff riff-size wave fmt fmt-size tag channels rate byte-rate
                       block-align bits data data-size)
      (bin:unpack "<4sI4s4sIHHIIHH4sI" (subseq octets 0 44))
    (list :riff (chars riff) :riff-size riff-size :wave (chars wave)
          :fmt (chars fmt) :fmt-size fmt-size :format-tag tag :channels channels
          :sample-rate rate :bytes-per-second byte-rate :block-align block-align
          :bits bits :data (chars data) :data-size data-size)))

(defun pcm16-codes (octets)
  "The signed 16-bit codes of a 44-byte-header PCM16 file's data chunk."
  (let ((n (floor (- (length octets) 44) 2)))
    (loop for i below n
          collect (let ((u (logior (aref octets (+ 44 (* 2 i)))
                                   (ash (aref octets (+ 45 (* 2 i))) 8))))
                    (if (>= u 32768) (- u 65536) u)))))

(defvar *tmp* nil "A fresh temporary directory for the WAV files of this run.")

(defun tmp-file (name)
  (merge-pathnames name *tmp*))

(defun shown (pathname)
  "PATHNAME with the run's temporary directory written <tmp>."
  (let ((name (namestring pathname))
        (tmp (namestring *tmp*)))
    (if (and (>= (length name) (length tmp)) (string= tmp name :end2 (length tmp)))
        (concatenate 'string "<tmp>/" (subseq name (length tmp)))
        name)))

(defvar *files* '() "(shown-name bytes sha256) of every WAV file written, in order.")

(defun note-file (pathname)
  (let ((octets (with-open-file (in pathname :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                    (read-sequence v in)
                    v))))
    (setf *files* (append *files* (list (list (shown pathname) (length octets)
                                              (sha256-hex octets)))))
    octets))

;;; 1. The triad ---------------------------------------------------------------

(defconstant +rate+ 44100 "Sample rate of the triad.")
(defparameter *triad-seconds* 3/2 "Length of the triad.")
(defparameter *voice-amplitude* 1/2 "Peak of each voice before the mix.")
(defparameter *fade-seconds* 1/50 "Linear fade in and out of each voice: 20 ms.")

(defparameter *voices*
  '(("C4" 60 -3 -0.6) ("E4" 64 -6 0) ("G4" 67 -6 0.6))
  "(name MIDI-key gain-dB pan) of each voice of the C-major triad.")

(defun sine-voice (key seconds)
  "A fresh mono PCM-BUFFER: a sine at (note-frequency KEY), *VOICE-AMPLITUDE*,
SECONDS long at +RATE+, with a linear fade in and out of *FADE-SECONDS*."
  (let* ((f (audio:note-frequency key))
         (frames (audio:seconds-to-frame seconds +rate+))
         (fade (audio:seconds-to-frame *fade-seconds* +rate+))
         (samples (make-array frames :element-type 'single-float)))
    (dotimes (n frames)
      (let ((envelope (min 1d0 (/ n fade) (/ (- frames 1 n) fade))))
        (setf (aref samples n)
              (coerce (* *voice-amplitude* envelope (sin (/ (* 2 pi f n) +rate+)))
                      'single-float))))
    (audio:make-pcm-buffer :sample-rate +rate+ :channels 1 :samples samples)))

(defvar *triad-voices* nil "The three mono voice buffers of section 1.")
(defvar *triad-mix* nil "The stereo mix of section 2.")
(defvar *triad-bytes* nil "The mix as a 16-bit WAV, section 3.")
(defvar *triad-read* nil "The buffer READ-WAV gave back from *TRIAD-BYTES*.")

(defun demo-voices ()
  (out "Three voices of a C-major triad, each a mono sine buffer made in code:~%~
        ~,1f s at ~d Hz, amplitude ~a, a ~d ms linear fade in and out.~%~%"
       (float *triad-seconds*) +rate+ *voice-amplitude* (* 1000 *fade-seconds*))
  (out "  voice  key  frequency (Hz)   frames  duration  peak~%")
  (setf *triad-voices*
        (loop for (name key) in *voices*
              collect (let ((buffer (sine-voice key *triad-seconds*)))
                        (out "  ~5a  ~3d  ~14,4f  ~7d  ~8a  ~,5f~%"
                             name key (audio:note-frequency key)
                             (audio:pcm-buffer-frames buffer)
                             (audio:pcm-buffer-duration buffer) (peak buffer))
                        (check (audio:pcm-buffer-frames buffer) 66150 (format nil "~a frames" name))
                        (check (audio:pcm-buffer-duration buffer) 3/2 (format nil "~a duration" name))
                        (check-true (< (abs (- (peak buffer) 0.5)) 1e-4) (format nil "~a peak" name))
                        buffer)))
  (check (audio:note-frequency 69) 440d0 "A4 is exactly 440 Hz")
  (out "~%(note-frequency 69) = ~a: A4 is exact. (seconds-to-frame 3/2 44100) = ~d.~%"
       (audio:note-frequency 69) (audio:seconds-to-frame 3/2 +rate+)))

;;; 2. The mix -------------------------------------------------------------------

(defun expected-mix (voices gains pans)
  "MIX's file-header formula worked out here for mono inputs into stereo at
master 0 dB: y[n][c] = sum_i (G_i * A_i[c]) * x_i[n] in DOUBLE-FLOAT, from 0,
rounded once to SINGLE-FLOAT."
  (let* ((frames (reduce #'max voices :key #'audio:pcm-buffer-frames))
         (y (make-array (* 2 frames) :element-type 'single-float)))
    (dotimes (n frames)
      (dotimes (c 2)
        (let ((acc 0d0))
          (loop for buffer in voices
                for gain in gains
                for pan in pans
                do (multiple-value-bind (left right) (audio:pan-gains pan 1)
                     (when (< n (audio:pcm-buffer-frames buffer))
                       (incf acc (* (* (audio:db-to-gain gain) (if (zerop c) left right))
                                    (aref (audio:pcm-buffer-samples buffer) n))))))
          (setf (aref y (+ (* 2 n) c)) (coerce (* 1d0 acc) 'single-float)))))
    y))

(defun demo-mix ()
  (let ((gains (mapcar #'third *voices*))
        (pans (mapcar #'fourth *voices*)))
    (out "PAN-GAINS for a mono input is the constant-power sine law:~%~%")
    (out "  voice  gain dB  linear   pan   left     right    left^2+right^2~%")
    (loop for (name nil gain pan) in *voices*
          do (multiple-value-bind (left right) (audio:pan-gains pan 1)
               (out "  ~5a  ~7d  ~6,4f  ~4@a  ~7,5f  ~7,5f  ~,12f~%"
                    name gain (audio:db-to-gain gain) pan left right (+ (* left left) (* right right)))
               (check-true (< (abs (- 1 (+ (* left left) (* right right)))) 1d-12)
                           (format nil "~a constant power" name))))
    (multiple-value-bind (l r) (audio:pan-gains 0 1)
      (out "~%  centre: ~,5f per side (~a dB); hard left (pan -1): ~s~%"
           l (db l) (multiple-value-list (audio:pan-gains -1 1)))
      (check (multiple-value-list (audio:pan-gains -1 1)) '(1d0 0d0) "hard left is exactly (1 0)")
      (check-true (= l r) "the centre is symmetric"))
    (multiple-value-bind (seconds mix)
        (timed (audio:mix *triad-voices* :gains gains :pans pans))
      (setf *triad-mix* mix)
      (out "~%(mix voices :gains '~s :pans '~s)~%  -> ~a in ~,1f ms~%"
           gains pans mix (* 1000 seconds))
      (out "  peak left ~,5f (~a dB), right ~,5f (~a dB): below 1, so :CLIP clamps nothing~%"
           (peak mix 0) (db (peak mix 0)) (peak mix 1) (db (peak mix 1)))
      (check (audio:pcm-buffer-channels mix) 2 "the mix is stereo")
      (check (audio:pcm-buffer-frames mix) 66150 "the mix has the voices' length")
      (check-true (> (peak mix 0) (peak mix 1)) "C4, the loudest voice, is panned left")
      (let ((expected (expected-mix *triad-voices* gains pans)))
        (out "  every one of the ~d samples equals the file header's formula worked out here: ~a~%"
             (length expected) (equalp expected (audio:pcm-buffer-samples mix)))
        (check (equalp expected (audio:pcm-buffer-samples mix)) t "MIX against its formula"))
      (let ((frame 4410))
        (out "  frame ~d (0.1 s): left ~,7f, right ~,7f~%" frame
             (audio:sample-ref mix frame 0) (audio:sample-ref mix frame 1))))
    (let ((muted (audio:mix *triad-voices* :gains gains :pans pans :mute '(nil t t)))
          (solo (audio:mix *triad-voices* :gains gains :pans pans :solo '(t nil nil))))
      (out "~%Muting E4 and G4 and soloing C4 give the same buffer: ~a~%" (equalp muted solo))
      (check (equalp muted solo) t "mute the others = solo C4"))))

;;; 3. 16-bit WAV ----------------------------------------------------------------

(defun demo-wav ()
  (let* ((bytes (audio:write-wav *triad-mix* nil :encoding :pcm16))
         (header (pcm-header bytes))
         (codes (pcm16-codes bytes)))
    (setf *triad-bytes* bytes)
    (out "(write-wav mix nil :encoding :pcm16) -> ~:d bytes. The 44-byte header, unpacked~%~
          with binary-protocol's \"<4sI4s4sIHHIIHH4sI\":~%~%" (length bytes))
    (loop for (key value) on header by #'cddr
          do (out "  ~18a ~s~%" (string-downcase (symbol-name key)) value))
    (check (length bytes) (+ 44 (* 66150 4)) "file size")
    (check header '(:riff "RIFF" :riff-size 264636 :wave "WAVE" :fmt "fmt " :fmt-size 16
                    :format-tag 1 :channels 2 :sample-rate 44100 :bytes-per-second 176400
                    :block-align 4 :bits 16 :data "data" :data-size 264600)
           "the PCM header")
    (out "~%Sample checksum: ~:d codes, sum ~:d, sum of |code| ~:d, min ~:d, max ~:d~%~
          SHA-256 of the data chunk  ~a~%~
          SHA-256 of the whole file  ~a~%"
         (length codes) (reduce #'+ codes) (reduce #'+ codes :key #'abs)
         (reduce #'min codes) (reduce #'max codes)
         (sha256-hex (subseq bytes 44)) (sha256-hex bytes))
    (multiple-value-bind (buffer encoding) (audio:read-wav bytes)
      (setf *triad-read* buffer)
      (out "~%(read-wav bytes) -> ~a, ~s~%" buffer encoding)
      (check encoding :pcm16 "encoding read")
      (check (list (audio:pcm-buffer-sample-rate buffer) (audio:pcm-buffer-channels buffer)
                   (audio:pcm-buffer-frames buffer))
             '(44100 2 66150) "rate, channels, frames read")
      (let* ((samples (audio:pcm-buffer-samples buffer))
             (exact (every (lambda (x) (integerp (* (rational x) 32768))) samples))
             (codes-back (map 'list (lambda (x) (round (* x 32768))) samples))
             (error (max-difference samples (audio:pcm-buffer-samples *triad-mix*))))
        (out "  every sample is code / 32768 exactly: ~a; the codes are the file's: ~a~%"
             exact (equal codes-back codes))
        (out "  largest difference from the float mix: ~,3e (half a code is ~,3e)~%"
             (float error 1f0) (/ 1f0 65536))
        (check exact t "samples are exact codes")
        (check (equal codes-back codes) t "codes read = codes written")
        (check-true (<= error (/ 1d0 65536)) "quantisation within half a code"))
      (let ((again (audio:write-wav buffer nil :encoding :pcm16)))
        (out "~%Round trip: write-wav of what was read gives the same ~:d bytes: ~a~%~
              (SHA-256 ~a)~%" (length again) (equalp again bytes) (sha256-hex again))
        (check (equalp again bytes) t "write -> read -> write is byte for byte")
        (check (equalp (audio:read-wav again) buffer) t "read -> write -> read is EQUALP")))
    (let ((path (tmp-file "triad.wav")))
      (audio:write-wav *triad-mix* path :encoding :pcm16)
      (let ((on-disk (note-file path)))
        (out "~%Written to ~a: ~:d bytes, the same bytes as the vector: ~a~%"
             (shown path) (length on-disk) (equalp on-disk bytes))
        (check (equalp on-disk bytes) t "the file holds the vector's bytes")
        (check (equalp (audio:read-wav path) *triad-read*) t "reading the path = reading the vector")))
    (let* ((f32 (audio:write-wav *triad-mix* nil :encoding :float32))
           (back (audio:read-wav f32)))
      (out "As float32 (~:d bytes: fmt 18, fact, data) it reads back EQUALP to the float mix: ~a~%"
           (length f32) (equalp back *triad-mix*))
      (check (equalp back *triad-mix*) t "float32 is lossless"))))

;;; 4. Resampling ------------------------------------------------------------------

(defun tone (frequency seconds rate &optional (amplitude 1/2))
  "A fresh mono sine buffer, no fade."
  (let* ((frames (audio:seconds-to-frame seconds rate))
         (samples (make-array frames :element-type 'single-float)))
    (dotimes (n frames)
      (setf (aref samples n)
            (coerce (* amplitude (sin (/ (* 2 pi frequency n) rate))) 'single-float)))
    (audio:make-pcm-buffer :sample-rate rate :channels 1 :samples samples)))

(defun interior-peak (buffer margin)
  "Largest |sample| of a mono BUFFER away from MARGIN frames at either end,
where the zero-padded edges ramp."
  (let ((samples (audio:pcm-buffer-samples buffer)))
    (loop for i from margin below (- (length samples) margin)
          maximize (abs (aref samples i)))))

(defun demo-resample ()
  (multiple-value-bind (seconds low) (timed (audio:resample *triad-read* 16000))
    (out "(resample triad 16000), the 16-bit triad read back in section 3:~%~%")
    (out "             rate   channels  frames  duration  peak left  peak right~%")
    (dolist (row (list (list "before" *triad-read*) (list "after" low)))
      (destructuring-bind (label b) row
        (out "  ~6a  ~7d  ~8d  ~6d  ~8a  ~9,5f  ~10,5f~%"
             label (audio:pcm-buffer-sample-rate b) (audio:pcm-buffer-channels b)
             (audio:pcm-buffer-frames b) (audio:pcm-buffer-duration b) (peak b 0) (peak b 1))))
    (out "~%  ~,2f s for ~:d output samples (about 93 multiply-adds each)~%"
         seconds (length (audio:pcm-buffer-samples low)))
    (check (audio:pcm-buffer-frames low) 24000 "frames at 16 kHz: 1.5 s")
    (check (audio:pcm-buffer-duration low) 3/2 "the duration is kept")
    (dotimes (c 2)
      (check-true (< (abs (- (peak low c) (peak *triad-read* c))) 0.01)
                  (format nil "channel ~d peak kept within 0.01 (the triad is far inside the passband)" c)))
    (check (equalp (audio:resample *triad-read* 44100) *triad-read*) t "same rate: a copy")
    (let ((path (tmp-file "triad-16k.wav")))
      (audio:write-wav low path :encoding :pcm16)
      (let ((octets (note-file path)))
        (out "  written as 16-bit to ~a: ~:d bytes~%" (shown path) (length octets))
        (check (length octets) (+ 44 (* 24000 4)) "16 kHz file size"))))
  (out "~%The kernel's passband and stopband, on 0.5 s tones of amplitude 0.5 at 44.1 kHz~%~
        (peaks away from the ramped edges, 64 output frames at each end):~%~%")
  (out "  tone      peak at 16 kHz   gain dB~%")
  (dolist (frequency '(1000 5000 7000 10000 15000))
    (let* ((low (audio:resample (tone frequency 1/2 +rate+) 16000))
           (p (interior-peak low 64))
           (gain (/ p 0.5d0)))
      (out "  ~5d Hz  ~14,6f   ~7a~%" frequency p (db gain))
      (cond ((<= frequency 5000)
             (check-true (< (abs (* 20 (log gain 10))) 0.1d0)
                         (format nil "~d Hz is passed within 0.1 dB" frequency)))
            ((>= frequency 10000)
             (check-true (< (* 20 (log (max gain 1d-30) 10)) -60)
                         (format nil "~d Hz is attenuated by more than 60 dB" frequency))))))
  (out "~%Below 6.3 kHz the tone passes; past 8.9 kHz (the end of the transition band~%~
        for 44100 -> 16000) it is gone by more than 60 dB, as resample.lisp states.~%"))

;;; 5. Rendering an SMF ------------------------------------------------------------

(defconstant +ppq+ 480 "Ticks per quarter note.")
(defconstant +tempo+ 500000 "Microseconds per quarter: 120 bpm.")

(defun conductor-track ()
  (midi:track-from-absolute
   (list (cons 0 (midi:make-track-name-event :text "Arpeggio in C"))
         (cons 0 (midi:make-time-signature-event :numerator 4 :denominator 4 :clocks-per-click 24
                                                 :thirty-seconds-per-quarter 8))
         (cons 0 (midi:make-tempo-event :tempo +tempo+))
         (cons (* 4 +ppq+) (midi:make-end-of-track-event)))))

(defun note-track (name channel velocity notes)
  "A track called NAME on CHANNEL: NOTES is ((key start-quarter length-quarters) ...)."
  (let ((timed (list (cons 0 (midi:make-track-name-event :text name)))))
    (loop for (key start length) in notes
          do (push (cons (* start +ppq+)
                         (midi:make-note-on-event :channel channel :key key :velocity velocity))
                   timed)
             (push (cons (* (+ start length) +ppq+)
                         (midi:make-note-off-event :channel channel :key key :velocity 64))
                   timed))
    (midi:track-from-absolute (stable-sort (nreverse timed) #'< :key #'car))))

(defun arpeggio-smf ()
  "A type-1 SMF: a conductor track, a melody (C4 E4 G4 C5, one quarter each,
velocity 100, channel 1) and a bass (C3 then G2, a half note each, velocity 80,
channel 2), 4 quarters at 120 bpm: 2 seconds."
  (midi:make-smf :format 1 :division +ppq+
                 :tracks (list (conductor-track)
                               (note-track "Melody" 0 100 '((60 0 1) (64 1 1) (67 2 1) (72 3 1)))
                               (note-track "Bass" 1 80 '((48 0 2) (43 2 2))))))

(defun demo-render ()
  (let* ((smf (arpeggio-smf))
         (bytes (midi:write-smf smf nil))
         (engine (make-instance 'audio:reference-engine :waveform :sine :amplitude 1/4)))
    (out "A type-1 SMF built in code with midi-protocol: a conductor track (120 bpm, 4/4),~%~
          \"Melody\" C4 E4 G4 C5 (quarters, velocity 100, channel 1) and \"Bass\" C3 G2~%~
          (halves, velocity 80, channel 2). write-smf -> ~d bytes.~%~%" (length bytes))
    (out "  smf-duration ~a s; (render-frames smf 44100) with the default 1 s tail: ~:d~%"
         (audio:smf-duration smf) (audio:render-frames smf 44100))
    (check (audio:smf-duration smf) 2 "SMF duration: 4 quarters at 120 bpm")
    (check (audio:render-frames smf 44100) 132300 "render frames: 3 s")
    (out "  engine: ~s, waveform ~s, amplitude ~a~%~%"
         (type-of engine) (audio:reference-engine-waveform engine) (audio:reference-engine-amplitude engine))
    (multiple-value-bind (seconds whole) (timed (audio:render-midi engine bytes))
      (out "(render-midi engine bytes) -> ~a in ~,1f ms~%" whole (* 1000 seconds))
      (out "  duration ~a s, sounding until ~a s, peak ~,5f (~a dB)~%"
           (audio:pcm-buffer-duration whole) (last-sounding-second whole) (peak whole)
           (db (peak whole)))
      (check (equalp whole (audio:render-midi engine smf)) t "rendering the bytes = rendering the SMF")
      (check (audio:pcm-buffer-duration whole) 3 "render duration with the tail")
      (check (last-sounding-second whole) 2 "the notes end at 2 s")
      (multiple-value-bind (seconds stems) (timed (audio:render-stems engine smf))
        (out "~%(render-stems engine smf) -> ~d stems in ~,1f ms:~%~%" (length stems) (* 1000 seconds))
        (out "  name     track  channel  frames   duration  sounding  peak     expected A*v/127~%")
        (loop for stem in stems
              for velocity in '(100 80)
              do (let ((b (audio:stem-buffer stem))
                       (expected (* 1/4 (/ velocity 127))))
                   (out "  ~7a  ~5d  ~7a  ~7d  ~8a  ~8a  ~,5f  ~,5f~%"
                        (audio:stem-name stem) (audio:stem-track stem) (audio:stem-channel stem)
                        (audio:pcm-buffer-frames b) (audio:pcm-buffer-duration b)
                        (last-sounding-second b) (peak b) (float expected))
                   (check-true (< (abs (- (peak b) expected)) (* 0.01 expected))
                               (format nil "~a peak within 1% of A*v/127" (audio:stem-name stem)))
                   (check (audio:pcm-buffer-frames b) 132300 (format nil "~a frames" (audio:stem-name stem)))))
        (check (mapcar #'audio:stem-name stems) '("Melody" "Bass") "stem names from the track names")
        (check (mapcar #'audio:stem-track stems) '(1 2) "stem tracks")
        (let* ((sum (audio:mix stems :clip :none))
               (difference (max-difference (audio:pcm-buffer-samples sum)
                                           (audio:pcm-buffer-samples whole))))
          (out "~%  (mix stems :clip :none) against the whole render: largest difference ~,2e~%"
               difference)
          (check-true (<= difference 1e-6) "the stems sum to the whole render"))
        (let ((rehearsal (audio:mix stems :gains '(0 -12) :pans '(-0.4 0.4))))
          (out "  a rehearsal mix, bass at -12 dB: peak left ~,5f, right ~,5f~%"
               (peak rehearsal 0) (peak rehearsal 1))
          (check-true (> (peak rehearsal 0) (peak rehearsal 1)) "melody left is louder")))
      (let ((by-channel (audio:render-stems engine smf :by :channel :sample-rate 16000 :channels 1)))
        (out "~%(render-stems engine smf :by :channel :sample-rate 16000 :channels 1):~%")
        (dolist (stem by-channel)
          (let ((b (audio:stem-buffer stem)))
            (out "  ~10a channel ~d  ~a  ~a s, peak ~,5f~%" (audio:stem-name stem)
                 (audio:stem-channel stem) b (audio:pcm-buffer-duration b) (peak b))))
        (check (mapcar #'audio:stem-name by-channel) '("Channel 1" "Channel 2") "names by channel")
        (check (mapcar (lambda (s) (audio:pcm-buffer-frames (audio:stem-buffer s))) by-channel)
               '(48000 48000) "16 kHz stems are 3 s")))
    (let ((path (tmp-file "arpeggio.wav")))
      (audio:write-wav (audio:render-midi engine smf) path :encoding :pcm16)
      (out "~%The whole render written as 16-bit to ~a: ~:d bytes~%"
           (shown path) (length (note-file path))))
    (let ((audio:*max-render-seconds* 2))
      (let ((caught nil))
        (handler-bind ((audio:audio-limit-exceeded
                         (lambda (c)
                           (setf caught (list (audio:audio-limit-exceeded-limit c)
                                              (audio:audio-limit-exceeded-value c)))
                           (continue c))))
          (let ((b (audio:render-midi engine smf :sample-rate 8000)))
            (out "With *max-render-seconds* 2: AUDIO-LIMIT-EXCEEDED, limit ~a, value ~a;~%~
                  CONTINUE renders anyway: ~a~%"
                 (first caught) (second caught) b)
            (check caught '(2 3) "the limit and the asked-for seconds")
            (check (audio:pcm-buffer-frames b) 24000 "rendered anyway")))))))

;;; 6. Conditions and restarts -----------------------------------------------------

(defun restart-names (condition)
  (remove-duplicates
   (loop for r in (compute-restarts condition)
         for name = (restart-name r)
         when (and name (member (symbol-name name)
                                '("USE-PARTIAL-DATA" "USE-VALUE")
                                :test #'string=))
           collect (string-downcase (symbol-name name)))
   :test #'string=))

(defun demo-truncated ()
  (let* ((cut (+ 44 4001))              ; 1000 whole frames and one byte of the next
         (truncated (subseq *triad-bytes* 0 cut)))
    (out "a. A truncated file: the first ~:d of the ~:d bytes (1,000 whole frames and one~%~
          byte of the next).~%~%" cut (length *triad-bytes*))
    (handler-case (audio:read-wav truncated)
      (audio:audio-format-error (c)
        (out "  (read-wav truncated) signals ~s~%    offset ~d (the data chunk), reason ~s~%    \"~a\"~%"
             (type-of c) (audio:audio-format-error-offset c) (audio:audio-error-reason c) c)
        (check (audio:audio-format-error-offset c) 36 "the error points at the data chunk")
        (check-true (typep c 'parse-error) "an AUDIO-FORMAT-ERROR is a PARSE-ERROR")))
    (let ((restarts nil) (warnings '()))
      (multiple-value-bind (buffer encoding)
          (handler-bind ((audio:audio-format-error
                           (lambda (c)
                             (setf restarts (restart-names c))
                             (audio:use-partial-data c)))
                         (audio:wav-format-warning
                           (lambda (w)
                             (push (list (audio:wav-format-warning-offset w) (audio:audio-warning-reason w))
                                   warnings)
                             (muffle-warning w))))
            (audio:read-wav truncated))
        (out "~%  restarts offered: ~{~a~^, ~}~%" restarts)
        (out "  with USE-PARTIAL-DATA: ~a, ~s~%" buffer encoding)
        (out "  and a WAV-FORMAT-WARNING: ~{offset ~d, ~s~}~%" (first warnings))
        (check (and (member "use-partial-data" restarts :test #'string=) t) t "USE-PARTIAL-DATA is offered")
        (check (audio:pcm-buffer-frames buffer) 1000 "the whole frames that are there")
        (check (first warnings) '(4044 "partial frame") "the partial frame warning")
        (let ((prefix (subseq (audio:pcm-buffer-samples *triad-read*) 0 2000)))
          (out "  its 2,000 samples are the first 2,000 of the full read: ~a~%"
               (equalp prefix (audio:pcm-buffer-samples buffer)))
          (check (equalp prefix (audio:pcm-buffer-samples buffer)) t "the partial read is a prefix"))))
    (out "~%  Without a handler the error is not resumed: (audio:use-partial-data) outside~%  ~
          one returns ~s.~%" (audio:use-partial-data))))

(defun infinity ()
  "+infinity as a SINGLE-FLOAT, decoded by binary-protocol (no float trap)."
  (first (bin:unpack "<f" (coerce #(0 0 #x80 #x7F) '(vector (unsigned-byte 8))))))

(defun quiet-nan ()
  "A quiet NaN as a SINGLE-FLOAT, decoded by binary-protocol (no float trap)."
  (first (bin:unpack "<f" (coerce #(0 0 #xC0 #x7F) '(vector (unsigned-byte 8))))))

(defun buffer-with (index value frames)
  "The first FRAMES frames of the triad mix with sample INDEX set to VALUE."
  (let ((samples (subseq (audio:pcm-buffer-samples *triad-mix*) 0 (* 2 frames))))
    (setf (aref samples index) value)
    (audio:make-pcm-buffer :sample-rate +rate+ :channels 2 :samples samples)))

(defun demo-non-finite-write ()
  (out "~%b. A non-finite sample on write. WRITE-WAV checks the whole buffer before it~%~
        writes a byte, so the error leaves no file behind.~%~%")
  (loop for (label value index) in (list (list "+infinity" (infinity) 37)
                                         (list "a NaN" (quiet-nan) 151))
        do (let ((buffer (buffer-with index value 100)))
             (dolist (encoding '(:pcm16 :float32))
               (let ((path (tmp-file (format nil "bad-~(~a~)-~d.wav" encoding index))))
                 (handler-case (progn (audio:write-wav buffer path :encoding encoding)
                                      (out "  !! no error~%"))
                   (audio:audio-encode-error (c)
                     (out "  ~10a at sample ~3d, ~8s: ~s index ~d, ~s; ~a exists: ~a~%"
                          label index encoding (type-of c) (audio:audio-encode-error-index c)
                          (audio:audio-error-reason c) (shown path) (and (probe-file path) t))
                     (check (audio:audio-encode-error-index c) index
                            (format nil "~a index (~a)" label encoding))
                     (check (probe-file path) nil (format nil "no file for ~a (~a)" label encoding))))))))
  (out "  The error has no restart: there is no sample to write in its place, the buffer~%~
        is the caller's to fix.~%"))

(defun demo-non-finite-read ()
  (let* ((buffer (audio:make-pcm-buffer :sample-rate 8000 :channels 1 :samples '(0.25 -0.5 0.75 1.0)))
         (bytes (audio:write-wav buffer nil :encoding :float32))
         (offset (+ 58 (* 4 2))))
    (replace bytes #(0 0 #x80 #x7F) :start1 offset) ; sample 2 becomes +infinity
    (out "~%c. A float32 file whose sample 2 is +infinity (bytes 00 00 80 7F at offset ~d).~%~%" offset)
    (handler-case (audio:read-wav bytes)
      (audio:audio-format-error (c)
        (out "  (read-wav bytes) signals ~s at offset ~d: ~s~%"
             (type-of c) (audio:audio-format-error-offset c) (audio:audio-error-reason c))
        (check (audio:audio-format-error-offset c) offset "the error points at the sample")))
    (let ((repaired (handler-bind ((audio:audio-format-error
                                     (lambda (c) (use-value 0 c))))
                      (audio:read-wav bytes))))
      (out "  with (use-value 0): ~s~%"
           (coerce (audio:pcm-buffer-samples repaired) 'list))
      (check (coerce (audio:pcm-buffer-samples repaired) 'list) '(0.25 -0.5 0.0 1.0)
             "USE-VALUE replaced the sample"))))

(defun demo-conditions ()
  (demo-truncated)
  (demo-non-finite-write)
  (demo-non-finite-read))

;;; Files and the run ---------------------------------------------------------------

(defun demo-files ()
  (let ((total (reduce #'+ *files* :key #'second)))
    (out "The WAV files of this run come to ~:d bytes. WAV outputs are committed only when~%~
          they come to less than 200 KB together, so these stay in a temporary directory~%~
          that is removed at the end; their sizes and SHA-256 are the record:~%~%"
         total)
    (loop for (name size sha) in *files*
          do (out "  ~22a ~9:d  ~a~%" name size sha))
    (out "~%The renders are deterministic: the same SHA-256 on every run of this version.~%")))

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()) (*files* '())
        (*print-right-margin* 110)
        (*tmp* (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "harness-demos-audio-~36r" (random (expt 36 8) (make-random-state t)))
                                 (uiop:temporary-directory)))))
    (ensure-directories-exist *tmp*)
    (unwind-protect
         (progn
           (format t "~&audio-protocol ~a on midi-protocol ~a and binary-protocol ~a (~a ~a)~%"
                   (version-of "audio-protocol") (version-of "midi-protocol")
                   (version-of "binary-protocol")
                   (lisp-implementation-type) (lisp-implementation-version))
           (section "1. Three voices of a C-major triad, one sine buffer each" (demo-voices))
           (section "2. MIX: per-voice gain and pan" (demo-mix))
           (section "3. A 16-bit WAV: header, checksum, round trip" (demo-wav))
           (section "4. RESAMPLE: 44.1 kHz to 16 kHz" (demo-resample))
           (section "5. Rendering an SMF with the reference engine: a mix and stems" (demo-render))
           (section "6. Conditions and restarts" (demo-conditions))
           (section "7. The files written" (demo-files)))
      (uiop:delete-directory-tree *tmp* :validate t :if-does-not-exist :ignore))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
