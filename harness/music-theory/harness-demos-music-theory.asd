(defsystem "harness-demos-music-theory"
  :version "0.3.0"
  :description "Demo of music-theory 0.3.0. MT0: spelled pitches parsed and printed in ASCII and Unicode, MIDI numbers and the scientific octave (B#3 and C4 are enharmonic, not equal), the line of fifths from Fbb to B##, intervals with quality, number, compound and simple forms, inversion and reversal, transposition with exact spelling (C major up an A4 is F# major), INVALID-PITCH and INVALID-INTERVAL with USE-VALUE restarts. MT1: meters (simple, compound, additive, C and C|) and their beats, the metric-weight hierarchy, dots and tuplets, tie splitting at barlines and strong beats, tempo conversion and phrase windows. MT2: keys and key signatures from Cb to C#, natural, harmonic and melodic minor scales, scale degrees and chromatic notes as altered degrees, Roman numerals (inversions, applied chords, the Neapolitan) to chords and back, mixture, and a I-vi-ii6-V7-I progression"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "music-theory" "0.3.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
