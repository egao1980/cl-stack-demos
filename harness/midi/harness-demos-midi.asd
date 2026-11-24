(defsystem "harness-demos-midi"
  :version "0.2.0"
  :description "Demo of midi-protocol 0.1.0 (MIDI 1.0 events and Standard MIDI Files, files only): a type-1 SMF built in code (tempo track with tempo, time and key signature, track names and markers; melody and bass tracks with program changes, controllers, notes and one lyric per note), written to bytes with an annotated hex dump, read back to an event listing with a byte-for-byte round trip, running status on and off, absolute time (TRACK-TO-ABSOLUTE, TRACK-FROM-ABSOLUTE, MERGE-TRACKS to a type-0 view, TEMPO-MAP with a tempo change, TICKS-TO-SECONDS for the lyrics), truncated and corrupted bytes with MIDI-PARSE-ERROR and the SKIP-EVENT, SKIP-CHUNK, USE-PARTIAL-SMF and USE-VALUE restarts, MIDI-FORMAT-WARNING, MIDI-ENCODE-ERROR, files and streams, and a cross-read by mido when uv is on PATH. Part 2, score-backend-midi 0.1.0 (slice SC4, score <-> SMF): a four-part OLD HUNDREDTH with two verses, chord symbols, dynamics, a tie and a repeat exported to SMF type 1 (channels, programs, the conductor track's tempo, time, key, lyric and chord text events, velocities from dynamics, the tie as one note) as written and unfolded, a rehearsal set with one file per voice, the round trip through IMPORT-MIDI, a humanised performance imported with the grid chosen per beat through SAT-PROTOCOL:MINIMIZE, key-aware spelling and the uncertainty report with LOW-CONFIDENCE-GUESS, and mido's view of the exported file"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "midi-protocol" "0.1.0")
               (:version "score-backend-midi" "0.1.0")
               (:version "score-protocol" "0.2.0")
               (:version "music-theory" "0.5.0")
               (:version "sat-protocol" "0.2.0")
               (:version "sat-backend-cdcl" "0.1.0")
               "fset"
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")
               (:file "score")))
