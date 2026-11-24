(defsystem "harness-demos-midi"
  :version "0.1.0"
  :description "Demo of midi-protocol 0.1.0 (MIDI 1.0 events and Standard MIDI Files, files only): a type-1 SMF built in code (tempo track with tempo, time and key signature, track names and markers; melody and bass tracks with program changes, controllers, notes and one lyric per note), written to bytes with an annotated hex dump, read back to an event listing with a byte-for-byte round trip, running status on and off, absolute time (TRACK-TO-ABSOLUTE, TRACK-FROM-ABSOLUTE, MERGE-TRACKS to a type-0 view, TEMPO-MAP with a tempo change, TICKS-TO-SECONDS for the lyrics), truncated and corrupted bytes with MIDI-PARSE-ERROR and the SKIP-EVENT, SKIP-CHUNK, USE-PARTIAL-SMF and USE-VALUE restarts, MIDI-FORMAT-WARNING, MIDI-ENCODE-ERROR, files and streams, and a cross-read by mido when uv is on PATH"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "midi-protocol" "0.1.0")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
