(defsystem "harness-demos-score-parity"
  :version "0.1.0"
  :description "Demo of score-parity 0.1.0 (slice SC5: the cross-format parity corpus and matrix) on score-protocol 0.2.0, score-backend-musicxml 0.1.0, score-backend-midi 0.1.0 and music-theory 0.5.1. The full matrix of every check on each of the 34 public-domain corpus files (MusicXML and .mxl round trips, the hymnal voice split, score -> MIDI -> score as written and unfolded, the MuseScore and XSD oracles), with the per-feature view of PRINT-MATRIX and its summary row. One check up close: the :midi round trip of a lead sheet, facet by facet (voice lines, lyrics, chord-symbol onsets, bar lengths) against the corpus facts. A deliberate failure: one written pitch corrupted in memory, and the :musicxml check's :fail with its reason and differences. A backend error (a truncated MIDI file) turned into :error results while the matrix runs on. The oracles: the XSD column when MUSICXML_XSD and xmllint are there, MuseScore skipped with its reason"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "score-parity" "0.1.0")
               (:version "score-protocol" "0.2.0")
               (:version "score-backend-musicxml" "0.1.0")
               (:version "score-backend-midi" "0.1.0")
               (:version "midi-protocol" "0.1.0")
               (:version "music-theory" "0.5.1")
               "fset"
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4")
               (:version "cl-stack-pathlib" "0.10.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
