(defsystem "harness-demos-score"
  :version "0.1.0"
  :description "Demo of score-protocol 0.1.0 (SC0, the canonical score model) on one public-domain hymn tune, NEW BRITAIN, built in code: two staves, a pickup, 3/4, G major, two verses of lyrics with syllabic types and extenders, chord symbols (one under a tied note), a tie, slurs, a fermata, dynamics, a tempo mark, rehearsal marks and a repeat with first and second endings; element ids in document order with their kind prefixes, the id counter, stability through COPY-SCORE, EQ sharing after a rebuild, no reuse of a deleted id, NCName ids and slice ids; bars, the pickup, ABSOLUTE-OFFSET and OFFSET-POSITION round trips, INVALID-POSITION and its USE-VALUE restart; CHECK-SCORE issues for an overfull bar, a tie to another pitch, a lyric on a rest and an ending past the end, VALIDATE-SCORE and CONTINUE; provenance and confidence from a pretend recognition step and UNCERTAIN-ELEMENTS; SCORE-TO-SEXP, SEXP-TO-SCORE, SCORE-EQUAL and SCORE-FORMAT-ERROR positions"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "score-protocol" "0.1.0")
               (:version "music-theory" "0.4.0")
               "fset")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
