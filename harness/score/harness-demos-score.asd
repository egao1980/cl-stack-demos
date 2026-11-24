(defsystem "harness-demos-score"
  :version "0.2.0"
  :description "Demo of score-protocol 0.2.0 (SC0, the canonical score model, and SC1, the edit-operation algebra and views) on one public-domain hymn tune, NEW BRITAIN, built in code: two staves, a pickup, 3/4, G major, two verses of lyrics with syllabic types and extenders, chord symbols (one under a tied note), a tie, slurs, a fermata, dynamics, a tempo mark, rehearsal marks and a repeat with first and second endings; element ids in document order with their kind prefixes, the id counter, stability through COPY-SCORE, EQ sharing after a rebuild, no reuse of a deleted id, NCName ids and slice ids; bars, the pickup, ABSOLUTE-OFFSET and OFFSET-POSITION round trips, INVALID-POSITION and its USE-VALUE restart; CHECK-SCORE issues for an overfull bar, a tie to another pitch, a lyric on a rest and an ending past the end, VALIDATE-SCORE and CONTINUE; provenance and confidence from a pretend recognition step and UNCERTAIN-ELEMENTS; SCORE-TO-SEXP, SEXP-TO-SCORE, SCORE-EQUAL and SCORE-FORMAT-ERROR positions. SC1: SET-PITCH, SET-DURATION across a barline (a tie, a filled rest, a cascaded slur), INSERT-EVENT into a full bar with MEASURE-OVERFULL and the REFLOW restart, ADD-SPANNER, ADD-MARK of a fermata and of a lyric, each with its change set, EQ sharing and minted ids; INVALID-EDIT and its subtypes with SKIP-EDIT and USE-VALUE; INVERT-EDIT for each step applied in reverse to a SCORE-EQUAL original, COMPOSE-EDITS, and the EDIT-TO-SEXP / SEXP-TO-EDIT round trip; SET-TIME 3/4 to 6/8 and 2/4 with ties across the new barlines and the refusal to drop marked measures; TRANSPOSE by a major second, to Ab major and by an augmented unison with CHOOSE-ENHARMONIC-KEY (music-theory 0.5.0), and over a range; EXTRACT-PART, CONDENSED-VIEW of a four-part OLD HUNDREDTH fragment, and UNFOLD of a repeat with two endings and a D.C. al Fine: the playback order as (pass . bar) and the onset to element-id map"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "score-protocol" "0.2.0")
               (:version "music-theory" "0.5.0")
               "fset")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
