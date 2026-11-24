(defsystem "harness-demos-musicxml"
  :version "0.1.0"
  :description "Demo of score-backend-musicxml 0.1.0 (SC2: MusicXML 4.0 partwise reader and writer, .mxl) on score-protocol and music-theory 0.5.1, on two public-domain hymns re-encoded by hand. Reading NEW BRITAIN (a pickup, two verses of lyrics, harmony chord symbols with N.C. and a slash chord, a tie, slurs, a dynamic, a tempo mark, a repeat with first and second endings): the measure map, events, lyrics, chord symbols and spanners, ids kept from id attributes (slice ids for measures, a dotted id not taken) and minted ids, and every source kind. SPLIT-VOICES on a two-staff SATB OLD HUNDREDTH excerpt: shared-stem chords and a two-voice bar become four voice lines, the unison LOW-CONFIDENCE-GUESS, ids that follow the notes. Harmony to chord symbol and back for sixteen kinds through music-theory 0.5.1, degree order, use-symbols and text as display, lost chord text. MUSICXML-UNSUPPORTED for an ornament with its path, SKIP-ELEMENT through HANDLER-BIND leaving no trace, the :warn and :skip policies, serdes errors. Writing: DOCTYPE and ids, read-write-read SCORE-EQUAL, byte-stable output, LOSSY-CONVERSION for provenance and confidence, .mxl archives and container.xml, the :musicxml and :mxl serdes formats. An optional xmllint check against a local MusicXML 4.0 XSD, and external entities refused at their declaration"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "score-backend-musicxml" "0.1.0")
               (:version "score-protocol" "0.1.0")
               (:version "music-theory" "0.5.1")
               "fset" "babel" "xml-protocol" "serdes-protocol" "compression-protocol"
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
