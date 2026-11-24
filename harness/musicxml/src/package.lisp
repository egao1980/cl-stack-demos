(defpackage #:harness-demos-musicxml
  (:use #:cl #:score-protocol #:score-backend-musicxml)
  (:import-from #:music-theory
                #:pitch-name #:meter-string #:key-name
                #:tempo-bpm #:tempo-beat-unit
                #:parse-chord-symbol #:print-chord-symbol #:parse-pitch-class
                #:chord-symbol-musicxml-kind #:musicxml-kind-chord-symbol)
  (:export #:run-demos))
