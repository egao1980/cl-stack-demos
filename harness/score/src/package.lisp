(defpackage #:harness-demos-score
  (:use #:cl #:score-protocol)
  (:import-from #:music-theory
                #:parse-pitch #:pitch-name #:parse-meter #:meter-string
                #:parse-key #:key-name #:make-tempo #:tempo-bpm #:tempo-beat-unit
                #:parse-chord-symbol #:print-chord-symbol)
  (:export #:run-demos))
