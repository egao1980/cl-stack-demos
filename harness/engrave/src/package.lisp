(defpackage #:harness-demos-engrave
  (:use #:cl #:score-protocol #:engrave-protocol)
  (:shadowing-import-from #:engrave-protocol #:skip-element)
  (:import-from #:music-theory
                #:parse-pitch #:pitch-name #:parse-meter #:parse-key
                #:make-tempo #:tempo-bpm #:tempo-beat-unit)
  (:local-nicknames (#:xml #:xml-protocol))
  (:export #:run-demos))
