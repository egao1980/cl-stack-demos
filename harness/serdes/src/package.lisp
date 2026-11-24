(defpackage #:harness-demos-serdes
  (:use #:cl #:score-protocol)
  (:import-from #:score-backend-musicxml #:read-musicxml #:write-musicxml #:read-mxl)
  (:import-from #:music-theory #:parse-pitch #:pitch-name #:parse-meter #:parse-key #:make-tempo)
  (:local-nicknames (#:serdes #:serdes-protocol))
  (:export #:run-demos))
