(defsystem "harness-demos-serdes"
  :version "0.1.0"
  :description "Demo of serdes-protocol 0.3.0 (serdes-protocol#11: backend errors translated in place, backend restarts reachable) through a real backend, score-backend-musicxml 0.1.1's :MUSICXML and :MXL formats, on score-protocol 0.3.0: a public-domain melody round-tripped through ENCODE / DECODE, ENCODE-TO-OCTETS / DECODE-OCTETS and :STREAM, the media types; a warning passing through untouched; a reader error arriving as SERDES-DECODE-ERROR with SERDES-ERROR-CAUSE the reader's MUSICXML-UNSUPPORTED and SKIP-ELEMENT invoked from a handler outside DECODE, once per element, the result SCORE-EQUAL to a direct :SKIP read; HANDLER-CASE keeping the cause; a writer's CHECK-TYPE answered with STORE-VALUE through ENCODE; SERDES-UNKNOWN-FORMAT passed through as is"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "serdes-protocol" "0.3.0")
               (:version "score-backend-musicxml" "0.1.1")
               (:version "score-protocol" "0.3.0")
               (:version "music-theory" "0.5.1")
               "fset")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
