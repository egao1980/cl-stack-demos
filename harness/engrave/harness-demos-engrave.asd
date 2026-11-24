(defsystem "harness-demos-engrave"
  :version "0.1.0"
  :description "Demo of engrave-protocol 0.1.0 (EN0: engines, views, SVG pages, id maps, timemaps, PDF) on score-protocol 0.3.0, with the reference NULL-ENGINE (no native engine is published yet), on the first line of OLD HUNDREDTH in four parts: ENSURE-ENGINE, *ENGRAVE-ENGINE*, ENGINE-UNAVAILABLE and USE-VALUE; MAKE-LAYOUT, page sizes, ENSURE-LAYOUT and INVALID-RENDER-OPTION; RENDER-SVG pages as xml-protocol SVG trees, page geometry, systems, SVG-PAGE-IDS, FIND-SVG-ELEMENT, ELEMENT-KIND; the id map from score element ids to (page . svg-id); page splitting, :PAGE, :SYSTEM-BREAKS and the break plan fed back; the :CONDENSED view and :PARTS; UNSUPPORTED-NOTATION with SKIP-ELEMENT; CHECK-ENGRAVING on good and tampered renderings; RENDER-TIMEMAP and SCORE-TIMEMAP in exact milliseconds with a tempo change, a fermata, and a repeat unfolded; CHECK-TIMEMAP; RENDER-PDF without a printer (PRINTER-UNAVAILABLE) and with a printer that returns no PDF (ENGINE-RENDER-ERROR)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "engrave-protocol" "0.1.0")
               (:version "score-protocol" "0.3.0")
               (:version "music-theory" "0.5.0")
               (:version "xml-protocol" "0.1.0")
               "fset" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
