(in-package #:harness-demos-serdes)

;;;; A short tour of serdes-protocol 0.3.0. The release changes one thing
;;;; (serdes-protocol#11): ENCODE and DECODE translate a backend's error where
;;;; it is signalled, before anything unwinds, and the backend's restarts stay
;;;; reachable from a handler outside the call. 0.2 caught the error with
;;;; HANDLER-CASE, so the stack was gone when the caller's handler ran, and the
;;;; original condition survived only as text. The demo uses a real backend:
;;;; score-backend-musicxml registers the formats :MUSICXML (text) and :MXL
;;;; (octets). A public-domain melody makes round trips through the registry
;;;; (1). A reader error then arrives as SERDES-DECODE-ERROR with its cause,
;;;; and SKIP-ELEMENT is invoked from outside DECODE (2). A writer's CHECK-TYPE
;;;; is answered with STORE-VALUE through ENCODE (3). Section 4 shows what is
;;;; passed through untouched. Every result with a known answer is checked:
;;;; RUN-DEMOS returns NIL when one differs or a section stops on an error.

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  "Print LINES of prose, then a blank line."
  (format t "~&~{~a~%~}~%" lines))

(defun code-text (object &key (margin 1000))
  "OBJECT printed as it reads in this package."
  (let ((*package* (find-package '#:harness-demos-serdes))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* margin))
    (prin1-to-string object)))

(defun indent (text columns)
  (with-output-to-string (s)
    (loop for char across text
          do (write-char char s)
             (when (char= char #\Newline)
               (dotimes (i columns) (write-char #\Space s))))))

(defun same-p (a b)
  (typecase a
    (string (and (stringp b) (string= a b)))
    (cons (and (consp b) (same-p (car a) (car b)) (same-p (cdr a) (cdr b))))
    (t (equalp a b))))

(defun check (value expected what)
  (incf *checks*)
  (unless (same-p value expected)
    (incf *mismatches*)
    (format t "~&  !! ~a: got ~a, expected ~a~%" what (code-text value) (code-text expected)))
  value)

(defun print-result (form value &optional note)
  (let ((ftext (indent (let ((line (code-text form)))
                         (if (> (length line) 82) (code-text form :margin 84) line))
                       2))
        (vtext (if note
                   (format nil "~a   ; ~a" (code-text value) note)
                   (code-text value))))
    (if (and (<= (+ 2 (length ftext) 4 (length vtext)) 84)
             (not (find #\Newline ftext)))
        (format t "~&  ~a => ~a~%" ftext vtext)
        (format t "~&  ~a~%    => ~a~%" ftext (indent vtext 7)))))

(defmacro show (form &optional (expected nil expected-p) note)
  "Print FORM and its value like a REPL transcript; check it against EXPECTED."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (print-result ',form ,value ,note)
       ,@(when expected-p `((check ,value ,expected (code-text ',form))))
       ,value)))

(defmacro shown (form)
  "Print FORM as code, then evaluate it."
  `(progn (format t "~&  ~a~%~%" (indent (code-text ',form :margin 96) 2))
          ,form))

(defun strip-identities (text)
  (with-output-to-string (s)
    (loop with i = 0
          while (< i (length text))
          do (let ((close (and (char= (char text i) #\Space)
                               (< (+ i 2) (length text))
                               (char= (char text (1+ i)) #\{)
                               (position #\} text :start i))))
               (if (and close (> close (+ i 2))
                        (every (lambda (c) (digit-char-p c 16)) (subseq text (+ i 2) close)))
                   (setf i (1+ close))
                   (progn (write-char (char text i) s) (incf i)))))))

(defun condition-line (c)
  "The type of the condition C and its report, on one line."
  (let* ((*package* (find-package '#:harness-demos-serdes))
         (*print-case* :downcase)
         (report (strip-identities (princ-to-string c))))
    (format nil "~(~a~): ~{~a~^ ~}" (type-of c)
            (remove "" (uiop:split-string report :separator '(#\Space #\Newline #\Tab))
                    :test #'string=))))

(defmacro show-signal (form type &key (checked t))
  `(handler-case (progn ,form
                        (format t "~&  ~a  (no condition)~%" (code-text ',form))
                        ,@(when checked `((check :returned :signalled (code-text ',form)))))
     (,type (c)
       (format t "~&  ~a~%    -> ~a~%" (indent (code-text ',form :margin 84) 2)
               (condition-line c)))))

(defmacro section (title &body body)
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (e)
         (push ,title *failed-sections*)
         (format t "~&!! unexpected ~s, the rest of this section is skipped:~%   ~a~%"
                 (type-of e) e)))))

;;; The melody ------------------------------------------------------------------
;;; The first phrase of the "Ode to Joy" melody (Beethoven, Symphony No. 9,
;;; 1824), in C major and 4/4, one part, piano.

(defparameter *bars*
  '((("E4" 1/4 :p) ("E4" 1/4) ("F4" 1/4) ("G4" 1/4))
    (("G4" 1/4) ("F4" 1/4) ("E4" 1/4) ("D4" 1/4))
    (("C4" 1/4) ("C4" 1/4) ("D4" 1/4) ("E4" 1/4))
    (("E4" 3/8) ("D4" 1/8) ("D4" 1/2)))
  "The phrase bar by bar: (PITCH DURATION [:P]).")

(defun make-melody (&key confidence)
  "The phrase as a score. CONFIDENCE, when given, goes on the first note: a
value MusicXML has no place for."
  (make-score
   :metadata '(:title "Ode to Joy" :composer "Ludwig van Beethoven, 1824")
   :measures (list* (make-measure-info :time (parse-meter "4/4") :key (parse-key "C major"))
                    (make-measure-info) (make-measure-info)
                    (list (make-measure-info :barline :final)))
   :parts (list (make-part
                 :name "Melody"
                 :staves (list (make-staff :clef :treble))
                 :voices (list (make-voice
                                :name "Melody" :staff 1
                                :measures (loop for bar in *bars*
                                                for b from 0
                                                collect (loop for (pitch duration dynamic) in bar
                                                              for j from 0
                                                              collect (make-note-event
                                                                       :pitch (parse-pitch pitch) :duration duration
                                                                       :dynamic (and dynamic (make-dynamic-mark :value dynamic))
                                                                       :confidence (and (= b 0) (= j 0) confidence))))))))))

(defvar *melody* nil "The phrase.")

(defparameter +trill+ "<notations><ornaments><trill-mark/></ornaments></notations>"
  "An ornament the score model has no place for.")

(defun with-trills (text &rest notes)
  "TEXT, a MusicXML document, with a trill on each of NOTES: the 1-based
positions of <note> elements in the document."
  (let ((out text))
    ;; From the last position down, so earlier positions don't move.
    (dolist (n (sort (copy-list notes) #'>) out)
      (let ((at (loop with start = 0
                      repeat n
                      do (setf start (+ (search "</note>" out :start2 start) 1))
                      finally (return (1- start)))))
        (setf out (concatenate 'string (subseq out 0 at) +trill+ (subseq out at)))))))

(defun text-lines (text) (uiop:split-string text :separator '(#\Newline)))

(defun restart-names (condition &optional (n 3))
  (subseq (mapcar #'restart-name (compute-restarts condition)) 0 n))

(defmacro inside-handler ((var type) form &body body)
  "Evaluate FORM; when it signals a condition of TYPE, return BODY's value,
computed inside the handler (VAR bound to the condition), before anything
unwinds. Return :NO-CONDITION when FORM returns."
  (let ((block (gensym "BLOCK")))
    `(block ,block
       (handler-bind ((,type (lambda (,var) (return-from ,block (progn ,@body)))))
         ,form
         :no-condition))))

;;; 1. Round trips --------------------------------------------------------------

(defvar *text* nil "The melody as MusicXML text.")

(defun demo-round-trips ()
  (para "serdes-protocol is a registry of formats: ENCODE and DECODE find the backend"
        "registered for :FORMAT and call it. Loading score-backend-musicxml registers"
        ":MUSICXML (text) and :MXL (a compressed archive, octets) with their media types."
        "The value is a score-protocol score: the first phrase of the \"Ode to Joy\""
        "melody (Beethoven, 1824), in C major and 4/4, built in code (MAKE-MELODY).")
  (show (serdes:format-media-type :musicxml) "application/vnd.recordare.musicxml+xml")
  (show (serdes:format-media-type :mxl) "application/vnd.recordare.musicxml")
  (show (list (serdes:format-binary-p :musicxml) (serdes:format-binary-p :mxl)) '(nil t))
  (show (serdes:find-format-for-media-type "application/vnd.recordare.musicxml") :mxl)
  (show (type-of (serdes:find-backend :musicxml)) 'score-backend-musicxml::musicxml-serdes-backend)
  (format t "~%")
  (setf *text* (serdes:encode *melody* :format :musicxml))
  (show (subseq (text-lines *text*) 0 3)
        '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 4.0 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">"
          "<score-partwise version=\"4.0\">"))
  (show (string= *text* (write-musicxml *melody*)) t "what the backend writes")
  (show (score-equal (serdes:decode *text* :format :musicxml) *melody*) t "ids and all")
  (let ((octets (serdes:encode-to-octets *melody* :format :musicxml)))
    (show (length octets) (length (babel:string-to-octets *text* :encoding :utf-8)))
    (show (score-equal (serdes:decode-octets octets :format :musicxml) *melody*) t))
  (show (string= (with-output-to-string (s) (serdes:encode *melody* :format :musicxml :stream s)) *text*)
        t ":STREAM")
  (let ((mxl (serdes:encode *melody* :format :mxl)))
    (show (coerce (subseq mxl 0 4) 'list) '(80 75 3 4) "a ZIP archive")
    (show (score-equal (serdes:decode mxl :format :mxl) *melody*) t))
  (format t "~%")
  (para "Warnings are not errors: serdes leaves them alone, so a handler around ENCODE"
        "gets the backend's own warning. A note with a confidence (MusicXML has no"
        "place for one) gives LOSSY-CONVERSION:")
  (let ((warnings '()))
    (handler-bind ((warning (lambda (w) (push w warnings) (muffle-warning w))))
      (serdes:encode (make-melody :confidence 9/10) :format :musicxml))
    (show (mapcar #'condition-line warnings)
          '("lossy-conversion: lossy conversion to musicxml: provenance and confidence dropped"))))

;;; 2. Errors translated in place ------------------------------------------------------

(defun demo-decode-errors ()
  (para "A MusicXML file with an element the model has no place for: a trill on the"
        "fifth note (WITH-TRILLS inserts the <ornaments> element). Read directly, the"
        "reader signals MUSICXML-UNSUPPORTED inside a SKIP-ELEMENT restart:")
  (let ((trill (with-trills *text* 5))
        (trills (with-trills *text* 5 13)))
    (show (count-if (lambda (line) (search "trill-mark" line)) (text-lines trill)) 1)
    (show-signal (read-musicxml trill) musicxml-unsupported)
    (format t "~%")
    (para "Through serdes the caller sees SERDES-DECODE-ERROR, as in 0.2. What 0.3.0"
          "changes is when and with what. The error is signalled where the reader"
          "signalled, before anything unwinds. Its SERDES-ERROR-CAUSE is the reader's"
          "condition, and the reader's restarts are associated with it. INSIDE-HANDLER"
          "(demo.lisp) looks from a HANDLER-BIND bound outside DECODE:")
    (show-signal (serdes:decode trill :format :musicxml) serdes:serdes-decode-error)
    (show (inside-handler (e serdes:serdes-decode-error) (serdes:decode trill :format :musicxml)
            (let ((cause (serdes:serdes-error-cause e)))
              (list (type-of cause) (musicxml-unsupported-element cause))))
          '(musicxml-unsupported "ornaments") "the cause: the reader's condition")
    (show (inside-handler (e serdes:serdes-decode-error) (serdes:decode trill :format :musicxml)
            (restart-names e 1))
          '(skip-element) "the most recent restart is the reader's")
    (show (inside-handler (e serdes:serdes-decode-error) (serdes:decode trill :format :musicxml)
            (and (find-restart 'skip-element e) t))
          t "found through the condition: associated with it")
    (format t "~%")
    (para "So the caller can take the reader's way out. A handler for"
          "SERDES-DECODE-ERROR that looks at the cause and calls SKIP-ELEMENT reads the"
          "file without the trills, once per trill, in document order. Here there are"
          "trills on the 5th and the 13th note:")
    (let ((skipped '()))
      (let ((score (shown (handler-bind ((serdes:serdes-decode-error
                                           (lambda (e)
                                             (let ((cause (serdes:serdes-error-cause e)))
                                               (when (typep cause 'musicxml-unsupported)
                                                 (push (score-format-error-position cause) skipped)
                                                 (skip-element e))))))
                            (serdes:decode trills :format :musicxml)))))
        (show (length skipped) 2)
        (dolist (position (reverse skipped))
          (format t "  skipped at ~a~%" (code-text position)))
        (show (score-equal score *melody*) t "the melody without its trills, ids and all")
        (show (score-equal score (read-musicxml trills :unsupported :skip)) t
              "the same as the reader's own :SKIP policy")))
    (format t "~%")
    (para "Rules that stay as they were. A handler for the reader's own type, bound"
          "around DECODE, is not called: the outside sees only the SERDES-DECODE-ERROR."
          "Bind SERDES-ERROR and test the cause instead. A HANDLER-CASE still unwinds"
          "first, so the restart is gone by the time its clause runs, but the cause is"
          "kept:")
    (let ((called 0))
      (show (handler-case (handler-bind ((musicxml-unsupported (lambda (c) (declare (ignore c)) (incf called))))
                            (serdes:decode trill :format :musicxml))
              (serdes:serdes-decode-error (e)
                (list called (type-of (serdes:serdes-error-cause e))
                      (and (find-restart 'skip-element e) t))))
            '(0 musicxml-unsupported nil)
            "(handler calls, cause, restart left)"))
    (para ""
          "And the backend called directly (BACKEND-DECODE on FIND-BACKEND) translates"
          "nothing:")
    (show-signal (serdes:backend-decode (serdes:find-backend :musicxml) trill) musicxml-unsupported)))

;;; 3. Encode errors and STORE-VALUE ---------------------------------------------------------

(defun demo-encode-errors ()
  (para "ENCODE works the same way. WRITE-MUSICXML checks its argument with CHECK-TYPE,"
        "which signals a TYPE-ERROR inside a STORE-VALUE restart. Through serdes 0.3.0"
        "the caller gets SERDES-ENCODE-ERROR with the TYPE-ERROR as its cause, and can"
        "answer the writer's STORE-VALUE with the score it meant to pass:")
  (show-signal (serdes:encode "Ode to Joy" :format :musicxml) serdes:serdes-encode-error)
  (show (inside-handler (e serdes:serdes-encode-error) (serdes:encode "Ode to Joy" :format :musicxml)
          (let ((cause (serdes:serdes-error-cause e)))
            (list (typep cause 'type-error) (type-error-datum cause)
                  (and (find-restart 'store-value e) t))))
        '(t "Ode to Joy" t) "(type-error-p datum store-value)")
  (let ((text (shown (handler-bind ((serdes:serdes-encode-error
                                      (lambda (e)
                                        (when (typep (serdes:serdes-error-cause e) 'type-error)
                                          (store-value *melody* e)))))
                       (serdes:encode "Ode to Joy" :format :musicxml)))))
    (show (string= text *text*) t "the melody, written"))
  (show (inside-handler (e serdes:serdes-encode-error) (serdes:encode 42 :format :mxl)
          (type-error-datum (serdes:serdes-error-cause e)))
        42 ":MXL alike"))

;;; 4. Passed through untouched -------------------------------------------------------------

(defun demo-pass-through ()
  (para "A SERDES-ERROR is never wrapped: one that serdes signals itself, or that a"
        "backend signals, reaches the caller as it is, with no cause. An unknown format"
        "is serdes's own:")
  (show-signal (serdes:decode *text* :format :abc) serdes:serdes-unknown-format)
  (show (inside-handler (e serdes:serdes-error) (serdes:decode *text* :format :abc)
          (list (type-of e) (serdes:serdes-error-cause e)))
        '(serdes:serdes-unknown-format nil))
  (format t "~%")
  (para "The reader wraps the errors of the layers below it (XML, ZIP) in"
        "SCORE-FORMAT-ERROR, which serdes translates like any other backend error."
        "Text that is not well-formed XML, and an archive cut short:")
  (show-signal (serdes:decode "<score-partwise version=\"4.0\">" :format :musicxml)
               serdes:serdes-decode-error)
  (show (inside-handler (e serdes:serdes-decode-error)
            (serdes:decode "<score-partwise version=\"4.0\">" :format :musicxml)
          (type-of (serdes:serdes-error-cause e)))
        'score-format-error)
  (let ((short (subseq (serdes:encode *melody* :format :mxl) 0 64)))
    (show-signal (serdes:decode short :format :mxl) serdes:serdes-decode-error)
    (show (inside-handler (e serdes:serdes-decode-error) (serdes:decode short :format :mxl)
            (type-of (serdes:serdes-error-cause e)))
          'score-format-error)))

;;; Running -----------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()))
    (format t "~&serdes-protocol ~a with score-backend-musicxml ~a on score-protocol ~a~%"
            (asdf:component-version (asdf:find-system "serdes-protocol"))
            (asdf:component-version (asdf:find-system "score-backend-musicxml"))
            (asdf:component-version (asdf:find-system "score-protocol")))
    (setf *melody* (make-melody))
    (section "1. Formats and round trips" (demo-round-trips))
    (section "2. Decode errors, translated in place: SKIP-ELEMENT from outside DECODE" (demo-decode-errors))
    (section "3. Encode errors: STORE-VALUE through ENCODE" (demo-encode-errors))
    (section "4. What is passed through" (demo-pass-through))
    (banner "Summary")
    (format t "~d results checked against expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
