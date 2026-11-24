(in-package #:harness-demos-score-parity)

;;;; A tour of score-parity 0.1.0, slice SC5 of the score family: a corpus of
;;;; 34 MusicXML files of 30 public-domain tunes, and the checks every
;;;; score-protocol format backend must pass on it. The sections cover the full
;;;; matrix (1), one check up close (2), a deliberate failure and an error kept
;;;; as data (3), and the two external oracles (4). Every result with a known
;;;; answer is also checked: RUN-DEMOS returns NIL when one differs or a
;;;; section stops on an error.

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  "Print LINES of prose, then a blank line."
  (format t "~&~{~a~%~}~%" lines))

(defun out (control &rest args)
  "FORMAT to *STANDARD-OUTPUT* with the padding at the end of each line removed."
  (let ((text (apply #'format nil control args)))
    (with-input-from-string (in text)
      (loop for (line missing-newline-p) = (multiple-value-list (read-line in nil))
            while line
            do (write-string (string-right-trim " " line))
               (unless missing-newline-p (terpri))))))

(defun code-text (object)
  "OBJECT printed as it reads in this package, which uses SCORE-PARITY."
  (let ((*package* (find-package '#:harness-demos-score-parity))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 1000))
    (prin1-to-string object)))

(defvar *xsd-directory* nil "The directory of MUSICXML_XSD, printed as <xsd-dir>/.")

(defun replace-all (text old new)
  (with-output-to-string (s)
    (loop with start = 0
          for hit = (search old text :start2 start)
          do (write-string text s :start start :end (or hit (length text)))
          while hit
          do (write-string new s)
             (setf start (+ hit (length old))))))

(defun masked (text)
  "TEXT with the XSD directory written as <xsd-dir>/ and the home directory as ~/."
  (let ((text (if *xsd-directory* (replace-all text *xsd-directory* "<xsd-dir>/") text)))
    (replace-all text (uiop:native-namestring (user-homedir-pathname)) "~/")))

(defun same-p (a b)
  "EQUALP, except that strings compare case-sensitively."
  (typecase a
    (string (and (stringp b) (string= a b)))
    (cons (and (consp b) (same-p (car a) (car b)) (same-p (cdr a) (cdr b))))
    (t (equalp a b))))

(defun check (value expected what)
  "Count a check of VALUE against EXPECTED; print a line only when they differ."
  (incf *checks*)
  (unless (same-p value expected)
    (incf *mismatches*)
    (format t "~&  !! ~a: got ~a, expected ~a~%" what (code-text value) (code-text expected)))
  value)

(defmacro show (form &optional (expected nil expected-p) note)
  "Print FORM => its value like a REPL transcript, on two lines when long; check
the value against EXPECTED when one is given. NOTE is a comment after the value."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (let ((ftext (code-text ',form))
             (vtext (masked (format nil "~a~@[   ; ~a~]" (code-text ,value) ,note))))
         (if (<= (+ 2 (length ftext) 4 (length vtext)) 84)
             (format t "~&  ~a => ~a~%" ftext vtext)
             (format t "~&  ~a~%    => ~a~%" ftext vtext)))
       ,@(when expected-p `((check ,value ,expected (code-text ',form))))
       ,value)))

(defmacro section (title &body body)
  "Print a banner, run BODY, and record the section as failed if it signals."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (e)
         (push ,title *failed-sections*)
         (format t "~&!! unexpected ~s, the rest of this section is skipped:~%   ~a~%"
                 (type-of e) (masked (princ-to-string e)))))))

(defun one-line (text)
  "TEXT with each run of blanks and newlines made one space."
  (format nil "~{~a~^ ~}"
          (remove "" (uiop:split-string text :separator '(#\Space #\Newline #\Tab #\Return))
                  :test #'string=)))

;;; Results ---------------------------------------------------------------------

(defvar *results* '() "PARITY-MATRIX over the whole corpus (section 1).")
(defvar *schema* nil "(MUSICXML-SCHEMA) of this run.")
(defvar *musescore* nil "(FIND-MUSESCORE) of this run.")

(defun check-label (check)
  (getf (rest (assoc check +checks+)) :label))

(defun all-checks () (mapcar #'first +checks+))

(defun result-of (results check piece)
  (find-if (lambda (r) (and (eq (parity-result-check r) check)
                            (equal (parity-result-piece r) piece)))
           results))

(defun status-word (result)
  "A matrix cell for one piece: pass, FAIL, ERROR, skip, or - when the check
does not apply to the piece."
  (if (null result)
      "-"
      (ecase (parity-result-status result)
        (:pass "pass") (:fail "FAIL") (:error "ERROR") (:skip "skip"))))

(defun column-width (check)
  (max 6 (+ 2 (length (check-label check)))))

(defun summary-cell (cell)
  "A cell of a MATRIX-CELLS row as PRINT-MATRIX writes it: P/N, skip or -."
  (destructuring-bind (check pass fail error skip) cell
    (declare (ignore check))
    (let ((n (+ pass fail error)))
      (cond ((plusp n) (format nil "~d/~d" pass n))
            ((plusp skip) "skip")
            (t "-")))))

(defun print-piece-matrix (results pieces checks)
  "One line per piece, one column per check, then the :ALL row of MATRIX-CELLS."
  (out "~&  ~21a~{~a~}~%" "piece"
       (mapcar (lambda (c) (format nil "~va" (column-width c) (check-label c))) checks))
  (dolist (piece pieces)
    (out "  ~21a~{~a~}~%" piece
         (mapcar (lambda (c) (format nil "~va" (column-width c)
                                     (status-word (result-of results c piece))))
                 checks)))
  (out "  ~a~%" (make-string (+ 21 (reduce #'+ (mapcar #'column-width checks))) :initial-element #\-))
  (let ((all (rest (assoc :all (matrix-cells results)))))
    (out "  ~21a~{~a~}~%" "all"
         (mapcar (lambda (c) (format nil "~va" (column-width c) (summary-cell (assoc c all))))
                 checks))))

(defun call-with-replaced-function (symbol wrapper thunk)
  "Run THUNK while SYMBOL's global function is (lambda (&rest args) (apply
WRAPPER original args)), then put the original back. score-parity reaches the
backends by name at call time (CALL-BACKEND), so a check run meanwhile calls the
wrapper."
  (let ((original (fdefinition symbol)))
    (setf (fdefinition symbol) (lambda (&rest args) (apply wrapper original args)))
    (unwind-protect (funcall thunk)
      (setf (fdefinition symbol) original))))

;;; 1. The matrix -------------------------------------------------------------------

(defun demo-matrix ()
  (para "The corpus is 34 MusicXML files of 30 public-domain tunes, re-encoded for"
        "score-parity (corpus/PROVENANCE.md): lead sheets with chord symbols and"
        "lyrics, four-part hymns hand-split and in hymnal layout, several verses,"
        "pickups, repeats and endings, D.C. and D.S. jumps, ties, tuplets, grace notes,"
        "key and meter changes. Two of them also come as .mxl archives written the way"
        "other programs write them. corpus/facts.sexp holds what each file contains,"
        "computed by a second implementation of the facet rules and checked with music21.")
  (show (length (corpus-pieces)) 34)
  (show (hymn-pieces) '("adeste-fideles" "hark-the-herald" "hymn-shared" "hymn-split" "hymn-to-joy"
                        "hymn-to-joy-shared" "new-britain" "old-hundredth" "old-hundredth-shared"))
  (show (archive-pieces) '("adeste-fideles" "jingle-bells"))
  (show (mapcar #'first +checks+)
        '(:musicxml :mxl :hymnal-split :midi :midi-unfold :musescore :xsd))
  (para ""
        "PARITY-MATRIX runs every check on every piece it applies to (the hymnal split on"
        "the nine hymns only) and returns one PARITY-RESULT each. The external tools are"
        "looked up once, by FIND-MUSESCORE and MUSICXML-SCHEMA. Per piece and check:")
  (setf *results* (parity-matrix))
  (show (length *results*) (+ (* 6 34) 9) "6 checks x 34 pieces, and 9 hymnal splits")
  (format t "~%")
  (print-piece-matrix *results* (corpus-pieces) (all-checks))
  (let ((all (rest (assoc :all (matrix-cells *results*)))))
    (dolist (c '(:musicxml :mxl :midi :midi-unfold))
      (check (assoc c all) (list c 34 0 0 0) (format nil "all ~(~s~)" c)))
    (check (assoc :hymnal-split all) '(:hymnal-split 9 0 0 0) "all :hymnal-split")
    (unless *musescore*
      (check (assoc :musescore all) '(:musescore 0 0 0 34) "all :musescore"))
    (check (assoc :xsd all) (if *schema* '(:xsd 34 0 0 0) '(:xsd 0 0 0 34)) "all :xsd"))
  (format t "~%")
  (let ((skips (remove-duplicates
                (loop for r in *results*
                      when (eq (parity-result-status r) :skip)
                        collect (cons (parity-result-check r) (parity-result-reason r)))
                :test #'equal)))
    (dolist (skip skips)
      (out "  ~a skipped: ~a~%" (check-label (car skip)) (cdr skip))))
  (para ""
        "MATRIX-CELLS counts the same results per notation feature (PASS FAIL ERROR SKIP"
        "per check, over the pieces that have the feature), and PRINT-MATRIX prints them,"
        "a P/N cell for N pieces run. The last row, all, is every piece. A failing or"
        "erroring cell would be listed under the table; there is none:")
  (let ((text (with-output-to-string (s) (print-matrix *results* s))))
    (out "~{  ~a~%~}" (uiop:split-string (string-right-trim '(#\Newline) text)
                                          :separator '(#\Newline)))
    (check (count-if (lambda (r) (member (parity-result-status r) '(:fail :error))) *results*)
           0 "failures and errors in the corpus matrix")))

;;; 2. One check up close -----------------------------------------------------------

(defun row-lines (rows &key (limit 6) (indent 6))
  "Print the first LIMIT ROWS, one per line, and say how many are left out."
  (loop for row in rows
        for i from 0
        while (< i limit)
        do (out "~a~a~%" (make-string indent :initial-element #\Space) (code-text row)))
  (when (> (length rows) limit)
    (out "~a... ~d more~%" (make-string indent :initial-element #\Space) (- (length rows) limit))))

(defun demo-one-check ()
  (let* ((piece "amazing-grace")
         (s (read-piece piece)))
    (para "The :midi check on one piece, step by step. The piece is the lead sheet of"
          "Amazing Grace (tune New Britain, 1829; text John Newton, 1779): one voice, two"
          "verses, chord symbols and a pickup. READ-PIECE reads it with"
          "score-backend-musicxml; the facets turn the score into plain lists that two"
          "formats can be compared on. Each is checked against corpus/facts.sexp.")
    (show (piece-fact piece :title) "Amazing Grace")
    (show (score-features s) '(:lyrics :verses :chord-symbols :lead-sheet :pickup))
    (check (score-features s) (piece-features piece) "features = facts")
    (show (bar-lengths s) (piece-fact piece :bars) "a 1/4 pickup, 3/4 bars, a short last bar")
    (format t "~%  Voice lines: per voice, one row (onset end key pitch) per sounding note, in~%~
               ~2@Twhole notes from the first bar; a tie chain is one row.~%")
    (let ((lines (voice-lines s)))
      (show (length lines) 1)
      (show (length (first lines)) 17)
      (row-lines (first lines))
      (check (voice-lines s) (piece-fact piece :lines) "voice lines = facts"))
    (format t "~%  Lyric lines: per voice line, (onset verse text syllabic) for each lyric.~%")
    (let ((lyrics (first (lyric-lines s))))
      (show (length lyrics) 28 "14 sung syllables in each of two verses")
      (row-lines lyrics)
      (check (lyric-lines s) (piece-fact piece :lyrics) "lyric lines = facts"))
    (format t "~%  Chord symbols: (onset text), the text as music-theory prints it (:pop).~%")
    (let ((chords (chord-symbol-rows s)))
      (row-lines chords :limit 8)
      (show (mapcar #'first (chord-symbol-rows s)) (piece-fact piece :chord-onsets)
            "the chord onsets of the facts"))
    (para ""
          "Now the check itself: the score out to a Standard MIDI File and back, as"
          "written (no unfolding), each LOSSY-CONVERSION muffled as the check does it.")
    (let* ((lossy (list '() '()))
           (octets (handler-bind ((score-protocol:lossy-conversion
                                    (lambda (c) (push c (first lossy)) (muffle-warning c))))
                     (score-backend-midi:export-midi s nil :unfold nil)))
           (m (handler-bind ((score-protocol:lossy-conversion
                               (lambda (c) (push c (second lossy)) (muffle-warning c))))
                (score-backend-midi:import-midi octets))))
      (out "  (export-midi s nil :unfold nil) => ~d octets~%" (length octets))
      (dolist (c (reverse (first lossy)))
        (out "    lossy-conversion, muffled: ~a~%" (one-line (princ-to-string c))))
      (out "  (import-midi octets) => a score of ~d voice line~:p~%" (length (voice-lines m)))
      (dolist (c (reverse (second lossy)))
        (out "    lossy-conversion, muffled: ~a~%" (one-line (princ-to-string c))))
      (format t "~%")
      (out "  facet           in +midi-facets+   score and MIDI agree~%")
      (dolist (facet +content-facets+)
        (out "  ~16a~19a~a~%" (code-text facet)
             (if (member facet +midi-facets+) "yes" "no")
             (if (equal (content-facet s facet) (content-facet m facet)) "yes" "no")))
      (format t "~%")
      (show +midi-facets+ '(:notes :verse-1 :chord-symbols :bars))
      (show (content-differences s m :facets +midi-facets+) '())
      (let ((all (content-differences s m)))
        (show (mapcar #'first (content-differences s m)) '(:lyrics))
        (let ((theirs (third (assoc :lyrics all))))
          (out "  MIDI holds one verse: the imported lyric line has ~d rows, all verse ~{~d~^, ~}.~%"
               (length (first theirs))
               (remove-duplicates (mapcar #'second (first theirs))))))
      (para ""
            "That is why +MIDI-FACETS+ compares verse 1 (with its hyphens) and not :lyrics."
            "RUN-CHECK does all of the above and returns the result:")
      (let ((r (run-check :midi piece)))
        (show (parity-result-status r) :pass)
        (show (parity-result-reason r) nil)
        (show (parity-result-differences r) '())))))

;;; 3. A failure, and an error kept as data ----------------------------------------

(defun first-pitch (text)
  "The first <pitch> element of the MusicXML TEXT, as written."
  (let* ((start (search "<pitch>" text))
         (end (+ (search "</pitch>" text :start2 start) (length "</pitch>"))))
    (values (subseq text start end) start)))

(defun corrupt-first-step (text)
  "TEXT with the <step> of its first <pitch> moved from D to E."
  (multiple-value-bind (element start) (first-pitch text)
    (declare (ignore element))
    (let ((step (search "<step>D</step>" text :start2 start)))
      (concatenate 'string (subseq text 0 step) "<step>E</step>"
                   (subseq text (+ step (length "<step>D</step>")))))))

(defun first-difference (a b)
  "(k row-a row-b): the first differing row of two facet values, K the 1-based
line, or NIL."
  (loop for la in a
        for lb in b
        for k from 1
        do (loop for ra in la
                 for rb in lb
                 unless (equal ra rb)
                   do (return-from first-difference (list k ra rb)))))

(defun demo-failure ()
  (let ((piece "amazing-grace"))
    (para "The :musicxml check reads a piece, writes it, reads the text back and writes it"
          "again: the score must come back SCORE-EQUAL and the second text must be the"
          "first. To see it fail, the writer is wrapped for one run so that the first"
          "text it returns has one written pitch changed, in memory: the pickup note D4"
          "becomes E4. The corpus file is not touched.")
    (let ((written (score-backend-musicxml:write-musicxml (read-piece piece))))
      (out "  first <pitch> as written~%    ~a~%"
           (one-line (first-pitch written)))
      (out "  first <pitch> after the corruption~%    ~a~%~%"
           (one-line (first-pitch (corrupt-first-step written)))))
    (let* ((fresh t)
           (r (call-with-replaced-function
               'score-backend-musicxml:write-musicxml
               (lambda (original &rest args)
                 (let ((text (apply original args)))
                   (if (and fresh (stringp text))
                       (progn (setf fresh nil) (corrupt-first-step text))
                       text)))
               (lambda () (run-check :musicxml piece)))))
      (show (parity-result-status r) :fail)
      (show (parity-result-reason r) "read, write, read is not SCORE-EQUAL")
      (show (mapcar #'first (parity-result-differences r)) '(:notes :spelled))
      (dolist (d (parity-result-differences r))
        (destructuring-bind (facet a b) d
          (destructuring-bind (k ra rb) (first-difference a b)
            (out "    ~(~s~), line ~d: ~a became ~a~%" facet k (code-text ra) (code-text rb)))))
      (check (first-difference (second (assoc :notes (parity-result-differences r)))
                               (third (assoc :notes (parity-result-differences r))))
             '(1 (0 1/4 62) (0 1/4 64)) "the :notes difference")
      (para ""
            "The other facets agree, so they are not listed: the lyrics and chord symbols"
            "still sit at the same onsets. With the writer put back, the same check passes:")
      (show (parity-result-status (run-check :musicxml piece)) :pass)
      (para ""
            "An error inside a check is a finding, not a crash. RUN-CHECK turns any ERROR"
            "into a result with status :ERROR and the condition's report as the reason"
            "(BACKEND-MISSING into :SKIP). Here the MIDI exporter is wrapped so that its file"
            "for Jingle Bells is cut in half; the importer then signals SCORE-FORMAT-ERROR."
            "The matrix over three pieces and both MIDI checks runs to the end:")
      (let ((results (call-with-replaced-function
                      'score-backend-midi:export-midi
                      (lambda (original score &rest args)
                        (let ((octets (apply original score args)))
                          (if (equal (getf (score-protocol:score-metadata score) :title) "Jingle Bells")
                              (subseq octets 0 (floor (length octets) 2))
                              octets)))
                      (lambda ()
                        (parity-matrix :pieces '("amazing-grace" "jingle-bells" "twinkle-twinkle")
                                       :checks '(:midi :midi-unfold))))))
        (show (length results) 6)
        (show (mapcar #'parity-result-status results) '(:pass :error :pass :pass :error :pass))
        (show (parity-result-reason (result-of results :midi "jingle-bells")))
        (format t "~%")
        (print-piece-matrix (cons r results) '("amazing-grace" "jingle-bells" "twinkle-twinkle")
                            '(:musicxml :midi :midi-unfold))
        (para ""
              "PRINT-MATRIX lists every :FAIL and :ERROR under its table, in result order"
              "(here with the failed :musicxml result of above added; feature rows cut):")
        (let* ((text (with-output-to-string (s) (print-matrix (cons r results) s)))
               (lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline)))
               (tail (member "all" lines :test (lambda (a b) (eql 0 (search a b))))))
          (out "  ~a~%  ...~%~{  ~a~%~}" (first lines) tail)
          (check (length (rest tail)) 3 "fail and error lines under the table"))
        (para ""
              "Only a call the harness cannot read as a check is the caller's error: an"
              "unknown check is a TYPE-ERROR from RUN-CHECK itself.")
        (handler-case (progn (run-check :lilypond piece)
                             (check :returned :type-error "(run-check :lilypond ...)"))
          (type-error (c)
            (out "  (run-check :lilypond \"amazing-grace\")~%    -> type-error: ~a~%"
                 (one-line (princ-to-string c)))))))))

;;; 4. The oracles ----------------------------------------------------------------

(defun demo-oracles ()
  (para "Two checks ask tools outside Lisp, run through process-protocol. Neither is"
        "required: without the tool the check is skipped and says why.")
  (para "MuseScore re-exports our MusicXML (mscore -o out.musicxml in.musicxml), and the"
        "result must keep the spelled pitches, the lyrics of every verse and the bar"
        "lengths. FIND-MUSESCORE takes MUSESCORE_BIN, else the first of these names on"
        "PATH:")
  (show +musescore-names+ '("mscore" "musescore" "MuseScore4" "mscore4"))
  (show (find-musescore :bin "/no/such/mscore") nil "a wrong MUSESCORE_BIN is not overridden by PATH")
  (cond (*musescore*
         (out "  (find-musescore) => ~a~%" (masked *musescore*))
         (let ((r (run-check :musescore "amazing-grace")))
           (show (parity-result-status r))
           (show (parity-result-reason r))))
        (t
         (show (find-musescore) nil "not installed here")
         (let ((r (run-check :musescore "amazing-grace")))
           (show (parity-result-status r) :skip)
           (show (parity-result-reason r)
                 "MuseScore not found: set MUSESCORE_BIN, or put mscore, musescore or MuseScore4 on PATH"))
         (check (every (lambda (r) (eq (parity-result-status r) :skip))
                       (remove-if-not (lambda (r) (eq (parity-result-check r) :musescore)) *results*))
                t "every :musescore result of section 1 skipped")))
  (para ""
        "The XSD check validates what WRITE-MUSICXML writes against the MusicXML 4.0 XSD"
        "with xmllint --nonet (no network: the schema's catalog maps its imports to the"
        "local copies). MUSICXML_XSD names a local musicxml.xsd; the schema is not part"
        "of the corpus. MUSICXML-SCHEMA is NIL without it or without xmllint:")
  (show (musicxml-schema :xsd "") nil)
  (show (musicxml-schema :xsd "/no/such/musicxml.xsd") nil)
  (cond
    (*schema*
     (show (musicxml-schema) *schema*)
     (let ((xsd (remove-if-not (lambda (r) (eq (parity-result-check r) :xsd)) *results*)))
       (show (count :pass xsd :key #'parity-result-status) 34
             "the xsd column of section 1: every written file valid"))
     (para ""
           "A file the schema rejects gives :FAIL with xmllint's message as the reason."
           "XMLLINT-VALIDATE on the written Amazing Grace with the pitch step H:")
     (let* ((written (score-backend-musicxml:write-musicxml (read-piece "amazing-grace")))
            (bad (replace-all written "<step>D</step>" "<step>H</step>")))
       (multiple-value-bind (valid message) (xmllint-validate bad *schema*)
         (show valid nil)
         (let ((first-line (first (uiop:split-string message :separator '(#\Newline)))))
           (out "  message, first line: ~a~%" (masked first-line))
           (check (and (search "step" first-line) t) t "the message names the element")))))
    (t
     (show (musicxml-schema) nil "MUSICXML_XSD unset")
     (let ((r (run-check :xsd "amazing-grace")))
       (show (parity-result-status r) :skip)
       (show (parity-result-reason r) "MUSICXML_XSD unset, or xmllint missing"))
     (para ""
           "So the xsd column of section 1 is skip on all 34 pieces. Set MUSICXML_XSD to"
           "the musicxml.xsd of github.com/w3c/musicxml, schema/ at tag v4.0 (with"
           "xml.xsd, xlink.xsd and catalog.xml beside it), and the column fills in."))))

;;; The tour -------------------------------------------------------------------------

(defun system-version (name)
  (asdf:component-version (asdf:find-system name)))

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let* ((*checks* 0) (*mismatches* 0) (*failed-sections* '()) (*results* '())
         (*schema* (musicxml-schema))
         (*musescore* (find-musescore))
         (*xsd-directory* (and *schema*
                               (uiop:native-namestring
                                (uiop:pathname-directory-pathname (first *schema*))))))
    (format t "~&score-parity ~a (slice SC5: the cross-format parity corpus and matrix)~%~
               on score-protocol ~a, score-backend-musicxml ~a, score-backend-midi ~a~%~
               and music-theory ~a~%"
            (system-version "score-parity") (system-version "score-protocol")
            (system-version "score-backend-musicxml") (system-version "score-backend-midi")
            (system-version "music-theory"))
    (format t "MuseScore: ~:[not found~;found~]. MusicXML XSD: ~:[not set~;set, with xmllint~].~%"
            *musescore* *schema*)
    (section "1. The matrix: every check on every piece" (demo-matrix))
    (section "2. One check up close: :midi on Amazing Grace" (demo-one-check))
    (section "3. A deliberate failure, and an error kept as data" (demo-failure))
    (section "4. The oracles: MuseScore and the MusicXML XSD" (demo-oracles))
    (banner "Summary")
    (format t "~d results checked against expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
