;;;; A tour of mime-protocol 0.3.0's multipart codec: the fix for
;;;; egao1980/mime-protocol#9 (a part ending in CRLF, which every nested
;;;; multipart does, made parse-mime signal "multipart boundary desync") and
;;;; the canonical header names on print. Every result with a known answer is
;;;; checked: RUN-DEMOS returns NIL when one differs or a section stops on an
;;;; error.

(in-package #:harness-demos-mime-multipart)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args))

(defun say (&rest lines)
  "Print each of LINES on its own line, indented by two spaces (\"\" is a blank line)."
  (dolist (line lines)
    (if (string= line "")
        (terpri)
        (out "  ~a~%" line))))

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (actual expected label &key (test #'equal))
  "Count one self-check; print it only when it fails. Returns ACTUAL."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (let ((*print-pretty* nil))
      (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected)))
  actual)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

;;; Helpers ------------------------------------------------------------------------

(defun crlf (&rest lines)
  "LINES joined with CRLF."
  (with-output-to-string (s)
    (loop for (line . more) on lines
          do (write-string line s)
             (when more
               (write-char #\Return s)
               (write-char #\Newline s)))))

(defun visible (string)
  "STRING in double quotes, with CR and LF written as \\r and \\n."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across string
          do (case c
               (#\Return (write-string "\\r" o))
               (#\Newline (write-string "\\n" o))
               (t (write-char c o))))
    (write-char #\" o)))

(defun utf8 (octets)
  (encoding-protocol:decode octets))

(defun octets (list)
  (coerce list '(vector (unsigned-byte 8))))

(defun type-string (entity)
  "\"type/subtype\" of ENTITY, or \"(no Content-Type)\": 0.3.0 leaves the type of
a part without the header NIL instead of the RFC 2045 default text/plain
(egao1980/mime-protocol#13)."
  (let ((mt (mime:mime-content-type entity)))
    (if mt
        (format nil "~(~a/~a~)" (mime:media-type-type mt) (mime:media-type-subtype mt))
        "(no Content-Type)")))

(defun tree (entity &optional (depth 0))
  "ENTITY and its parts, depth first, as (depth type filename content) rows.
CONTENT is a string for a text part (or one with no Content-Type), a list of
octets for any other leaf and NIL for a multipart."
  (let* ((cd (mime:mime-content-disposition entity))
         (mt (mime:mime-content-type entity))
         (row (list depth (type-string entity)
                    (and cd (mime:disposition-filename cd))
                    (cond ((mime:multipart-p entity) nil)
                          ((or (null mt) (string-equal "text" (mime:media-type-type mt)))
                           (utf8 (mime:mime-content entity)))
                          (t (coerce (mime:mime-content entity) 'list))))))
    (cons row (loop for part in (mime:mime-parts entity) append (tree part (1+ depth))))))

(defun show-tree (rows)
  (dolist (row rows)
    (destructuring-bind (depth type filename content) row
      (out "    ~va~a~@[  filename ~s~]~@[  ~a~]~%" (* 2 depth) "" type filename
           (cond ((null content) nil)
                 ((stringp content) (visible content))
                 (t (format nil "~d octets" (length content))))))))

(defun wire-lines (wire)
  "WIRE split at CRLF, and the number of LF not preceded by CR."
  (values (uiop:split-string (remove #\Return wire) :separator '(#\Newline))
          (loop for i from 0 below (length wire)
                count (and (char= #\Newline (char wire i))
                           (or (zerop i) (char/= #\Return (char wire (1- i))))))))

(defparameter +text+ (crlf "Hello Bob," "the weekly report is attached." "")
  "The plain-text body. It ends in CRLF, as text bodies do.")

(defparameter +html+ (crlf "<p>Hello Bob,<br>the weekly <b>report</b> is attached.</p>" ""))

(defparameter +pdf+ '(37 80 68 70 45 49 46 55 10 37 226 227 207 211 10 1 2 3 255)
  "The first octets of a PDF file, and a few that are not text.")

(defun mail-layout ()
  "multipart/mixed(multipart/alternative(text, html), pdf), with fixed
boundaries so the printed text is the same on every run."
  (let ((m (mime:make-multipart-entity
            (list (mime:make-multipart-entity
                   (list (mime:make-text-entity +text+)
                         (mime:make-text-entity +html+ :subtype "html"))
                   :subtype "alternative" :boundary "alt-7f3a")
                  (mime:make-binary-entity (octets +pdf+) :subtype "pdf" :filename "report.pdf"))
            :subtype "mixed" :boundary "mixed-91c2")))
    (mime:set-header m "from" "ann@example.com")
    (mime:set-header m "to" "bob@example.com")
    (mime:set-header m "subject" "Weekly report")
    (mime:set-header m "message-id" "<weekly-1@example.com>")
    (mime:set-header m "mime-version" "1.0")
    m))

(defun demo-print ()
  (say "The layout every mail client sends: a text body with an HTML alternative, and"
       "an attachment. Built with make-multipart-entity / make-text-entity /"
       "make-binary-entity and SET-HEADER (names stored lowercase), then PRINT-MIME:"
       "")
  (let ((wire (mime:print-mime (mail-layout))))
    (multiple-value-bind (lines bare-lf) (wire-lines wire)
      (dolist (line lines) (out "    | ~a~%" line))
      (say ""
           (format nil "~d octets, ~d lines, every one ending in CRLF (~d bare LF)."
                   (length wire) (length lines) bare-lf))
      (check bare-lf 0 "no bare LF on the wire"))
    (dolist (header '("MIME-Version: 1.0" "Message-ID: <weekly-1@example.com>"
                      "Content-Type: multipart/mixed; boundary=mixed-91c2"
                      "Content-Transfer-Encoding: base64"))
      (check (and (search header wire) t) t (format nil "the wire holds ~s" header)))))

(defun demo-round-trip ()
  (let* ((original (mail-layout))
         (wire (mime:print-mime original))
         (parsed (mime:parse-mime wire)))
    (say "PARSE-MIME of that text (0.2.0 signalled \"multipart boundary desync\" here,"
         "because the alternative part ends in CRLF):"
         "")
    (show-tree (tree parsed))
    (check (tree parsed) (tree original) "the parsed tree equals the built one")
    (let ((text (fourth (third (tree parsed)))))
      (say ""
           (format nil "the text part keeps its own final CRLF: ~:[no~;yes~]"
                   (equal text +text+)))
      (check text +text+ "text part content, CRLF included"))
    (let ((again (mime:print-mime parsed)))
      (say (format nil "print -> parse -> print gives the same ~d octets: ~:[no~;yes~]"
                   (length again) (string= again wire)))
      (check (string= again wire) t "print is a fixed point"))
    (let ((from (mime:header-value parsed "From"))
          (filename (mime:disposition-filename
                     (mime:mime-content-disposition (second (mime:mime-parts parsed))))))
      (say (format nil "header-value \"From\" -> ~s; the attachment's filename -> ~s" from filename))
      (check (list from filename) '("ann@example.com" "report.pdf") "headers and filename"))))

(defun leaf-texts (entity)
  (loop for (nil nil nil content) in (tree entity)
        when (stringp content) collect content))

(defun demo-edges ()
  (let* ((inner (mime:make-multipart-entity (list (mime:make-text-entity "in")) :boundary "b2"))
         (outer (mime:make-multipart-entity (list inner (mime:make-text-entity "out")) :boundary "b"))
         (rows (tree (mime:parse-mime (mime:print-mime outer)))))
    (say "an inner boundary \"b2\" that has the outer \"b\" as a prefix (\"--b\" inside"
         "\"--b2\" does not end the outer part):")
    (show-tree rows)
    (check (mapcar #'second rows) '("multipart/mixed" "multipart/mixed" "text/plain" "text/plain")
           "prefix boundaries"))
  (say "" "part contents that end, or are made of, line breaks, each round-tripped alone:")
  (dolist (content (list "" "x" (crlf "" "") (crlf "a" "" "b" "") (crlf "--not-the-boundary" "")))
    (let* ((e (mime:make-multipart-entity (list (mime:make-text-entity content)) :boundary "b"))
           (back (first (leaf-texts (mime:parse-mime (mime:print-mime e))))))
      (out "    ~26a -> ~a~%" (visible content) (visible back))
      (check back content (format nil "content ~s" (visible content)))))
  (let* ((wire (format nil "Content-Type: multipart/mixed; boundary=b~%~%~
                            --b~%Content-Type: text/plain~%~%one~%~
                            --b~%Content-Type: text/html~%~%<p>two</p>~%--b--~%"))
         (rows (rest (tree (mime:parse-mime wire)))))
    (say "" "an LF-only message (no CR at all):")
    (show-tree rows)
    (check (mapcar (lambda (r) (list (second r) (fourth r))) rows)
           '(("text/plain" "one") ("text/html" "<p>two</p>")) "LF-only wire"))
  (let* ((wire (crlf "Content-Type: multipart/mixed; boundary=b" ""
                     "This is the preamble." "--b" "" "no headers in this part" "--b--" "the epilogue"))
         (e (mime:parse-mime wire)))
    (say "" "a preamble, a part with no headers (it starts with the blank line) and an epilogue:"
         (format nil "  preamble ~s, part ~s, epilogue ~s"
                 (utf8 (mime:mime-preamble e)) (first (leaf-texts e)) (utf8 (mime:mime-epilogue e)))
         (format nil "  the headerless part's type: ~a (RFC 2045 says text/plain; filed as"
                 (type-string (first (mime:mime-parts e))))
         "  egao1980/mime-protocol#13)")
    (check (list (utf8 (mime:mime-preamble e)) (leaf-texts e) (utf8 (mime:mime-epilogue e)))
           '("This is the preamble." ("no headers in this part") "the epilogue")
           "preamble, headerless part, epilogue")
    (check (string= (mime:print-mime e) (mime:print-mime (mime:parse-mime (mime:print-mime e)))) t
           "preamble and epilogue survive print and parse")))

(defun demo-header-names ()
  (say "The model keeps names lowercase; PRINT-MIME writes them with CANONICAL-HEADER-NAME:")
  (loop for (name expected) in '(("from" "From") ("subject" "Subject") ("mime-version" "MIME-Version")
                                 ("message-id" "Message-ID")
                                 ("content-transfer-encoding" "Content-Transfer-Encoding")
                                 ("content-md5" "Content-MD5") ("dkim-signature" "DKIM-Signature")
                                 ("x-mailer" "X-Mailer"))
        do (let ((got (mime:canonical-header-name name)))
             (out "    ~28a -> ~a~%" name got)
             (check got expected (format nil "canonical name of ~a" name))))
  (let* ((e (mime:parse-mime (mime:print-mime (mail-layout))))
         (values (mapcar (lambda (n) (mime:header-value e n)) '("MIME-Version" "mime-version" "MIME-VERSION"))))
    (say (format nil "lookups stay case-insensitive: header-value MIME-Version / mime-version / MIME-VERSION -> ~s"
                 values))
    (check values '("1.0" "1.0" "1.0") "case-insensitive lookups")))

(defparameter *python-read*
  "import sys, email, email.policy
def s(x):
    return '\"' + x.replace(chr(92), chr(92) * 2).replace('\"', chr(92) + '\"') + '\"'
def rows(p, depth):
    out = []
    fn = p.get_filename()
    if p.is_multipart():
        content = 'nil'
    elif p.get_content_maintype() == 'text':
        content = s(p.get_content())
    else:
        content = '(' + ' '.join(str(b) for b in p.get_content()) + ')'
    out.append('(%d %s %s %s)' % (depth, s(p.get_content_type()), s(fn) if fn else 'nil', content))
    if p.is_multipart():
        for q in p.iter_parts():
            out.extend(rows(q, depth + 1))
    return out
msg = email.message_from_bytes(open(sys.argv[1], 'rb').read(), policy=email.policy.default)
print('(' + s(sys.version.split()[0]) + ' (' + ' '.join(rows(msg, 0)) + '))')
"
  "Python for `python -I -c`: the file named by argv[1] parsed with the email
package, printed as (version rows) in the shape of TREE.")

(defparameter *python-write*
  "import sys, email.policy
from email.message import EmailMessage
nl = chr(10)
m = EmailMessage()
m['From'] = 'bob@example.com'
m['To'] = 'ann@example.com'
m['Subject'] = 'Re: Weekly report'
m.set_content('Thanks Ann,' + nl + 'the numbers look right.' + nl)
m.add_alternative('<p>Thanks Ann,<br>the numbers look right.</p>' + nl, subtype='html')
m.add_attachment(bytes([37, 80, 68, 70, 45, 49, 46, 55, 10, 1, 2, 3, 255]),
                 maintype='application', subtype='pdf', filename='notes.pdf')
open(sys.argv[1], 'wb').write(m.as_bytes(policy=email.policy.SMTP))
"
  "Python for `python -I -c`: an EmailMessage with an HTML alternative and an
attachment, written to argv[1] with CRLF line ends (policy SMTP).")

(defun uv-available-p ()
  (handler-case (zerop (proc:run '("uv" "--version") :output nil :error-output nil :timeout 60))
    (error () nil)))

(defun python (script &rest args)
  "Run SCRIPT under `uv run --no-project python -I -c`. (values code stdout stderr)."
  (proc:run (list* "uv" "run" "--no-project" "python" "-I" "-c" script args)
            :output :string :error-output :string :timeout 600
            :env '(("PYTHONIOENCODING" . "utf-8"))))

(defun scratch-file (name)
  (merge-pathnames (format nil "harness-mime-~36r-~a" (random (expt 36 6)) name)
                   (uiop:temporary-directory)))

(defun read-octets (path)
  (with-open-file (i path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8))))
      (read-sequence v i)
      v)))

(defun python-reads-ours (path)
  (with-open-file (o path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (encoding-protocol:encode (mime:print-mime (mail-layout))) o))
  (multiple-value-bind (code stdout stderr) (python *python-read* (namestring path))
    (check code 0 "uv run exit code (read)")
    (if (/= code 0)
        (out "  ~a~%" stderr)
        (destructuring-bind (version rows)
            (let ((*read-eval* nil)
                  (*package* (find-package '#:harness-demos-mime-multipart)))
              (read-from-string stdout))
          (say (format nil "Python ~a: email.message_from_bytes(policy=default) of the section 1 text:"
                       version))
          (show-tree rows)
          (check rows (tree (mail-layout)) "Python sees the same tree, contents included")))))

(defun we-read-pythons (path)
  (multiple-value-bind (code stdout stderr) (python *python-write* (namestring path))
    (declare (ignore stdout))
    (check code 0 "uv run exit code (write)")
    (if (/= code 0)
        (out "  ~a~%" stderr)
        (let* ((bytes (read-octets path))
               (parsed (mime:parse-mime bytes))
               (rows (tree parsed)))
          (say ""
               "Python's EmailMessage (set_content, add_alternative, add_attachment),"
               (format nil "as_bytes(policy=SMTP), ~d octets, read by PARSE-MIME:" (length bytes)))
          (show-tree rows)
          (check (mapcar #'second rows)
                 '("multipart/mixed" "multipart/alternative" "text/plain" "text/html" "application/pdf")
                 "the layout of Python's message")
          (check (fourth (third rows)) (crlf "Thanks Ann," "the numbers look right." "")
                 "Python's text part")
          (check (list (third (fifth rows)) (fourth (fifth rows)))
                 '("notes.pdf" (37 80 68 70 45 49 46 55 10 1 2 3 255))
                 "Python's attachment")
          (check (mime:header-value parsed "subject") "Re: Weekly report" "Python's Subject")))))

(defun demo-python ()
  (if (not (uv-available-p))
      (say "uv is not on PATH, so the cross-read with Python's email package is skipped.")
      (let ((ours (scratch-file "ours.eml"))
            (theirs (scratch-file "theirs.eml")))
        (unwind-protect
            (progn (python-reads-ours ours)
              (we-read-pythons theirs))
          (dolist (f (list ours theirs))
            (when (probe-file f) (delete-file f)))))))

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 110)
        (*random-state* (make-random-state t)))
    (format t "~&mime-protocol ~a (~a ~a, ~a): nested multiparts and header names~%"
            (asdf:component-version (asdf:find-system "mime-protocol"))
            (lisp-implementation-type) (lisp-implementation-version) (software-type))
    (section "1. The mail layout, printed" (demo-print))
    (section "2. Parsed back: the same tree, the same text (#9)" (demo-round-trip))
    (section "3. Edge cases: prefix boundaries, line breaks, LF-only, preamble" (demo-edges))
    (section "4. Canonical header names on print, case-insensitive lookups" (demo-header-names))
    (section "5. Cross-read with Python's email package, both ways" (demo-python))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
