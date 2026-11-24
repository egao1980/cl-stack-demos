;;;; A tour of mime-protocol 0.3.0's multipart codec: the fix for
;;;; egao1980/mime-protocol#9 (a part ending in CRLF, which every nested
;;;; multipart does, made parse-mime signal "multipart boundary desync") and
;;;; the canonical header names on print. Every result with a known answer is
;;;; checked: RUN-DEMOS returns NIL when one differs or a section stops on an
;;;; error.

;;;; Sections 6 and 7 add mime-protocol 0.3.1, the fix for
;;;; egao1980/mime-protocol#14: multipart/form-data part headers exactly as a
;;;; browser sends them (UTF-8 names, a literal backslash, %22 %0D %0A),
;;;; cross-read with python-multipart and werkzeug.

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
  "\"type/subtype\" of ENTITY, or \"(no Content-Type)\": mime-protocol (0.3.1
too) leaves the type of a part without the header NIL instead of the RFC 2045
default text/plain (egao1980/mime-protocol#13)."
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

(defun wire-octets (&rest pieces)
  "PIECES as octets: a string is UTF-8, an integer one raw octet, :CRLF CR LF."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (piece pieces (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase piece
        ((eql :crlf) (vector-push-extend 13 out) (vector-push-extend 10 out))
        (integer (vector-push-extend piece out))
        (string (loop for b across (encoding-protocol:encode piece :encoding :utf-8)
                      do (vector-push-extend b out)))))))

(defparameter +browser-parts+
  (list (list "form-data; name=\"имя\"" nil "Prélude")
        (list "form-data; name=\"u\"; filename=\"Prélude.svg\"" "image/svg+xml" "<svg/>")
        (list "form-data; name=\"w\"; filename=\"a\\b.svg\"" "image/svg+xml" "<svg/>")
        (list "form-data; name=\"q\"; filename=\"say %22hi%22.svg\"" "image/svg+xml" "<svg/>")
        (list "form-data; name=\"a%0D%0Ab\"" nil "x"))
  "A form as Chrome and Firefox post it, one (Content-Disposition Content-Type
body) per part: a text input named имя holding Prélude; file inputs holding
Prélude.svg, a\\b.svg (a legal name on macOS and Linux) and say \"hi\".svg; and
a field added with FormData.append(\"a\\nb\", \"x\"). The HTML multipart/form-data
encoding writes names as UTF-8, sends no filename*, and escapes only \" CR LF,
as %22 %0D %0A (a lone LF first becomes CR LF).")

(defparameter +browser-expected+
  (list '("имя" nil) '("u" "Prélude.svg") '("w" "a\\b.svg") '("q" "say \"hi\".svg")
        (list (crlf "a" "b") nil))
  "(name filename) of each browser part, as the user typed or chose them.")

(defun browser-body ()
  "+BROWSER-PARTS+ as the octets of a multipart/form-data body, boundary b."
  (apply #'wire-octets
         (append (loop for (disposition type body) in +browser-parts+
                       append (append (list "--b" :crlf "Content-Disposition: " disposition :crlf)
                                      (when type (list "Content-Type: " type :crlf))
                                      (list :crlf body :crlf)))
                 (list "--b--" :crlf))))

(defun browser-request ()
  "BROWSER-BODY after the Content-Type header of the request that carries it."
  (concatenate '(vector (unsigned-byte 8))
               (wire-octets "Content-Type: multipart/form-data; boundary=b" :crlf :crlf)
               (browser-body)))

(defun octet-lines (octets)
  "OCTETS split at CRLF: ASCII as is, any other octet as \\xHH."
  (let ((lines (uiop:split-string
                (with-output-to-string (o)
                  (loop for b across octets
                        do (cond ((= b 13))
                                 ((< b 128) (write-char (code-char b) o))
                                 (t (format o "\\x~2,'0X" b)))))
                :separator '(#\Newline))))
    (if (equal "" (car (last lines))) (butlast lines) lines)))

(defun name-rows (entity)
  "(name filename) of each part of ENTITY."
  (mapcar (lambda (part)
            (let ((cd (mime:mime-content-disposition part)))
              (list (mime:disposition-name cd) (mime:disposition-filename cd))))
          (mime:mime-parts entity)))

(defun pair-string (row)
  (destructuring-bind (name filename) row
    (format nil "~a~@[ / ~a~]" (visible name) (and filename (visible filename)))))

(defun show-names (rows)
  (dolist (row rows)
    (out "    ~a~%" (pair-string row))))

(defun form-request (&rest disposition)
  "A one-part multipart/form-data request whose Content-Disposition value is
the WIRE-OCTETS pieces DISPOSITION."
  (apply #'wire-octets
         (append (list "Content-Type: multipart/form-data; boundary=b" :crlf :crlf
                       "--b" :crlf "Content-Disposition: ")
                 disposition
                 (list :crlf :crlf "x" :crlf "--b--" :crlf))))

(defun demo-browser ()
  (say "A form posted by Chrome or Firefox: a text input named имя, file inputs holding"
       "Prélude.svg, a\\b.svg and say \"hi\".svg, and a FormData field named \"a\\nb\". The"
       "body exactly as sent, octets above 7F written as \\xHH:"
       "")
  (let* ((parsed (mime:parse-mime (browser-request)))
         (rows (name-rows parsed)))
    (dolist (line (octet-lines (browser-body))) (out "    | ~a~%" line))
    (say "" "PARSE-MIME, then disposition-name / disposition-filename of each part:" "")
    (show-names rows)
    (check rows +browser-expected+ "the names the user typed or chose")
    (check (utf8 (mime:mime-content (first (mime:mime-parts parsed)))) "Prélude"
           "the text field's value")
    (check (loop for part in (mime:mime-parts parsed)
                 thereis (mime:disposition-parameter (mime:mime-content-disposition part) "filename*"))
           nil "a browser sends no filename*"))
  (let ((latin-1 (encoding-protocol:decode (wire-octets "Prélude.svg") :encoding :iso-8859-1))
        (quoted-pair (mime:disposition-filename
                      (mime:parse-content-disposition "attachment; filename=\"a\\b.svg\""))))
    (say ""
         "0.3.0 read the header octets as ISO-8859-1 and a backslash as an RFC 822"
         "quoted-pair. Both readings, as they still apply elsewhere:"
         (format nil "  the UTF-8 octets of Prélude.svg read as ISO-8859-1  -> ~s" latin-1)
         (format nil "  attachment; filename=\"a\\b.svg\" (quoted-pairs stay) -> ~s" quoted-pair))
    (check latin-1 "PrÃ©lude.svg" "the ISO-8859-1 reading of UTF-8")
    (check quoted-pair "ab.svg" "a quoted-pair outside form-data names"))
  (let ((filename (second (first (name-rows (mime:parse-mime
                                             (form-request "form-data; name=\"f\"; filename=\"caf"
                                                           #xE9 ".txt\"")))))))
    (say ""
         (format nil "a field whose octets are not UTF-8 stays ISO-8859-1: filename=\"caf\\xE9.txt\" -> ~s"
                 filename))
    (check filename "café.txt" "the ISO-8859-1 fallback"))
  (let* ((value (mime:format-form-data-disposition "file" :filename "Prélude.svg"))
         (cd (mime:mime-content-disposition
              (first (mime:mime-parts (mime:parse-mime (form-request value)))))))
    (say ""
         "Our own encoders. http-protocol's multipart writes FORMAT-FORM-DATA-DISPOSITION"
         "as UTF-8; it adds filename*, which still wins:"
         (format nil "  ~a" value)
         (format nil "  -> filename ~s, filename* ~s, disposition-filename ~s"
                 (mime:disposition-parameter cd "filename")
                 (mime:disposition-parameter cd "filename*")
                 (mime:disposition-filename cd)))
    (check (list (mime:disposition-parameter cd "filename") (mime:disposition-filename cd))
           '("Prélude.svg" "Prélude.svg") "format-form-data-disposition parsed back"))
  (let* ((wire (mime:print-mime
                (mime:make-form-data '(("w" :filename "a\\b.svg" :content "<svg/>")
                                       ("q" :filename "say \"hi\".svg" :content "<svg/>"))
                                     :boundary "b")))
         (rows (name-rows (mime:parse-mime wire))))
    (say "" "make-form-data + print-mime quotes them the browser way too:")
    (dolist (line (uiop:split-string (remove #\Return wire) :separator '(#\Newline)))
      (when (search "Content-Disposition" line)
        (out "    | ~a~%" line)))
    (show-names rows)
    (check rows '(("w" "a\\b.svg") ("q" "say \"hi\".svg")) "make-form-data round trip")))

(defparameter *python-form*
  "import sys, importlib.metadata as md
from python_multipart import MultipartParser
from python_multipart.multipart import parse_options_header
from werkzeug.sansio.multipart import MultipartDecoder, Field, File, Epilogue, NeedData
def s(x):
    return 'nil' if x is None else '\"' + x.replace(chr(92), chr(92) * 2).replace('\"', chr(92) + '\"') + '\"'
body = open(sys.argv[1], 'rb').read()
pm = []
cur = {}
def on_header_field(d, a, b): cur['f'] = cur.get('f', b'') + d[a:b]
def on_header_value(d, a, b): cur['v'] = cur.get('v', b'') + d[a:b]
def on_header_end():
    if cur.get('f', b'').lower() == b'content-disposition':
        _, opts = parse_options_header(cur['v'])
        fn = opts.get(b'filename')
        pm.append((opts[b'name'].decode('utf-8'), None if fn is None else fn.decode('utf-8')))
    cur.clear()
p = MultipartParser(b'b', {'on_header_field': on_header_field,
                           'on_header_value': on_header_value,
                           'on_header_end': on_header_end})
p.write(body)
p.finalize()
wz = []
d = MultipartDecoder(b'b')
d.receive_data(body)
d.receive_data(None)
while True:
    ev = d.next_event()
    if isinstance(ev, File):
        wz.append((ev.name, ev.filename))
    elif isinstance(ev, Field):
        wz.append((ev.name, None))
    elif isinstance(ev, (Epilogue, NeedData)):
        break
def rows(rs):
    return '(' + ' '.join('(' + s(n) + ' ' + s(f) + ')' for n, f in rs) + ')'
with open(sys.argv[2], 'w', encoding='utf-8') as o:
    o.write('(%s %s %s %s %s)' % (s(sys.version.split()[0]), s(md.version('python-multipart')),
                                  s(md.version('werkzeug')), rows(pm), rows(wz)))
"
  "Python for `python -I -c`: the multipart/form-data body in argv[1] (boundary
b) read by python-multipart and by werkzeug, each part's (name filename)
written to argv[2] as (python-version pm-version wz-version pm-rows wz-rows).
python-multipart hands back header octets; they are decoded as UTF-8 here, as
Starlette does.")

(defparameter +python-packages+ '("python-multipart==0.0.34" "werkzeug==3.1.9")
  "Pinned, so the checked readings below are the same on every run.")

(defparameter +python-multipart-expected+
  (list '("имя" nil) '("u" "Prélude.svg") '("w" "a\\b.svg") '("q" "say %22hi%22.svg")
        '("a%0D%0Ab" nil))
  "python-multipart 0.0.34: UTF-8 octets and a literal backslash; %22 %0D %0A
are left to the application.")

(defparameter +werkzeug-expected+
  (list '("имя" nil) '("u" "Prélude.svg") '("w" "ab.svg") '("q" "say \"hi\".svg")
        '("a%0D%0Ab" nil))
  "werkzeug 3.1.9: UTF-8 names and %22; a backslash is unescaped as a
quoted-pair (as mime-protocol 0.3.0 did) and %0D %0A are kept.")

(defun python-with (packages script &rest args)
  "Run SCRIPT under `uv run --no-project --with P ... python -I -c`.
(values code stdout stderr)."
  (proc:run (append (list "uv" "run" "--no-project")
                    (loop for p in packages append (list "--with" p))
                    (list* "python" "-I" "-c" script args))
            :output :string :error-output :string :timeout 600
            :env '(("PYTHONIOENCODING" . "utf-8"))))

(defun python-reads-the-form (body result)
  (with-open-file (o body :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (browser-body) o))
  (multiple-value-bind (code stdout stderr)
      (python-with +python-packages+ *python-form* (namestring body) (namestring result))
    (declare (ignore stdout))
    (check code 0 "uv run exit code (form)")
    (if (/= code 0)
        (out "  ~a~%" stderr)
        (destructuring-bind (python pm-version wz-version pm wz)
            (let ((*read-eval* nil)
                  (*package* (find-package '#:harness-demos-mime-multipart)))
              (read-from-string (utf8 (read-octets result))))
          (let ((ours (name-rows (mime:parse-mime (browser-request))))
                (mime-version (asdf:component-version (asdf:find-system "mime-protocol"))))
            (say (format nil "The section 6 body, read by python-multipart ~a and werkzeug ~a (Python ~a)."
                         pm-version wz-version python)
                 "\"same\" means the same (name filename) as mime-protocol:"
                 "")
            (out "    ~32a ~28a ~a~%" (format nil "mime-protocol ~a" mime-version)
                 "python-multipart" "werkzeug")
            (loop for o in ours
                  for p in pm
                  for w in wz
                  do (out "    ~32a ~28a ~a~%" (pair-string o)
                          (if (equal p o) "same" (pair-string p))
                          (if (equal w o) "same" (pair-string w))))
            (check pm +python-multipart-expected+ "python-multipart's reading")
            (check wz +werkzeug-expected+ "werkzeug's reading")
            (check (list (subseq pm 0 2) (subseq wz 0 2)) (list (subseq ours 0 2) (subseq ours 0 2))
                   "all three read the UTF-8 names alike")
            (say ""
                 "All three read the UTF-8 octets as UTF-8. python-multipart also keeps the"
                 "backslash, and leaves %22 %0D %0A to the application. werkzeug decodes %22,"
                 "keeps %0D %0A, and drops the backslash: it unescapes quoted-pairs, which is"
                 "what mime-protocol 0.3.0 did."))))))

(defun demo-python-form ()
  (if (not (uv-available-p))
      (say "uv is not on PATH, so the cross-read with python-multipart and werkzeug is skipped.")
      (let ((body (scratch-file "form.bin"))
            (result (scratch-file "form.sexp")))
        (unwind-protect (python-reads-the-form body result)
          (dolist (f (list body result))
            (when (probe-file f) (delete-file f)))))))

(defparameter +old-boundary-form+
  "(format t \"BOUNDARY ----cl-stack-~A~%\" (random (expt 36 10)))"
  "The body of http-protocol 0.4.3's MAKE-MULTIPART-BOUNDARY, printed.")

(defparameter +new-boundary-form+
  "(format t \"BOUNDARY ~A~%\"
           (nth-value 3 (http-protocol:make-multipart-body '((\"a\" . \"b\")) nil)))"
  "The first boundary MAKE-MULTIPART-BODY draws in a process, printed.")

(defun fresh-boundary (&key (via-cl-repo t))
  "The boundary a fresh SBCL process prints. VIA-CL-REPO: started by `cl-repo
run`, which loads this project's http-protocol from GHCR, it evaluates
+NEW-BOUNDARY-FORM+; otherwise a bare sbcl evaluates +OLD-BOUNDARY-FORM+."
  (let ((command (append (when via-cl-repo (list "cl-repo" "run" "--"))
                         (list "sbcl" "--noinform" "--non-interactive"
                               "--no-sysinit" "--no-userinit")
                         (when via-cl-repo
                           (list "--eval" "(require \"asdf\")"
                                 "--eval" "(let ((*standard-output* (make-broadcast-stream)))
                                             (asdf:load-system \"http-protocol\"))"))
                         (list "--eval" (if via-cl-repo +new-boundary-form+ +old-boundary-form+)))))
    (multiple-value-bind (code stdout stderr)
        (proc:run command :output :string :error-output :string :timeout 900)
      (check code 0 (format nil "exit code of a fresh ~:[sbcl~;cl-repo run sbcl~]" via-cl-repo))
      (let ((line (find-if (lambda (l) (eql 0 (search "BOUNDARY " l)))
                           (uiop:split-string stdout :separator '(#\Newline)))))
        (unless line
          (out "  ~a~%" stderr)
          (error "no BOUNDARY line from ~{~a~^ ~}" command))
        (string-right-trim '(#\Return) (subseq line 9))))))

(defun replace-all (string old new)
  "STRING with every OLD replaced by NEW."
  (with-output-to-string (o)
    (loop with start = 0
          for pos = (search old string :start2 start)
          do (write-string string o :start start :end (or pos (length string)))
          while pos
          do (write-string new o)
             (setf start (+ pos (length old))))))

(defun read-form (content-type octets)
  "mime-protocol's reading of the multipart/form-data body OCTETS sent under
CONTENT-TYPE: ((name filename) content-as-text) per part."
  (let ((entity (mime:parse-mime (concatenate '(vector (unsigned-byte 8))
                                              (wire-octets "Content-Type: " content-type :crlf :crlf)
                                              octets)
                                 :decode nil)))
    (mapcar (lambda (row part) (list row (utf8 (mime:mime-content part))))
            (name-rows entity)
            (mime:mime-parts entity))))

(defun show-form (rows)
  (dolist (row rows)
    (destructuring-bind (pair text) row
      (out "    ~28a ~a~%" (pair-string pair) (visible text)))))

(defun demo-http-boundaries ()
  (say "0.4.4 in two fresh `cl-repo run -- sbcl` processes, the first boundary of each:")
  (let ((new1 (fresh-boundary))
        (new2 (fresh-boundary)))
    (out "    ~a~%    ~a~%" new1 new2)
    (check (string= new1 new2) nil "0.4.4: two fresh processes, two boundaries")
    (check (list (length new1) (subseq new1 0 13)
                 (every (lambda (c) (digit-char-p c 16)) (subseq new1 13)))
           (list 45 "----cl-stack-" t)
           "----cl-stack- and 32 hex digits")
    (say ""
         "Each process seeds its own state once with (make-random-state t), and the state"
         "is cleared before an image is saved: 128 bits, 45 characters, all bchars.")))

(defun demo-http-collision (old1)
  (let* ((evil (crlf "notes" (format nil "--~a" old1)
                     "Content-Disposition: form-data; name=\"role\"" "" "admin"))
         (files `(("upload" . (:content ,evil :filename "notes.txt"
                               :content-type "text/plain")))))
    (say "An upload whose content holds a delimiter line for the boundary 0.4.3 used"
         "first in every fresh image, then a forged \"role\" field:")
    (dolist (line (uiop:split-string evil :separator '(#\Newline)))
      (out "    | ~a~%" (string-right-trim '(#\Return) line)))
    (multiple-value-bind (stream ct clen boundary)
        (hp:make-multipart-body '(("role" . "user")) files :boundary old1)
      (let ((octets (hp:slurp-octets stream)))
        (check (length octets) clen "Content-Length of the body")
        (say ""
             "make-multipart-body, given that boundary as if it had drawn it:")
        (out "    boundary given  ~a~%    boundary used   ~a~%    Content-Type    ~a~%"
             old1 boundary ct)
        (check (string= boundary old1) nil "a boundary that occurs in a part is replaced")
        (say "" "mime-protocol reads the body back: two parts, the upload intact.")
        (let ((rows (read-form ct octets)))
          (show-form rows)
          (check rows (list (list '("role" nil) "user") (list '("upload" "notes.txt") evil))
                 "0.4.4: the parts as sent"))
        (let ((old-rows (read-form (format nil "multipart/form-data; boundary=~a" old1)
                                   (encoding-protocol:encode
                                    (replace-all (utf8 octets) boundary old1)))))
          (say ""
               "0.4.3 wrote the same bytes with that boundary. The same reader then sees three"
               "parts, the last a \"role\" field the sender never set:")
          (show-form old-rows)
          (check old-rows (list (list '("role" nil) "user")
                                (list '("upload" "notes.txt") "notes")
                                (list '("role" nil) "admin"))
                 "0.4.3: a forged role field"))))))

(defun demo-http-content ()
  (say "Content that is not a stream, a string or octets is refused where the file or"
       "part is made (#57), not later when the body is streamed. So is a :boundary"
       "that RFC 2046 does not allow:")
  (flet ((try (label thunk)
           ;; The message names the content's type: print it on one line.
           (let ((*print-pretty* nil))
             (handler-case (progn (funcall thunk)
                             (out "    ~42a accepted~%" label)
                             (check :accepted :refused label))
               (hp:http-protocol-error (c)
                 (out "    ~42a http-protocol-error~%      ~a~%" label c)
                 (check :refused :refused label))))))
    (try "(make-http-file (vector 1 2))"
         (lambda () (hp:make-http-file (vector 1 2) :filename "a.bin")))
    (try "(make-http-file <(signed-byte 8) array>)"
         (lambda () (hp:make-http-file (make-array 2 :element-type '(signed-byte 8)
                                                     :initial-element -1))))
    (try ":files ((\"f\" :content #(1 2) ...))"
         (lambda () (hp:make-multipart-body nil `(("f" . (:content ,(vector 1 2)
                                                          :filename "a"))))))
    (try ":form-data ((\"n\" . #(1 2)))"
         (lambda () (hp:make-multipart-body `(("n" . ,(vector 1 2))) nil)))
    (try ":boundary \"semi;colon\""
         (lambda () (hp:make-multipart-body '(("a" . "b")) nil :boundary "semi;colon"))))
  (say "" "Accepted, and sent as: octets as given, a string as UTF-8, a stream as read.")
  (multiple-value-bind (stream ct)
      (hp:make-multipart-body
       nil `(("o" . ,(hp:make-http-file (octets '(1 2 255)) :filename "o.bin"))
             ("s" . ,(hp:make-http-file "ñ" :filename "s.txt"))
             ("t" . ,(hp:make-http-file (hp:make-octet-input-stream (octets '(7)))
                                        :filename "t.bin"))))
    (let* ((body (hp:slurp-octets stream))
           (entity (mime:parse-mime (concatenate '(vector (unsigned-byte 8))
                                                 (wire-octets "Content-Type: " ct :crlf :crlf)
                                                 body)
                                    :decode nil))
           (contents (mapcar (lambda (p) (coerce (mime:mime-content p) 'list))
                             (mime:mime-parts entity))))
      (loop for (name filename) in (name-rows entity)
            for content in contents
            do (out "    ~a / ~a  ~{~2,'0X~^ ~}~%" name filename content))
      (check contents '((1 2 255) (195 177) (7)) "part contents on the wire"))))

(defun demo-http-multipart ()
  (say (format nil "http-protocol ~a's form-data encoder, MAKE-MULTIPART-BODY: the one under"
               (asdf:component-version (asdf:find-system "http-protocol")))
       "cl-stack-http's :files and rpc-backend-htmx's uploads."
       ""
       "0.4.3 drew boundaries from CL:*RANDOM-STATE*, which every fresh SBCL image"
       "starts with the same. Its formula, (format nil \"----cl-stack-~A\" (random"
       "(expt 36 10))), in two fresh sbcl processes:")
  (let ((old1 (fresh-boundary :via-cl-repo nil))
        (old2 (fresh-boundary :via-cl-repo nil)))
    (out "    ~a~%    ~a~%" old1 old2)
    (check (string= old1 old2) t "0.4.3's formula: one string in every fresh image")
    (say "")
    (demo-http-boundaries)
    (say "")
    (demo-http-collision old1)
    (say "")
    (demo-http-content)))

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 110)
        (*random-state* (make-random-state t)))
    (format t "~&mime-protocol ~a, http-protocol ~a (~a ~a, ~a): nested multiparts, header names, browser form-data names, form-data boundaries~%"
            (asdf:component-version (asdf:find-system "mime-protocol"))
            (asdf:component-version (asdf:find-system "http-protocol"))
            (lisp-implementation-type) (lisp-implementation-version) (software-type))
    (section "1. The mail layout, printed" (demo-print))
    (section "2. Parsed back: the same tree, the same text (#9)" (demo-round-trip))
    (section "3. Edge cases: prefix boundaries, line breaks, LF-only, preamble" (demo-edges))
    (section "4. Canonical header names on print, case-insensitive lookups" (demo-header-names))
    (section "5. Cross-read with Python's email package, both ways" (demo-python))
    (section "6. A browser's form-data part headers: UTF-8 names, a literal backslash (#14)"
             (demo-browser))
    (section "7. The same form read by python-multipart and werkzeug" (demo-python-form))
    (section "8. http-protocol's form-data encoder: fresh boundaries, no collisions, octet content (http-protocol#56, #57)"
             (demo-http-multipart))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
