(in-package #:harness-demos-imports)

;;;; The python-to-cl libraries that cl-stack-systems#75 and #76 (first tier)
;;;; made installable: each one installed from ghcr.io/egao1980/cl-systems by
;;;; cl-repo and doing the job its python-to-cl page names. Before
;;;; cl-stack-systems#79 every one of these failed at `cl-repo sync` ("system
;;;; ... is not in the registry"). RUN-DEMOS returns NIL when a check differs
;;;; or a section stops on an error.

(defvar *checks* 0)
(defvar *mismatches* 0)
(defvar *failed-sections* '())

(defun shown (text)
  "TEXT with the home and temporary directories replaced by ~ and <tmp>."
  (let ((text (princ-to-string text)))
    (flet ((swap (from to)
             (let ((from (string-right-trim "/" from)))
               (loop for at = (search from text)
                     while (and at (plusp (length from)))
                     do (setf text (concatenate 'string (subseq text 0 at) to
                                                (subseq text (+ at (length from)))))))))
      (swap (namestring (uiop:temporary-directory)) "<tmp>")
      (swap (namestring (user-homedir-pathname)) "~")
      text)))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  (format t "~&~{~a~%~}~%" lines))

(defun show (label value)
  (format t "~&  ~38a ~a~%" label (shown value)))

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (let ((ok (funcall test got expected)))
    (unless ok (incf *mismatches*))
    (format t "~&  ~:[MISMATCH~;ok      ~] ~a: ~a~:[ (want ~a)~;~*~]~%"
            ok label (shown (prin1-to-string got)) ok (prin1-to-string expected))
    ok))

(defmacro section (title &body body)
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (format t "~&  SECTION FAILED: ~a: ~a~%" (type-of c) (shown c))))))

(defun tmp-file (name)
  (merge-pathnames (format nil "harness-imports-~a" name) (uiop:temporary-directory)))

(defun file-head (path n)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((octets (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence octets s)
      (map 'string #'code-char octets))))

(defun markdown-html (text)
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream text s)))

(defun demo-jose ()
  (section "1. jose: JWT HS256 (PyJWT) — cl-stack-systems#75"
    (para "jose 0.1.0 was on GHCR, but its jws.lisp and jwt.lisp import cl-json and"
          "assoc-utils, which were not, so `cl-repo sync` failed. #79 imports both.")
    (let* ((key (ironclad:ascii-string-to-byte-array "my$ecret"))
           (claims '(("hello" . "world") ("sub" . "1234")))
           (token (jose:encode :hs256 key claims)))
      (show "token" (format nil "~a… (~d chars, ~d dots)"
                            (subseq token 0 20) (length token) (count #\. token)))
      (check "decode with the key (claims as a set)" (jose:decode :hs256 key token) claims
             :test (lambda (got want)
                     (and (= (length got) (length want))
                          (subsetp want got :test #'equal))))
      (check "decode with another key"
             (handler-case
                 (progn (jose:decode :hs256 (ironclad:ascii-string-to-byte-array "guess") token)
                        :accepted)
               (jose:jose-error (e) (type-of e)))
             'jose:jws-verification-error)
      (check "cl-json (jose's dependency)"
             (cl-json:encode-json-to-string '((:a . 1) (:b . "two")))
             "{\"a\":1,\"b\":\"two\"}")
      (check "assoc-utils (jose's dependency)"
             (assoc-utils:aget '(("a" . 1) ("b" . 2)) "b") 2))))

(defun demo-lparallel ()
  (section "2. lparallel: a worker pool (multiprocessing / concurrent.futures)"
    (let ((lparallel:*kernel* (lparallel:make-kernel 4)))
      (unwind-protect
           (progn
             (check "pmap #'* over 1..8"
                    (lparallel:pmap 'list (lambda (x) (* x x)) '(1 2 3 4 5 6 7 8))
                    '(1 4 9 16 25 36 49 64))
             (check "preduce #'+ over 1..1000"
                    (lparallel:preduce #'+ (loop for i from 1 to 1000 collect i)) 500500)
             (let ((futures (loop for n in '(10 20 30)
                                  collect (let ((n n))
                                            (lparallel:future (loop for i below n sum i))))))
               (check "three futures, forced"
                      (mapcar #'lparallel:force futures) '(45 190 435))))
        (lparallel:end-kernel :wait t)))))

(defun demo-csv ()
  (section "3. cl-csv: write and read CSV (csv module)"
    (let* ((rows '(("name" "city") ("Ada" "London, UK") ("Grace" "say \"hi\"")))
           (text (cl-csv:write-csv rows)))
      (format t "~&~a" text)
      (check "read back" (cl-csv:read-csv text) rows))))

(defun demo-markdown ()
  (section "4. 3bmd: Markdown to HTML (markdown / mistune)"
    (para "Fenced code blocks need colorize and html-encode; #79 imports them too.")
    (check "heading" (markdown-html "# Title") (format nil "~%<h1>Title</h1>~%")
           :test (lambda (got want) (search (string-trim '(#\Newline) want) got)))
    (let ((html (let ((3bmd-code-blocks:*code-blocks* t))
                  (markdown-html (format nil "```lisp~%(defun f (x) (* x 2))~%```~%")))))
      (check "fenced lisp block is colorized" (and (search "<span class=\"paren1\">" html) t) t))
    (let ((html (let ((3bmd-tables:*tables* t))
                  (markdown-html (format nil "| a | b |~%|---|---|~%| 1 | 2 |~%")))))
      (check "table cells" (list (and (search "<table" html) t) (count-matches "<td" html))
             '(t 2)))))

(defun count-matches (needle haystack)
  (loop with start = 0
        for at = (search needle haystack :start2 start)
        while at
        count t
        do (setf start (1+ at))))

(defun demo-pdf ()
  (section "5. cl-pdf / cl-typesetting / cl-pdf-parser (ReportLab / pypdf)"
    (para "cl-pdf comes from the egao1980/cl-pdf fork (d8d9f77): upstream master calls"
          "EXTENDED-ASCII-P while writing any PDF name, but only cl-pdf-parser defines"
          "it, so no document could be written. The fork moves it into pdf.lisp.")
    (let ((f (tmp-file "report.pdf")))
      (pdf:with-document ()
        (dotimes (i 2)
          (pdf:with-page ()
            (pdf:in-text-mode
             (pdf:set-font (pdf:get-font "Helvetica") 24.0)
             (pdf:move-text 72 770)
             (pdf:draw-text (format nil "Quarterly report, page ~d" (1+ i))))))
        (pdf:write-document f))
      (check "cl-pdf file starts with" (file-head f 5) "%PDF-")
      (show "size" (format nil "~d bytes" (with-open-file (s f) (file-length s))))
      (check "cl-pdf-parser reads back pages"
             (length (pdf::pages (pdf::root-page (pdf::read-pdf-file f)))) 2)
      (check "a non-ASCII name" (with-output-to-string (pdf::*pdf-stream*)
                                  (pdf::write-object (format nil "/Caf~c" (code-char 233))))
             "/Caf#E9"))
    (let ((f (tmp-file "typeset.pdf")))
      (tt:with-document ()
        (let ((content (tt:compile-text ()
                         (tt:paragraph (:font "Helvetica" :font-size 12)
                           "Hello from cl-typesetting." :eol))))
          (tt:draw-pages content)
          (when pdf:*page* (tt:finalize-page pdf:*page*))
          (tt:write-document f)))
      (check "cl-typesetting file starts with" (file-head f 5) "%PDF-"))))

(defun demo-image ()
  (section "6. opticl: images (Pillow)"
    (let ((img (opticl:make-8-bit-rgb-image 3 4))
          (f (tmp-file "red.png")))
      (dotimes (y 3)
        (dotimes (x 4)
          (setf (opticl:pixel img y x) (values 200 (* 50 x) (* 100 y)))))
      (opticl:write-png-file f img)
      (check "PNG signature" (subseq (file-head f 4) 1) "PNG")
      (let ((back (opticl:read-png-file f)))
        (check "read back: height width channels" (array-dimensions back) '(3 4 3))
        (check "pixel (2, 3)" (multiple-value-list (opticl:pixel back 2 3)) '(200 150 200))))))

(defvar *square-calls* 0)

(function-cache:defcached cached-square (x)
  (incf *square-calls*)
  (* x x))

(defun demo-cache ()
  (section "7. function-cache: memoization (functools.lru_cache)"
    (setf *square-calls* 0)
    (check "cached-square 12, three times"
           (list (cached-square 12) (cached-square 12) (cached-square 12)) '(144 144 144))
    (check "body ran" *square-calls* 1)))

(defun demo-html ()
  (section "8. lquery / clss / plump: HTML queries (BeautifulSoup)"
    (let ((doc (plump:parse "<ul><li class=x>one</li><li>two</li><li class=x>three</li></ul>")))
      (check "clss li.x" (map 'list #'plump:text (clss:select "li.x" doc)) '("one" "three"))
      (check "lquery $ li text"
             (coerce (lquery:$ doc "li" (text)) 'list) '("one" "two" "three")))))

(defun demo-misc ()
  (section "9. machine-state, check-it, parcom (psutil, hypothesis, parsec)"
           (check "machine-cores is a positive integer"
                  (typep (org.shirakumo.machine-state:machine-cores) '(integer 1)) t)
           (check "check-it: generated values are integers"
                  (loop repeat 20 always (integerp (check-it:generate (check-it:generator (integer)))))
                  t)
           (check "check-it: x + 0 = x on random integers"
                  (check-it:check-it (check-it:generator (integer))
                                     (lambda (x) (= (+ x 0) x)))
                  t)
           (check "parcom: (char #\\a) on \"a\"" (parcom:parse (parcom:char #\a) "a") #\a)))

(defun demo-load-only ()
  (section "10. Loaded, not run here (they need a server, a workbook or a repo)"
    (para "cl-smtp needs frugal-uuid/non-frugal: cl-stack-systems#84 publishes it under"
          "its primary name, since the client refuses a system name with a slash.")
    (dolist (pkg '("CL-SMTP" "REDIS" "CL-EXCEL" "LEGIT" "CL-PROGRESS-BAR"))
      (check (format nil "package ~a" pkg) (and (find-package pkg) t) t))))

(defun run-demos ()
  (setf *checks* 0 *mismatches* 0 *failed-sections* '())
  (demo-jose)
  (demo-lparallel)
  (demo-csv)
  (demo-markdown)
  (demo-pdf)
  (demo-image)
  (demo-cache)
  (demo-html)
  (demo-misc)
  (demo-load-only)
  (format t "~&~%~d checks, ~d mismatched~@[, failed sections: ~{~a~^; ~}~]~%"
          *checks* *mismatches* (reverse *failed-sections*))
  (and (zerop *mismatches*) (null *failed-sections*)))
