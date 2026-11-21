(in-package #:harness-demos-supervisor)

;;; Demo 8: supervisor-protocol/search 0.3.0 (H8c) used directly: the
;;; tokenizer and the kind rules on sample lines, three units' logs indexed and
;;; searched (ranked hits with unit:segment:line refs), kinds-only search by
;;; severity, a search across a rotation and after a drop, .six persistence.

(defparameter +sample-lines+
  '("src/parser.c:7:12: error: use of undeclared identifier 'tokn'; did you mean 'tok'?"
    "src/util.c:2:32: warning: unused variable 'y' [-Wunused-variable]"
    "make: *** [src/parser.o] Error 1"
    "1 error generated."
    "0 errors, 2 warnings"
    "cc -Wall -Wextra -O2 -c -o src/errors.o src/errors.c"
    "  × 1) leading currency sign is skipped"
    "TypeError: Cannot read properties of undefined (reading 'roles')"
    "    at userRoles (/home/dev/webapp/src/routes.js:5:23)"
    "(node:11554) [DEP0005] DeprecationWarning: Buffer() is deprecated"
    "--- FAIL: TestParseHTTPResponse_v2 (0.00s)"
    "FAIL add: got -1, expected 5"
    "no error in my-package::parse-amount"))

(defun plist-remove (plist &rest keys)
  (loop for (k v) on plist by #'cddr unless (member k keys) append (list k v)))

(defun print-hit (index hit &key (context 0) (width 90))
  (note "~a  ~(~{~a~^,~}~)~@[ score ~,3f~]~@[ extent ~a~]  ~a"
        (ss:hit-ref hit) (ss:hit-kinds hit) (ss:hit-score hit)
        (let ((e (ss:hit-extent hit))) (and e (format nil "~d-~d" (car e) (cdr e))))
        (clip (ss:strip-ansi (ss:hit-text hit)) width))
  (when (plusp context)
    (multiple-value-bind (records gaps) (ss:hit-excerpt index hit :context context)
      (dolist (r records)
        (note "  ~:[ ~;>~]~5d ~(~6a~) ~a" (= (sp:log-record-line r) (ss:hit-line hit))
              (sp:log-record-line r) (sp:log-record-stream r)
              (clip (ss:strip-ansi (sp:log-record-text r)) width)))
      (dolist (g gaps) (print-gap g)))))

(defun %search-print (index query &rest keys &key (k 3) (context 0) &allow-other-keys)
  (multiple-value-bind (hits total)
      (apply #'ss:search-logs index query :k k (plist-remove keys :context))
    (stage "search ~s~@[ ~s~] → ~d of ~d hit~:p" query (plist-remove keys :k :context)
           (length hits) total)
    (loop for h in hits for i from 0
          do (print-hit index h :context (if (zerop i) context 0)))
    hits))

(defun six-files (dir)
  (sort (directory (merge-pathnames "*.six" (uiop:ensure-directory-pathname dir)))
        #'string< :key #'namestring))

(defun file-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun unit-log-dir (sup unit)
  (sp:unit-log-pathname (sp:supervisor-state-directory sup) (sp:supervisor-id sup) (sp:unit-id unit)))

(defmacro timed ((secs) form &body body)
  "Evaluate FORM, binding SECS to its wall time (seconds, float), then BODY."
  (let ((t0 (gensym)) (v (gensym)))
    `(let* ((,t0 (get-internal-real-time))
            (,v (multiple-value-list ,form))
            (,secs (float (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second))))
       (declare (ignorable ,secs))
       (values-list (progn ,@body ,v)))))

(defun %search-tokens ()
  (stage "8a. tokenize and line-kinds (tokenizer version ~d; kinds ~s)" ss:+tokenizer-version+ ss:+hit-kinds+)
  (dolist (l +sample-lines+)
    (note "~a" l)
    (note "    kinds ~s  terms ~s" (ss:line-kinds l) (ss:tokenize l)))
  (multiple-value-bind (terms phrases) (ss:query-terms "TypeError \"reading 'roles'\" routes.js")
    (note "query-terms of TypeError \"reading 'roles'\" routes.js → terms ~s phrases ~s" terms phrases))
  (stage "8a'. classify-lines finds a stack trace (kind on the first line, extent to the last)")
  (let ((lines (subseq (fixture-lines "devserver") 6 12)))
    (loop for (i kinds end) in (ss:classify-lines lines)
          do (note "line ~d ~s~@[ ends at ~d~]  ~a" i kinds end (clip (nth (1- i) lines) 60)))))

(defun %search-units (sup index)
  (stage "8b. three units' logs: make (a compile error), cl-repo test (Rove failures), npm run dev (TypeErrors)")
  (let ((build (printing-unit sup (fixture-lines "build") :labels '(:tool "make")))
        (rove (printing-unit sup (fixture-lines "rove") :labels '(:tool "cl-repo test")))
        (dev (printing-unit sup (fixture-lines "devserver") :labels '(:tool "npm run dev")
                                                          :stderr-p #'devserver-stderr-p)))
    (dolist (u (list build rove dev))
      (note "~a ~s: ~d lines, state ~s" (sp:unit-id u) (sp:unit-labels u) (sp:log-last-line u) (sp:unit-state u)))
    (show "index-log-keys" (ss:index-log-keys index))
    (show "refresh-index → records tokenized" (ss:refresh-index index))
    (show "index-stats" (ss:index-stats index))
    (let* ((h1 (%search-print index "undeclared identifier" :context 2))
           (h2 (%search-print index "TYPE-ERROR parse-amount" :context 2))
           (h3 (%search-print index "TypeError roles" :context 1))
           (h4 (%search-print index "src/util.c unused"))
           (h5 (%search-print index "\"Cannot read properties\"" :streams '(:stderr) :k 5)))
      (check "undeclared identifier → the cc error line of the build unit (line 20; line 1 is the segment header)"
             (equal (ss:hit-ref (first h1)) (ss:format-log-ref (sp:unit-id build) 1 20)))
      (check "TYPE-ERROR parse-amount → the Rove unit"
             (equal (ss:hit-key (first h2)) (sp:unit-id rove)))
      (check "TypeError roles → a stack trace in the dev server's stderr"
             (and h3 (member :stack-trace (ss:hit-kinds (first h3))) (eq :stderr (ss:hit-stream (first h3)))))
      (check "src/util.c unused → the warning (line 47)"
             (equal (ss:hit-ref (first h4)) (ss:format-log-ref (sp:unit-id build) 1 47)))
      (check "a phrase on stderr → both TypeErrors" (= 2 (length h5))))
    (let ((hits (%search-print index "" :kinds '(:error :stack-trace :test-failure :warning) :k 12)))
      (note "(kinds only: no score, ordered by severity (test failure, stack trace, error, warning), then newest first)")
      (check "kinds-only: test failures first" (member :test-failure (ss:hit-kinds (first hits)))))
    (%search-print index "" :kinds '(:exit) :k 5)
    (%search-print index "error" :units (list (sp:unit-id build)) :k 3)
    (list build rove dev)))

(defun %search-rotation (sup index)
  (stage "8c. a search across a rotation and after a drop: 400 lines, 2 KiB segments, 4 kept")
  (let* ((u (sp:start-unit sup :shell-job
                           :labels '(:tool "chatty")
                           :log-segment-size 2048 :log-segments 4
                           :function (lambda (u)
                                       (declare (ignore u))
                                       (loop for i from 1 to 400
                                             do (format t "~a~%"
                                                        (case i
                                                          (5 "error: early failure in stage-a (output line 5)")
                                                          (340 "error: middle failure in stage-b (output line 340)")
                                                          (395 "error: late failure in stage-c (output line 395)")
                                                          (t (format nil "step ~3,'0d of 400 ok" i)))))
                                       (finish-output)
                                       :done))))
    (settle u)
    (note "(log line numbers count the segment header lines too, so output line 395 is log line ~d)"
          (sp:log-record-line (find "error: late failure in stage-c (output line 395)" (sp:log-read u)
                                    :key #'sp:log-record-text :test #'string=)))
    (print-segments u)
    (multiple-value-bind (records gaps) (sp:log-read u :start 1 :end 3)
      (show "log-read lines 1-3 → records" (length records))
      (dolist (g gaps) (print-gap g)))
    (let ((hits (%search-print index "failure" :units (list (sp:unit-id u)) :k 5 :context 0)))
      (check "the early failure (rotated out) is not found; the late and middle ones are, newest first"
             (and (= 2 (length hits))
                  (search "late" (ss:hit-text (first hits))) (search "middle" (ss:hit-text (second hits)))))
      (check "the hits are in two different retained segments"
             (= 2 (length (remove-duplicates (mapcar #'ss:hit-segment hits))))))
    (let ((hits (%search-print index "stage-a" :units (list (sp:unit-id u)))))
      (check "stage-a (dropped) → no hit" (null hits)))
    (let* ((first-kept (first (sp:log-read u :limit 1)))
           (hit (first (ss:search-logs index (format nil "~s" (sp:log-record-text first-kept))
                                       :units (list (sp:unit-id u))))))
      (stage "8c'. hit-excerpt of the first retained output line (~d) with context 3: the gap before it is reported"
             (sp:log-record-line first-kept))
      (when hit (print-hit index hit :context 3))
      (check "the excerpt reports the rotated gap"
             (and hit (nth-value 1 (ss:hit-excerpt index hit :context 3)))))
    u))

(defun %search-persistence (dir)
  (stage "8d. .six persistence: 20 000 lines in 64 KiB segments, a fresh index loads them, a rebuild is byte-identical")
  (with-supervisor (sup :state-directory dir)
    (let* ((u (sp:start-unit sup :shell-job
                             :log-segment-size 65536 :log-segments 64
                             :function (lambda (u)
                                         (declare (ignore u))
                                         (dotimes (i 20000)
                                           (format t (cond ((= 0 (mod i 997)) "ERROR worker-~d: request ~d timed out after 30s~%")
                                                           ((= 0 (mod i 101)) "WARN worker-~d: slow query for request ~d~%")
                                                           ((evenp i) "INFO worker-~d handled request ~d in 12ms~%")
                                                           (t "DEBUG worker-~d cache hit for key user:~d~%"))
                                                   (mod i 16) i))
                                         (finish-output)
                                         :done)))
           (ldir (progn (settle u :timeout 60) (unit-log-dir sup u))))
      (note "~d lines in ~d segments" (sp:log-last-line u) (length (sp:log-segments u)))
      (let ((i1 (ss:make-log-index :supervisor sup)))
        (timed (secs) (ss:refresh-index i1)
          (note "index 1, first refresh: ~,3f s  stats ~s" secs (ss:index-stats i1)))
        (timed (secs) (ss:refresh-index i1)
          (note "index 1, refresh with nothing new: ~,4f s" secs))
        (timed (secs) (ss:search-logs i1 "timed out worker-3")
          (note "index 1, search \"timed out worker-3\": ~,4f s" secs))
        (ss:close-log-index i1))
      (let* ((sixes (six-files ldir))
             (bytes (mapcar #'file-octets sixes)))
        (note ".six files: ~d (one per sealed segment; the newest segment of a closed log too), ~d bytes in all"
              (length sixes) (reduce #'+ bytes :key #'length))
        (show "first .six header bytes (\"SPSI\", format, tokenizer)"
              (coerce (subseq (first bytes) 0 12) 'list))
        (let ((i2 (ss:make-log-index :supervisor sup)))
          (timed (secs) (ss:refresh-index i2)
            (note "index 2 (fresh, loads .six): ~,3f s  stats ~s" secs (ss:index-stats i2)))
          (check "a fresh index tokenizes nothing" (= 0 (getf (ss:index-stats i2) :tokenized)))
          (check "same top hit as index 1"
                 (equal (ss:hit-ref (first (ss:search-logs i2 "timed out worker-3")))
                        (let ((i (ss:make-log-index :supervisor sup :persist nil)))
                          (prog1 (ss:hit-ref (first (ss:search-logs i "timed out worker-3")))
                            (ss:close-log-index i)))))
          (ss:close-log-index i2))
        (delete-file (first sixes))
        (with-open-file (s (second sixes) :direction :io :element-type '(unsigned-byte 8) :if-exists :overwrite)
          (file-position s 20) (write-byte 255 s))
        (note "deleted ~a, corrupted a byte of ~a"
              (file-namestring (first sixes)) (file-namestring (second sixes)))
        (let ((i3 (ss:make-log-index :supervisor sup)))
          (timed (secs) (ss:refresh-index i3)
            (note "index 3: ~,3f s  stats ~s" secs (ss:index-stats i3)))
          (ss:close-log-index i3))
        (check "the rebuilt .six files are byte-identical"
               (every #'equalp bytes (mapcar #'file-octets (six-files ldir))))))))

(defun demo-search ()
  (banner "Demo 8: the log search index (supervisor-protocol/search 0.3.0, H8c)")
  (with-checks ()
    (%search-tokens)
    (with-temp-dir (dir "sup-search")
      ;; records of at most 512 octets, so a log segment can be as small as 2 KiB (8c)
      (with-supervisor (sup :state-directory dir :log-max-record 512)
        (let ((index (ss:make-log-index :supervisor sup)))
          (unwind-protect
               (progn (%search-units sup index)
                      (%search-rotation sup index))
            (ss:close-log-index index)))))
    (with-temp-dir (dir "sup-six")
      (%search-persistence dir))))
