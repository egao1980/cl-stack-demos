(in-package #:harness-demos-supervisor)

;;; Demo 7: supervisor-protocol 0.2.0, H8b — rotated, capped unit logs.

(defun octets-of (string) (sb-ext:string-to-octets string :external-format :utf-8))

(defun stream-bytes (records stream)
  "STREAM's exact bytes rebuilt from :OUTPUT RECORDS: each record's octets, plus
LF unless the record is continued."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (r records (coerce out '(simple-array (unsigned-byte 8) (*))))
      (when (and (eq :output (sp:log-record-type r)) (eq stream (sp:log-record-stream r)))
        (loop for b across (sp:log-record-octets r) do (vector-push-extend b out))
        (unless (sp:log-record-continued-p r) (vector-push-extend 10 out))))))

(defun files-in (dir type)
  (sort (directory (merge-pathnames (format nil "*.~a" type) (uiop:ensure-directory-pathname dir)))
        #'string< :key #'namestring))

(defun file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))

(defun dir-bytes (dir)
  (reduce #'+ (append (files-in dir "seg") (files-in dir "idx")) :key #'file-bytes))

(defun print-gap (g)
  (note "gap lines ~d-~d: ~d records, ~d bytes, reasons ~s"
        (sp:log-gap-first-line g) (sp:log-gap-last-line g) (sp:log-gap-records g)
        (sp:log-gap-bytes g) (sp:log-gap-reasons g)))

(defun print-segments (log)
  (dolist (s (sp:log-segments log))
    (note "segment ~8,'0d: lines ~6d-~6d, ~4d records, ~7d bytes on disk, ~7d output bytes~:[~; TORN~]"
          (sp:log-segment-number s) (sp:log-segment-first-line s) (sp:log-segment-last-line s)
          (sp:log-segment-records s) (sp:log-segment-bytes s) (sp:log-segment-output-bytes s)
          (sp:log-segment-torn-p s))))

(defun print-record (r)
  (note "line ~6d  seg ~d  ~7s ~7s~:[ ~;+~] ~a"
        (sp:log-record-line r) (sp:log-record-segment r) (sp:log-record-type r)
        (sp:log-record-stream r) (sp:log-record-continued-p r)
        (if (eq :output (sp:log-record-type r))
            (clip (sp:log-record-text r) 24)
            (sp:log-record-detail r))))

(defun chatty-argv (n width)
  "A child writing N lines of WIDTH octets (+ LF): \"line-00001 xxxx…\"."
  (lisp-argv (format nil "(let ((pad (make-string ~d :initial-element #\\x))) (dotimes (i ~d) (format t \"line-~~5,'0d ~~a~~%\" (1+ i) pad)) (finish-output))"
                     (- width 11) n)))

(defun first-raw-line-with (path tag &optional containing)
  "The first physical line of PATH whose tag field is TAG (and that contains the
string CONTAINING, if given), as a string."
  (with-open-file (s path :external-format :latin-1)
    (loop for l = (read-line s nil) while l
          for fields = (uiop:split-string l :separator " ")
          when (and (equal tag (fourth fields)) (or (null containing) (search containing l)))
            return l)))

(defun demo-logs ()
  (banner "Demo 7: supervisor-protocol 0.2.0 (H8b) — rotated, capped unit logs")
  (with-checks ()
    (with-temp-dir (dir "hd-sup-logs")
      (with-supervisor (sup :state-directory dir :log-total-cap (* 4 1024 1024))
        (show "defaults: segment size × segments, total cap, max record"
              (list sp:+default-log-segment-size+ sp:+default-log-segments+
                    sp:+default-log-total-cap+ sp:+default-log-max-record+))
        (show "this supervisor: total cap" (sp:supervisor-log-total-cap sup))

        ;; 7a: a chatty unit under a small per-unit cap
        (stage "7a. a chatty unit: 3000 lines × 1024 bytes (3 MB) with :log-segment-size 256 KiB × :log-segments 4")
        (let* ((n 3000) (width 1023)
               (written (* n (1+ width)))
               (start (get-internal-real-time))
               (u (sp:start-unit sup :shell-job :command (chatty-argv n width)
                                                :log-segment-size (* 256 1024) :log-segments 4)))
          (show "unit-log-settings" (sp:unit-log-settings u))
          (check "the unit exited" (eq :exited (sp:wait-unit u :timeout 60)))
          (wait-until (lambda () (sp:log-closed-p u)))
          (show "seconds" (elapsed-since start))
          (let* ((ldir (sp:unit-log-directory (sp:unit-log u)))
                 (segs (files-in ldir "seg")))
            (show "segment files on disk" (mapcar #'file-namestring segs))
            (show "their sizes" (mapcar #'file-bytes segs))
            (check "≤ 4 segment files, each ≤ 262144 bytes"
                   (and (<= (length segs) 4) (every (lambda (p) (<= (file-bytes p) (* 256 1024))) segs)))
            (print-segments u)
            (multiple-value-bind (records gaps) (sp:log-read u)
              (dolist (g gaps) (print-gap g))
              (let* ((kept (length (stream-bytes records :stdout)))
                     (dropped (reduce #'+ gaps :key #'sp:log-gap-bytes)))
                (show "output bytes written / kept / dropped" (list written kept dropped))
                (check "dropped + kept = every byte written (exact)" (= written (+ kept dropped)))
                (check "the gap is the oldest prefix, reason :rotated"
                       (and (= 1 (length gaps)) (= 1 (sp:log-gap-first-line (first gaps)))
                            (equal '(:rotated) (sp:log-gap-reasons (first gaps)))))
                (show "first kept / last line" (list (clip (sp:log-record-text (first records)) 10)
                                                     (clip (sp:log-record-text (car (last records))) 10)))))
            (let* ((all (sp:log-read u :types t))
                   (drops (remove :drop all :key #'sp:log-record-type :test-not #'eq))
                   (header (find :header all :key #'sp:log-record-type :from-end t)))
              (show "drop markers in the retained segments" (length drops))
              (note "first and last drop marker:")
              (print-record (first drops))
              (print-record (car (last drops)))
              (note "newest segment's header (cumulative drops):")
              (print-record header)
              (note "end record:")
              (print-record (car (last all)))
              (let ((later (remove-if (lambda (r) (< (sp:log-record-line r) (sp:log-record-line header))) drops)))
                (check "gap bytes = the newest header's :dropped-bytes + the markers written after it"
                       (= (+ (getf (sp:log-record-detail header) :dropped-bytes)
                             (reduce #'+ later :key (lambda (r) (getf (sp:log-record-detail r) :bytes))))
                          (reduce #'+ (nth-value 1 (sp:log-read u)) :key #'sp:log-gap-bytes)))))
            (show "memory: sink-stats total, unit-output length"
                  (list (sp:sink-stats (sp:unit-output-sink u)) (length (sp:unit-output u))))
            (check "memory keeps only the head + tail"
                   (<= (length (sp:unit-output u)) (+ sp:+default-head-size+ sp:+default-tail-size+ 64)))
            (show "disk: this unit's segment + index bytes vs 4 × 256 KiB" (list (dir-bytes ldir) (* 4 256 1024)))

            ;; 7b: range reads across a rotation boundary, paging
            (stage "7b. log-read by line range across a rotation boundary")
            (let* ((infos (sp:log-segments u))
                   (boundary (sp:log-segment-last-line (second infos))))
              (note "segment ~d ends at line ~d; read lines ~d-~d"
                    (sp:log-segment-number (second infos)) boundary (- boundary 1) (+ boundary 3))
              (multiple-value-bind (records gaps next) (sp:log-read u :start (- boundary 1) :end (+ boundary 3))
                (dolist (r records) (print-record r))
                (show "gaps / next-line" (list gaps next))
                (check "the output lines continue across the segments (the header line is not output)"
                       (and (>= (length records) 3)
                            (/= (sp:log-record-segment (first records)) (sp:log-record-segment (car (last records)))))))
              (note ":types t on the same range shows the next segment's header:")
              (dolist (r (sp:log-read u :start boundary :end (+ boundary 1) :types t)) (print-record r)))
            (multiple-value-bind (records gaps next) (sp:log-read u :limit 2)
              (show "paging: :limit 2 → lines / gaps / next-line"
                    (list (mapcar #'sp:log-record-line records) (length gaps) next))
              (show "  then :start next-line :limit 2"
                    (mapcar #'sp:log-record-line (sp:log-read u :start next :limit 2))))

            ;; 7c: tail
            (stage "7c. log-tail 3")
            (dolist (r (sp:log-tail u 3)) (print-record r))))

        ;; 7d: follow a live server
        (stage "7d. log-follow a live echo server")
        (let* ((srv (sp:start-unit sup :rpc :command (echo-server-argv)))
               (next (nth-value 2 (sp:log-follow srv))))
          (show "follow with nothing new, :wait 0.3 → (records ended-p)"
                (multiple-value-bind (r g n e) (sp:log-follow srv :from next :wait 0.3)
                  (declare (ignore g n)) (list r e)))
          (bt2:make-thread (lambda () (sleep 0.5) (sp:send-unit-input srv (format nil "ping-1~%ping-2~%")))
                           :name "sender")
          (let ((start (get-internal-real-time)))
            (multiple-value-bind (records gaps next2 ended) (sp:log-follow srv :from next :wait 10)
              (declare (ignore gaps))
              (show "woken after (s, input sent at 0.5)" (elapsed-since start))
              (dolist (r records) (print-record r))
              (show "ended-p while running" ended)
              (check "follow returned the echoed lines" (member "ping-1" (mapcar #'sp:log-record-text records)
                                                                :test #'equal))
              (sp:stop-unit srv :grace 2)
              (wait-until (lambda () (sp:log-closed-p srv)))
              (multiple-value-bind (r2 g2 n2 ended2) (sp:log-follow srv :from next2 :wait 5)
                (declare (ignore g2 n2))
                (show "after stop-unit: records / ended-p" (list (mapcar #'sp:log-record-text r2) ended2))
                (check "ENDED-P once the end record is there" ended2)))))

        ;; 7e: binary output
        (stage "7e. binary output round trip (two text lines, then bytes 0..255, 255..0, 2000 pseudo-random)")
        (let* ((state (sb-ext:seed-random-state 7))
               (data (concatenate '(vector (unsigned-byte 8))
                                  (octets-of (format nil "a text line~%tab~cthere, ESC ~c[1m bold~%" (code-char 9) (code-char 27)))
                                  (loop for i below 256 collect i) (loop for i from 255 downto 0 collect i)
                                  (loop repeat 2000 collect (random 256 state))))
               (u (sp:start-unit sup :shell-job
                                     :command (lisp-argv
                                               (format nil "(progn (write-sequence (coerce '~A '(vector (unsigned-byte 8))) sb-sys:*stdout*) (finish-output sb-sys:*stdout*))"
                                                       (lit (coerce data 'list)))))))
          (check "exited" (eq :exited (sp:wait-unit u :timeout 60)))
          (let* ((records (sp:log-read u))
                 (back (stream-bytes records :stdout))
                 (seg (sp:log-segment-path (first (sp:log-segments u)))))
            (show "bytes written / rebuilt from records" (list (length data) (length back)))
            (check "every byte unchanged (equalp)" (equalp data back))
            (show "records" (length records))
            (show "a raw base64 record in the segment file" (clip (or (first-raw-line-with seg "ob.")
                                                                       (first-raw-line-with seg "ob+") "")
                                                                   72))
            (note "a raw escaped-text record (TAB kept raw, ESC written \\x1b): ~a"
                  (first-raw-line-with seg "ot." "ESC"))))

        (stage "disk usage vs the total cap")
        (show "supervisor-log-usage / log-total-cap" (list (sp:supervisor-log-usage sup) (sp:supervisor-log-total-cap sup)))
        (check "under the cap" (<= (sp:supervisor-log-usage sup) (sp:supervisor-log-total-cap sup))))

      ;; 7f: torn tail after SIGKILL
      (stage "7f. a child supervisor with a chatty thread unit, SIGKILLed mid-output")
      (demo-logs-torn-tail)

      ;; 7g: total cap
      (stage "7g. total-cap eviction: :log-total-cap 6000, segments of 2048 bytes")
      (demo-logs-total-cap))))

(defun chatty-child-form (state-dir)
  (sp-form
   (format nil "(let* ((ready nil) (sup (sp:make-supervisor :state-directory ~A :exit-hook nil :log-segment-size 16384 :log-segments 1000 :log-max-record 1024)) (u (sp:start-unit sup :subagent :function (lambda (u) (declare (ignore u)) (loop for i from 1 do (format t \"n-~~D~~%\" i) (finish-output) (when (= i 2000) (setf ready t)) (sleep 0.0005)))))) (loop repeat 6000 until ready do (sleep 0.01)) (format t \"READY ~~S ~~S~~%\" (sp:supervisor-id sup) (sp:unit-id u)) (finish-output) (sleep 300))"
           (lit (namestring state-dir)))))

(defun demo-logs-torn-tail ()
  (with-temp-dir (dir "hd-sup-crash")
    (let* ((child (launch-child-image (chatty-child-form dir)))
           (line (child-ready-line child))
           (pid (pp:pid (child-handle child))))
      (unwind-protect
           (when (check "the child is writing" line)
             (sleep 0.2)
             (os-kill pid)
             (check "the child image is gone" (all-dead-p (list pid)))
             (let* ((logs (sp:find-unit-logs dir))
                    (log (first logs)))
               (show "find-unit-logs" (length logs))
               (show "unit id / closed-p (no end record after a SIGKILL)"
                     (list (sp:unit-log-unit-id log) (sp:log-closed-p log)))
               (multiple-value-bind (records gaps) (sp:log-read log)
                 (let ((k (length records)))
                   (show "records read by this image / gaps" (list k gaps))
                   (show "segments" (length (sp:log-segments log)))
                   (check "contiguous n-1 … n-K, no gap"
                          (and (null gaps)
                               (equal (sp:log-record-text (car (last records))) (format nil "n-~d" k))))
                   (show "last segment torn-p as the kill left it" (sp:log-segment-torn-p (car (last (sp:log-segments log)))))
                   (note "append half a record (no LF, no CRC), as a kill mid-write leaves it")
                   (with-open-file (s (sp:log-segment-path (car (last (sp:log-segments log))))
                                      :direction :output :element-type '(unsigned-byte 8) :if-exists :append)
                     (write-sequence (octets-of "999999 17000000 1 ot. n-tor") s))
                   (let ((again (sp:log-read log)))
                     (show "torn-p now" (sp:log-segment-torn-p (car (last (sp:log-segments log)))))
                     (check "the torn tail is skipped: the same records"
                            (equal (mapcar #'sp:log-record-text records) (mapcar #'sp:log-record-text again))))
                   (show "log-tail 2" (mapcar #'sp:log-record-text (sp:log-tail log 2)))))))
        (ignore-errors (os-kill pid))))))

(defun writer (sup tag n &optional then)
  "A thread unit printing N lines of 100 bytes (+ LF)."
  (sp:start-unit sup :subagent
                     :function (lambda (u)
                                 (dotimes (i n)
                                   (format t "~a~5,'0d~a~%" tag (1+ i) (make-string 93 :initial-element #\x)))
                                 (finish-output)
                                 (if then (funcall then u) :done))))

(defun demo-logs-total-cap ()
  (with-temp-dir (dir "hd-sup-cap")
    (with-supervisor (sup :state-directory dir :log-segment-size 2048 :log-segments 20
                          :log-max-record 256 :log-total-cap 6000)
      (labels ((disk (u) (dir-bytes (sp:unit-log-directory (sp:unit-log u))))
               (line-count (u) (length (sp:log-read u)))
               (reasons (u) (reduce #'append (nth-value 1 (sp:log-read u)) :key #'sp:log-gap-reasons))
               (usage () (format nil "usage ~d / cap 6000" (sp:supervisor-log-usage sup))))
        (let ((a (writer sup "a-" 26)))
          (sp:wait-unit a :timeout 10)
          (wait-until (lambda () (sp:log-closed-p a)))
          (note "a (26 lines × 101 bytes) finished: ~a, a on disk ~d, ~d lines" (usage) (disk a) (line-count a))
          (let ((b (writer sup "b-" 26)))
            (sp:wait-unit b :timeout 10)
            (wait-until (lambda () (<= (sp:supervisor-log-usage sup) 6000)) :timeout 5)
            (note "b (26 lines) finished: ~a" (usage))
            (note "a: ~d lines, ~d segment file(s), ~d bytes, gaps ~s, closed-p ~s"
                  (line-count a) (length (files-in (sp:unit-log-directory (sp:unit-log a)) "seg"))
                  (disk a) (reasons a) (sp:log-closed-p a))
            (note "b: ~d lines, ~d bytes, gaps ~s" (line-count b) (disk b) (reasons b))
            (check "a (finished first) lost segments to :total-cap, b kept all 26 lines"
                   (and (member :total-cap (reasons a)) (= 26 (line-count b))))
            (let* ((release nil)
                   (c (writer sup "c-" 120 (lambda (u) (loop until (or release (sp:unit-stop-requested-p u))
                                                             do (sleep 0.01))))))
              (wait-until (lambda () (let ((r (sp:log-tail c 1)))
                                       (and r (search "c-00120" (sp:log-record-text (first r)))))))
              (wait-until (lambda () (<= (sp:supervisor-log-usage sup) 6000)) :timeout 5)
              (note "c (running) wrote 120 lines: ~a" (usage))
              (note "a: ~d lines, stub file ~s, ~d bytes" (line-count a)
                    (mapcar #'file-namestring (files-in (sp:unit-log-directory (sp:unit-log a)) "seg")) (disk a))
              (note "b: ~d lines, ~d bytes, gaps ~s" (line-count b) (disk b) (reasons b))
              (note "c: ~d lines kept, ~d bytes, gaps ~s, state ~s" (line-count c) (disk c) (reasons c)
                    (sp:unit-state c))
              (note "a's stub:")
              (dolist (r (sp:log-read a :types t)) (print-record r))
              (let ((a-gaps (nth-value 1 (sp:log-read a))))
                (dolist (g a-gaps) (print-gap g))
                (check "a's stub accounts for every byte a wrote (26 × 101)"
                       (= (* 26 101) (reduce #'+ a-gaps :key #'sp:log-gap-bytes))))
              (check "finished units emptied first, then c's old segments; c's newest kept"
                     (and (= 0 (line-count a)) (= 0 (line-count b)) (member :total-cap (reasons c))
                          (probe-file (sp:log-segment-path (car (last (sp:log-segments c)))))))
              (check "usage = the bytes of every segment and index file"
                     (= (sp:supervisor-log-usage sup) (+ (disk a) (disk b) (disk c))))
              (setf release t)
              (sp:wait-unit c :timeout 10))))))))
