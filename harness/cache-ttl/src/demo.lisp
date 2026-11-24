;;;; A tour of cache-protocol 0.2.0's in-memory TTL cache: the fixes for
;;;; egao1980/cache-protocol#1 (a TTL lived up to 1 s too long; incr and cas
;;;; were not atomic), #2 (incr dropped the TTL) and #4 (incr took text that
;;;; Redis INCR refuses). Every result with a known answer is checked:
;;;; RUN-DEMOS returns NIL when one differs or a section stops on an error.

(in-package #:harness-demos-cache-ttl)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args))

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

(defun condition-text (c)
  (format nil "~(~a~): ~a" (type-of c) c))

(defmacro outcome (form)
  "FORM's value, or the text of the CACHE-ERROR it signalled."
  `(handler-case ,form
     (cache:cache-error (e) (condition-text e))))

(defun error-p (outcome)
  (and (stringp outcome) (eql 0 (search "cache-error" outcome))))

(defun ms (seconds)
  (format nil "~,3f ms" (* 1000 seconds)))

(defun say (&rest lines)
  "Print each of LINES on its own line, indented by two spaces (\"\" is a blank line)."
  (dolist (line lines)
    (if (string= line "")
        (terpri)
        (out "  ~a~%" line))))

;;; 1. The default clock -----------------------------------------------------

(defun wait-for-second-boundary ()
  "Return just after GET-UNIVERSAL-TIME (0.1.0's clock) steps to a new second."
  (let ((start (get-universal-time)))
    (loop while (= start (get-universal-time)) do (sleep 0.001))))

(defun ttl-cut-short-count (c ttl tries)
  "Puts of TTL seconds on C whose entry was gone before TTL had elapsed,
measured with the cache's own default clock read before the put."
  (loop repeat tries
        count (let ((t0 (cache:monotonic-seconds)))
                (cache:cache-put c "k" "v" :ttl ttl)
                (loop
                  (let ((present (cache:cache-get c "k"))
                        (elapsed (- (cache:monotonic-seconds) t0)))
                    (when (>= elapsed (* 98/100 ttl)) (return nil))
                    (unless present (return t)))))))

(defun observed-lifetime (c ttl)
  "Seconds from just before a put of TTL until C no longer returns the entry,
polled without sleeping."
  (let ((t0 (cache:monotonic-seconds)))
    (cache:cache-put c "life" "v" :ttl ttl)
    (loop while (cache:cache-get c "life"))
    (- (cache:monotonic-seconds) t0)))

(defun demo-clock ()
  (say "The default clock is MONOTONIC-SECONDS (0.1.0 used GET-UNIVERSAL-TIME, whole"
       (format nil "seconds). It steps by +monotonic-tick+ = ~a s here." cache:+monotonic-tick+)
       "")
  (let ((c (cache:make-in-memory-ttl-cache)))
    (wait-for-second-boundary)
    (cache:cache-put c "session" "alice" :ttl 1)
    (let ((now (cache:cache-get c "session")))
      (sleep 1.1)
      (let ((later (cache:cache-get c "session")))
        (say "issue #1's repro: :ttl 1 put just after a whole second"
             (format nil "  read at once        -> ~s" now)
             (format nil "  read 1.1 s later    -> ~s   (0.1.0 still returned \"alice\")" later))
        (check now "alice" "read at once")
        (check later nil ":ttl 1 is gone 1.1 s later"))))
  (let ((c (cache:make-in-memory-ttl-cache)))
    (cache:cache-put c "token" "t-1" :ttl 0.3)
    (let ((now (cache:cache-get c "token")))
      (sleep 0.35)
      (let ((later (cache:cache-get c "token")))
        (say ":ttl 0.3 (sub-second)"
             (format nil "  read at once        -> ~s" now)
             (format nil "  read 0.35 s later   -> ~s" later))
        (check now "t-1" ":ttl 0.3 read at once")
        (check later nil ":ttl 0.3 gone after 0.35 s"))))
  (let ((n (ttl-cut-short-count (cache:make-in-memory-ttl-cache) 1/200 200)))
    (say (format nil ":ttl 1/200 (5 ms), 200 puts: gone before 5 ms had elapsed in ~d of them" n))
    (check n 0 "a 5 ms TTL is never cut short"))
  (let* ((c (cache:make-in-memory-ttl-cache))
         (lives (loop repeat 20 collect (observed-lifetime c 1/50))))
    (say (format nil ":ttl 1/50 (20 ms), 20 puts polled without sleeping: lifetime ~a .. ~a"
                 (ms (reduce #'min lives)) (ms (reduce #'max lives))))
    (check (every (lambda (s) (>= s 1/50)) lives) t "every 20 ms entry lived at least 20 ms")))

;;; 2. An injected clock -----------------------------------------------------

(defun demo-injected-clock ()
  (say ":clock is any function returning seconds as a real. Its readings are taken as"
       "exact: an entry expires once clock >= put time + TTL (cachetools: timer >= expires)."
       "")
  (let* ((now 1000)
         (c (cache:make-in-memory-ttl-cache :clock (lambda () now))))
    (cache:cache-put c "page" "<html>" :ttl 600)
    (say "put \"page\" :ttl 600 at t=1000")
    (dolist (at '(1599 1600))
      (setf now at)
      (let ((v (cache:cache-get c "page")))
        (out "    t=~d  get -> ~s~%" at v)
        (check v (if (= at 1599) "<html>" nil) (format nil "get at t=~d" at))))
    (let ((deleted (cache:cache-delete c "page")))
      (out "    t=1600  delete of the expired key -> ~s (Redis DEL answers 0)~%" deleted)
      (check deleted nil "delete of an expired key")))
  (let* ((now 1000)
         (c (cache:make-in-memory-ttl-cache :clock (lambda () now))))
    (cache:cache-put c "f" "half" :ttl 1/2)
    (cache:cache-put c "g" "float" :ttl 0.1)
    (say "put \"f\" :ttl 1/2 and \"g\" :ttl 0.1 at t=1000 (a float TTL is RATIONALIZEd: 0.1 -> 1/10)")
    (loop for (at f g) in '((100009/100 "half" "float") (10001/10 "half" nil)
                            (100049/100 "half" nil) (2001/2 nil nil))
          do (setf now at)
             (let ((fv (cache:cache-get c "f"))
                   (gv (cache:cache-get c "g")))
               (out "    t=~,2f  f -> ~s  g -> ~s~%" at fv gv)
               (check (list fv gv) (list f g) (format nil "f and g at t=~a" at))))))

;;; 3. TTL values --------------------------------------------------------------

(defun nan ()
  "A quiet double-float NaN, or NIL where this Lisp has no way to make one."
  #+sbcl (sb-kernel:make-double-float #x7FF80000 0)
  #-sbcl nil)

(defun demo-ttl-values ()
  (let* ((now 0)
         (c (cache:make-in-memory-ttl-cache :clock (lambda () now)))
         (rows `(("nil (no TTL)" nil "kept" "kept")
                 ("1/3" 1/3 "kept" "gone")
                 ("0" 0 "gone" "gone")
                 ("-5" -5 "gone" "gone")
                 #+sbcl ("+inf (double)" ,sb-ext:double-float-positive-infinity "kept" "kept")
                 #+sbcl ("+inf (single)" ,sb-ext:single-float-positive-infinity "kept" "kept")
                 #+sbcl ("-inf" ,sb-ext:double-float-negative-infinity "gone" "gone"))))
    (say "Each value put at t=0 and read at t=0 and at t=10^9 (cachetools semantics for"
         "the infinities):")
    (out "    ~16a ~8a ~a~%" ":ttl" "t=0" "t=10^9")
    (dolist (row rows)
      (destructuring-bind (label ttl expect-0 expect-later) row
        (setf now 0)
        (cache:cache-put c label "v" :ttl ttl)
        (let ((at-0 (if (cache:cache-get c label) "kept" "gone")))
          (setf now (expt 10 9))
          (let ((later (if (cache:cache-get c label) "kept" "gone")))
            (out "    ~16a ~8a ~a~%" label at-0 later)
            (check (list at-0 later) (list expect-0 expect-later)
                   (format nil ":ttl ~a at t=0 and t=10^9" label)))))))
  (let ((nan (nan)))
    (when nan
      (let ((r (outcome (cache:cache-put (cache:make-in-memory-ttl-cache) "k" "v" :ttl nan))))
        (out "  :ttl NaN -> ~a~%" r)
        (check (error-p r) t ":ttl NaN signals cache-error")))))

;;; 4. Atomic incr and cas -------------------------------------------------------

(defun run-threads (n fn)
  (mapc #'bt2:join-thread
        (loop for i below n
              collect (let ((i i)) (bt2:make-thread (lambda () (funcall fn i)))))))

(defun demo-atomicity ()
  (let ((c (cache:make-in-memory-ttl-cache)))
    (run-threads 8 (lambda (i) (declare (ignore i)) (dotimes (k 20000) (cache:cache-incr c "n"))))
    (let ((n (cache:cache-get c "n")))
      (out "  8 threads x 20000 cache-incr          -> ~d (0.1.0: 30609 .. 56113 of 160000)~%" n)
      (check n 160000 "no incr lost")))
  (let ((c (cache:make-in-memory-ttl-cache)))
    (run-threads 8 (lambda (i) (dotimes (k 10000) (cache:cache-incr c "net" :delta (if (evenp i) 3 -2)))))
    (let ((n (cache:cache-get c "net")))
      (out "  4 threads :delta 3 and 4 threads :delta -2, 10000 each -> ~d~%" n)
      (check n 40000 "mixed deltas")))
  (let ((c (cache:make-in-memory-ttl-cache))
        (wins 0)
        (lock (bt2:make-lock)))
    (cache:cache-put c "k" 0)
    (run-threads 8 (lambda (i)
                     (declare (ignore i))
                     (dotimes (k 5000)
                       (let ((seen (cache:cache-get c "k")))
                         (handler-case (progn (cache:cache-cas c "k" seen (1+ seen))
                                              (bt2:with-lock-held (lock) (incf wins)))
                           (cache:cache-cas-mismatch () nil))))))
    (let ((v (cache:cache-get c "k")))
      (out "  8 threads racing cas N -> N+1, 5000 tries each: ~d wins, value ~d~%~
       ~4@T(equal: one winner per value; 0.1.0 had 33983 wins for value 31100)~%" wins v)
      (check v wins "each value has exactly one winner (0.1.0: 33983 wins, value 31100)")))
  (let ((c (cache:make-in-memory-ttl-cache))
        (owners (make-array 8 :initial-element nil)))
    (run-threads 8 (lambda (i)
                     (setf (aref owners i)
                           (handler-case (progn (cache:cache-cas c "lock" nil (format nil "worker-~d" i)) t)
                             (cache:cache-cas-mismatch () nil)))))
    (let ((n (count t owners)))
      (out "  8 threads: cas \"lock\" nil -> \"worker-i\" (create if absent): ~d winner, held by ~s~%"
           n (cache:cache-get c "lock"))
      (check n 1 "exactly one thread creates the key"))))

;;; 5. incr keeps the TTL --------------------------------------------------------

(defun demo-incr-ttl ()
  (say "A rate limit of 3 requests per 60 s window, on an injected clock: the first"
       "request creates the counter with cas nil -> 1 :ttl 60, later ones cache-incr it."
       "cache-incr keeps the entry's TTL, as Redis INCR does (0.1.0 dropped it, so the"
       "window never ended)."
       "")
  (let* ((now 1000)
         (c (cache:make-in-memory-ttl-cache :clock (lambda () now))))
    (flet ((request (at)
             (setf now at)
             (let* ((n (or (handler-case (cache:cache-cas c "hits:ann" nil 1 :ttl 60)
                             (cache:cache-cas-mismatch () nil))
                           (cache:cache-incr c "hits:ann")))
                    (ok (<= n 3)))
               (out "    t=~4d  hits ~d -> ~:[429 Too Many Requests~;allowed~]~%" at n ok)
               ok)))
      (let ((answers (mapcar #'request '(1000 1010 1030 1059 1060 1061))))
        (check answers '(t t t nil t t) "allowed at t=1000, 1010, 1030; refused at 1059; new window at 1060")))))

;;; 6. incr follows Redis INCR ------------------------------------------------

(defun demo-incr-rules ()
  (say "The value's text must be what Redis's string2ll accepts, and the value and"
       "result must fit in a signed 64-bit integer:"
       "")
  (let ((rows '(("41" 1 42) ("-1" 1 0) ("0" 1 1) (41 1 42)
                ("007" 1 :error) ("-0" 1 :error) ("00" 1 :error) (" 1" 1 :error)
                ("+1" 1 :error) ("1-2" 1 :error) ("" 1 :error) ("abc" 1 :error)
                (1.5 1 :error) (:keyword 1 :error)
                ("9223372036854775806" 1 9223372036854775807)
                ("9223372036854775807" 1 :error)
                ("-9223372036854775807" -1 -9223372036854775808)
                ("-9223372036854775808" -1 :error)
                ("99999999999999999999" 1 :error))))
    (dolist (row rows)
      (destructuring-bind (value delta expect) row
        (let* ((c (cache:make-in-memory-ttl-cache))
               (r (progn (cache:cache-put c "x" value)
                         (outcome (cache:cache-incr c "x" :delta delta)))))
          (out "    ~24s :delta ~2d -> ~a~%" value delta (if (stringp r) r (prin1-to-string r)))
          (check (if (error-p r) :error r) expect (format nil "incr of ~s" value))))))
  (let* ((c (cache:make-in-memory-ttl-cache))
         (r (progn (cache:cache-put c "x" "007") (outcome (cache:cache-incr c "x")))))
    (declare (ignore r))
    (let ((kept (cache:cache-get c "x")))
      (out "  after a refused incr the value is unchanged: ~s~%" kept)
      (check kept "007" "refused incr leaves the value"))))

;;; 7. The CAS mismatch condition ----------------------------------------------

(defun demo-cas-condition ()
  (let ((c (cache:make-in-memory-ttl-cache)))
    (cache:cache-put c "config" "v2")
    (handler-case (cache:cache-cas c "config" "v1" "v3")
      (cache:cache-cas-mismatch (e)
        (out "  cas \"config\" \"v1\" -> \"v3\" while it holds \"v2\":~%    ~a~%    expected ~s, actual ~s~%"
             (condition-text e) (cache:cache-cas-mismatch-expected e) (cache:cache-cas-mismatch-actual e))
        (check (cache:cache-cas-mismatch-actual e) "v2" "mismatch carries the actual value")))
    (let ((r (handler-bind ((cache:cache-cas-mismatch
                              (lambda (e) (declare (ignore e)) (invoke-restart 'use-value :stale-ok))))
               (cache:cache-cas c "config" "v1" "v3"))))
      (out "  the same cas under handler-bind with the USE-VALUE restart -> ~s; value still ~s~%"
           r (cache:cache-get c "config"))
      (check (list r (cache:cache-get c "config")) '(:stale-ok "v2") "use-value restart"))))

;;; Run ---------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 110))
    (format t "~&cache-protocol ~a (~a ~a, ~a): the in-memory TTL cache~%"
            (asdf:component-version (asdf:find-system "cache-protocol"))
            (lisp-implementation-type) (lisp-implementation-version) (software-type))
    (section "1. The default clock: sub-second, never early (#1)" (demo-clock))
    (section "2. An injected clock is exact" (demo-injected-clock))
    (section "3. TTL values: fractions, 0, negative, infinities, NaN" (demo-ttl-values))
    (section "4. cache-incr and cache-cas are atomic across threads (#1)" (demo-atomicity))
    (section "5. cache-incr keeps the TTL: a rate-limit window (#2)" (demo-incr-ttl))
    (section "6. cache-incr follows Redis INCR: text rules and int64 range (#4)" (demo-incr-rules))
    (section "7. cache-cas-mismatch and its USE-VALUE restart" (demo-cas-condition))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
