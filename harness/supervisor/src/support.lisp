(in-package #:harness-demos-supervisor)

;;; Output helpers: one banner per demo, one line per step, values printed as is.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun stage (fmt &rest args)
  (format t "~&~%-- ~?~%" fmt args))

(defun show (label value)
  (format t "~&   ~a: ~s~%" label value))

(defun note (fmt &rest args)
  (format t "~&   ~?~%" fmt args))

(defvar *failed-checks* nil "Labels of the failed CHECKs of the running demo.")

(defun check (label ok)
  "Print LABEL with OK or FAILED; a failed check makes the demo fail at its end."
  (format t "~&   [~:[FAILED~;ok~]] ~a~%" ok label)
  (unless ok (push label *failed-checks*))
  ok)

(defmacro with-checks (() &body body)
  "BODY, then an error when any CHECK in it failed."
  `(let ((*failed-checks* nil))
     (multiple-value-prog1 (progn ,@body)
       (when *failed-checks*
         (error "~d check~:p failed: ~{~a~^; ~}" (length *failed-checks*)
                (reverse *failed-checks*))))))

(defvar *outer-restarts* nil
  "Restarts established outside the demos (toplevel ABORT, EXIT …), left out of listings.")

(defun restart-names (condition)
  "Names of the restarts CONDITION offers, without the toplevel ones."
  (loop for r in (compute-restarts condition)
        for name = (restart-name r)
        when (and name (not (member r *outer-restarts*))) collect name))

(defun now () (/ (get-internal-real-time) internal-time-units-per-second))

(defun elapsed-since (start)
  "Seconds since START (an internal real time), as a float with one decimal."
  (/ (round (* 10 (/ (- (get-internal-real-time) start) internal-time-units-per-second))) 10.0))

(defun wait-until (predicate &key (timeout 10) (step 0.02))
  "Poll PREDICATE until true or TIMEOUT seconds; → its last value."
  (loop with deadline = (+ (now) timeout)
        for v = (funcall predicate)
        when (or v (> (now) deadline)) return v
        do (sleep step)))

(defun windows-p () (uiop:os-windows-p))

;;; Temp directories (created and deleted by each demo).

(defun make-temp-dir (prefix)
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~a-~36r" prefix (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defmacro with-temp-dir ((var prefix) &body body)
  `(let ((,var (make-temp-dir ,prefix)))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore)))))

;;; Supervisors made by the demos are shut down quietly at the end.

(defun quiet-shutdown (sup)
  "Shut SUP down for cleanup: short grace, abandon what will not die."
  (ignore-errors
   (handler-bind ((sp:stop-failed (lambda (c)
                                    (let ((r (find-restart 'sp:abandon c)))
                                      (when r (invoke-restart r))))))
     (sp:shutdown-supervisor sup :grace 0.5 :force-grace 1))))

(defmacro with-supervisor ((var &rest keys) &body body)
  `(let ((,var (sp:make-supervisor :exit-hook nil ,@keys)))
     (unwind-protect (progn ,@body)
       (quiet-shutdown ,var))))

;;; Child programs: this SBCL itself (runtime + core, no init files), so the same
;;; programs run on macOS, Linux and Windows.

(defun lisp-argv (&rest forms)
  "argv running this SBCL with no init files, evaluating FORMS (strings) in order."
  (append (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                "--core" (uiop:native-namestring sb-ext:*core-pathname*)
                "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
          (loop for f in forms append (list "--eval" f))))

(defun lit (object)
  "OBJECT printed readably for embedding in a child's --eval form."
  (with-standard-io-syntax (prin1-to-string object)))

(defparameter +pid-expr+
  "#+win32 (sb-alien:alien-funcall (sb-alien:extern-alien \"GetCurrentProcessId\" (function sb-alien:unsigned-long))) #-win32 (sb-unix:unix-getpid)")

(defun shell-argv (line)
  "A shell job: /bin/sh -c LINE, cmd.exe /d /s /c LINE on Windows."
  (if (windows-p)
      (list "cmd.exe" "/d" "/s" "/c" line)
      (list "/bin/sh" "-c" line)))

(defun shell-sleep-argv (seconds)
  (shell-argv (if (windows-p)
                  (format nil "ping -n ~D 127.0.0.1 >nul" (1+ seconds))
                  (format nil "sleep ~D" seconds))))

(defun echo-server-argv ()
  "A tiny stdio \"server\": echoes every stdin line to stdout until EOF."
  (lisp-argv "(loop for l = (read-line *standard-input* nil) while l do (write-line l) (finish-output))"))

(defun pid-file-form (pid-file)
  (format nil "(with-open-file (s ~A :direction :output :if-exists :supersede) (print ~A s))"
          (lit (namestring pid-file)) +pid-expr+))

(defun sleeper-argv (seconds &key pid-file)
  (lisp-argv (format nil "(progn ~A (sleep ~D))" (if pid-file (pid-file-form pid-file) "nil") seconds)))

(defun tree-argv (pid-dir n seconds)
  "A child that writes PID-DIR/root.pid, starts N grandchild sleepers (each writes
PID-DIR/gc-<i>.pid; stdin inherited, so they stay in its process group on Unix),
then sleeps SECONDS."
  (let ((dir (uiop:ensure-directory-pathname pid-dir)))
    (lisp-argv
     (format nil "(progn ~A ~{~A ~}(sleep ~D))"
             (pid-file-form (merge-pathnames "root.pid" dir))
             (loop for i from 1 to n
                   for argv = (sleeper-argv seconds
                                            :pid-file (merge-pathnames (format nil "gc-~D.pid" i) dir))
                   collect (format nil "(sb-ext:run-program ~A '~A :wait nil :input t :output nil :error nil)"
                                   (lit (first argv)) (lit (rest argv))))
             seconds))))

(defun read-pid-files (dir)
  "Pids in DIR/*.pid (complete files only)."
  (loop for p in (directory (merge-pathnames "*.pid" (uiop:ensure-directory-pathname dir)))
        for pid = (ignore-errors
                   (with-open-file (s p) (let ((*read-eval* nil)) (read s nil nil))))
        when (integerp pid) collect pid))

;;; Liveness and external kills, through ps / kill and tasklist / taskkill
;;; directly (not through the supervisor under demonstration).

(defun pid-alive-p (pid)
  (if (windows-p)
      (let ((out (nth-value 1 (pp:run (list "tasklist" "/FI" (format nil "PID eq ~D" pid) "/NH" "/FO" "CSV")
                                      :output :string :error-output nil))))
        (and (search (format nil "\"~D\"" pid) out) t))
      (let ((out (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (nth-value 1 (pp:run (list "ps" "-o" "stat=" "-p" (princ-to-string pid))
                                                   :output :string :error-output nil)))))
        (and (plusp (length out)) (char/= (char out 0) #\Z)))))

(defun os-kill (pid)
  "Kill PID from outside (SIGKILL / taskkill /F), as a crash or the OOM killer would."
  (if (windows-p)
      (pp:run (list "taskkill" "/F" "/PID" (princ-to-string pid)) :output nil :error-output nil)
      (pp:run (list "kill" "-9" (princ-to-string pid)) :output nil :error-output nil)))

(defun all-dead-p (pids &key (timeout 10))
  (wait-until (lambda () (notany #'pid-alive-p pids)) :timeout timeout :step 0.1))

;;; Units, printed.

(defun scope-label (scope)
  (if (sp:scope-path scope)
      (format nil "~{~a~^ › ~}" (sp:scope-path scope))
      "(root)"))

(defun unit-table (units)
  (format t "~&   ~16a ~11a ~38a ~9a ~a~%" "id" "kind" "owner" "state" "pid")
  (dolist (u units)
    (format t "~&   ~16a ~11s ~38a ~9s ~a~%"
            (sp:unit-id u) (sp:unit-kind u) (scope-label (sp:unit-owner u))
            (sp:unit-state u) (or (sp:unit-pid u) "-"))))

(defun ids (units) (mapcar #'sp:unit-id units))

;;; Events, collected from a subscription.

(defstruct (collector (:constructor %make-collector))
  (lock (bt2:make-lock :name "collector"))
  (events '())
  subscription)

(defun make-collector (sup &key since)
  (let ((c (%make-collector)))
    (setf (collector-subscription c)
          (sp:subscribe sup (lambda (e)
                              (bt2:with-lock-held ((collector-lock c))
                                (push e (collector-events c))))
                        :since since))
    c))

(defun collected (c)
  (bt2:with-lock-held ((collector-lock c)) (reverse (collector-events c))))

(defun wait-collected (c predicate &key (timeout 10))
  (wait-until (lambda () (funcall predicate (collected c))) :timeout timeout)
  (collected c))

(defun print-event (e &optional t0)
  (format t "~&   #~3d ~10a ~18s ~9s → ~9s~@[ ~s~]~@[  (+~,2f s)~]~%"
          (sp:event-seq e) (sp:event-unit-id e) (sp:event-type e)
          (sp:event-from e) (sp:event-to e) (sp:event-detail e)
          (and t0 (- (sp:event-monotonic e) t0))))

;;; Shared fixture of Demos 1-3 (one supervisor, its scopes and units).

(defvar *shared* nil "Plist of the Demos 1-3 fixture.")

(defun shared (key) (getf *shared* key))

(defun shutdown-shared ()
  (let ((sup (shared :sup)))
    (when sup (quiet-shutdown sup)))
  (setf *shared* nil))

;;; Strings

(defun clip (string &optional (n 40))
  (if (> (length string) n)
      (format nil "~a…(~d chars)" (subseq string 0 n) (length string))
      string))
