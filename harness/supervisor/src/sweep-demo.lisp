(in-package #:harness-demos-supervisor)

;;; Demo 6: crash recovery. A child SBCL loads supervisor-protocol, starts a
;;; supervisor on a state directory with a shell-job tree (a root and two
;;; grandchildren), an :rpc server and a thread, and is killed with SIGKILL
;;; (taskkill /F on Windows): no exit hook runs, its units become orphans. A new
;;; supervisor on the same state directory sweeps them first.

;;; Child images (also used by Demo 7's torn-tail step).

(defun loaded-system-directories ()
  (remove-duplicates
   (loop for name in (asdf:already-loaded-systems)
         for dir = (ignore-errors (asdf:system-source-directory name))
         when dir collect dir)
   :test #'equal))

(defun child-setup-form ()
  "Load supervisor-protocol in the child from the directories this image loaded it from."
  (format nil "(progn (require :asdf) (funcall (intern \"INITIALIZE-SOURCE-REGISTRY\" :asdf) '~A) (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (funcall (intern \"LOAD-SYSTEM\" :asdf) \"supervisor-protocol\")))"
          (lit `(:source-registry
                 ,@(mapcar (lambda (d) (list :directory d)) (loaded-system-directories))
                 :ignore-inherited-configuration))))

(defun sp-form (string)
  "STRING with sp: standing for supervisor-protocol: (child forms are read in CL-USER)."
  (let ((out (make-string-output-stream)))
    (loop with start = 0
          for pos = (search "sp:" string :start2 start)
          do (write-string string out :start start :end (or pos (length string)))
             (if pos
                 (progn (write-string "supervisor-protocol:" out) (setf start (+ pos 3)))
                 (return)))
    (get-output-stream-string out)))

(defstruct child handle (lock (bt2:make-lock :name "child")) (lines '()))

(defun launch-child-image (form)
  "A child SBCL that loads supervisor-protocol, then evaluates FORM. Its stdout
lines are collected. No process group: a kill reaches the child alone."
  (let* ((h (pp:launch (lisp-argv (child-setup-form) form)
                       :input nil :output :stream :error-output :output))
         (c (make-child :handle h)))
    (bt2:make-thread
     (lambda ()
       (ignore-errors
        (loop for l = (read-line (pp:stdout h) nil) while l
              do (bt2:with-lock-held ((child-lock c)) (push l (child-lines c))))))
     :name "child-reader")
    c))

(defun child-ready-line (c &key (timeout 240))
  "The child's first stdout line starting with READY, or NIL (its output printed)."
  (wait-until (lambda ()
                (or (bt2:with-lock-held ((child-lock c))
                      (find-if (lambda (l) (uiop:string-prefix-p "READY" l)) (child-lines c)))
                    (when (pp:exit-code (child-handle c))
                      (sleep 0.2)
                      (note "child exited early:~%~{     ~A~%~}" (last (reverse (child-lines c)) 20))
                      (return-from child-ready-line nil))))
              :timeout timeout :step 0.1))

(defun ready-values (line)
  (with-input-from-string (s (subseq line 5))
    (let ((*read-eval* nil))
      (loop for x = (read s nil nil) while x collect x))))

(defun child-units-form (state-dir pid-dir)
  (sp-form
   (format nil "(let* ((sup (sp:make-supervisor :state-directory ~A :exit-hook nil)) (tree (sp:start-unit sup :shell-job :command '~A)) (srv (sp:start-unit sup :rpc :command '~A))) (sp:start-unit sup :subagent :function (lambda (u) (loop until (sp:unit-stop-requested-p u) do (sleep 0.02)))) (loop repeat 600 until (= 3 (length (directory ~A))) do (sleep 0.05)) (format t \"READY ~~D ~~D ~~D~~%\" (sp:unit-pid tree) (sp:unit-pid srv) (length (sp:supervisor-units sup))) (finish-output) (sleep 600))"
           (lit (namestring state-dir))
           (lit (tree-argv pid-dir 2 300))
           (lit (sleeper-argv 300))
           (lit (namestring (merge-pathnames "*.pid" (uiop:ensure-directory-pathname pid-dir)))))))

(defun unit-files (state-dir)
  (directory (merge-pathnames "*.unit" (sp:unit-file-directory state-dir))))

(defun demo-sweep ()
  (banner "Demo 6: crash recovery — SIGKILL a child supervisor, the next one sweeps its orphans")
  (with-checks ()
    (with-temp-dir (state "hd-sup-state")
      (with-temp-dir (pids "hd-sup-pids")
        (let* ((child (launch-child-image (child-units-form state pids)))
               (start (get-internal-real-time))
               (line (child-ready-line child))
               (child-pid (pp:pid (child-handle child)))
               (orphans '()))
          (unwind-protect
               (when (check "the child supervisor started its units" line)
                 (destructuring-bind (tree-pid rpc-pid n) (ready-values line)
                   (show "child image ready after (s)" (elapsed-since start))
                   (setf orphans (list* rpc-pid (read-pid-files pids)))
                   (stage "before the crash")
                   (show "child image pid" child-pid)
                   (show "units in the child (shell-job tree, rpc, subagent thread)" n)
                   (show "shell-job root pid" tree-pid)
                   (show "grandchild pids" (remove tree-pid (read-pid-files pids)))
                   (show "rpc server pid" rpc-pid)
                   (show "unit files (process units only)"
                         (sort (mapcar #'file-namestring-tail (unit-files state)) #'string<))
                   (let ((plist (sp:read-unit-file (first (unit-files state)))))
                     (show "one unit file's keys" (loop for k in plist by #'cddr collect k)))
                   (check "4 processes: tree root, 2 grandchildren, rpc server" (= 4 (length orphans)))

                   (stage "SIGKILL the child image (taskkill /F on Windows): no exit hook runs")
                   (os-kill child-pid)
                   (pp:wait (child-handle child) :timeout 30)
                   (sleep 0.5)
                   (show "child alive" (pid-alive-p child-pid))
                   (show "orphans alive (pid alive-p)" (mapcar (lambda (p) (list p (pid-alive-p p))) orphans))
                   (check "the crash left every unit process behind" (every #'pid-alive-p orphans))

                   (stage "make-supervisor :state-directory (same directory) → sweep-orphans first")
                   (let ((t1 (get-internal-real-time)))
                     (with-supervisor (sup :state-directory state)
                       (show "make-supervisor seconds (incl. the sweep)" (elapsed-since t1))
                       (let ((report (sp:supervisor-sweep-report sup)))
                         (dolist (r (sort (copy-list report) #'string< :key #'sp:sweep-record-unit-id))
                           (note "~12a pid ~6d pgid ~6@a  ~s killed ~s"
                                 (sp:sweep-record-unit-id r) (sp:sweep-record-pid r)
                                 (or (sp:sweep-record-pgid r) "-") (sp:sweep-record-action r)
                                 (sort (copy-list (sp:sweep-record-killed-pids r)) #'<)))
                         (check "two records (rpc-1, shell-job-1), both :killed"
                                (and (equal '("rpc-1" "shell-job-1")
                                            (sort (mapcar #'sp:sweep-record-unit-id report) #'string<))
                                     (every (lambda (r) (eq :killed (sp:sweep-record-action r))) report)))
                         (check "the killed pids are exactly the four orphans"
                                (equal (sort (copy-list orphans) #'<)
                                       (sort (reduce #'append report :key #'sp:sweep-record-killed-pids) #'<))))
                       (stage "after the sweep")
                       (check "every orphan is dead, grandchildren included" (all-dead-p orphans))
                       (show "orphans alive" (mapcar (lambda (p) (list p (pid-alive-p p))) orphans))
                       (show "unit files left" (length (unit-files state)))))))
            (dolist (p (cons child-pid orphans)) (ignore-errors (os-kill p)))))))))

(defun file-namestring-tail (path)
  "The unit file's name with the supervisor id cut to <pid>-…"
  (let* ((name (file-namestring path))
         (sep (search "--" name)))
    (if sep (format nil "<sup>~a" (subseq name sep)) name)))
