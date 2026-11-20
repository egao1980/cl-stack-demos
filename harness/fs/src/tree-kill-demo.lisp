(in-package #:harness-demos-fs)

;;; Demo 6: process-protocol 0.4.0 / process-backend-uiop 0.3.1 — process groups,
;;; kill :tree t, process-pid, supports-p. The command is a shell whose background
;;; child (the grandchild) prints its pid and would outlive a plain kill.

(defparameter *tree-script*
  "sleep 30 & echo $!; wait"
  "Shell script: start a grandchild, print its pid, wait for it.")

(defun pid-alive-p (pid)
  "True when PID names a live process (kill -0), asked through process-protocol."
  (zerop (proc:run (list "/bin/sh" "-c" (format nil "kill -0 ~d 2>/dev/null" pid)))))

(defun launch-tree (&rest keys)
  "Launch *TREE-SCRIPT* → (values handle grandchild-pid)."
  (let* ((h (apply #'proc:launch (list "/bin/sh" "-c" *tree-script*) keys))
         (line (read-line (proc:stdout h))))
    (values h (parse-integer line))))

(defun settle () (sleep 0.3))

(defun demo-tree-kill ()
  (banner "Demo 6: process-protocol 0.4.0 / process-backend-uiop 0.3.1 (process groups, tree kill)")
  (stage "supports-p: capability queries on *process-backend*")
  (dolist (cap '(:process-group :pid :job-objects))
    (show (format nil "supports-p ~s" cap) (proc:supports-p cap)))
  (when (skipped-on-windows "the process-group and tree-kill steps"
                            "they use POSIX process groups and signals (sh, kill, ps -o pgid=); Windows would need job objects")
    (return-from demo-tree-kill nil))

  (stage "control: launch without :process-group, plain kill → the grandchild survives")
  (multiple-value-bind (h grandchild) (launch-tree)
    (show "pid of the shell (process-pid)" (integerp (proc:pid h)))
    (show "grandchild alive before kill" (pid-alive-p grandchild))
    (proc:kill h)
    (show "shell exit code after kill" (proc:wait h :timeout 5))
    (settle)
    (show "grandchild alive after kill (expected T: kill reaches the child only)"
          (pid-alive-p grandchild))
    (proc:run (list "/bin/sh" "-c" (format nil "kill ~d" grandchild)))
    (settle)
    (show "grandchild alive after cleanup" (pid-alive-p grandchild)))

  (stage "launch :process-group t, kill :tree t → the grandchild is stopped too")
  (multiple-value-bind (h grandchild) (launch-tree :process-group t)
    (let ((pid (proc:pid h)))
      (show "process-pid is a positive integer" (and (integerp pid) (plusp pid)))
      (show "process group of the shell = its pid (leads its own group)"
            (= pid (parse-integer (string-trim '(#\Space #\Newline)
                                               (nth-value 1 (proc:run (list "ps" "-o" "pgid=" "-p" (princ-to-string pid))
                                                                      :output :string))))))
      (show "grandchild in the same group"
            (= pid (parse-integer (string-trim '(#\Space #\Newline)
                                               (nth-value 1 (proc:run (list "ps" "-o" "pgid=" "-p" (princ-to-string grandchild))
                                                                      :output :string)))))))
    (show "grandchild alive before kill" (pid-alive-p grandchild))
    (show "kill :tree t" (proc:kill h :tree t))
    (show "shell exit code" (proc:wait h :timeout 5))
    (settle)
    (show "grandchild alive after kill :tree t (expected NIL)" (pid-alive-p grandchild))
    (show "process-pid unchanged after exit" (integerp (proc:pid h))))

  (stage "run :process-group t :timeout 1 → unwinding kills the whole tree")
  (let ((marker (format nil "/tmp/hd-tree-~36r" (random (expt 36 6) (make-random-state t))))
        (start (get-internal-real-time)))
    (handler-case
        (proc:run (list "/bin/sh" "-c" (format nil "(sleep 2; touch ~a) & wait" marker))
                  :process-group t :timeout 1)
      (proc:process-timeout-error (c)
        (show "signalled" (type-of c))))
    (show "elapsed seconds" (elapsed-since start))
    (sleep 2.5)
    (show "marker written by the grandchild 2 s later (expected NIL)" (and (probe-file marker) t))
    (ignore-errors (delete-file marker)))

  (stage "kill :tree t on a handle launched without :process-group → process-group-error")
  (multiple-value-bind (h grandchild) (launch-tree)
    (handler-bind ((proc:process-group-error
                     (lambda (c)
                       (show "signalled" (type-of c))
                       (show "  report" (princ-to-string c))
                       (show "  restarts" (restart-names c))
                       (note "→ CONTINUE (kill the direct child only)")
                       (continue c))))
      (show "kill :tree t" (proc:kill h :tree t)))
    (show "shell exit code" (proc:wait h :timeout 5))
    (settle)
    (show "grandchild alive (only the child was killed)" (pid-alive-p grandchild))
    (proc:run (list "/bin/sh" "-c" (format nil "kill ~d" grandchild)))))
