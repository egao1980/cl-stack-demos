(in-package #:harness-demos-supervisor)

;;; Demo 9: agent-tools 0.3.0's unit tools (H8c) as an agent sees them: each
;;; call with its exact text result. bash runs as supervised units; job_list /
;;; job_output keep their H2c text; unit_control asks for approval unless a
;;; rule decides; the output caps.

(defun find-on-path (name)
  "Native namestring of program NAME (NAME.exe on Windows) on PATH, or NIL."
  (let ((file (if (windows-p) (concatenate 'string name ".exe") name)))
    (dolist (dir (uiop:split-string (or (uiop:getenv "PATH") "")
                                    :separator (if (windows-p) ";" ":")))
      (unless (string= dir "")
        (let ((p (probe-file (merge-pathnames file (uiop:ensure-directory-pathname
                                                    (uiop:parse-native-namestring dir))))))
          (when p (return (uiop:native-namestring p))))))))

(defun shell-keys ()
  "On Windows a POSIX shell (Git for Windows' sh on PATH) instead of cmd.exe, so
the commands mean the same everywhere; NIL elsewhere."
  (when (windows-p)
    (let ((sh (find-on-path "sh")))
      (and sh (list :shell (list sh "-c"))))))

(defun make-supervised-shell (sup root &rest keys)
  (apply #'at:make-shell-tools :root (namestring root) :supervisor sup (append keys (shell-keys))))

(defun write-text (dir name text)
  (with-open-file (s (merge-pathnames name dir) :direction :output :if-exists :supersede
                                                :external-format :utf-8)
    (write-string text s))
  (merge-pathnames name dir))

(defun lines-text (&rest lines) (format nil "~{~a~%~}" lines))

(defparameter +build-sh+
  (lines-text "#!/bin/sh"
              "# a noisy build: 200 compile lines, one error in the middle"
              "i=1"
              "while [ $i -le 200 ]; do"
              "  echo \"cc -O2 -c -o build/mod$i.o src/mod$i.c\""
              "  if [ $i -eq 120 ]; then"
              "    echo \"src/mod120.c:14:5: error: 'count' undeclared (first use in this function)\""
              "    echo \"   14 |     count += step;\""
              "    echo \"      |     ^~~~~\""
              "  fi"
              "  i=$((i+1))"
              "done"
              "echo \"make: *** [build/mod120.o] Error 1\" >&2"
              "exit 2"))

(defparameter +echo-sh+
  (lines-text "#!/bin/sh"
              "echo \"echo server ready\""
              "while read line; do echo \"got: $line\"; done"
              "echo \"stdin closed, bye\""))

(defun first-ref (search-result)
  "The ref of hit [1] in a unit_search result string."
  (let* ((line (first (uiop:split-string search-result :separator (string #\Newline))))
         (fields (uiop:split-string line :separator " ")))
    (second fields)))

(defun after-line-of (text)
  "The L of \"… after_line L\" in a tool result."
  (let ((p (search "after_line " text :from-end t)))
    (and p (parse-integer text :start (+ p (length "after_line ")) :junk-allowed t))))

(defun %unit-tools-basics (units)
  (stage "9a. make-unit-tools: names, caps, approval, permission resources")
  (show "names" (at:unit-tools-names units))
  (show "caps (max-output max-results default-k max-k context max-read-lines max-wait)"
        (list (at:unit-tools-max-output units) (at:unit-tools-max-results units)
              (at:unit-tools-default-k units) (at:unit-tools-max-k units)
              (at:unit-tools-context units) (at:unit-tools-max-read-lines units)
              (at:unit-tools-max-wait units)))
  (show "needs approval (list search read tail control)"
        (mapcar (lambda (n) (agent:tool-needs-approval-p units n "{}")) at:+unit-tool-names+))
  (dolist (call (list (list "unit_list" (json-args))
                      (list "unit_search" (json-args "query" "error"))
                      (list "unit_search" (json-args "query" "error" "units" (vector "shell-job-*" "rpc-1")))
                      (list "unit_read" (json-args "ref" "shell-job-2:1:120"))
                      (list "unit_tail" (json-args "unit" "shell-job-3"))
                      (list "unit_control" (json-args "unit" "shell-job-3" "action" "stop"))
                      (list "unit_control" (json-args "unit" "subagent-1" "action" "restart"))
                      (list "unit_control" "not json")))
    (note "~12a ~52a → ~s" (first call) (second call)
          (agent:tool-permission-resources units (first call) (second call)))))

(defun %unit-tools-shell (shell)
  (stage "9b. bash under a supervisor: a foreground command is a unit; the H2c text is unchanged")
  (tool-call shell "bash" "command" "echo hello; sleep 0.2; echo oops >&2; exit 3")
  (stage "9c. a background build (noisy, fails): job_output and job_list keep their H2c text, backed by the unit log")
  (tool-call shell "bash" "command" "sh build.sh" "run_in_background" t)
  (tool-call shell "job_output" "job_id" "shell-job-2" "wait_ms" 20000)
  (tool-call shell "bash" "command" "sh echo.sh" "run_in_background" t)
  (tool-call shell "job_output" "job_id" "shell-job-3" "wait_ms" 300)
  (tool-call shell "job_list")
  (tool-call shell "job_output" "job_id" "job-1"))

(defun %unit-tools-reading (units)
  (stage "9d. unit_list (all, live only, by kind)")
  (tool-call units "unit_list")
  (tool-call units "unit_list" "state" "live")
  (tool-call units "unit_list" "kind" "subagent")
  (stage "9e. unit_search: ranked hits with refs, a kinds filter, no results")
  (let* ((r (tool-call units "unit_search" "query" "undeclared count" "k" 2))
         (ref (and r (first-ref r))))
    (tool-call units "unit_search" "query" "" "kinds" (vector "error") "context" 0)
    (tool-call units "unit_search" "query" "segfault")
    (tool-call units "unit_search" "query" "" )
    (stage "9f. unit_read around the ref, then by range")
    (check "the top hit is the error line of the build (shell-job-2, line 122: line 1 is the segment header)"
           (equal ref "shell-job-2:1:122"))
    (tool-call units "unit_read" "ref" ref)
    (note "(a ref read starts 10 lines before the ref; with limit 8 the ref line itself is not shown:)")
    (tool-call units "unit_read" "ref" ref "limit" 8)
    (tool-call units "unit_read" "unit" "shell-job-2" "start_line" 199 "limit" 5)
    (tool-call units "unit_read" "unit" "shell-job-2" "start_line" 900)
    (stage "9g. unit_tail: the last lines; follow with after_line + wait_ms")
    (tool-call units "unit_tail" "unit" "shell-job-2" "lines" 3)
    (let* ((sent (tool-call units "unit_control" "unit" "shell-job-3" "action" "input" "input" (format nil "ping~%")))
           (after (and sent (after-line-of sent)))
           (t0 (get-internal-real-time))
           (tail (tool-call units "unit_tail" "unit" "shell-job-3" "after_line" after "wait_ms" 10000))
           (secs (float (/ (- (get-internal-real-time) t0) internal-time-units-per-second))))
      (note "(unit_tail returned after ~,1f s of its 10 s wait)" secs)
      (check "follow returned the reply as soon as it arrived"
             (and tail (search "got: ping" tail) (< secs 5)))
      (let ((t1 (get-internal-real-time)))
        (tool-call units "unit_tail" "unit" "shell-job-3" "after_line" (after-line-of tail) "wait_ms" 600)
        (note "(a wait with nothing new returned after ~,1f s)"
              (float (/ (- (get-internal-real-time) t1) internal-time-units-per-second)))))
    (tool-call units "unit_tail" "unit" "nope-1")))

(defun %control-run (label units shell steps rules)
  "A scripted agent run of STEPS with RULES; asks are printed and denied."
  (stage "~a" label)
  (dolist (r rules) (print-rule r))
  (let* ((asked '())
         (run (with-agent-loop
                (handler-bind ((agent:agent-approval-required
                                 (lambda (c)
                                   (let ((inv (agent:agent-approval-required-invocation c)))
                                     (push (agent:agent-invocation-arguments inv) asked)
                                     (note "  [approval asked] ~a ~a → deny"
                                           (agent:agent-invocation-name inv) (agent:agent-invocation-arguments inv))
                                     (agent:invoke-deny c "not now")))))
                  (agent:run-ai-agent
                   (agent:make-ai-agent :name "coder" :backend (scripted-backend steps)
                                        :tools (list units shell) :permissions rules)
                   "manage the units")))))
    (print-run run)
    (reverse asked)))

(defun %unit-tools-control (units shell)
  (stage "9h. unit_control: input with eof (direct call), then through agent runs")
  (tool-call units "unit_control" "unit" "shell-job-3" "action" "input" "input" (format nil "last~%") "eof" t)
  (sp:wait-unit (sp:find-unit (at:unit-tools-supervisor units) "shell-job-3") :timeout 10)
  (tool-call units "unit_tail" "unit" "shell-job-3" "after_line" 3 "wait_ms" 5000)
  (tool-call shell "bash" "command" "sleep 60" "run_in_background" t)
  (let ((asked (%control-run "9h'. no rule: unit_control asks (approval by default); the demo denies"
                             units shell
                             (list (list (list "unit_control" (json-args "unit" "shell-job-4" "action" "stop")))
                                   "asked")
                             '())))
    (check "one approval asked, the job still runs"
           (and (= 1 (length asked)) (eq :running (sp:unit-state (sp:find-unit (at:unit-tools-supervisor units) "shell-job-4"))))))
  (let ((asked (%control-run "9h''. with (\"unit_control\" \"stop:shell-job-*\" :allow): the stop runs without asking; a restart still asks"
                             units shell
                             (list (list (list "unit_control" (json-args "unit" "shell-job-4" "action" "stop")))
                                   (list (list "unit_control" (json-args "unit" "shell-job-2" "action" "restart")))
                                   (list (list "unit_control" (json-args "unit" "shell-job-2" "action" "input" "input" "x")))
                                   "done")
                             (list (agent:make-permission-rule "unit_control" "stop:shell-job-*" :allow)))))
    (check "the stop was not asked; the restart and the input were"
           (= 2 (length asked))))
  (tool-call shell "job_list"))

(defun %unit-tools-caps (sup)
  (stage "9i. output caps: max-output 700, max-results 2, k max 50, a 600-character line")
  (let ((long (sp:start-unit sup :shell-job :labels '(:tool "minified")
                             :function (lambda (u) (declare (ignore u))
                                         (format t "bundle.js:1:9 error: ~a~%" (make-string 600 :initial-element #\x))
                                         (finish-output) :done)))
        (small (at:make-unit-tools :supervisor sup :max-output 700 :max-results 2)))
    (settle long)
    (unwind-protect
         (progn
           (tool-call small "unit_list")
           (tool-call small "unit_search" "query" "error" "k" 5)
           (tool-call small "unit_search" "query" "error" "k" 51)
           (tool-call small "unit_read" "unit" (sp:unit-id long)))
      (at:close-unit-tools small))))

(defun demo-unit-tools ()
  (banner "Demo 9: the unit tools as an agent sees them (agent-tools 0.3.0, H8c)")
  (with-checks ()
    (with-temp-dir (dir "sup-tools")
      (let ((root (uiop:ensure-directory-pathname (merge-pathnames "project/" dir)))
            (state (uiop:ensure-directory-pathname (merge-pathnames "state/" dir))))
        (ensure-directories-exist root)
        (write-text root "build.sh" +build-sh+)
        (write-text root "echo.sh" +echo-sh+)
        (with-scrubbed-path (dir "<tmp>")
          (with-supervisor (sup :state-directory state)
            (let ((units (at:make-unit-tools :supervisor sup))
                  (shell (make-supervised-shell sup root :max-output 1200)))
              (sp:start-unit sup :subagent :labels '(:role "reviewer")
                                           :function (lambda (u) (declare (ignore u))
                                                       (write-line "reviewer: looking at src/mod120.c") :done))
              (unwind-protect
                   (progn
                     (%unit-tools-basics units)
                     (%unit-tools-shell shell)
                     (%unit-tools-reading units)
                     (%unit-tools-control units shell)
                     (%unit-tools-caps sup))
                (at:close-shell-tools shell)
                (at:close-unit-tools units)))))))))
