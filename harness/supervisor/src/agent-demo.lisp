(in-package #:harness-demos-supervisor)

;;; Demos 10 and 11: an agent finds a test failure in a noisy background run
;;; through the unit tools (unit_search, unit_read), fixes the code, reruns the
;;; tests and confirms with a kinds search. Demo 10 on a scripted mock
;;; (deterministic transcript), Demo 11 on a live model when one answers.

(defparameter +calc-sh+
  (lines-text "# calc.sh: small arithmetic helpers for the shell"
              "add() { echo $(($1 - $2)); }"
              "mul() { echo $(($1 * $2)); }"
              "sub() { echo $(($1 - $2)); }"))

(defparameter +test-sh+
  (lines-text "#!/bin/sh"
              "# the calc test suite: noisy setup, a benchmark and teardown around three checks"
              ". ./calc.sh"
              "failed=0"
              "check() {"
              "  if [ \"$2\" = \"$3\" ]; then echo \"ok - $1\"; else echo \"FAIL $1: got $2, expected $3 (calc.sh)\"; failed=$((failed+1)); fi"
              "}"
              "i=1; while [ $i -le 80 ]; do echo \"[setup] fixture $i of 80 loaded\"; i=$((i+1)); done"
              "echo \"warning: TEST_SEED not set, using 42\" >&2"
              "check \"mul 3 4\" \"$(mul 3 4)\" 12"
              "i=1; while [ $i -le 40 ]; do echo \"[bench] iteration $i took 0.$((i % 7))ms\"; i=$((i+1)); done"
              "check \"add 2 3\" \"$(add 2 3)\" 5"
              "check \"sub 7 2\" \"$(sub 7 2)\" 5"
              "i=1; while [ $i -le 60 ]; do echo \"[teardown] released handle $i\"; i=$((i+1)); done"
              "if [ $failed -gt 0 ]; then echo \"$failed check(s) failed\"; exit 1; fi"
              "echo \"all checks passed\""))

(defparameter +fix-prompt+
  "The calc project's test suite fails. Run it in the background with bash (command \"sh test.sh\", run_in_background true) and wait for it with job_output (wait_ms 30000). Its output is long and noisy, so do not read it all: find the failure with unit_search (kinds [\"test-failure\"], units [the job id]), look at the lines around the hit with unit_read (its ref), read calc.sh and fix the faulty function with edit (do not change test.sh). Then run \"sh test.sh\" again with bash and confirm with unit_search (query \"\", kinds [\"test-failure\"], units [that run's unit id from the truncation note or unit_list]) that nothing failed. End with one sentence naming the fix.")

(defun make-calc-project (dir)
  (let ((root (uiop:ensure-directory-pathname (merge-pathnames "calc/" dir))))
    (ensure-directories-exist root)
    (write-text root "calc.sh" +calc-sh+)
    (write-text root "test.sh" +test-sh+)
    root))

(defun run-tests-directly (root)
  "Run sh test.sh in ROOT outside the agent (process-protocol) → exit code."
  (let ((sh (or (and (windows-p) (find-on-path "sh")) "sh")))
    (values (pp:run (list sh "test.sh") :directory (uiop:native-namestring root)
                                             :output nil :error-output nil))))

(defun read-text (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((str (make-string (file-length s))))
      (subseq str 0 (read-sequence str s)))))

(defun last-tool-result-text (turns)
  "The content of the newest tool result in TURNS (what the model just saw)."
  (loop for turn in (reverse turns)
        for part = (find-if #'llm:llm-tool-result-part-p (reverse (llm:llm-turn-parts turn)))
        when part return (princ-to-string (llm:llm-tool-result-part-content part))))

(defun reactive-backend (steps)
  "Like SCRIPTED-BACKEND, but a step may be a function of the last tool result
text → a step: the mock reads the ref it acts on from what the tool returned."
  (let ((n 0))
    (llm:make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (let* ((raw (nth (min n (1- (length steps))) steps))
                       (step (if (functionp raw) (funcall raw (last-tool-result-text turns)) raw)))
                  (incf n)
                  (if (stringp step)
                      (llm:make-llm-response :parts (list (llm:make-llm-text-part :text step))
                                             :finish-reason :stop)
                      (llm:make-llm-response
                       :parts (loop for (name args) in step
                                    for k from 1
                                    collect (llm:make-llm-tool-call-part
                                             :id (format nil "g~d-c~d" n k) :name name :arguments args))
                       :finish-reason :tool-use)))))))

(defun unit-of-truncation (text)
  "The unit named by bash's truncation note (\"… in unit shell-job-N: …\")."
  (let ((p (search "in unit " text)))
    (and p (let* ((s (+ p 8)) (e (position #\: text :start s))) (subseq text s e)))))

(defun %mock-fix-steps ()
  (list (list (list "bash" (json-args "command" "sh test.sh" "run_in_background" t)))
        (list (list "job_output" (json-args "job_id" "shell-job-1" "wait_ms" 30000)))
        (list (list "unit_search" (json-args "query" "" "kinds" (vector "test-failure") "units" (vector "shell-job-1"))))
        (lambda (search-result)
          (list (list "unit_read" (json-args "ref" (first-ref search-result) "limit" 21))))
        (list (list "read" (json-args "file_path" "calc.sh")))
        (list (list "edit" (json-args "file_path" "calc.sh"
                                      "old_string" "add() { echo $(($1 - $2)); }"
                                      "new_string" "add() { echo $(($1 + $2)); }")))
        (list (list "bash" (json-args "command" "sh test.sh")))
        (lambda (bash-result)
          (list (list "unit_search" (json-args "query" "" "kinds" (vector "test-failure" "error")
                                               "units" (vector (or (unit-of-truncation bash-result) "shell-job-2"))))))
        "Fixed add() in calc.sh: it subtracted ($1 - $2) instead of adding; the rerun passes and has no test failures."))

(defun fix-rules ()
  (list (agent:make-permission-rule "bash" "*" :allow)
        (agent:make-permission-rule "bash" "program:rm" :deny :feedback "do not delete files")
        (agent:make-permission-rule "edit" "path:calc.sh" :allow)
        (agent:make-permission-rule "multi_edit" "path:calc.sh" :allow)
        (agent:make-permission-rule "write" "path:calc.sh" :allow)
        (agent:make-permission-rule "*" "path:test.sh" :deny
                                    :feedback "fix the code under test (calc.sh), not the test")))

(defun fix-run (label root state backend model &key (max-steps 20) max-lines)
  "One agent run over the calc project with unit, shell and file tools →
(values fixed-p run seconds)."
  (with-supervisor (sup :state-directory state)
    (let* ((units (at:make-unit-tools :supervisor sup))
           (shell (make-supervised-shell sup root :max-output 1500))
           (files (at:make-file-tools :root (namestring root) :tools '("read" "edit" "grep" "ls")))
           (asked '())
           (t0 (get-internal-real-time))
           run)
      (unwind-protect
           (progn
             (stage "~a: the run" label)
             (show "tools" (append (at:unit-tools-names units) (at:shell-tools-names shell) '("read" "edit" "grep" "ls")))
             (dolist (r (fix-rules)) (print-rule r))
             (print-block +fix-prompt+ :prefix "   prompt | ")
             (setf run
                   (let ((hp:*http-backend* (http-backend-async:make-async-backend))
                         (agent:*off-loop-specials* (union '(hp:*http-backend* http-backend-async:*event-backend-maker*)
                                                           agent:*off-loop-specials*)))
                     (with-agent-loop
                       (handler-bind ((agent:agent-approval-required
                                        (lambda (c)
                                          (let ((inv (agent:agent-approval-required-invocation c)))
                                            (push (list (agent:agent-invocation-name inv)
                                                        (agent:agent-invocation-arguments inv))
                                                  asked)
                                            (agent:invoke-deny c "not allowed in this unattended run")))))
                         (agent:run-ai-agent
                          (agent:make-ai-agent
                           :name "fixer" :backend backend :model model
                           :instructions "You are a coding agent in a small shell project. Use the tools; keep answers short."
                           :tools (list units shell files) :permissions (fix-rules)
                           :settings (agent:make-agent-settings :max-steps max-steps))
                          +fix-prompt+)))))
             (print-run run :max-lines max-lines)
             (when asked (show "asks (denied: unattended)" (reverse asked))))
        (at:close-shell-tools shell)
        (at:close-unit-tools units))
      (let* ((secs (float (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
             (code (run-tests-directly root))
             (calc (read-text (merge-pathnames "calc.sh" root))))
        (stage "~a: checked by the demo after the run" label)
        (print-block calc :prefix "   calc.sh | ")
        (show "sh test.sh exit code (run by the demo)" code)
        (show "test.sh unchanged" (string= (read-text (merge-pathnames "test.sh" root)) +test-sh+))
        (note "steps (tool calls) ~d; tools used: ~a; wall time ~,1f s"
              (length (agent:agent-run-invocations run)) (tools-used run) secs)
        (values (and (eql 0 code) (string= (read-text (merge-pathnames "test.sh" root)) +test-sh+))
                run secs)))))

(defun demo-agent-fix-mock ()
  (banner "Demo 10: an agent finds and fixes a failure from process logs (scripted mock)")
  (with-checks ()
    (with-temp-dir (dir "sup-fix-mock")
      (with-scrubbed-path (dir "<tmp>")
        (let ((root (make-calc-project dir)))
          (show "sh test.sh before the run (exit code)" (run-tests-directly root))
          (multiple-value-bind (fixed run) (fix-run "scripted mock" root (merge-pathnames "state/" dir)
                                                    (reactive-backend (%mock-fix-steps)) nil)
            (check "the tests pass after the run" fixed)
            (check "the tools used were unit_search and unit_read, not a dump"
                   (let ((names (mapcar #'agent:agent-invocation-name (agent:agent-run-invocations run))))
                     (equal names '("bash" "job_output" "unit_search" "unit_read" "read" "edit"
                                    "bash" "unit_search"))))
            (let ((results (mapcar (lambda (p) (princ-to-string (llm:llm-tool-result-part-content p)))
                                   (tool-results run))))
              (check "the first search found the FAIL line"
                     (search "FAIL add 2 3: got -1, expected 5" (nth 2 results)))
              (check "the confirming search finds no failure"
                     (search "No results" (nth 7 results))))))))))

(defun demo-agent-fix-live ()
  (banner "Demo 11: the same task on a live model")
  (unless (detect-model)
    (note "skipped: no live model (set HARNESS_DEMOS_BASE_URL / HARNESS_DEMOS_API_KEY / HARNESS_DEMOS_MODEL, or run LM Studio on ~a); CI runs Demo 10 only"
          (%url-host *lm-url*))
    (return-from demo-agent-fix-live t))
  (setf *lm-usage* '())
  (with-temp-dir (dir "sup-fix-live")
    (with-scrubbed-path (dir "<tmp>")
      (let ((root (make-calc-project dir)))
        (multiple-value-bind (fixed run secs)
            ;; A model server that answers but fails (a model that does not load, a
            ;; timeout) is printed as the live result; it does not fail the set.
            (handler-case (fix-run (format nil "live ~a (varies run to run)" *lm-model*) root
                                   (merge-pathnames "state/" dir)
                                   (lm-backend) *lm-model* :max-steps 24 :max-lines 40)
              (llm:llm-error (c)
                (note "LIVE RESULT: the model server failed: ~a (~s)" (clip (princ-to-string c) 300) (type-of c))
                (print-lm-usage)
                (return-from demo-agent-fix-live t)))
          (declare (ignore run))
          (multiple-value-bind (calls max-in out model-secs) (print-lm-usage)
            (declare (ignore model-secs))
            (note "LIVE RESULT: ~a; ~d model calls, max input ~d tokens, ~d output tokens, ~,1f s wall"
                  (if fixed "fixed (sh test.sh exits 0, test.sh untouched)" "did not fix it") calls max-in out secs))
          t)))))
