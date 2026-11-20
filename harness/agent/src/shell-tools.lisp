(in-package #:harness-demos-agent)

;;; Demo 7: agent-tools 0.2.0 shell tools (H2c): bash in the foreground (output
;;; cleaning, the cap, a timeout and its restarts), background jobs, the command
;;; scanner's permission resources, and a mock-model agent run with a deny rule.

(defvar *on-timeout* nil
  "When set, a SHELL-TIMEOUT handler bound inside %CALL-SHELL (inside its HANDLER-CASE).")

(defun %call-shell (tools name &rest kv)
  "Call shell tool NAME with KV as JSON; print the result or the failure."
  (let ((args (apply #'json-args kv)))
    (format t "~&> ~a ~a~%" name args)
    (handler-case (let ((r (if *on-timeout*
                               (handler-bind ((at:shell-timeout *on-timeout*))
                                 (at:call-shell-tool tools name args))
                               (at:call-shell-tool tools name args))))
                    (show "result" r)
                    r)
      (error (c)
        (format t "~&FAILED with ~s:~%  ~a~%" (type-of c) c)
        (finish-output)
        nil))))

(defun %elapsed (start)
  (/ (round (* 10 (- (get-internal-real-time) start)) internal-time-units-per-second) 10.0))

(defun %shell-foreground (tools)
  (stage "7b. foreground bash: ANSI colours, a progress bar (CR), stderr merged, exit code")
  (%call-shell tools "bash" "command"
               "printf '\\033[1;31mred\\033[0m plain\\n'; printf 'progress 10%%\\rprogress 55%%\\rprogress 100%%\\n'; echo 'to stderr' >&2; exit 3")
  (%call-shell tools "bash" "command" "pwd; ls" "workdir" "src")
  (%call-shell tools "bash" "command" "echo TERM=$TERM PAGER=$PAGER NO_COLOR=$NO_COLOR; cat"
               "description" "stdin is at EOF, so cat returns at once")
  (%call-shell tools "bash" "command" "echo x" "workdir" "../..")
  (%call-shell tools "bash" "command" "   ")

  (stage "7c. the output cap: head + tail with the omitted count")
  (let ((small (at:make-shell-tools :root (at:shell-tools-root tools) :max-output 120)))
    (unwind-protect (%call-shell small "bash" "command" "seq 1 400")
      (at:close-shell-tools small)))
  (show "truncate-shell-output (pure), max 20"
        (at:truncate-shell-output (format nil "~{line ~d~%~}" '(1 2 3 4 5 6 7 8)) 20))
  (show "clean-shell-output of octets (invalid UTF-8, CR LF, OSC title)"
        (at:clean-shell-output
         (coerce (append (map 'list #'char-code (format nil "~c]0;title~c" #\Esc (code-char 7)))
                         '(104 105 255 13 10 111 107))
                 '(vector (unsigned-byte 8))))))

(defun %shell-timeout (tools)
  (stage "7d. timeout_ms: unhandled → the tree is killed and the call fails")
  (let ((start (get-internal-real-time)))
    (%call-shell tools "bash" "command" "echo started; sleep 30" "timeout_ms" 700)
    (show "elapsed seconds" (%elapsed start)))
  (stage "7d'. the shell-timeout restarts: CONTINUE once, then RUN-IN-BACKGROUND")
  (let ((start (get-internal-real-time))
        (seen 0))
    (let ((*on-timeout*
                     (lambda (c)
                       (incf seen)
                       (note "  [handler] ~s after ~as: timeout ~a ms, output so far ~s"
                             (type-of c) (%elapsed start) (at:shell-timeout-timeout c)
                             (at:shell-timeout-output c))
                       (note "  [handler] restarts: ~{~(~a~)~^ ~}"
                             (remove nil (mapcar #'restart-name (compute-restarts c))))
                       (if (= seen 1)
                           (progn (note "  [handler] → continue") (continue c))
                           (progn (note "  [handler] → run-in-background")
                                  (invoke-restart (find-restart 'at:run-in-background)))))))
      (%call-shell tools "bash" "command" "for i in 1 2 3 4 5 6; do echo step $i; sleep 0.4; done"
                   "timeout_ms" 700))
    (%call-shell tools "job_output" "job_id" "job-1" "wait_ms" 5000)))

(defun %shell-jobs (tools)
  (stage "7e. a background job: job_output, job_input, job_list, job_kill")
  (%call-shell tools "bash" "run_in_background" t "command"
               "for i in 1 2 3; do echo tick $i; sleep 0.2; done; read name; echo \"hello, $name\"; sleep 30")
  (%call-shell tools "job_output" "job_id" "job-2" "wait_ms" 1200)
  (%call-shell tools "job_input" "job_id" "job-2" "input" (format nil "demo~%") "wait_ms" 600)
  (%call-shell tools "job_list")
  (%call-shell tools "job_kill" "job_id" "job-2")
  (%call-shell tools "job_input" "job_id" "job-2" "input" "late")
  (%call-shell tools "job_output" "job_id" "job-9")
  (note "jobs: ~{~a~^, ~}"
        (mapcar (lambda (j) (format nil "~a ~(~a~)~@[ code ~a~]" (at:shell-job-id j)
                                    (at:shell-job-status j) (at:shell-job-exit-code j)))
                (at:shell-tools-jobs tools))))

(defun %shell-scanner (tools)
  (stage "7f. shell-command-resources: what permission rules see")
  (let ((root (fs:as-posix (at:shell-tools-root tools))))
    (dolist (cmd '("git status && git diff --stat"
                   "cd src && rm -rf build > ../log.txt"
                   "cd src; rm a.lisp"
                   "echo \"$(date)\" | tee out.txt"
                   "FOO=1 ./build.sh 2>&1 >/dev/null"
                   "cp a.lisp b.lisp /tmp/backup/"
                   "find . -name '*.o' -delete"
                   "sudo rm -rf /"
                   "(cd docs && make html); ls"
                   "# just a comment"))
      (format t "  ~38s → ~s~%" cmd (at:shell-command-resources cmd :root root))))
  (note "on the tool (workdir joins the resources as cd:):")
  (dolist (args (list (json-args "command" "rm notes.txt" "workdir" "src")
                      (json-args "command" "ls")
                      (json-args "command" "ls" "workdir" "../..")
                      "not json"))
    (format t "  bash ~a → ~s~%" args (agent:tool-permission-resources tools "bash" args)))
  (format t "  job_kill {\"job_id\":\"job-1\"} → ~s~%"
          (agent:tool-permission-resources tools "job_kill" (json-args "job_id" "job-1"))))

(defun %shell-agent-run (root)
  (stage "7g. a mock-model agent run: ls allowed, rm denied by (\"bash\" \"program:rm\" :deny), a job")
  (note "rules allow bash:ls* bash:echo * bash:sleep * and program:ls / echo / sleep (every resource must be decided:
   an allow on program:ls alone leaves bash:ls src undecided, which asks); deny program:rm")
  (with-agent-loop
    (let* ((files (at:make-file-tools :root (namestring root)))
           (shell (at:make-shell-tools :root (namestring root)))
           (asked '()))
      (unwind-protect
           (let* ((agent (agent:make-ai-agent
                          :name "coder"
                          :backend (scripted-backend
                                    (list (list (list "bash" (json-args "command" "ls src")))
                                          (list (list "bash" (json-args "command" "ls && rm src/util.lisp")))
                                          (list (list "bash" (json-args "command" "echo built; sleep 0.2; echo done"
                                                                        "run_in_background" t)))
                                          (list (list "job_output" (json-args "job_id" "job-1" "wait_ms" 5000)))
                                          (list (list "bash" (json-args "command" "git log -1")))
                                          "Listed src; deleting was refused; the job finished."))
                          :tools (list files shell)
                          :permissions (list (agent:make-permission-rule "bash" "bash:ls*" :allow)
                                             (agent:make-permission-rule "bash" "bash:echo *" :allow)
                                             (agent:make-permission-rule "bash" "bash:sleep *" :allow)
                                             (agent:make-permission-rule "bash" "program:ls" :allow)
                                             (agent:make-permission-rule "bash" "program:echo" :allow)
                                             (agent:make-permission-rule "bash" "program:sleep" :allow)
                                             (agent:make-permission-rule
                                              "bash" "program:rm" :deny
                                              :feedback "do not delete files; ask the user"))))
                  (run (handler-bind ((agent:agent-approval-required
                                        (lambda (c)
                                          (let ((inv (agent:agent-approval-required-invocation c)))
                                            (push (agent:agent-invocation-arguments inv) asked)
                                            (note "  [approval asked] ~a ~a → deny"
                                                  (agent:agent-invocation-name inv)
                                                  (agent:agent-invocation-arguments inv))
                                            (agent:invoke-deny c "no git in this demo")))))
                        (agent:run-ai-agent agent "tidy src"))))
             (show "finish reason" (agent:agent-run-finish-reason run))
             (show "tools offered" (mapcar #'llm:llm-tool-name (agent:collect-run-tools agent)))
             (dolist (p (tool-results run))
               (format t "~&tool result ~a (error-p ~a):~%" (llm:llm-tool-result-part-id p)
                       (llm:llm-tool-result-part-error-p p))
               (show "  content" (llm:llm-tool-result-part-content p)))
             (show "approval prompts" (length asked))
             (show "final text" (agent:agent-run-text run))
             (show "src/util.lisp still on disk" (and (probe-file (merge-pathnames "src/util.lisp" root)) t)))
        (at:close-shell-tools shell)))))

(defun demo-shell-tools ()
  (banner "Demo 7: agent-tools 0.2.0 shell tools")
  (let ((root (%make-project)))
    (unwind-protect
         (let ((tools (at:make-shell-tools :root (namestring root))))
           (unwind-protect
                (progn
                  (stage "7a. make-shell-tools")
                  (show "names" (at:shell-tools-names tools))
                  (show "executor" (at:shell-tools-executor tools))
                  (show "shell" (at:shell-tools-shell tools))
                  (show "dialect" (at:shell-tools-dialect tools))
                  (show "+shell-environment+" at:+shell-environment+)
                  (show "needs approval: bash / job_output"
                        (list (agent:tool-needs-approval-p tools "bash" (json-args "command" "ls"))
                              (agent:tool-needs-approval-p tools "job_output" (json-args "job_id" "job-1"))))
                  (%shell-foreground tools)
                  (%shell-timeout tools)
                  (%shell-jobs tools)
                  (%shell-scanner tools))
             (at:close-shell-tools tools)
             (show "closed-p" (at:shell-tools-closed-p tools))
             (%call-shell tools "bash" "command" "echo after close"))
           (%shell-agent-run root))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
