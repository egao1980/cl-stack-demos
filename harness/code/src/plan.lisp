(in-package #:harness-demos-code)

;;; Demos g–i: cl-stack-code 0.4.1 plan mode (H5a). The agent's edits and
;;; shell commands run in a plan area (a git worktree of the working copy's
;;; state, or a folder copy); the product prints the projected diff; the plan
;;; is applied, rejected, or rebased when the working copy drifted. The
;;; headless and interactive steps run on the scripted mock so every value is
;;; exact; one extra step runs on LM Studio when a model is loaded.

;;; A small project with a failing test: add() subtracts.

(defparameter *calc-sh*
  (format nil "# calc.sh: arithmetic helpers~%add() {~%  echo $(( $1 - $2 ))~%}~%~%mul() {~%  echo $(( $1 * $2 ))~%}~%"))

(defparameter *test-sh*
  (format nil "#!/bin/sh~%# Run: sh test.sh~%. ./calc.sh~%fail=0~%check () {~%  if [ \"$2\" = \"$3\" ]; then echo \"ok   $1\"; else echo \"FAIL $1: expected $3, got $2\"; fail=1; fi~%}~%check \"add 2 3\" \"$(add 2 3)\" 5~%check \"mul 4 5\" \"$(mul 4 5)\" 20~%[ $fail = 0 ] && echo \"all tests passed\"~%exit $fail~%"))

(defun calc-files ()
  `(("calc.sh" ,*calc-sh*)
    ("test.sh" ,*test-sh*)
    ("README.md" ,(format nil "# calc~%~%`sh test.sh` runs the tests.~%"))))

(defparameter *plan-prompt*
  "The tests fail. Run sh test.sh, fix calc.sh, and run the tests again.")

(defun fix-script ()
  "The mock agent: run the tests, fix add(), run them again, report."
  (list (list (list "bash" (json-args "command" "sh test.sh")))
        (list (list "edit" (json-args "file_path" "calc.sh"
                                      "old_string" "$(( $1 - $2 ))"
                                      "new_string" "$(( $1 + $2 ))")))
        (list (list "bash" (json-args "command" "sh test.sh")))
        (lambda (turns)
          (format nil "Fixed add() in calc.sh (it subtracted). sh test.sh now prints:~%~a"
                  (string-right-trim '(#\Newline) (last-tool-result-text turns))))))

;;; Processes and git.

(defun %run (dir argv)
  "Run ARGV in DIR. → (values stdout stderr exit)."
  (uiop:run-program argv :directory dir :output :string :error-output :string
                         :ignore-error-status t))

(defun %git (dir &rest args)
  "git -C DIR ARGS → stdout; a non-zero exit signals an error with git's stderr."
  (multiple-value-bind (out err code) (%run dir (list* "git" "-C" (%native dir) args))
    (unless (zerop code)
      (error "git ~{~a~^ ~} failed (~d): ~a" args code err))
    out))

(defun %git-ok (dir &rest args)
  "True when git ARGS exits 0."
  (zerop (nth-value 2 (%run dir (list* "git" "-C" (%native dir) args)))))

(defun %trim (s) (string-trim '(#\Newline #\Return #\Space) s))

(defun make-git-project (name files)
  "A real git repository NAME/ on branch main, local identity, FILES committed."
  (let ((root (scratch-dir name)))
    (%git root "init" "-q" "-b" "main")
    (%git root "config" "user.name" "Plan Demo")
    (%git root "config" "user.email" "plan-demo@invalid")
    (%git root "config" "commit.gpgsign" "false")
    (%git root "config" "core.autocrlf" "false")
    (loop for (relative text) in files do (write-file root relative text))
    (%git root "add" "-A")
    (%git root "commit" "-q" "-m" "init")
    root))

(defun %head (project) (%trim (%git project "rev-parse" "HEAD")))

(defun %porcelain (project)
  (let ((s (%trim (%git project "status" "--porcelain"))))
    (if (string= s "") "(clean)" s)))

(defun %worktrees (project)
  "git worktree list, one line per worktree (paths redacted on print)."
  (%trim (%git project "worktree" "list")))

(defun %tests (dir)
  "sh test.sh in DIR → \"<output>[exit N]\"."
  (multiple-value-bind (out err code) (%run dir '("sh" "test.sh"))
    (format nil "~a~a[exit ~d]" out err code)))

(defun %plans-dir (project) (code:plans-directory project :data-dir (data-dir)))

(defun %find-plan (project id)
  (find id (code:list-plans (%plans-dir project)) :key #'code:plan-id :test #'equal))

(defun %event (events type &optional subtype)
  (find-if (lambda (e) (and (equal type (jget e "type"))
                            (or (null subtype) (equal subtype (jget e "subtype")))))
           events))

(defun %plan-run (project config argv)
  "Run a stream-json plan run on the mock fix script; print it. → (values
exit events stderr)."
  (setf *mock-script* (fix-script))
  (let ((argv (append (list "-p" "--output-format" "stream-json" "--permission-mode" "plan"
                            "--max-turns" "8")
                      argv (list *plan-prompt*))))
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
      (values exit (decode-lines out) err))))

(defun %one-line (x)
  (format nil "~{~a~^ ⏎ ~}" (lines (princ-to-string x))))

(defun %show-tool-results (events)
  (dolist (r (%tool-results events))
    (note "  tool_result ~a ~a: ~a" (jget r "name") (jget r "status")
          (clip (%one-line (jget r "content")) 200))))

;;; g) headless plan, apply, drift, reject, plain folder.

(defun %g1-plan (project config)
  (stage "g1: -p --permission-mode plan on a git repository: edit + bash in the area, no prompt")
  (let ((head (%head project)))
    (multiple-value-bind (exit events err) (%plan-run project config '())
      (let* ((opened (%event events "plan" "opened"))
             (projected (%event events "plan" "projected"))
             (result (%event events "result"))
             (id (jget opened "plan_id"))
             (plan (%find-plan project id)))
        (show "exit code" exit)
        (show "stderr" err)
        (show "event types in order" (mapcar (lambda (e) (jget e "type")) events))
        (stage "g1: the events (text_delta folded)")
        (%events-summary events)
        (show "result subtype" (jget result "subtype"))
        (show "result permission_denials" (json:encode (jget result "permission_denials")))
        (show "result plan (id kind status changes)"
              (list (jget result "plan" "id") (jget result "plan" "kind") (jget result "plan" "status")
                    (json:encode (jget result "plan" "changes"))))
        (stage "g1: the real tree is unchanged")
        (show "git status --porcelain" (%porcelain project))
        (show "HEAD unchanged" (equal head (%head project)))
        (show "calc.sh in the project is the original" (equal *calc-sh* (read-file project "calc.sh")))
        (show "sh test.sh in the project" (%tests project))
        (stage "g1: the area, a linked worktree of the base commit")
        (show "plan" (list (code:plan-kind plan) (code:plan-status plan)))
        (show "plan base (commit) = opened.base" (equal (code:plan-base plan) (jget opened "base")))
        (show "base's parent is HEAD"
              (equal head (%trim (%git project "rev-parse" (format nil "~a^" (code:plan-base plan))))))
        (show "plan ref" (%trim (%git project "for-each-ref" "--format=%(refname)" code:+plan-ref-prefix+)))
        (show "git worktree list" (%worktrees project))
        (show "git status --porcelain in the area" (%porcelain (code:plan-root plan)))
        (stage "g1: the projected diff (the \"projected\" event)")
        (show "changes" (json:encode (jget projected "changes")))
        (show "diff" (jget projected "diff"))
        (show "plan-diff-text = the event's diff" (equal (code:plan-diff-text plan) (jget projected "diff")))
        (values (jget result "session_id") (jget projected "diff") id)))))

(defun %g2-apply (project config session id)
  (stage "g2: -p --resume SESSION --apply: no model call, the plan is applied")
  (let ((head (%head project))
        (argv (list "-p" "--resume" session "--apply")))
    (setf *mock-script* '())
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
      (show "exit code" exit)
      (show "stdout (the applied diff)" out)
      (show "stderr" err)
      (show "calc.sh in the project" (read-file project "calc.sh"))
      (show "sh test.sh in the project" (%tests project))
      (show "git status --porcelain (uncommitted edit; HEAD, branch, index untouched)" (%porcelain project))
      (show "HEAD unchanged" (equal head (%head project)))
      (let ((plan (%find-plan project id)))
        (show "plan status" (code:plan-status plan))
        (show "area removed" (not (uiop:directory-exists-p (code:plan-root plan))))
        (show "git worktree list" (%worktrees project))
        (show "the plan ref's commit: subject, and parent = the base"
              (list (%trim (%git project "log" "-1" "--format=%s" (concatenate 'string code:+plan-ref-prefix+ id)))
                    (equal (code:plan-base plan)
                           (%trim (%git project "rev-parse"
                                        (format nil "~a~a^" code:+plan-ref-prefix+ id)))))))
      (eql exit 0))))

(defun %g3-drift ()
  (stage "g3: drift: the working copy changes calc.sh while the plan is open")
  (let* ((project (make-git-project "plan-drift" (calc-files)))
         (config (scratch-dir "plan-drift-config"))
         (user (concatenate 'string *calc-sh*
                            (format nil "~%sub() {~%  echo $(( $1 - $2 ))~%}~%"))))
    (multiple-value-bind (exit events) (%plan-run project config '())
      (let* ((session (jget (%event events "result") "session_id"))
             (id (jget (%event events "plan" "opened") "plan_id")))
        (show "plan run exit" exit)
        (write-file project "calc.sh" user)
        (show "the user appended sub() to calc.sh in the project; git diff"
              (%git project "diff"))
        (stage "g3: --resume SESSION --apply (default --on-drift abort)")
        (let ((argv (list "-p" "--output-format" "stream-json" "--resume" session "--apply")))
          (setf *mock-script* '())
          (show-argv argv)
          (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
            (show "exit code" exit)
            (show "stderr" err)
            (dolist (e (decode-lines out)) (note "  ~a" (clip (json:encode e) 400)))
            (show "calc.sh in the project is still the user's" (equal user (read-file project "calc.sh")))))
        (stage "g3: approve-plan in Lisp: plan-drift, its restarts, rebase-plan")
        (let ((plan (%find-plan project id)))
          (multiple-value-bind (conflicts status)
              (handler-bind ((code:plan-drift
                               (lambda (c)
                                 (show "condition" (type-of c))
                                 (show "message" (princ-to-string c))
                                 (show "plan-drift-paths" (code:plan-drift-paths c))
                                 (show "restarts" (mapcar #'restart-name
                                                          (remove-if-not (lambda (r)
                                                                           (member (restart-name r)
                                                                                   '(code:rebase-plan code:overwrite-plan code:abort-approval)))
                                                                         (compute-restarts c))))
                                 (invoke-restart 'code:rebase-plan))))
                (code:approve-plan plan))
            (show "approve-plan → (values conflicts status)" (list conflicts status)))
          (let ((plan (%find-plan project id)))
            (show "plan status" (code:plan-status plan))
            (show "calc.sh in the area: the merge (the plan's fix + the user's sub())"
                  (area-text plan "calc.sh"))
            (show "the projected diff against the new base (only the plan's change)"
                  (code:plan-diff-text plan))
            (show "calc.sh in the project is still the user's (a rebase writes nothing)"
                  (equal user (read-file project "calc.sh")))))
        (stage "g3: --resume SESSION --apply again")
        (let ((argv (list "-p" "--resume" session "--apply")))
          (show-argv argv)
          (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
            (show "exit code" exit)
            (show "stdout" out)
            (show "stderr" err)
            (show "calc.sh in the project" (read-file project "calc.sh"))
            (show "sh test.sh in the project" (%tests project))
            (eql exit 0)))))))

(defun area-text (plan relative)
  (read-file (code:plan-root plan) relative))

(defun %g4-reject ()
  (stage "g4: --resume SESSION --reject: the area is discarded")
  (let* ((project (make-git-project "plan-reject" (calc-files)))
         (config (scratch-dir "plan-reject-config")))
    (multiple-value-bind (exit events) (%plan-run project config '())
      (let* ((session (jget (%event events "result") "session_id"))
             (id (jget (%event events "plan" "opened") "plan_id"))
             (plan (%find-plan project id))
             (ref (concatenate 'string code:+plan-ref-prefix+ id)))
        (show "plan run exit" exit)
        (show "before: area exists, plan ref exists, worktrees"
              (list (and (uiop:directory-exists-p (code:plan-root plan)) t)
                    (%git-ok project "rev-parse" "--verify" "-q" ref)
                    (length (lines (%worktrees project)))))
        (let ((argv (list "-p" "--resume" session "--reject")))
          (setf *mock-script* '())
          (show-argv argv)
          (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
            (show "exit code" exit)
            (show "stdout" (if (string= out "") "(empty)" out))
            (show "stderr" err)
            (let ((plan (%find-plan project id)))
              (show "plan status" (code:plan-status plan))
              (show "after: area exists, plan ref exists, worktrees"
                    (list (and (uiop:directory-exists-p (code:plan-root plan)) t)
                          (%git-ok project "rev-parse" "--verify" "-q" ref)
                          (length (lines (%worktrees project))))))
            (show "git status --porcelain" (%porcelain project))
            (show "calc.sh in the project is the original" (equal *calc-sh* (read-file project "calc.sh")))
            (stage "g4: --apply after the reject")
            (let ((argv (list "-p" "--resume" session "--apply")))
              (show-argv argv)
              (multiple-value-bind (exit2 out2 err2) (cli argv :cwd project :config config)
                (show "exit code" exit2)
                (show "stdout" (if (string= out2 "") "(empty)" out2))
                (show "stderr" err2)))
            (eql exit 0)))))))

(defun %g5-folder (git-diff)
  (stage "g5: the same flow on a plain folder (--plan-area copy), text output")
  (let* ((project (scratch-dir "plan-folder"))
         (config (scratch-dir "plan-folder-config")))
    (loop for (relative text) in (calc-files) do (write-file project relative text))
    (setf *mock-script* (fix-script))
    (let ((argv (list "-p" "--permission-mode" "plan" "--plan-area" "copy" "--max-turns" "8" *plan-prompt*)))
      (show-argv argv)
      (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
        (show "exit code" exit)
        (show "stdout (the answer, an empty line, the projected diff)" out)
        (show "stderr" err)
        (let ((plan (first (code:list-plans (%plans-dir project)))))
          (show "plan kind, status" (list (code:plan-kind plan) (code:plan-status plan)))
          (show "the area has no .git" (not (probe-file (merge-pathnames ".git/" (code:plan-root plan)))))
          (show "the base is a snapshot id in the plans store" (code:plan-base plan))
          (show "calc.sh in the project is the original" (equal *calc-sh* (read-file project "calc.sh")))
          (show "diff text = the git kind's diff (g1), byte for byte" (equal git-diff (code:plan-diff-text plan)))
          (show "stdout ends with that diff" (uiop:string-suffix-p out git-diff))
          (stage "g5: -p --continue --apply")
          (let ((argv (list "-p" "--continue" "--apply")))
            (setf *mock-script* '())
            (show-argv argv)
            (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
              (show "exit code" exit)
              (show "stdout = the same diff" (equal out git-diff))
              (show "stderr" err)
              (show "sh test.sh in the folder" (%tests project))
              (show "plan status" (code:plan-status (first (code:list-plans (%plans-dir project)))))
              (eql exit 0))))))))

(defun demo-plan-headless ()
  (banner "Demo g: plan mode (H5a) headless: plan, project, apply, drift + rebase, reject, plain folder")
  (let ((*model* '(:mock)))
    (note "model: the scripted mock (bash sh test.sh → edit calc.sh → bash sh test.sh → answer)")
    (show "git --version" (%trim (%git (uiop:getcwd) "--version")))
    (let* ((project (make-git-project "plan-git" (calc-files)))
           (config (scratch-dir "plan-git-config")))
      (show "project files" (mapcar #'first (calc-files)))
      (show "sh test.sh in the project before" (%tests project))
      (multiple-value-bind (session diff id) (%g1-plan project config)
        (let ((ok (list (%g2-apply project config session id)
                        (%g3-drift)
                        (%g4-reject)
                        (%g5-folder diff))))
          (show "steps that ended as expected (g2 g3 g4 g5)" ok)
          (unless (every #'identity ok)
            (error "a plan step did not end as expected: ~s" ok))
          t)))))

;;; h) interactive: the plan dialog on the headless terminal.

(defun %tui-fix-steps (&rest more)
  (append (list (list :tool "bash" (json-args "command" "sh test.sh"))
                (list :tool "edit" (%edit-args "calc.sh" "$(( $1 - $2 ))" "$(( $1 + $2 ))"))
                (list :tool "bash" (json-args "command" "sh test.sh"))
                "Fixed `add` in calc.sh; `sh test.sh` passes in the plan area.")
          more))

(defun %plan-tui (name steps fn)
  (let* ((project (make-git-project name (calc-files)))
         (backend (make-tui-mock steps)))
    (with-tui (tui program :argv '("--permission-mode" "plan") :cols 100 :rows 30)
        (project (scratch-dir (format nil "~a-config" name)) backend)
      (%wait tui "Message, @file")
      (funcall fn tui program project))))

(defun %h-turn (tui)
  (send-line tui "the tests fail; fix calc.sh")
  (%wait tui "Keep planning"))

(defun %h1-approve ()
  (stage "h1: a plan turn: the plan dialog, then Approve (1)")
  (%plan-tui "tui-plan-approve" (%tui-fix-steps)
             (lambda (tui program project)
               (declare (ignore program))
               (when (%h-turn tui)
                 (snap tui "the turn ended: transcript entry, diff, plan dialog")
                 (show "calc.sh in the project (untouched)" (equal *calc-sh* (read-file project "calc.sh")))
                 (h:tui-type tui "1")
                 (prog1 (%wait tui "applied")
                   (snap tui "Approve")
                   (show "sh test.sh in the project" (%tests project))
                   (show "git status --porcelain" (%porcelain project)))))))

(defun %h2-reject ()
  (stage "h2: Reject (3): the area is discarded")
  (%plan-tui "tui-plan-reject" (%tui-fix-steps)
             (lambda (tui program project)
               (declare (ignore program))
               (when (%h-turn tui)
                 (h:tui-type tui "3")
                 (prog1 (%wait tui "discarded")
                   (snap tui "Reject")
                   (show "calc.sh in the project is the original" (equal *calc-sh* (read-file project "calc.sh")))
                   (show "plans" (mapcar (lambda (p) (list (code:plan-status p)
                                                           (and (uiop:directory-exists-p (code:plan-root p)) t)))
                                         (code:list-plans (%plans-dir project)))))))))

(defun %h3-continue ()
  (stage "h3: Keep planning (2), another turn in the same area, then Approve")
  (%plan-tui "tui-plan-continue"
             (%tui-fix-steps (list :tool "edit" (%edit-args "calc.sh" "# calc.sh: arithmetic helpers"
                                                           "# calc.sh: arithmetic helpers (tested by test.sh)"))
                             "Comment updated; the fix from the first turn is still there.")
             (lambda (tui program project)
               (when (%h-turn tui)
                 (h:tui-type tui "2")
                 (%wait tui "stays open")
                 (snap tui "Keep planning: the plan stays open")
                 (send-line tui "also mention test.sh in the header comment")
                 (%wait tui "still there")
                 (%wait-until tui (lambda () (%idle-p program)) "idle")
                 (when (%wait tui "Keep planning")
                   (snap tui "the second turn: the dialog shows both changes")
                   (h:tui-type tui "1"))
                 (prog1 (%wait tui "applied")
                   (show "calc.sh in the project" (read-file project "calc.sh"))
                   (show "plans (one plan for both turns)"
                         (mapcar #'code:plan-status (code:list-plans (%plans-dir project)))))))))

(defun %h4-drift ()
  (stage "h4: the working copy changes before Approve: the drift dialog, Rebase (1), then Approve")
  (%plan-tui "tui-plan-drift" (%tui-fix-steps)
             (lambda (tui program project)
               (declare (ignore program))
               (when (%h-turn tui)
                 (write-file project "calc.sh"
                             (concatenate 'string *calc-sh* (format nil "~%sub() {~%  echo $(( $1 - $2 ))~%}~%")))
                 (h:tui-type tui "1")
                 (when (%wait tui "the working copy changed")
                   (snap tui "the drift dialog")
                   (h:tui-type tui "1")
                   (%wait tui "rebase")
                   (snap tui "Rebase onto the working copy")
                   (when (%wait tui "Keep planning" :timeout 5)
                     (h:tui-type tui "1"))
                   (unless (h:tui-wait-for tui "applied" :timeout 5)
                     (send-line tui "/approve"))
                   (prog1 (%wait tui "applied")
                     (snap tui "approved after the rebase")
                     (show "calc.sh in the project (the fix and the user's sub())" (read-file project "calc.sh"))))))))

(defun demo-plan-interactive ()
  (banner "Demo h: plan mode (H5a) interactive: the plan dialog on the headless terminal (scripted streaming mock)")
  (let ((ok (list (%h1-approve) (%h2-reject) (%h3-continue) (%h4-drift))))
    (show "scenarios that reached their last screen (h1 h2 h3 h4)" (mapcar (lambda (x) (and x t)) ok))
    (unless (every #'identity ok)
      (error "an interactive plan scenario did not finish: ~s" ok))
    t))

;;; i) a real model fixes the failing test in the area.

(defparameter *plan-system*
  "Answer briefly. The working directory already holds the project: use relative paths such as calc.sh and test.sh, never absolute ones, and do not cd. Use the tools; do not guess file contents."
  "Appended to the real-model plan run's system prompt.")

(defparameter *real-plan-prompt*
  "sh test.sh fails. Run sh test.sh with the bash tool, read calc.sh, fix the function that the failing test names with the edit tool (file_path calc.sh), then run sh test.sh again.")

(defun %real-plan-attempt (k)
  "One real-model plan run in a fresh project. → true when the area's tests pass."
  (stage (format nil "attempt ~d" k))
  (let* ((project (make-git-project (format nil "plan-real-~d" k) (calc-files)))
         (config (scratch-dir (format nil "plan-real-~d-config" k)))
         (argv (list "-p" "--output-format" "stream-json" "--permission-mode" "plan"
                     "--max-turns" "16" "--append-system-prompt" *plan-system* *real-plan-prompt*)))
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
      (let* ((events (decode-lines out))
             (projected (%event events "plan" "projected"))
             (result (%event events "result"))
             (plan (%find-plan project (jget (%event events "plan" "opened") "plan_id")))
             (tests (%tests (code:plan-root plan)))
             (fixed (search "[exit 0]" tests)))
        (show "exit code" exit)
        (show "stderr" err)
        (dolist (e events)
          (cond ((equal "tool_use" (jget e "type"))
                 (note "  tool_use ~a ~a" (jget e "name") (clip (json:encode (jget e "input")) 160)))
                ((equal "tool_result" (jget e "type"))
                 (note "    → ~a: ~a" (jget e "status") (clip (%one-line (jget e "content")) 160)))))
        (show "result subtype, turns" (list (jget result "subtype") (jget result "num_turns")))
        (show "answer" (clip (or (jget result "result") "") 600))
        (show "the projected diff" (or (jget projected "diff") "(no projected event)"))
        (show "sh test.sh in the area (run by the demo)" tests)
        (show "the project is untouched" (list (%porcelain project)
                                               (equal *calc-sh* (read-file project "calc.sh"))))
        (let ((argv (list "-p" "--resume" (jget result "session_id") "--reject")))
          (show-argv argv)
          (show "stderr" (nth-value 2 (cli argv :cwd project :config config))))
        fixed))))

(defun demo-plan-real-model ()
  (banner "Demo i: plan mode on the LM Studio model: fix the failing test in the area")
  (cond ((not (real-model-p))
         (note "skipped: no LM Studio model (Demo g shows the same flow on the scripted mock)")
         t)
        (t
         (note "model: LM Studio ~a (real; output varies run to run; up to 3 attempts)" (second *model*))
         (let ((fixed (loop for k from 1 to 3 thereis (%real-plan-attempt k))))
           (note "the real model ~:[did not fix~;fixed~] the test in the area" fixed)
           t))))
