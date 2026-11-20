(in-package #:harness-demos-code)

;;; The five demos. Each builds its own small project under the scratch
;;; directory and drives CL-STACK-CODE:RUN-CLI with an argv, as the binary
;;; would.

(defparameter *no-think*
  "Answer briefly. Use the tools when the task needs a file; do not guess file contents. File paths are relative to the working directory: write greeting.txt, not /greeting.txt."
  "Appended to every real-model run's system prompt.")

(defun %events-summary (events)
  "Print stream-json EVENTS: every event but text_delta in full (clipped),
text_delta events counted and concatenated per step."
  (let ((deltas (make-hash-table)))
    (dolist (e events)
      (when (equal (jget e "type") "text_delta")
        (push (jget e "text") (gethash (jget e "step") deltas))))
    (dolist (e events)
      (let ((type (jget e "type")))
        (cond ((equal type "text_delta"))
              (t (note "  ~a" (clip (json:encode e) 400))
                 (when (and (equal type "assistant") (gethash (jget e "step") deltas))
                   (note "    (~d text_delta events before it for step ~d)"
                         (length (gethash (jget e "step") deltas)) (jget e "step")))))))))

(defun %result (events)
  (find "result" events :key (lambda (e) (jget e "type")) :test #'equal))

(defun %tool-results (events)
  (remove "tool_result" events :key (lambda (e) (jget e "type")) :test-not #'equal))

;;; a) text output, a read and an answer.

(defun demo-text ()
  (banner "Demo a: -p --output-format text, read a file and answer")
  (call-with-mock-fallback "a successful answer" (lambda ()
  (let* ((project (make-project "text" `(("notes.txt" ,(format nil "Release notes~%Codename: Juniper~%Ships: 2026-12-01~%")))))
         (argv (list "-p" "--output-format" "text" "--max-turns" "8"
                     "--append-system-prompt" *no-think*
                     "Read notes.txt and tell me the release codename in one sentence.")))
    (setf *mock-script*
          (list (list (list "read" (json-args "file_path" "notes.txt")))
                (lambda (turns)
                  (let ((line (find-if (lambda (l) (search "Codename" l))
                                       (lines (last-tool-result-text turns)))))
                    (format nil "The release codename is ~a."
                            (if line (string-trim " " (subseq line (1+ (position #\: line)))) "unknown"))))))
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config (scratch-dir "text-config"))
      (show "exit code" exit)
      (show "stdout" out)
      (show "stderr" (if (string= err "") "(empty)" err))
      (eql exit 0))))))

;;; b) stream-json events of an edit allowed by a rule.

(defun demo-stream-json ()
  (banner "Demo b: --output-format stream-json, an edit allowed by --allowed-tools 'edit(greeting.txt)'")
  (call-with-mock-fallback "the edit" (lambda ()
  (let* ((project (make-project "stream" `(("greeting.txt" ,(format nil "Hello, world!~%")))))
         (argv (list "-p" "--output-format" "stream-json" "--max-turns" "8"
                     "--allowed-tools" "edit(greeting.txt)"
                     "--append-system-prompt" *no-think*
                     "In greeting.txt replace the word Hello with Goodbye using the edit tool, then say done.")))
    (setf *mock-script*
          (list (list (list "edit" (json-args "file_path" "greeting.txt"
                                              "old_string" "Hello" "new_string" "Goodbye")))
                "Done: greeting.txt now says Goodbye, world!"))
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config (scratch-dir "stream-config"))
      (let ((events (decode-lines out)))
        (show "exit code" exit)
        (show "stderr" (if (string= err "") "(empty)" err))
        (show "events" (length events))
        (show "event types in order"
              (remove-duplicates (mapcar (lambda (e) (jget e "type")) events)
                                 :test #'equal :from-end t))
        (stage "the events (text_delta folded)")
        (%events-summary events)
        (stage "greeting.txt after the run")
        (show "greeting.txt" (read-file project "greeting.txt"))
        (show "tool results (name status)"
              (mapcar (lambda (e) (list (jget e "name") (jget e "status")))
                      (%tool-results events)))
        (search "Goodbye" (read-file project "greeting.txt"))))))))

;;; c) headless denial of a write.

(defun demo-headless-denial ()
  (banner "Demo c: headless denial, a write without permission (default mode)")
  (call-with-mock-fallback "a denied write" (lambda ()
  (let* ((project (make-project "deny" '(("README.md" "# demo
"))))
         (argv (list "-p" "--output-format" "stream-json" "--max-turns" "4"
                     "--append-system-prompt" *no-think*
                     "Create a file todo.txt containing the line 'ship it' using the write tool.")))
    (setf *mock-script*
          (list (list (list "write" (json-args "file_path" "todo.txt" "content" (format nil "ship it~%"))))
                (lambda (turns)
                  (format nil "I could not write todo.txt: ~a"
                          (clip (last-tool-result-text turns) 120)))))
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config (scratch-dir "deny-config"))
      (let* ((events (decode-lines out))
             (result (%result events)))
        (show "exit code" exit)
        (show "stderr" (if (string= err "") "(empty)" err))
        (stage "tool_use / tool_result / result events")
        (dolist (e events)
          (when (member (jget e "type") '("tool_use" "tool_result" "result") :test #'equal)
            (note "  ~a" (clip (json:encode e) 600))))
        (stage "verdict")
        (show "todo.txt exists" (and (probe-file (merge-pathnames "todo.txt" project)) t))
        (show "result subtype" (jget result "subtype"))
        (show "permission_denials"
              (map 'list (lambda (d) (list (jget d "name") (jget d "reason")))
                   (or (jget result "permission_denials") #())))
        (let ((denied (find "denied" (%tool-results events)
                            :key (lambda (e) (jget e "status")) :test #'equal)))
          (if denied
              (note "a tool_result with status \"denied\" reached the model and the run finished (~a)."
                    (jget result "subtype"))
              (note "NOTE: the model made no write call this run, so nothing was denied."))
          denied)))))))

;;; d) layered settings.

(defun %rule-text (rule)
  (format nil "~a ~a ~(~a~)" (agent:permission-rule-action rule)
          (agent:permission-rule-resource rule) (agent:permission-rule-effect rule)))

(defun %print-settings (label s)
  (note "~a:" label)
  (note "  model ~s  maxTurns ~s  permissionMode ~s"
        (code:settings-model s) (code:settings-max-turns s)
        (and (code:settings-permission-mode s)
             (code:permission-mode-name (code:settings-permission-mode s))))
  (note "  allow ~s" (mapcar #'%rule-text (code:settings-allow s)))
  (note "  ask   ~s" (mapcar #'%rule-text (code:settings-ask s)))
  (note "  deny  ~s" (mapcar #'%rule-text (code:settings-deny s)))
  (note "  env   ~s" (code:settings-env s))
  (note "  appendSystemPrompt ~s" (code:settings-append-system-prompt s)))

(defun demo-settings ()
  (banner "Demo d: layered settings, user + project + local + flags")
  (let* ((config (scratch-dir "settings-config"))
         (project (make-project "settings" '(("src/app.lisp" "(defun app ())
")))))
    (write-file config "settings.json"
                "{\"model\": \"openai:user-model\", \"maxTurns\": 20,
 \"permissions\": {\"allow\": [\"read\", \"bash(git status)\"], \"deny\": [\"bash(rm *)\"]},
 \"env\": {\"DEMO_A\": \"user\", \"DEMO_B\": \"user\"},
 \"appendSystemPrompt\": \"user: be terse\"}")
    (write-file project ".cl-stack-code/settings.json"
                "{\"permissionMode\": \"acceptEdits\",
 \"permissions\": {\"allow\": [\"edit(src/*)\"], \"ask\": [\"bash(git push *)\"], \"deny\": [\"job_kill\"]},
 \"env\": {\"DEMO_B\": null}, \"maxTurns\": 12}")
    (write-file project ".cl-stack-code/settings.local.json"
                "{\"maxTurns\": 8, \"permissions\": {\"allow\": [\"read\", {\"tool\": \"grep\", \"resource\": \"path:src/*\"}]},
 \"env\": {\"DEMO_B\": \"local\", \"DEMO_C\": \"local\"}}")
    (dolist (f '("settings.json"))
      (show (format nil "user ~a" f) (read-file config f)))
    (show "project .cl-stack-code/settings.json" (read-file project ".cl-stack-code/settings.json"))
    (show "local .cl-stack-code/settings.local.json" (read-file project ".cl-stack-code/settings.local.json"))
    (let* ((flags (list "-p" "--model" "mock:flag-model" "--allowed-tools" "grep, bash(git log --oneline)"
                        "--disallowed-tools" "write(secrets/*)" "--append-system-prompt" "flags: cite files"
                        "hi"))
           (inv (code:parse-arguments flags)))
      (note "flags: ~{~a~^ ~}" flags)
      (stage "each layer")
      (destructuring-bind (user proj local) (code:settings-file-paths project :user-config-dir config)
        (%print-settings "user" (code:read-settings-file user))
        (%print-settings "project" (code:read-settings-file proj))
        (%print-settings "local" (code:read-settings-file local)))
      (%print-settings "flags" (code:invocation-settings inv))
      (stage "merged (load-settings)")
      (let ((merged (code:load-settings project :user-config-dir config
                                                :cli (code:invocation-settings inv))))
        (%print-settings "merged" merged)
        (note "  files read, in order:~{~%    ~a~}" (mapcar #'%native (code:settings-files merged)))
        (stage "the agent's rule list (mode base rules, then allow, ask, deny: deny beats ask beats allow)")
        (dolist (r (code:settings-permission-rules merged))
          (note "  ~a" (%rule-text r)))
        (show "hidden tools (a bare deny)"
              (code:hidden-tool-names merged '("read" "write" "edit" "grep" "bash" "job_output" "job_kill")))
        (stage "a mock run on these files: its system init event")
        (let ((*model* '(:mock))
              (*mock-script* '("ok")))
          (let* ((s (make-string-output-stream))
                 (exit (code:run-cli (list "--cwd" (%native project) "-p" "--output-format" "stream-json"
                                           "--model" "mock:flag-model" "hi")
                                     :output s :user-config-dir config
                                     :providers (model-providers) :getenv (constantly nil)
                                     :data-dir (data-dir))))
            (show "exit" exit)
            (note "  ~a" (json:encode (first (decode-lines (get-output-stream-string s)))))))))))

;;; e) AGENTS.md discovery and the system prompt.

(defun demo-instructions ()
  (banner "Demo e: AGENTS.md discovery and the assembled system prompt")
  (let* ((config (scratch-dir "instr-config"))
         (project (make-project "instr"
                                '(("AGENTS.md" "# Project rules
Run `cl-repo test` before you report.
")
                                  ("CLAUDE.md" "@AGENTS.md (a fallback; AGENTS.md wins in this directory)
")
                                  ("pkg/CLAUDE.md" "# pkg
Only CLAUDE.md here, so it counts.
")
                                  ("pkg/core/AGENTS.md" "# core
Keep the exported API stable.
")
                                  ("pkg/core/empty/AGENTS.md" "   
")
                                  ("pkg/core/lib/AGENTS.md" "# lib (below the working directory: lazy)
")))))
    (write-file config "AGENTS.md" "# User-level
Prefer small commits.
")
    (let ((cwd (merge-pathnames "pkg/core/" project)))
      (show "project root" (%native (code:find-project-root cwd)))
      (show "working directory" (%native cwd))
      (show "files in the tree" (list "AGENTS.md" "CLAUDE.md" "pkg/CLAUDE.md" "pkg/core/AGENTS.md"
                                      "pkg/core/empty/AGENTS.md (blank)" "pkg/core/lib/AGENTS.md"))
      (stage "instruction-files (prompt order)")
      (dolist (f (code:instruction-files cwd :user-config-dir config))
        (note "  ~a" (%native f)))
      (stage "assemble-system-prompt: the first lines")
      (let* ((tools '("read" "write" "edit" "multi_edit" "glob" "grep" "ls" "bash"
                      "job_output" "job_input" "job_kill" "job_list"))
             (prompt (code:assemble-system-prompt
                      cwd :tools tools :user-config-dir config
                          :settings (code:make-settings :permission-mode :accept-edits
                                                        :append-system-prompt "Appended by --append-system-prompt.")))
             (all (uiop:split-string prompt :separator '(#\Newline))))
        (show "prompt chars" (length prompt))
        (show "first 12 lines" (format nil "~{~a~^~%~}" (subseq all 0 (min 12 (length all)))))
        (let ((env (position "# Environment" all :test #'string=)))
          (when env
            (show "from # Environment to the end"
                  (format nil "~{~a~^~%~}" (subseq all env)))))))))
