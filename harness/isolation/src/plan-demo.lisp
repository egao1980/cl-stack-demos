(in-package #:harness-demos-isolation)

;;; Demos 6-7: R4d-4b, plan mode confined (cl-stack-code 0.10.0). The real CLI
;;; (CL-STACK-CODE:RUN-CLI, the entry point of the cl-stack-code binary) runs
;;; `-p --permission-mode plan` in-process with private config and data
;;; directories; on macOS its plan shell is the real Seatbelt backend
;;; (*PLAN-COMPUTE-BACKEND* :DEFAULT). Demo 6 scripts the model (llm-protocol's
;;; mock backend answering one step per request, as cl-stack-code's own tests'
;;; scripted backend does: a list of tool calls, or a final text); Demo 7 is a
;;; live model fixing a failing test in the plan area.

(defvar *system-prompts* '() "The system prompt of each run's first request, newest first.")

(defun %system-text (turns)
  (format nil "~{~a~^~%~}"
          (loop for turn in (llm:coerce-turns turns)
                when (eq :system (llm:llm-turn-role turn))
                  collect (llm:turn-text turn))))

(defun scripted-provider (script)
  "A \"mock\" provider for RUN-CLI: a fresh llm-protocol mock backend per run;
request N answers with step N of SCRIPT (a list of (tool json-arguments)
calls → finish :tool-use; a string → the final text). The first request's
system prompt is pushed on *SYSTEM-PROMPTS*. Call ids are call-N-K."
  (cons "mock"
        (lambda (model env)
          (declare (ignore model env))
          (let ((steps script) (n 0))
            (llm:make-mock-llm-backend
             :handler (lambda (b turns &key &allow-other-keys)
                        (declare (ignore b))
                        (when (zerop n) (push (%system-text turns) *system-prompts*))
                        (incf n)
                        (let ((step (if steps (pop steps) "(the script is out of steps)")))
                          (if (stringp step)
                              (llm:make-llm-response :parts (list (llm:make-llm-text-part :text step))
                                                     :finish-reason :stop :model "mock")
                              (llm:make-llm-response
                               :parts (loop for (name args) in step
                                            for k from 1
                                            collect (llm:make-llm-tool-call-part
                                                     :id (format nil "call-~d-~d" n k)
                                                     :name name :arguments args))
                               :finish-reason :tool-use :model "mock")))))))))

(defun bash-step (command) (list (list "bash" (json-args "command" command))))

(defun model-requests () (length *system-prompts*))

;;; Running the CLI.

(defun run-code (argv &key cwd config data providers getenv)
  "RUN-CLI with --cwd CWD and ARGV. → (values exit stdout stderr)."
  (let* ((argv (append (list "--cwd" (native cwd)) argv))
         (err (make-string-output-stream))
         (exit nil)
         (out (with-output-to-string (o)
                (setf exit (code:run-cli argv :output o :error-output err
                                              :input (make-string-input-stream "")
                                              :providers providers
                                              :user-config-dir config
                                              :getenv (or getenv (constantly nil))
                                              :data-dir data)))))
    (values exit out (get-output-stream-string err))))

(defun show-argv (argv)
  (print-block (format nil "$ cl-stack-code ~{~a~^ ~}"
                       (mapcar (lambda (a) (if (or (find #\Space a) (find #\' a)) (format nil "'~a'" a) a))
                               argv))
               :prefix "   "))

(defun decode-events (out)
  (loop for line in (uiop:split-string out :separator (string #\Newline))
        when (plusp (length (string-trim " " line)))
          collect (json:decode line)))

(defun jget (object &rest keys)
  (dolist (k keys object)
    (setf object (and (hash-table-p object) (gethash k object)))))

(defun find-event (events type &optional subtype)
  (find-if (lambda (e) (and (equal type (jget e "type"))
                            (or (null subtype) (equal subtype (jget e "subtype")))))
           events))

(defun tool-result-events (events)
  (remove "tool_result" events :key (lambda (e) (jget e "type")) :test-not #'equal))

(defun print-tool-results (events &key (max-lines 14))
  (loop for r in (tool-result-events events)
        for k from 1
        do (note "tool_result ~d: ~a, status ~a" k (jget r "name") (jget r "status"))
           (print-block (princ-to-string (or (jget r "content") "")) :prefix "     | " :max-lines max-lines)))

(defun print-json (label object)
  (note "~a:" label)
  (print-block (json:encode object) :prefix "     "))

(defun sandbox-section (system)
  "The \"# Sandbox\" section of SYSTEM (up to the next top-level heading), or NIL."
  (let ((start (search (format nil "# Sandbox~%") system)))
    (when start
      (let ((end (search (format nil "~%# ") system :start2 (+ start 2))))
        (string-right-trim '(#\Newline) (subseq system start end))))))

(defun print-stderr (err)
  (if (zerop (length (string-trim '(#\Newline #\Space) err)))
      (note "stderr: (empty)")
      (progn (note "stderr:") (print-block err :prefix "     | "))))

(defmacro with-plan-scratch ((root project config data) files &body body)
  "A temp ROOT with ROOT/project/ (holding FILES, ((relative text) …)), a private
user config dir and data dir; printed as <tmp>/ (and the home as ~/)."
  `(with-temp-dir (,root "hd-plan")
     (with-scrubbed-path ((user-homedir-pathname) "~/")
       (with-scrubbed-path (,root "<tmp>/")
         ;; the plans directory names the project by its path with / as -
         (push (cons (substitute #\- #\/ (string-right-trim "/" (native ,root))) "<tmp-key>")
               *scrub-paths*)
         ;; the login name (ls -l output of the live model)
         (let ((user (uiop:getenv "USER")))
           (when (and user (>= (length user) 4))
             (setf *scrub-paths* (append *scrub-paths* (list (cons user "<user>"))))))
         (let ((,project (merge-pathnames "project/" ,root))
               (,config (merge-pathnames "config/" ,root))
               (,data (merge-pathnames "data/" ,root)))
           (ensure-directories-exist ,project)
           (ensure-directories-exist ,config)
           (ensure-directories-exist ,data)
           (loop for (relative text) in ,files do (write-file (merge-pathnames relative ,project) text))
           ,@body)))))

(defun only-plan (project data)
  (first (code:list-plans (code:plans-directory project :data-dir data))))

(defun plan-argv (&rest more)
  (append (list "-p" "--model" "mock" "--output-format" "stream-json" "--permission-mode" "plan"
                "--plan-area" "copy" "--max-turns" "8")
          more))

;;; ------------------------------------------------------------------
;;; Demo 6: the scripted model

(defun %curl-command (port)
  (format nil "curl -s -m 5 -o /dev/null -w 'http %{http_code}\\n' http://127.0.0.1:~d/" port))

(defun %a-confined (project config data victim port hits)
  (stage "(a) -p --permission-mode plan, confined: a write in the area, a write outside, curl to loopback")
  (setf *system-prompts* '())
  (let* ((script (list (bash-step "mkdir -p build && echo built > build/stamp.txt && cat build/stamp.txt")
                       (bash-step (format nil "echo pwned > '~a'" (native victim)))
                       (bash-step (%curl-command port))
                       "Done: build/stamp.txt is in the plan; the outside write and the curl were blocked."))
         (argv (plan-argv "Build the stamp, update the outside file, check the local server.")))
    (show-argv argv)
    (multiple-value-bind (exit out err)
        (run-code argv :cwd project :config config :data data :providers (list (scripted-provider script)))
      (let* ((events (decode-events out))
             (opened (find-event events "plan" "opened"))
             (projected (find-event events "plan" "projected"))
             (results (tool-result-events events))
             (plan (only-plan project data)))
        (show "exit code" exit)
        (print-stderr err)
        (show "event types" (mapcar (lambda (e) (jget e "type")) events))
        (print-json "plan opened event, \"sandbox\"" (jget opened "sandbox"))
        (show "sandbox.status" (jget opened "sandbox" "status"))
        (show "sandbox.writable[0] is the plan area"
              (equal (string-right-trim "/" (aref (jget opened "sandbox" "writable") 0))
                     (string-right-trim "/" (native (sc:physical-path (code:plan-root plan))))))
        (stage "(a) the system prompt's # Sandbox section (first request)")
        (print-block (or (sandbox-section (first *system-prompts*)) "(no # Sandbox section)") :prefix "   | ")
        (stage "(a) the bash results")
        (print-tool-results events)
        (show "1: in the area, ran" (has "built" (jget (first results) "content")))
        (show "2: outside, carries the [sandbox: …] note" (has "[sandbox: " (jget (second results) "content")))
        (show "2: victim unchanged" (equal (read-file victim) (lines "orig")))
        (show "3: curl failed (no [exit code 0])" (not (has "[exit code 0]" (jget (third results) "content"))))
        (show "3: connections the loopback server saw" (funcall hits))
        (stage "(a) the projected diff (the plan \"projected\" event)")
        (show "changes" (json:encode (jget projected "changes")))
        (print-block (or (jget projected "diff") "") :prefix "   diff | ")
        (show "project unchanged (no build/ in the real tree)"
              (not (probe-file (merge-pathnames "build/stamp.txt" project))))
        (and (eql exit 0) (equal "confined" (jget opened "sandbox" "status"))
             (equal (read-file victim) (lines "orig")))))))

(defun %b-allow-network (project config data port hits)
  (stage "(b) the same curl with --allow-network")
  (setf *system-prompts* '())
  (let* ((before (funcall hits))
         (script (list (bash-step (%curl-command port)) "The server answered."))
         (argv (plan-argv "--allow-network" "Check the local server.")))
    (show-argv argv)
    (multiple-value-bind (exit out err)
        (run-code argv :cwd project :config config :data data :providers (list (scripted-provider script)))
      (let* ((events (decode-events out))
             (opened (find-event events "plan" "opened"))
             (r (first (tool-result-events events))))
        (show "exit code" exit)
        (print-stderr err)
        (print-json "plan opened event, \"sandbox\"" (jget opened "sandbox"))
        (print-tool-results events)
        (show "curl got http 200" (has "http 200" (jget r "content")))
        (show "new connections the server saw" (- (funcall hits) before))
        (note "the # Sandbox section now says:")
        (print-block (or (sandbox-section (first *system-prompts*)) "") :prefix "   | ")
        (and (eql exit 0) (has "http 200" (jget r "content")))))))

(defun %c-unavailable (project config data)
  (stage "(c) confinement unavailable: *plan-compute-backend* NIL (no backend), the same -p plan run")
  (setf *system-prompts* '())
  (let* ((marker (merge-pathnames "ran.txt" project))
         (script (list (bash-step "echo ran > ran.txt")
                       (list (list "write" (json-args "file_path" "notes.txt" "content" (lines "written by the file tool"))))
                       "bash was refused; notes.txt is written with the file tool."))
         (argv (plan-argv "Write ran.txt with bash, then notes.txt.")))
    (show-argv argv)
    (multiple-value-bind (exit out err)
        (let ((code:*plan-compute-backend* nil))
          (run-code argv :cwd project :config config :data data :providers (list (scripted-provider script))))
      (let* ((events (decode-events out))
             (opened (find-event events "plan" "opened"))
             (projected (find-event events "plan" "projected"))
             (results (tool-result-events events))
             (plan (find (jget opened "plan_id") (code:list-plans (code:plans-directory project :data-dir data))
                         :key #'code:plan-id :test #'equal)))
        (show "exit code" exit)
        (print-stderr err)
        (print-json "plan opened event, \"sandbox\"" (jget opened "sandbox"))
        (print-tool-results events)
        (show "bash status (the ask -p refuses)" (jget (first results) "status"))
        (show "nothing ran: ran.txt absent in the project and the area"
              (not (or (probe-file marker) (and plan (probe-file (merge-pathnames "ran.txt" (code:plan-root plan)))))))
        (show "the file tool still works in the area" (json:encode (jget projected "changes")))
        (note "the # Sandbox section:")
        (print-block (or (sandbox-section (first *system-prompts*)) "") :prefix "   | ")))
    (stage "(c) the same, with sandbox.failIfUnavailable in the user settings")
    (setf *system-prompts* '())
    (write-file (merge-pathnames "settings.json" config) "{\"sandbox\": {\"failIfUnavailable\": true}}")
    (let ((argv (plan-argv "Write ran.txt with bash.")))
      (note "user settings.json: {\"sandbox\": {\"failIfUnavailable\": true}}")
      (show-argv argv)
      (multiple-value-bind (exit out err)
          (let ((code:*plan-compute-backend* nil))
            (run-code argv :cwd project :config config :data data
                           :providers (list (scripted-provider (list (bash-step "echo ran > ran.txt") "ok")))))
        (show "exit code" exit)
        (print-stderr err)
        (show "stdout events" (mapcar (lambda (e) (list (jget e "type") (jget e "subtype"))) (decode-events out)))
        (show "model requests (must be 0)" (model-requests))
        (show "ran.txt absent" (not (probe-file marker)))
        (delete-file (merge-pathnames "settings.json" config))
        (eql exit 1)))))

(defun demo-plan-confined ()
  (banner "Demo 6: R4d-4b plan mode confined — cl-stack-code 0.10.0 -p --permission-mode plan (scripted model)")
  (let ((backend (code:plan-compute-backend)))
    (show "plan-compute-backend (:default resolved)" (and backend (type-of backend)))
    (unless (and backend (sc:confinement-available-p backend))
      (note "skipped: no confining backend is available on this host (~a)" (software-type))
      (return-from demo-plan-confined)))
  (unless (have-curl-p)
    (note "skipped: no curl on PATH")
    (return-from demo-plan-confined))
  (with-plan-scratch (root project config data)
      `(("README.md" ,(lines "# demo" "" "A plain folder (plan area: a copy)."))
        ("src/main.sh" ,(lines "echo main")))
    (let ((victim (outside-victim root)))
      (show "victim (outside the project and the area)" (scrub (native victim)))
      (call-with-loopback-server
       (lambda (port hits)
         (let ((ok (list (%a-confined project config data victim port hits)
                         (%b-allow-network project config data port hits)
                         (%c-unavailable project config data))))
           (show "steps that ended as expected (a b c)" ok)
           (unless (every #'identity ok)
             (error "a plan step did not end as expected: ~s" ok))))))))

;;; ------------------------------------------------------------------
;;; Demo 7: a live model fixes a failing test in the confined plan area

(defparameter *calc-sh*
  (format nil "# calc.sh: arithmetic helpers~%add() {~%  echo $(( $1 - $2 ))~%}~%~%mul() {~%  echo $(( $1 * $2 ))~%}~%"))

(defparameter *test-sh*
  (format nil "#!/bin/sh~%# Run: sh test.sh~%. ./calc.sh~%fail=0~%check () {~%  if [ \"$2\" = \"$3\" ]; then echo \"ok   $1\"; else echo \"FAIL $1: expected $3, got $2\"; fail=1; fi~%}~%check \"add 2 3\" \"$(add 2 3)\" 5~%check \"mul 4 5\" \"$(mul 4 5)\" 20~%[ $fail = 0 ] && echo \"all tests passed\"~%exit $fail~%"))

(defparameter *fix-prompt*
  "The tests fail. Run sh test.sh, fix calc.sh, and run the tests again. Keep the answer short.")

(defun sampling-flags ()
  "The preset as cl-stack-code --model-param KEY=VALUE flags (VALUE as JSON)."
  (loop for (k . v) in (sampling-fields)
        append (list "--model-param" (format nil "~a=~a" k (json:encode v)))))

(defun live-provider ()
  "\"openai\" for RUN-CLI on the live server, with this set's request timeout
(*LM-REQUEST-TIMEOUT*; llm-protocol-openai's own transport waits 180 s) and its
usage tap. The key is read here and never printed."
  (cons "openai"
        (lambda (model env)
          (declare (ignore env))
          (change-class (oa:make-openai-compat-backend :base-url *lm-url* :api-key (%lm-token)
                                                       :default-model model
                                                       :request-fn (%request-fn *lm-request-timeout*))
                        'usage-tapping-backend))))

(defun run-tests-in (dir)
  (multiple-value-bind (out err code)
      (uiop:run-program '("sh" "test.sh") :directory dir :output :string :error-output :string
                                         :ignore-error-status t)
    (format nil "~a~a[exit ~d]" out err code)))

(defun demo-plan-confined-live ()
  (banner "Demo 7: a live model in confined plan mode — fix a failing test, then --apply")
  (let ((backend (code:plan-compute-backend)))
    (unless (and backend (sc:confinement-available-p backend))
      (note "skipped: no confining backend is available on this host (~a)" (software-type))
      (return-from demo-plan-confined-live)))
  (unless (detect-model)
    (note "skipped: no live model (set HARNESS_DEMOS_BASE_URL / HARNESS_DEMOS_API_KEY / HARNESS_DEMOS_MODEL)")
    (return-from demo-plan-confined-live))
  (setf *lm-usage* '())
  (with-plan-scratch (root project config data)
      `(("calc.sh" ,*calc-sh*) ("test.sh" ,*test-sh*)
        ("README.md" ,(lines "# calc" "" "`sh test.sh` runs the tests.")))
    (show "sh test.sh in the project before" (run-tests-in project))
    (let* ((designator (format nil "openai:~a" *lm-model*))
           (argv (append (list "-p" "--model" designator)
                         (sampling-flags)
                         (list "--output-format" "stream-json" "--permission-mode" "plan"
                               "--plan-area" "copy" "--max-turns" "16" *fix-prompt*)))
           (t0 (get-internal-real-time)))
      (show-argv argv)
      (multiple-value-bind (exit out err)
          (run-code argv :cwd project :config config :data data :providers (list (live-provider)))
        (let* ((secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second))
               (events (decode-events out))
               (opened (find-event events "plan" "opened"))
               (projected (find-event events "plan" "projected"))
               (result (find-event events "result"))
               (bashes (remove "bash" (tool-result-events events) :key (lambda (e) (jget e "name"))
                                                                  :test-not #'equal)))
          (show "exit code" exit)
          (print-stderr err)
          (show "result subtype" (jget result "subtype"))
          (show "seconds" (round secs))
          (print-json "plan opened event, \"sandbox\"" (jget opened "sandbox"))
          (stage "the tool calls, live ~a (varies run to run; results trimmed)" *lm-model*)
          (dolist (e events)
            (when (equal "tool_use" (jget e "type"))
              (note "tool_use ~a ~a" (jget e "name") (clip (scrub (json:encode (jget e "input"))) 200))))
          (print-tool-results events :max-lines 10)
          (show "bash calls" (length bashes))
          (show "bash calls that ran (status done, no approval: confined)"
                (count "done" bashes :key (lambda (e) (jget e "status")) :test #'equal))
          (show "bash results with a [sandbox: …] note"
                (count-if (lambda (e) (has "[sandbox: " (jget e "content"))) bashes))
          (note "answer:")
          (print-block (princ-to-string (or (jget result "result") "")) :prefix "   answer | ")
          (stage "the projected diff")
          (show "changes" (json:encode (jget projected "changes")))
          (print-block (or (jget projected "diff") "(no diff)") :prefix "   diff | ")
          (show "sh test.sh in the project (not applied yet)" (run-tests-in project))
          (print-lm-usage)
          (let ((session (jget result "session_id")))
            (when (and (eql exit 0) projected (plusp (length (jget projected "changes"))) session)
              (stage "-p --resume SESSION --apply (no model call)")
              (let ((argv (list "-p" "--model" designator "--resume" session "--apply")))
                (show-argv (list "-p" "--model" designator "--resume" "<session>" "--apply"))
                (multiple-value-bind (exit out err)
                    (run-code argv :cwd project :config config :data data :providers (list (live-provider)))
                  (show "exit code" exit)
                  (print-block out :prefix "   stdout | ")
                  (print-stderr err)
                  (show "sh test.sh in the project after --apply" (run-tests-in project))
                  (note "LIVE RESULT (d): ~:[the applied plan does not pass the tests~;fixed in the confined area, applied, tests pass~]"
                        (has "all tests passed" (run-tests-in project))))))))))))
