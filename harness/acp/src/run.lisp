(in-package #:harness-demos-acp)

;;; run-demos: the headless wire test. A copy of sample/ in a temp
;;; directory is the project; the scripted client (DEMO-CLIENT) spawns
;;; acp-agent.sh with spawn-acp-agent (the same launcher Zed runs), with the
;;; wire log on (ACP_DEMO_WIRE_LOG = wire-log.txt next to this file), and
;;; runs initialize, session/new and one session/prompt, approving the
;;; edit. Then the same again with the edit rejected (Demo 2, logged too).
;;; Checks print "ok" / "FAIL"; the result is T when all passed.

(defvar *here* (asdf:system-source-directory "harness-demos-acp"))

(defvar *failures* 0)

(defun check (label ok)
  (format t "  [~:[FAIL~;ok~]] ~A~%" ok label)
  (unless ok (incf *failures*))
  ok)

(defun %copy-sample (dir)
  (dolist (name '("fib.lisp" "test.lisp" "README.md") dir)
    (uiop:copy-file (merge-pathnames name (merge-pathnames "sample/" *here*))
                    (merge-pathnames name (ensure-directories-exist dir)))))

(defun %client-caps ()
  (acp:parse-acp-object 'acp:client-capabilities
                        (json-protocol:decode
                         "{\"fs\":{\"readTextFile\":true,\"writeTextFile\":true},\"terminal\":true}")))

(defun %run-turn (label project permission wire-log)
  "One client session against a fresh agent. → (values stop-reason client)."
  (format t "~%~A~%~72,,,'-A~%" label "")
  (let* ((dir (string-right-trim "/" (uiop:native-namestring project)))
         (client (make-instance 'demo-client :permission permission :redact dir))
         (extra (uiop:getenv "ACP_DEMO_EXTRA_TREE")))
    (multiple-value-bind (connection agent)
        (stdio:spawn-acp-agent
         (list (uiop:native-namestring (merge-pathnames "acp-agent.sh" *here*)))
         :client client
         :env (append (list (cons "ACP_DEMO_WIRE_LOG" (uiop:native-namestring wire-log))
                            (cons "ACP_DEMO_REDACT" dir))
                      (and extra (list (cons "ACP_DEMO_EXTRA_TREE" extra)))))
      (unwind-protect
           (let* ((init (acp:acp-initialize
                         agent (acp:make-acp 'acp:initialize-request
                                             :protocol-version 1
                                             :client-capabilities (%client-caps)
                                             :client-info (acp:make-acp 'acp:implementation
                                                                        :name "harness-demos-acp client"
                                                                        :version "0.1.0"))))
                  (info (acp:agent-info init)))
             (format t "  initialize → protocolVersion ~A, agent ~A ~A~%"
                     (acp:protocol-version init) (acp:name info) (acp:version info))
             (let* ((sid (acp:session-id
                          (acp:acp-new-session agent (acp:make-acp 'acp:new-session-request
                                                                   :cwd dir :mcp-servers (vector)))))
                    (_ (format t "  session/new → ~A~%" sid))
                    (response (acp:acp-prompt
                               agent (acp:make-acp 'acp:prompt-request
                                                   :session-id sid
                                                   :prompt (vector (%text "Fix the bug in fib.lisp and run the tests."))))))
               (declare (ignore _))
               (format t "  session/prompt → stopReason ~A~%" (acp:stop-reason response))
               (values (acp:stop-reason response) client)))
        (format t "  agent exit code: ~A~%" (stdio:close-acp-connection connection :timeout 10))))))

(defun %has (client prefix)
  (some (lambda (e) (and (>= (length e) (length prefix)) (string= prefix e :end2 (length prefix))))
        (client-events client)))

(defun run-demos ()
  (let* ((*failures* 0)
         (wire-log (merge-pathnames "wire-log.txt" *here*))
         (root (merge-pathnames (format nil "acp-demo-~36R/" (random (expt 36 8) (make-random-state t)))
                                (uiop:temporary-directory)))
         (approve (merge-pathnames "approve/" root))
         (reject (merge-pathnames "reject/" root)))
    (when (probe-file wire-log) (delete-file wire-log))
    (format t "harness-demos-acp 0.1.0: acp-protocol ~A, acp-backend-stdio ~A~%"
            (asdf:component-version (asdf:find-system "acp-protocol"))
            (asdf:component-version (asdf:find-system "acp-backend-stdio")))
    (%copy-sample approve)
    (%copy-sample reject)
    (unwind-protect
         (progn
           ;; Demo 1: approve
           (multiple-value-bind (stop client)
               (%run-turn "Demo 1: the fix turn, edit approved (allow_once)" approve 'acp:allow-once wire-log)
             (let ((after (uiop:read-file-string (merge-pathnames "fib.lisp" approve))))
               (format t "~%  fib.lisp after the turn:~%~A~%" after)
               (check "stopReason end_turn" (equal stop "end_turn"))
               (check "plan sent" (%has client "session/update plan"))
               (check "fs/read_text_file" (%has client "fs/read_text_file"))
               (check "tool_call with diff content" (%has client "session/update tool_call edit-fib"))
               (check "session/request_permission" (%has client "session/request_permission"))
               (check "fs/write_text_file" (%has client "fs/write_text_file"))
               (check "fib.lisp fixed on disk" (search "(loop repeat n" after))
               (check "tests ran in a terminal, exit 0" (%has client "terminal/wait_for_exit term-1 → 0"))
               (check "plan finished: 4 entries completed"
                      (%has client "session/update plan [completed: Read fib.lisp through the editor (fs/read_text_file) | completed: Propose the off-by-one fix as a diff | completed: Apply the fix once you approve it (fs/write_text_file) | completed: Run"))))
           ;; Demo 2: reject
           (multiple-value-bind (stop client)
               (%run-turn "Demo 2: the same turn, edit rejected (reject_once)" reject 'acp:reject-once wire-log)
             (let ((after (uiop:read-file-string (merge-pathnames "fib.lisp" reject))))
               (check "stopReason end_turn" (equal stop "end_turn"))
               (check "no fs/write_text_file" (not (%has client "fs/write_text_file")))
               (check "edit tool call failed" (%has client "session/update tool_call_update edit-fib status failed"))
               (check "fib.lisp unchanged" (search "(loop repeat (1- n)" after))
               (check "no terminal" (not (%has client "terminal/create")))))
           (let ((lines (with-open-file (in wire-log) (loop while (read-line in nil) count t))))
             (format t "~%Wire log: wire-log.txt, ~D messages~%" lines)
             (check "wire log written" (plusp lines))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
    (format t "~%~72,,,'=A~%Checks failed: ~D~%" "" *failures*)
    (finish-output)
    (zerop *failures*)))
