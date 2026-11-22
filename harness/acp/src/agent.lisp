(in-package #:harness-demos-acp)

;;; The scripted demo agent. No model: every prompt runs the same turn on the
;;; session's project (the directory the client opened):
;;;   1. agent_message_chunk: what it will do (streamed word by word);
;;;   2. plan: four entries, re-sent as each one starts and finishes;
;;;   3. tool_call "read" + fs/read_text_file <cwd>/fib.lisp;
;;;   4. tool_call "edit" with diff content (the off-by-one fix);
;;;   5. session/request_permission (allow_once / reject_once);
;;;   6. allowed: fs/write_text_file, the edit tool call completed;
;;;      rejected: the edit tool call failed, the turn ends (end_turn);
;;;   7. client advertises `terminal`: terminal/create `sbcl --script
;;;      test.lisp` in the project, a tool_call with terminal content,
;;;      terminal/wait_for_exit, terminal/output, terminal/release;
;;;      otherwise a message saying the tests were not run;
;;;   8. a final message, stopReason end_turn.
;;; session/cancel ends the turn at the next step (stopReason cancelled).
;;; ACP_DEMO_PACE (seconds, default 0) pauses between steps so a screen
;;; recording can follow the turn.

(defparameter *bug* "(loop repeat (1- n)")
(defparameter *fix* "(loop repeat n")

(defclass demo-agent (acp:acp-agent)
  ((lock :initform (bt2:make-lock :name "demo agent") :reader %agent-lock)
   (sessions :initform (make-hash-table :test 'equal) :reader %sessions
             :documentation "session id → cwd, under LOCK.")
   (cancelled :initform '() :accessor %cancelled :documentation "Session ids, under LOCK.")
   (counter :initform 0 :accessor %counter)
   (pace :initarg :pace :initform 0 :reader agent-pace)))

(defun %pause (agent &optional (factor 1))
  (let ((s (* factor (agent-pace agent))))
    (when (plusp s) (sleep s))))

(defun %check-cancel (agent sid)
  (when (bt2:with-lock-held ((%agent-lock agent))
          (member sid (%cancelled agent) :test #'equal))
    (error 'acp:acp-request-cancelled)))

(defun %text (s) (acp:make-acp 'acp:text-content :text s))

(defun %update (sid update)
  (acp:acp-send-update acp:*acp-remote* sid update))

(defun %say (agent sid text &key (stream t))
  "TEXT as agent_message_chunk updates: word by word when STREAM and the
agent is paced, else one chunk."
  (if (and stream (plusp (agent-pace agent)))
      (loop with words = (uiop:split-string text :separator " ")
            for (w . more) on words
            do (%update sid (acp:make-acp 'acp:update-agent-message-chunk
                                          :content (%text (if more (format nil "~A " w) w))))
               (sleep (/ (agent-pace agent) 12)))
      (%update sid (acp:make-acp 'acp:update-agent-message-chunk :content (%text text)))))

(defparameter *plan-steps*
  '("Read fib.lisp through the editor (fs/read_text_file)"
    "Propose the off-by-one fix as a diff"
    "Apply the fix once you approve it (fs/write_text_file)"
    "Run the tests: sbcl --script test.lisp (terminal/*)"))

(defun %send-plan (sid statuses)
  (%update sid (acp:make-acp
                'acp:update-plan
                :entries (map 'vector (lambda (content status)
                                        (acp:make-acp 'acp:plan-entry :content content
                                                                      :priority "high"
                                                                      :status status))
                              *plan-steps* statuses))))

(defun %plan-at (sid step &key (done nil))
  "Plan with entries before STEP completed, STEP in_progress (completed when
DONE), the rest pending."
  (%send-plan sid (loop for i from 0 below (length *plan-steps*)
                        collect (cond ((< i step) "completed")
                                      ((= i step) (if done "completed" "in_progress"))
                                      (t "pending")))))

(defun %location (path) (vector (acp:make-acp 'acp:tool-call-location :path path)))

(defun %client-has (feature)
  (acp:client-supports-p (acp:acp-connection-client-capabilities acp:*acp-connection*)
                         feature))

(defmethod acp:acp-initialize ((agent demo-agent) request)
  (declare (ignore request))
  (acp:make-acp 'acp:initialize-response
                :protocol-version 1
                :agent-capabilities (acp:make-acp 'acp:agent-capabilities :load-session nil)
                :auth-methods (vector)
                :agent-info (acp:make-acp 'acp:implementation
                                          :name "cl-stack-acp-demo" :version "0.1.0")))

(defmethod acp:acp-authenticate ((agent demo-agent) request)
  (declare (ignore request))
  (acp:make-acp 'acp:authenticate-response))

(defmethod acp:acp-new-session ((agent demo-agent) request)
  (let ((id (bt2:with-lock-held ((%agent-lock agent))
              (let ((id (format nil "demo-~D" (incf (%counter agent)))))
                (setf (gethash id (%sessions agent)) (acp:cwd request))
                id))))
    (acp:make-acp 'acp:new-session-response :session-id id)))

(defmethod acp:acp-cancel ((agent demo-agent) notification)
  (bt2:with-lock-held ((%agent-lock agent))
    (push (acp:session-id notification) (%cancelled agent))))

(defun %join (dir name)
  (format nil "~A/~A" (string-right-trim "/" dir) name))

(defun %run-tests (agent sid cwd)
  "Steps 7: the tests in a client terminal. → (values exit-code output)."
  (let* ((rc acp:*acp-remote*)
         (tid (acp:terminal-id
               (acp:acp-create-terminal
                rc (acp:make-acp 'acp:create-terminal-request
                                 :session-id sid :command "sbcl"
                                 :args (vector "--script" "test.lisp")
                                 :cwd cwd :output-byte-limit 65536)))))
    (%update sid (acp:make-acp 'acp:update-tool-call
                               :tool-call-id "run-tests" :title "sbcl --script test.lisp"
                               :kind "execute" :status "in_progress"
                               :content (vector (acp:make-acp 'acp:terminal :terminal-id tid))))
    (let* ((exit (acp:acp-wait-for-terminal-exit
                  rc (acp:make-acp 'acp:wait-for-terminal-exit-request
                                   :session-id sid :terminal-id tid)))
           (code (and (slot-boundp exit 'acp:exit-code) (acp:exit-code exit)))
           (output (acp:output (acp:acp-terminal-output
                                rc (acp:make-acp 'acp:terminal-output-request
                                                 :session-id sid :terminal-id tid)))))
      (%update sid (acp:make-acp 'acp:update-tool-call-update
                                 :tool-call-id "run-tests"
                                 :status (if (eql code 0) "completed" "failed")))
      (%pause agent)
      (acp:acp-release-terminal rc (acp:make-acp 'acp:release-terminal-request
                                                 :session-id sid :terminal-id tid))
      (values code output))))

(defun %last-line (text)
  (let ((lines (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=)))
    (car (last lines))))

(defun %turn (agent sid cwd)
  (let* ((rc acp:*acp-remote*)
         (path (%join cwd "fib.lisp")))
    ;; 1. what it will do
    (%say agent sid (format nil "I'll fix the off-by-one in fib.lisp: read it through the editor, ~
show you the change as a diff, apply it once you approve, then run the tests."))
    (%pause agent)
    ;; 2. the plan
    (%plan-at sid 0)
    (%pause agent)
    (%check-cancel agent sid)
    ;; 3. read
    (%update sid (acp:make-acp 'acp:update-tool-call :tool-call-id "read-fib"
                                                     :title "Read fib.lisp" :kind "read"
                                                     :status "in_progress"
                                                     :locations (%location path)))
    (let ((old (handler-case
                   (acp:content (acp:acp-read-text-file
                                 rc (acp:make-acp 'acp:read-text-file-request
                                                  :session-id sid :path path)))
                 (acp:acp-error (c)
                   (%update sid (acp:make-acp 'acp:update-tool-call-update
                                              :tool-call-id "read-fib" :status "failed"))
                   (%say agent sid (format nil "I could not read ~A: ~A" path c) :stream nil)
                   (return-from %turn "end_turn")))))
      (%update sid (acp:make-acp 'acp:update-tool-call-update :tool-call-id "read-fib"
                                                              :status "completed"))
      (%plan-at sid 0 :done t)
      (%pause agent)
      (%check-cancel agent sid)
      (let ((pos (search *bug* old)))
        (unless pos
          (%say agent sid "fib.lisp has no `(loop repeat (1- n)` any more: nothing to fix.")
          (return-from %turn "end_turn"))
        ;; 4. the diff
        (%plan-at sid 1)
        (%say agent sid (format nil "~%~%Found it: `(loop repeat (1- n) …)` runs one step too few, ~
so (fib 1) is 0 and (fib 10) is 34. It should repeat n times."))
        (let* ((new (concatenate 'string (subseq old 0 pos) *fix*
                                 (subseq old (+ pos (length *bug*)))))
               (diff (vector (acp:make-acp 'acp:diff :path path :old-text old :new-text new))))
          (%update sid (acp:make-acp 'acp:update-tool-call :tool-call-id "edit-fib"
                                                           :title "Fix the off-by-one in fib.lisp"
                                                           :kind "edit" :status "pending"
                                                           :content diff
                                                           :locations (%location path)))
          (%plan-at sid 1 :done t)
          (%pause agent)
          ;; 5. permission
          (%plan-at sid 2)
          (multiple-value-bind (kind option)
              (acp:acp-ask-permission
               rc sid
               (acp:make-acp 'acp:tool-call-update :tool-call-id "edit-fib"
                                                   :title "Fix the off-by-one in fib.lisp"
                                                   :kind "edit" :status "pending"
                                                   :content diff
                                                   :locations (%location path))
               (list (acp:make-acp 'acp:permission-option :option-id "apply"
                                                          :name "Apply the fix" :kind "allow_once")
                     (acp:make-acp 'acp:permission-option :option-id "reject"
                                                          :name "Leave fib.lisp as it is"
                                                          :kind "reject_once")))
            (declare (ignore option))
            (%check-cancel agent sid)
            (unless (member kind '(:allow-once :allow-always))
              (%update sid (acp:make-acp 'acp:update-tool-call-update :tool-call-id "edit-fib"
                                                                      :status "failed"))
              (%say agent sid (format nil "~%~%OK, I left fib.lisp unchanged (permission: ~(~A~))."
                                      kind))
              (return-from %turn (if (eq kind :cancelled) "cancelled" "end_turn")))
            ;; 6. write
            (%update sid (acp:make-acp 'acp:update-tool-call-update :tool-call-id "edit-fib"
                                                                    :status "in_progress"))
            (acp:acp-write-text-file rc (acp:make-acp 'acp:write-text-file-request
                                                      :session-id sid :path path :content new))
            (%update sid (acp:make-acp 'acp:update-tool-call-update :tool-call-id "edit-fib"
                                                                    :status "completed"))
            (%plan-at sid 2 :done t)
            (%pause agent)
            (%check-cancel agent sid)
            ;; 7. tests
            (%plan-at sid 3)
            (cond ((%client-has :terminal)
                   (handler-case
                       (multiple-value-bind (code output) (%run-tests agent sid cwd)
                         (%plan-at sid 3 :done (eql code 0))
                         (%say agent sid (format nil "~%~%Tests: ~A (exit code ~A)."
                                                 (or (%last-line output) "no output") code)))
                     (acp:acp-error (c)
                       (%say agent sid (format nil "~%~%The client could not run the tests: ~A" c)))))
                  (t
                   (%say agent sid (format nil "~%~%This client has no terminal capability, ~
so I did not run the tests; run `sbcl --script test.lisp` in ~A." cwd))))
            ;; 8. done
            (%say agent sid (format nil "~%~%Done: fib.lisp now loops `repeat n`."))
            "end_turn"))))))

(defmethod acp:acp-prompt ((agent demo-agent) request)
  (let* ((sid (acp:session-id request))
         (cwd (bt2:with-lock-held ((%agent-lock agent))
                (setf (%cancelled agent) (remove sid (%cancelled agent) :test #'equal))
                (gethash sid (%sessions agent)))))
    (unless cwd
      (error 'acp:acp-resource-not-found :message "Unknown session"))
    (acp:make-acp 'acp:prompt-response :stop-reason (%turn agent sid cwd))))

(defun serve-demo-agent ()
  "The agent process's entry: serve a DEMO-AGENT on stdin / stdout. With
ACP_DEMO_WIRE_LOG set, every line is logged there (ACP_DEMO_REDACT, a
directory, becomes <project>). ACP_DEMO_PACE paces the turn."
  (let* ((pace (let ((p (uiop:getenv "ACP_DEMO_PACE")))
                 (if (and p (plusp (length p)))
                     (let ((*read-eval* nil)) (coerce (read-from-string p) 'single-float))
                     0)))
         (agent (make-instance 'demo-agent :pace pace))
         (log-path (uiop:getenv "ACP_DEMO_WIRE_LOG")))
    (if (and log-path (plusp (length log-path)))
        (multiple-value-bind (in out) (stdio-streams)
          (let ((log (open-wire-log (uiop:parse-native-namestring log-path)
                                    :project (uiop:getenv "ACP_DEMO_REDACT"))))
            (stdio:serve-acp-agent agent
                                   :input (make-instance 'logging-input :target in :log log)
                                   :output (make-instance 'logging-output :target out :log log))))
        (stdio:serve-acp-agent agent))))

(defun agent-toplevel ()
  "The :TOPLEVEL of the saved agent executable (build-core.sh): serve, then
exit 0. No banner, no REPL: stdout carries ACP messages only."
  (handler-case (serve-demo-agent)
    (sb-sys:interactive-interrupt () nil))
  (uiop:quit 0))
