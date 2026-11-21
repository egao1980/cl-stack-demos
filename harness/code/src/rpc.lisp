(in-package #:harness-demos-code)

;;; cl-stack-code 0.7.0 (H3d): one core for an in-process Lisp SDK and a
;;; JSON-RPC 2.0 server over stdio (--rpc). Demo q: the SDK (MAKE-CLIENT,
;;; sessions, turns whose events are stream-json's objects, an ON-APPROVAL
;;; callback, steering, an interrupt, resume and fork, CLOSE-CLIENT). Demo r:
;;; SERVE-RPC on rpc-backend-stdio 0.2.0's in-memory pipes, every raw line
;;; recorded both ways. Demo s: a child `cl-stack-code --rpc` process (SBCL
;;; running RUN-CLI --rpc on its own stdin / stdout, as cl-stack-code's
;;; rpc-process-test starts it) driven by CONNECT-RPC-CLIENT :COMMAND. The
;;; turns run on the scripted streaming mock (TUI-MOCK, with gates), so every
;;; event is exact; Demo q adds one LM Studio turn when a model is there.

(defparameter *rpc-timeout* 60 "Seconds for a turn or an RPC call.")

(defun %mock-providers (backend)
  (list (cons "mock" (lambda (model env) (declare (ignore model env)) backend))))

(defun %wait-for (predicate what &key (timeout *rpc-timeout*))
  "Poll PREDICATE every 20 ms. → its value, or an error naming WHAT."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop (let ((v (funcall predicate))) (when v (return v)))
          (when (> (get-internal-real-time) deadline)
            (error "timed out after ~d s waiting for ~a" timeout what))
          (sleep 0.02))))

(defun %event-line (event &optional (width 260))
  (clip (json:encode event) width))

(defun %types (events)
  (mapcar (lambda (e) (code:event-type e)) events))

(defun %compact-types (events)
  "Event types in order, a run of text_delta shown once as text_delta×N."
  (let ((out '()) (n 0))
    (flet ((flush () (when (plusp n) (push (format nil "text_delta×~d" n) out) (setf n 0))))
      (dolist (ty (%types events))
        (if (equal ty "text_delta") (incf n) (progn (flush) (push ty out))))
      (flush))
    (nreverse out)))

(defun %of-type (events type)
  (remove type events :key #'code:event-type :test-not #'equal))

(defstruct (bag (:constructor make-bag ()))
  (lock (bt:make-lock "bag"))
  (items '()))

(defun bag-add (bag item)
  (bt:with-lock-held ((bag-lock bag)) (push item (bag-items bag))))

(defun bag-list (bag)
  (bt:with-lock-held ((bag-lock bag)) (reverse (bag-items bag))))

;;; Demo q: the SDK.

(defun %q-client (project config backend events &key on-approval)
  (code:make-client :cwd project
                    :settings (code:make-settings :model "mock")
                    :providers (%mock-providers backend)
                    :user-config-dir config
                    :data-dir (data-dir)
                    :getenv (lambda (name) (declare (ignore name)) nil)
                    :on-event (lambda (session event)
                                (declare (ignore session))
                                (bag-add events event))
                    :on-approval on-approval))

(defun %q-real-model (project config)
  (stage "q8: one turn on the LM Studio model (the same SDK, the built-in openai provider)")
  (unless (real-model-p)
    (note "  skipped: no LM Studio model (the scripted mock ran every step above)")
    (return-from %q-real-model t))
  (code:with-client (client :cwd project
                            :settings (code:make-settings :model (model-designator))
                            :user-config-dir config
                            :data-dir (data-dir)
                            :getenv (model-getenv)
                            ;; an ask is answered at once, so the turn never waits
                            :on-approval (lambda (session request)
                                           (declare (ignore session request))
                                           (list :reject "Do not change any file: answer in one sentence.")))
    (let ((s (code:client-new-session client)))
      (show "init model" (code:event-value (code:code-session-init s) "model"))
      (multiple-value-bind (result events)
          (code:client-query s "Read a.txt and say in one sentence what it contains. Do not write or edit any file."
                             :timeout 420)
        (show "event types" (%compact-types events))
        (dolist (e events)
          (when (member (code:event-type e) '("tool_use" "tool_result") :test #'equal)
            (note "  ~a" (%event-line e 220))))
        (show "result subtype" (code:event-value result "subtype"))
        (show "result text" (clip (princ-to-string (code:event-value result "result")) 400))
        t))))

(defun demo-sdk ()
  (banner "Demo q: the in-process SDK (cl-stack-code 0.7.0, H3d): make-client, sessions, events, approvals, steering, resume, fork")
  (let* ((project (make-project "sdk" `(("a.txt" ,(format nil "alpha~%"))
                                        ("b.txt" ,(format nil "beta~%"))
                                        ("AGENTS.md" "Keep answers short."))))
         (config (scratch-dir "sdk-config"))
         (steer-gate (make-gate))
         (stop-gate (make-gate))
         (backend (make-tui-mock
                   (list
                    ;; q2: a read, then an answer
                    (list :tool "read" (json-args "file_path" "a.txt"))
                    "a.txt holds one line: `alpha`."
                    ;; q3: a write (approved once), an edit (rejected with feedback)
                    (list :tool "write" (json-args "file_path" "c.txt" "content" (format nil "gamma~%")))
                    (list :tool "edit" (%edit-args "a.txt" "alpha" "ALPHA"))
                    "Wrote c.txt; left a.txt as it is."
                    ;; q4: steering taken between two generates
                    (list :gate steer-gate (list :tool "ls" "{}"))
                    "Listed the files and checked b.txt too."
                    ;; q5: interrupted while the generate waits
                    (list :gate stop-gate "This answer is never seen.")
                    ;; q6: the fork's turn
                    "In the fork: only the first turn is behind me.")))
         (events (make-bag))
         (asked 0)
         (client (%q-client project config backend events
                            :on-approval (lambda (session request)
                                           (declare (ignore session))
                                           (if (= 1 (incf asked))
                                               :once
                                               (list :reject (format nil "keep ~a lowercase"
                                                                     (code:event-value request "input" "file_path"))))))))
    (unwind-protect
         (let (s id)
           (stage "q1: make-client, client-new-session: the init event (-p's stream-json system object)")
           (note "  (code:make-client :cwd PROJECT :settings (code:make-settings :model \"mock\") :providers … :on-event … :on-approval …)")
           (setf s (code:client-new-session client)
                 id (code:code-session-id s))
           (let ((init (code:code-session-init s)))
             (show "the first event is the init object" (eq init (first (bag-list events))))
             (note "  ~a" (%event-line init 700))
             (show "on disk before the first turn" (code:client-list-sessions client)))

           (stage "q2: client-query: a read and an answer; the turn's events")
           (multiple-value-bind (result turn-events) (code:client-query s "what is in a.txt?" :timeout *rpc-timeout*)
             (show "event types" (%compact-types turn-events))
             (%events-summary turn-events)
             (show "result subtype / text" (list (code:event-value result "subtype") (code:event-value result "result")))
             (%check "q2" (equal "success" (code:event-value result "subtype"))))

           (stage "q3: the default mode asks for write and edit; on-approval answers :once, then (:reject feedback)")
           (multiple-value-bind (result turn-events) (code:client-query s "add c.txt and capitalise a.txt" :timeout *rpc-timeout*)
             (dolist (e turn-events)
               (when (member (code:event-type e) '("approval_request" "approval_resolved" "tool_result" "result")
                             :test #'equal)
                 (note "  ~a" (%event-line e 320))))
             (show "c.txt" (read-file project "c.txt"))
             (show "a.txt" (read-file project "a.txt"))
             (let ((feedback (search "keep a.txt lowercase"
                                     (%turns-text (llm:coerce-turns (fifth (tui-mock-requests backend)))))))
               (show "the next generate sees the feedback" (and feedback t))
               (%check "q3" (and (equal "success" (code:event-value result "subtype"))
                                 (equal (format nil "alpha~%") (read-file project "a.txt"))
                                 feedback))))

           (stage "q4: client-turn returns once the turn runs; client-steer posts into it")
           (let ((turn (code:client-turn s "list the files")))
             (%wait-for (lambda () (gate-waiting-p steer-gate)) "the generate at the gate")
             (show "client-busy-p" (code:client-busy-p s))
             (show "(client-steer s \"also check b.txt\")" (code:client-steer s "also check b.txt"))
             (open-gate steer-gate)
             (let ((result (code:client-wait turn :timeout *rpc-timeout*)))
               (show "event types" (%compact-types (code:code-turn-events turn)))
               (dolist (e (%of-type (code:code-turn-events turn) "steering"))
                 (note "  ~a" (%event-line e)))
               (show "the generate after ls has the steering text"
                     (and (search "also check b.txt" (%turns-text (llm:coerce-turns (seventh (tui-mock-requests backend))))) t))
               (show "result text" (code:event-value result "result"))
               (%check "q4" (%of-type (code:code-turn-events turn) "steering"))))

           (stage "q5: client-interrupt while the generate waits; a steer that is never taken is dropped")
           (let ((turn (code:client-turn s "say something slow")))
             (%wait-for (lambda () (gate-waiting-p stop-gate)) "the generate at the gate")
             (code:client-steer s "and this one is too late")
             (show "(client-interrupt s)" (code:client-interrupt s))
             (open-gate stop-gate)
             (let ((result (code:client-wait turn :timeout *rpc-timeout*)))
               (show "last two events" (last (%types (code:code-turn-events turn)) 2))
               (dolist (e (%of-type (code:code-turn-events turn) "steering"))
                 (note "  ~a" (%event-line e)))
               (show "result subtype / error" (list (code:event-value result "subtype") (code:event-value result "error")))
               (show "(client-interrupt s) when idle" (code:client-interrupt s))
               (%check "q5" (search "interrupted" (princ-to-string (code:event-value result "error"))))))

           (stage "q6: client-list-sessions, close, resume by a 6-character prefix, fork at turn 1")
           (dolist (o (code:client-list-sessions client)) (note "  ~a" (%event-line o)))
           (show "(client-close-session s)" (code:client-close-session s))
           (show "a call on the closed session signals"
                 (handler-case (progn (code:client-steer s "x") :no-error)
                   (error (c) (type-of c))))
           (let* ((r (code:client-resume-session client (subseq id 0 6)))
                  (init (code:code-session-init r)))
             (show "resumed id = the id" (equal id (code:code-session-id r)))
             (show "resume init: turns" (code:event-value init "turns"))
             (let* ((f (code:client-fork-session r :at 1))
                    (finit (code:code-session-init f)))
               (show "fork init: turns, forked_from = the id"
                     (list (code:event-value finit "turns") (equal id (code:event-value finit "forked_from"))))
               (let ((result (code:client-query f "where are we?" :timeout *rpc-timeout*)))
                 (show "fork result" (code:event-value result "result"))
                 (let ((users (remove-if-not (lambda (turn) (eq :user (llm:llm-turn-role turn)))
                                             (llm:coerce-turns (car (last (tui-mock-requests backend)))))))
                   (show "the fork's request: user turns"
                         (mapcar (lambda (u) (clip (or (llm:turn-text u) "(tool results)") 60)) users))))
               (show "client-sessions" (length (code:client-sessions client)))
               (show "sessions on disk (id prefix, turns, prompt)"
                     (mapcar (lambda (o) (list (subseq (code:event-value o "session_id") 0 8)
                                               (code:event-value o "turns")
                                               (code:event-value o "prompt")))
                             (code:client-list-sessions client)))))
           (stage "q7: close-client")
           (code:close-client client)
           (show "client-closed-p" (code:client-closed-p client))
           (show "client-new-session afterwards signals"
                 (handler-case (progn (code:client-new-session client) :no-error)
                   (error (c) (type-of c))))
           (show "events seen by the client's on-event" (length (bag-list events)))
           (%q-real-model project (scratch-dir "sdk-real-config")))
      (code:close-client client))))

;;; Demo r: --rpc over in-memory pipes.

(defun %rpc-call (rpc method &rest kv)
  (let ((params (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k params) v))
    (code:rpc-client-call rpc method params :timeout *rpc-timeout*)))

(defun %rpc-fault (rpc method &rest kv)
  (handler-case (progn (apply #'%rpc-call rpc method kv) nil)
    (rpc:rpc-error (e)
      (list (rpc:rpc-error-code e) (rpc:rpc-error-message e)
            (let ((d (rpc:rpc-error-data e))) (and (hash-table-p d) (json:encode d)))))))

(defun %wire-lines (stream)
  (lines (get-output-stream-string stream)))

(defun %print-wire (label lines &key (width 220))
  "LINES, a text_delta run collapsed to its first line and a count."
  (note "  ~a (~d lines):" label (length lines))
  (let ((skipped 0))
    (flet ((flush () (when (plusp skipped)
                       (note "      … ~d more text_delta notifications" skipped)
                       (setf skipped 0))))
      (loop for line in lines
            for prev = nil then this
            for this = (search "\"text_delta\"" line)
            do (if (and this prev)
                   (incf skipped)
                   (progn (flush) (note "    ~a" (clip line width)))))
      (flush))))

(defun demo-rpc-wire ()
  (banner "Demo r: serve-rpc on rpc-backend-stdio 0.2.0 in-memory pipes; the raw JSON-RPC lines both ways")
  (let* ((project (make-project "rpc-wire" `(("a.txt" ,(format nil "alpha~%")))))
         (config (scratch-dir "rpc-wire-config"))
         (backend (make-tui-mock
                   (list (list :tool "write" (json-args "file_path" "b.txt" "content" (format nil "beta~%")))
                         "Wrote b.txt.")))
         (to-server (make-string-output-stream))
         (to-client (make-string-output-stream))
         (cell (list :running))
         ;; computed here: the server thread does not see this thread's *SCRATCH*
         (data (data-dir)))
    (multiple-value-bind (server-in client-out) (stdio:make-stdio-pipe)
      (multiple-value-bind (client-in server-out) (stdio:make-stdio-pipe)
        (let* ((server-tx (stdio:make-stdio-rpc-transport
                           :input server-in :output (make-broadcast-stream server-out to-client)))
               (client-tx (stdio:make-stdio-rpc-transport
                           :input client-in :output (make-broadcast-stream client-out to-server)))
               (thread (bt:make-thread
                        (lambda ()
                          (setf (car cell)
                                (handler-case
                                    (code:serve-rpc server-tx :cwd project
                                                              :settings (code:make-settings :model "mock")
                                                              :providers (%mock-providers backend)
                                                              :user-config-dir config
                                                              :data-dir data
                                                              :getenv (lambda (n) (declare (ignore n)) nil))
                                  (error (e) (list :error (princ-to-string e))))))
                        :name "rpc server"))
               (rpc (code:connect-rpc-client :transport client-tx)))
          (note "  each side's output is a broadcast stream: the pipe and a string stream that records the lines")
          (unwind-protect
               (let (id)
                 (sleep 0.5)
                 (unless (bt:thread-alive-p thread)
                   (error "the server thread ended at once: ~a" (car cell)))
                 (stage "r1: initialize, session/new, an error response")
                 (let ((init (%rpc-call rpc "initialize")))
                   (show "initialize → name, version, protocol"
                         (list (code:event-value init "name") (code:event-value init "version")
                               (code:event-value init "protocol")))
                   (show "methods" (coerce (code:event-value init "methods") 'list)))
                 (setf id (code:event-value (%rpc-call rpc "session/new") "session_id"))
                 (show "turn/steer while idle → code, message, data" (%rpc-fault rpc "turn/steer" "session_id" id "text" "hi"))
                 (show "no/such → code, message" (subseq (%rpc-fault rpc "no/such") 0 2))
                 (stage "r2: turn/start; the write asks; approval/respond once")
                 (show "turn/start" (json:encode (%rpc-call rpc "turn/start" "session_id" id "prompt" "write b.txt")))
                 (let ((req (%wait-for (lambda () (first (code:rpc-client-events rpc :type "approval_request" :session-id id)))
                                       "approval_request")))
                   (show "approval_request id, name, summary"
                         (list (code:event-value req "id") (code:event-value req "name") (code:event-value req "summary")))
                   (show "approval/respond"
                         (json:encode (%rpc-call rpc "approval/respond" "session_id" id
                                                 "id" (code:event-value req "id") "decision" "once"))))
                 (let ((result (%wait-for (lambda () (first (code:rpc-client-events rpc :type "result" :session-id id)))
                                          "the result notification")))
                   (show "result subtype / text" (list (code:event-value result "subtype") (code:event-value result "result")))
                   (show "b.txt" (read-file project "b.txt"))
                   (show "notification types" (%compact-types (code:rpc-client-events rpc :session-id id)))
                   (show "tools/list count" (length (code:event-value (%rpc-call rpc "tools/list" "session_id" id) "tools")))
                   (show "checkpoint/list (ordinal, reason, files_kind)"
                         (map 'list (lambda (c) (list (code:event-value c "ordinal") (code:event-value c "reason")
                                                      (code:event-value c "files_kind")))
                              (code:event-value (%rpc-call rpc "checkpoint/list" "session_id" id) "checkpoints")))
                   (show "session/close" (json:encode (%rpc-call rpc "session/close" "session_id" id)))
                   (%check "r2" (equal "success" (code:event-value result "subtype")))))
            (code:close-rpc-client rpc)
            ;; The client's output is a broadcast stream, which CLOSE does not
            ;; pass on: close the pipe itself, so the server reads EOF.
            (close client-out)
            (%wait-for (lambda () (not (bt:thread-alive-p thread))) "the server thread to end" :timeout 30))
          (stage "r3: the wire")
          (show "serve-rpc returned (EOF)" (car cell))
          (%print-wire "client → server" (%wire-lines to-server) :width 260)
          (%print-wire "server → client" (%wire-lines to-client))
          (%check "r3" (eql 0 (car cell))))))))

;;; Demo s: a child cl-stack-code --rpc process.

(defun rpc-child-main (project config steps-text)
  "The child: RUN-CLI --rpc on this process's stdin / stdout, the mock
scripted with the steps READ from STEPS-TEXT. Quits with RUN-CLI's code."
  (let* ((steps (let ((*read-eval* nil)) (read-from-string steps-text)))
         (backend (make-tui-mock steps)))
    (uiop:quit
     (code:run-cli (list "--rpc" "--cwd" project "--model" "mock")
                   :input *standard-input* :output *standard-output* :error-output *error-output*
                   :providers (%mock-providers backend)
                   :user-config-dir (uiop:ensure-directory-pathname config)
                   :data-dir (merge-pathnames "data/" (uiop:ensure-directory-pathname config))
                   :getenv (lambda (n) (declare (ignore n)) nil)))))

(defun %asdf-load-form ()
  #+sbcl
  (let* ((home-fn (find-symbol "SBCL-HOMEDIR-PATHNAME" "SB-INT"))
         (home (and home-fn (ignore-errors (funcall home-fn))))
         (fasl (and home (probe-file (merge-pathnames "contrib/asdf.fasl" home)))))
    (if fasl (format nil "(load ~s)" (uiop:native-namestring fasl)) "(require :asdf)"))
  #-sbcl "(require :asdf)")

(defun %registry-form ()
  "This image's loaded systems' directories, so the child finds the same ones."
  (let ((dirs '()))
    (dolist (name (asdf:already-loaded-systems))
      (let ((dir (ignore-errors (asdf:system-source-directory name))))
        (when dir (pushnew (uiop:native-namestring dir) dirs :test #'string=))))
    `(:source-registry ,@(mapcar (lambda (d) (list :directory d)) dirs)
                       :ignore-inherited-configuration)))

(defun rpc-child-argv (project config steps)
  #-sbcl (declare (ignore project config steps))
  #-sbcl (error "the --rpc child is started with SBCL only")
  #+sbcl
  (append
   (list (uiop:native-namestring sb-ext:*runtime-pathname*)
         "--core" (uiop:native-namestring sb-ext:*core-pathname*) "--noinform"
         "--non-interactive" "--no-sysinit" "--no-userinit")
   (loop for form in (list (%asdf-load-form)
                           (format nil "(uiop:symbol-call :asdf :initialize-source-registry '~s)"
                                   (%registry-form))
                           ;; stdout is the protocol: compiler output must not reach it
                           "(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (uiop:symbol-call :asdf :load-system \"harness-demos-code\"))"
                           (format nil "(uiop:symbol-call :harness-demos-code :rpc-child-main ~s ~s ~s)"
                                   (%native project) (%native config)
                                   (let ((*print-readably* t)) (prin1-to-string steps))))
         append (list "--eval" form))))

(defun demo-rpc-process ()
  (banner "Demo s: a child `cl-stack-code --rpc` process driven by connect-rpc-client :command")
  (let* ((project (make-project "rpc-process" `(("a.txt" ,(format nil "alpha~%")))))
         (config (scratch-dir "rpc-process-config"))
         (argv (rpc-child-argv project config
                               (list (list :tool "read" (json-args "file_path" "a.txt"))
                                     "It says alpha.")))
         (seen (make-bag))
         (rpc (code:connect-rpc-client :command argv
                                       :on-event (lambda (e) (bag-add seen (code:event-type e))))))
    (note "$ sbcl --non-interactive --no-userinit --eval <load harness-demos-code> --eval '(run-cli (list \"--rpc\" \"--cwd\" PROJECT \"--model\" \"mock\") …)'")
    (note "  (argv: ~d strings; the child runs run-cli --rpc on its own stdin and stdout)" (length argv))
    (unwind-protect
         (let* ((started (get-internal-real-time))
                (init (code:rpc-client-call rpc "initialize" nil :timeout 600)))
           (show "initialize → name, version (the child may compile first)"
                 (list (code:event-value init "name") (code:event-value init "version")))
           (note "  (initialize answered after ~,1f s)"
                 (/ (- (get-internal-real-time) started) internal-time-units-per-second))
           (show "a child process" (and (code:rpc-client-process rpc) t))
           (let* ((id (code:event-value (%rpc-call rpc "session/new") "session_id"))
                  (started (%rpc-call rpc "turn/start" "session_id" id "prompt" "read a.txt"))
                  (result (code:rpc-client-wait-event rpc (lambda (e) (and (equal "result" (code:event-type e))
                                                                          (equal id (code:event-value e "session_id"))))
                                                      :timeout *rpc-timeout*)))
             (show "turn/start → session_id is the session" (equal id (code:event-value started "session_id")))
             (show "events (on-event, reader thread)" (%compact-types (code:rpc-client-events rpc :session-id id)))
             (dolist (e (code:rpc-client-events rpc :session-id id))
               (when (member (code:event-type e) '("tool_use" "tool_result" "result") :test #'equal)
                 (note "  ~a" (%event-line e 300))))
             (show "on-event saw" (length (bag-list seen)))
             (let ((exit (code:close-rpc-client rpc)))
               (show "close-rpc-client: stdin closed, the child's exit code" exit)
               (show "close-rpc-client again (idempotent)" (code:close-rpc-client rpc))
               (show "the child committed the turn: sessions on disk"
                     (length (code:list-sessions (code:sessions-directory project
                                                                          :data-dir (merge-pathnames "data/" config)))))
               (%check "s" (and result (equal "success" (code:event-value result "subtype"))
                                (eql 0 exit))))))
      (code:close-rpc-client rpc))))
