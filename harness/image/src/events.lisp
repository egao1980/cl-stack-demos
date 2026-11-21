(in-package #:harness-demos-image)

;;; lisp-mcp 0.7.0 (H4b): image events, the debugger → agent inbox bridge.
;;; Demo 6: a background thread's debugger reaches a running agent's inbox;
;;; the agent (scripted mock) inspects it and restarts it under a
;;; restart:<name> rule. Demo 7: the same with no rule: an unattended run
;;; denies the restart and the thread keeps waiting. Demo 8: detached jobs
;;; as :job events. Demo 9: observe mode. Demo 10: LM Studio answers a
;;; background debugger from a seeded bug in a loaded system.

(defparameter +bg-error+
  "(progn (sb-thread:make-thread (lambda () (restart-case (error \"bg boom: the worker lost its queue\") (retry-later () :report \"Retry later\" 7))) :name \"bg-worker\") (sleep 1) :started)"
  "Starts a thread that enters the debugger at once; the eval returns
:STARTED a second later (the event arrives while the call runs).")

(defun wait-until (predicate &key (timeout 20))
  "Poll PREDICATE every 50 ms until it is true (→ its value) or TIMEOUT s pass (→ NIL)."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (let ((value (funcall predicate)))
        (when value (return value)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep 0.05))))

(defun make-recorder (&optional run-box)
  "→ (values callback seen). CALLBACK is an :ON-IMAGE-EVENT that records
each event and, when RUN-BOX holds a run, posts the queued events into it
(POST-IMAGE-EVENTS; the bridge's callback runs on the wire's reader thread).
SEEN → the events, oldest first."
  (let ((lock (sb-thread:make-mutex :name "recorder")) (events '()))
    (values (lambda (tools event)
              (sb-thread:with-mutex (lock) (push event events))
              (let ((run (and run-box (car run-box))))
                (when (and run (null (agent:agent-run-finish-reason run)))
                  (lt:post-image-events tools run))))
            (lambda () (sb-thread:with-mutex (lock) (reverse events))))))

(defun %kinds (events) (mapcar #'lt:image-event-kind events))

(defun show-event (label event)
  (show label event)
  (show "  kind" (lt:image-event-kind event))
  (show "  connection" (lt:image-event-connection event))
  (when (lt:image-event-thread event)
    (show "  thread" (lt:image-event-thread event))
    (show "  level" (lt:image-event-level event)))
  (when (lt:image-event-condition event)
    (show "  condition" (lt:image-event-condition event))
    (show "  restarts" (lt:image-event-restarts event))
    (show "  frames" (length (lt:image-event-frames event))))
  (when (lt:image-event-job event)
    (show "  job" (lt:image-event-job event))
    (show "  job-state" (lt:image-event-job-state event)))
  (unless (string= "" (lt:image-event-text event))
    (show "  text" (lt:image-event-text event))))

(defun capture-run (run-box)
  "An :ON-EVENT handler keeping the run of the :STARTED event in RUN-BOX."
  (lambda (kind payload)
    (when (eq kind :started) (setf (car run-box) payload))))

(defun %request-image-turn (request)
  (find-if #'lt:image-event-turn-p request))

(defun %print-requests (log)
  "One line per generate: its turn count and whether an event turn is in it."
  (loop for request in (car log)
        for k from 1
        do (let ((at (position-if #'lt:image-event-turn-p request)))
             (note "  generate ~d: ~d turns~@[, image event turn at ~d~]" k (length request) at))))

(defun %headless-deny (asked tools)
  "An AGENT-APPROVAL-REQUIRED handler that denies as an unattended (-p) run
does, recording (name resources) in the car of ASKED."
  (lambda (c)
    (let ((inv (agent:agent-approval-required-invocation c)))
      (push (list (agent:agent-invocation-name inv)
                  (agent:tool-permission-resources tools (agent:agent-invocation-name inv)
                                                   (agent:agent-invocation-arguments inv)))
            (car asked))
      (agent:invoke-deny c "headless: no rule allows this and nobody can approve it"))))

;;; Demo 6.

(defun demo-image-events ()
  (banner "Demo 6: a background debugger reaches the agent's inbox; the agent restarts it under a restart: rule")
  (let* ((root (make-project "events" '()))
         (run-box (list nil))
         (log (list nil))
         (asked (list nil))
         (ok t))
    (multiple-value-bind (callback seen) (make-recorder run-box)
      (let ((tools (lt:make-lisp-tools :root root :on-image-event callback)))
        (unwind-protect
             (let* ((rules (list (agent:make-permission-rule "repl-spawn" "image:spawn" :allow)
                                 (agent:make-permission-rule "repl-eval" "*" :allow)
                                 (agent:make-permission-rule "debug-state" "*" :allow)
                                 (agent:make-permission-rule "debug-backtrace" "*" :allow)
                                 (agent:make-permission-rule "inspect-condition" "*" :allow)
                                 (agent:make-permission-rule "debug-invoke-restart" "restart:retry-later" :allow)
                                 (agent:make-permission-rule "debug-invoke-restart" "restart:abort" :deny
                                                             :feedback "keep the worker: choose retry-later")))
                    (steps (list (cons "repl-spawn" (json-args "backend" "swank"))
                                 (cons "repl-eval" (json-args "code" +bg-error+))
                                 (cons "debug-state" (json-args "connection" "swank-1"))
                                 (cons "debug-backtrace" (json-args "end" 4))
                                 (cons "inspect-condition" (json-args))
                                 (cons "debug-invoke-restart" (json-args "restart" 1))
                                 (cons "debug-invoke-restart" (json-args "restart" 0))
                                 (cons "debug-state" (json-args))
                                 "The worker hit \"bg boom\"; abort was refused, so I chose retry-later and the thread left the debugger.")))
               (show "image-events-p" (lt:lisp-tools-image-events-p tools))
               (show "instructions, IMAGE EVENTS line"
                     (find-if (lambda (l) (uiop:string-prefix-p "IMAGE EVENTS:" l))
                              (uiop:split-string (lt:lisp-tools-instructions tools) :separator '(#\Newline))))
               (show "rules" (mapcar #'princ-to-string rules))
               (stage "the run (scripted mock); the callback posts each event into it")
               (let ((run (with-agent-loop
                            (handler-bind ((agent:agent-approval-required (%headless-deny asked tools)))
                              (agent:run-ai-agent
                               (agent:make-ai-agent :name "watcher" :backend (scripted-backend steps log)
                                                    :tools (list tools) :permissions rules)
                               "start the worker in a fresh image and look after it"
                               :on-event (capture-run run-box))))))
                 (%print-run run :width 700)
                 (stage "what each generate saw")
                 (%print-requests log)
                 (let ((turn (%request-image-turn (third (car log)))))
                   (setf ok (and turn (null (%request-image-turn (second (car log)))) ok))
                   (when turn (show "the posted turn (user role, generate 3)" (llm:turn-text turn))))
                 (show "event turns in the run" (count-if #'lt:image-event-turn-p (agent:agent-run-turns run)))
                 (show "approval asks the demo denied" (reverse (car asked)))
                 (let ((results (%tool-results run)))
                   (setf ok (and (search "Invoked restart 0 [RETRY-LATER] in thread"
                                         (princ-to-string (llm:llm-tool-result-part-content (nth 6 results))))
                                 (llm:llm-tool-result-part-error-p (nth 5 results))
                                 ok))))
               (stage "the events the callback saw")
               (wait-until (lambda () (member :debugger-return (%kinds (funcall seen)))) :timeout 5)
               (let ((events (funcall seen)))
                 (show "kinds" (%kinds events))
                 (show-event "the :debugger event" (first events))
                 (let ((back (find :debugger-return events :key #'lt:image-event-kind)))
                   (when back (show "the :debugger-return event" back))
                   (setf ok (and back ok))))
               (show "pending after the run" (lt:lisp-tools-pending-events tools)))
          (lt:close-lisp-tools tools))))
    ok))

;;; Demo 7.

(defun demo-image-events-denied ()
  (banner "Demo 7: no rule allows the restart: an unattended run denies it, the thread stays in the debugger")
  (let* ((root (make-project "events-denied" '()))
         (run-box (list nil))
         (log (list nil))
         (asked (list nil))
         (ok nil))
    (multiple-value-bind (callback seen) (make-recorder run-box)
      (declare (ignore seen))
      (let ((tools (lt:make-lisp-tools :root root :on-image-event callback)))
        (unwind-protect
             (let ((rules (list (agent:make-permission-rule "repl-*" "*" :allow)
                                (agent:make-permission-rule "debug-state" "*" :allow)))
                   (steps (list (cons "repl-spawn" (json-args))
                                (cons "repl-eval" (json-args "code" +bg-error+))
                                (cons "debug-invoke-restart" (json-args "restart" 0))
                                "I could not restart the worker: nobody approved it.")))
               (show "rules" (mapcar #'princ-to-string rules))
               (show "approval asked for restart 0 (an +approval-tools+ entry)"
                     (agent:tool-needs-approval-p tools "debug-invoke-restart" (json-args "restart" 0)))
               (let ((run (with-agent-loop
                            (handler-bind ((agent:agent-approval-required (%headless-deny asked tools)))
                              (agent:run-ai-agent
                               (agent:make-ai-agent :name "unattended" :backend (scripted-backend steps log)
                                                    :tools (list tools) :permissions rules)
                               "start the worker"
                               :on-event (capture-run run-box))))))
                 (%print-run run :width 400)
                 (%print-requests log)
                 (show "approval asks the demo denied (as -p does)" (reverse (car asked)))
                 (stage "after the run")
                 (let ((state (tool tools "debug-state")))
                   (setf ok (and (search "DEBUGGER level 1" state)
                                 (llm:llm-tool-result-part-error-p (third (%tool-results run)))
                                 t)))
                 (note "  (the demo itself restarts the thread, outside any agent run)")
                 (tool tools "debug-invoke-restart" "restart" 0)))
          (lt:close-lisp-tools tools))))
    ok))

;;; Demo 8.

(defun demo-image-job-events ()
  (banner "Demo 8: detached jobs: one :job event when each finishes, none for a call that finishes in time")
  (let* ((root (make-project "events-jobs" '()))
         (ok t))
    (multiple-value-bind (callback seen) (make-recorder)
      (let ((tools (lt:make-lisp-tools :root root :on-image-event callback)))
        (unwind-protect
             (progn
               (tool tools "repl-spawn")
               (tool tools "repl-eval" "code" +seeded+)
               (stage "a job that finishes after its call returned")
               (let ((r (tool tools "repl-eval" "code" "(progn (sleep 2) :done)" "timeout" 1)))
                 (setf ok (and (search "as job " r) ok)))
               (setf ok (and (wait-until (lambda () (lt:lisp-tools-pending-events tools))) ok))
               (sleep 0.5)
               (let ((events (lt:take-image-events tools)))
                 (show "events queued" (length events))
                 (dolist (e events) (show-event "the :job event" e))
                 (setf ok (and (= 1 (length events)) (eq :job (lt:image-event-kind (first events)))
                               (eq :done (lt:image-event-job-state (first events))) ok)))
               (stage "a job that ends in the debugger: reported once, by the job (no :debugger event)")
               (tool tools "repl-eval" "code" "(progn (sleep 2) (seeded 1))" "timeout" 1)
               (wait-until (lambda () (lt:lisp-tools-pending-events tools)))
               (sleep 0.5)
               (let ((events (lt:take-image-events tools)))
                 (show "events queued" (length events))
                 (dolist (e events) (show-event "the :job event" e))
                 (setf ok (and (= 1 (length events)) (eq :job (lt:image-event-kind (first events))) ok)))
               (setf ok (and (search "=> 42" (tool tools "debug-invoke-restart" "restart" 0)) ok))
               (stage "a call that finishes within its wait")
               (tool tools "repl-eval" "code" "(progn (sleep 0.2) :quick)" "timeout" 5)
               (sleep 0.5)
               (show "events queued" (length (lt:lisp-tools-pending-events tools)))
               (show "kinds the callback saw" (%kinds (funcall seen)))
               (setf ok (and (null (lt:lisp-tools-pending-events tools))
                             (not (member :debugger (%kinds (funcall seen))))
                             ok)))
          (lt:close-lisp-tools tools))))
    ok))

;;; Demo 9.

(defparameter +observed-error+
  "(progn (sb-thread:make-thread (lambda () (sleep 1) (restart-case (error \"prod boom: payment gateway timed out\") (retry-later () :report \"Retry later\" 7))) :name \"prod-worker\") :started)")

(defun demo-image-events-observe ()
  (banner "Demo 9: observe mode: the event arrives, with the observe footer; a restart is refused")
  (let* ((root (make-project "events-observe" '()))
         (server (lt:make-lisp-tools :root root :image-events nil))
         (watch nil)
         (ok t))
    (multiple-value-bind (callback seen) (make-recorder)
      (unwind-protect
           (let* ((r (progn (tool server "repl-spawn")
                            (lt:call-lisp-tool server "repl-eval"
                                               (json-args "code" "(swank:create-server :port 0 :dont-close t)"))))
                  (port (parse-integer r :start (+ 3 (search "=> " r)) :junk-allowed t)))
             (setf watch (lt:make-lisp-tools :root root :mode :observe :on-image-event callback))
             (note "  an observe source attaches to the image's second Swank server (port not printed)")
             (lt:call-lisp-tool watch "repl-connect" (json-args "port" port "name" "prod"))
             (show "debug-invoke-restart offered to the observer"
                   (and (member "debug-invoke-restart" (lt:lisp-tools-names watch) :test #'string=) t))
             (stage "the image's own code starts a worker that fails a second later")
             (note "  (the image's owner runs it, through the other connection; the observer may not)")
             ;; The background thread's debugger goes to the newest Swank
             ;; connection, which is the observer's.
             (tool server "repl-eval" "code" +observed-error+)
             (setf ok (and (wait-until (lambda () (lt:lisp-tools-pending-events watch))) ok))
             (let ((event (first (lt:lisp-tools-pending-events watch))))
               (when event
                 (show-event "the observer's :debugger event" event)
                 (setf ok (and (search "observe mode: it cannot choose a restart" (lt:image-event-text event))
                               ok))))
             (stage "the observer tries the restart")
             (let ((c (handler-case (progn (lt:call-lisp-tool watch "debug-invoke-restart" (json-args "restart" 0)) nil)
                        (error (c) c))))
               (show "condition" (type-of c))
               (when (typep c 'agent:tool-permission-denied)
                 (show "feedback" (agent:tool-permission-denied-feedback c)))
               (setf ok (and (typep c 'lt:observe-mode-refused) ok)))
             (show "debug-state (a read: allowed)" (%elide (lt:call-lisp-tool watch "debug-state" "{}")))
             (show "kinds the observer's callback saw" (%kinds (funcall seen)))
             (stage "the owner's connection, before and after the observer closes")
             (note "  Swank gives a thread no request runs in to the newest connection: the")
             (note "  observer's. The owner's connection never sees that debugger (egao1980/lisp-mcp#22).")
             (tool server "debug-state")
             (lt:close-lisp-tools watch)
             (setf watch nil)
             (sleep 1)
             (tool server "debug-state")
             (tool server "repl-eval" "code"
                   "(let ((th (find \"prod-worker\" (sb-thread:list-all-threads) :key #'sb-thread:thread-name :test #'equal))) (list :prod-worker-alive (and th (sb-thread:thread-alive-p th) t)))"))
        (when watch (lt:close-lisp-tools watch))
        (lt:close-lisp-tools server)))
    ok))

;;; Demo 10: a seeded bug in a loaded system, met by a background worker.

(defparameter +ledger-asd+
  (format nil "(defsystem \"shop-ledger\"~%  :components ((:file \"ledger\")))~%"))

(defparameter +ledger-source+
  ";;; A ledger worker: it sums order amounts (decimal strings) in cents.

(defpackage #:shop-ledger
  (:use #:cl)
  (:export #:parse-amount #:decimal-cents #:start-worker #:*total* #:*done*))

(in-package #:shop-ledger)

(defparameter *orders* '(\"12\" \"7\" \"3.50\" \"20.25\"))

(defvar *total* 0 \"The sum of the processed orders, in cents.\")
(defvar *done* nil \"True once the worker processed every order.\")

(defun decimal-cents (text)
  \"TEXT, \\\"D\\\" or \\\"D.CC\\\" dollars, in cents.\"
  (let ((dot (position #\\. text)))
    (if dot
        (+ (* 100 (parse-integer text :end dot))
           (parse-integer (subseq (concatenate 'string (subseq text (1+ dot)) \"00\") 0 2)))
        (* 100 (parse-integer text)))))

(defun parse-amount (text)
  \"TEXT, an order amount such as \\\"3.50\\\", in cents.\"
  (* 100 (parse-integer text)))

(defun process-order (text)
  (restart-case (incf *total* (parse-amount text))
    (use-decimal ()
      :report \"Add this amount read with decimal-cents instead\"
      (incf *total* (decimal-cents text)))
    (retry-order ()
      :report \"Parse this order again (after fixing parse-amount)\"
      (process-order text))
    (skip-order ()
      :report \"Skip this order (its amount is lost)\"
      nil)))

(defun start-worker ()
  \"Process *ORDERS* in a background thread.\"
  (setf *total* 0 *done* nil)
  (sb-thread:make-thread (lambda () (mapc #'process-order *orders*) (setf *done* t))
                         :name \"ledger-worker\")
  :started)
")

(defparameter +ledger-check+ "(list shop-ledger:*done* shop-ledger:*total*)")

(defparameter +ledger-fixed+
  "(defun parse-amount (text)
  \"TEXT, an order amount such as \\\"3.50\\\", in cents.\"
  (decimal-cents text))")

(defparameter +ledger-restart-instructions+
  "You look after a live Common Lisp image with the tools. A message tagged <lisp-image-event> means a thread of the image waits in the debugger until you choose one of its restarts with debug-invoke-restart (its index). The system \"shop-ledger\" is loaded; its worker adds up order amounts. Every amount must count: never skip an order and never abort the worker. Read the restarts in the event, choose the one that still adds the amount, then say in one sentence which restart you chose and why.")

(defparameter +ledger-instructions+
  "You look after a live Common Lisp image with the tools. A message tagged <lisp-image-event> means a thread of the image waits in the debugger until you choose one of its restarts with debug-invoke-restart. The system \"shop-ledger\" (file ledger.lisp, package shop-ledger) is loaded; its worker adds up order amounts. Every amount must count: never skip an order or abort the worker. Either choose the restart that still adds the amount, or fix the function in ledger.lisp with edit-replace (locator \"defun NAME\", compile true) and retry the order. Be brief.")

(defun %ledger-mock-steps ()
  "What a model that fixes the code calls, scripted."
  (list (cons "debug-invoke-restart" (json-args "restart" 1))
        (cons "debug-state" (json-args))
        (cons "source-read" (json-args "path" "ledger.lisp" "locator" "defun parse-amount"))
        (cons "edit-replace" (json-args "path" "ledger.lisp" "locator" "defun parse-amount"
                                        "code" +ledger-fixed+ "compile" t))
        (cons "debug-invoke-restart" (json-args "restart" 1))
        "A retry without a fix failed again (a second event). parse-amount ignored the cents; it now calls decimal-cents, and retry-order added the order."))

(defun %ledger-run (label slug backend model &key (edit t))
  "Spawn an image, load shop-ledger, start its worker; the :debugger event is the
agent's first message, and later events are posted into the run. EDIT: the
agent also gets source-read and edit-replace (else it can only choose a
restart). → true when the worker then finished with the right total."
  (let* ((run-box (list nil))
         (recorder (multiple-value-list (make-recorder run-box)))
         (callback (first recorder))
         (seen (second recorder))
         (root (make-project (format nil "ledger-~a" slug)
                             `(("shop-ledger.asd" ,+ledger-asd+) ("ledger.lisp" ,+ledger-source+))))
         (tools (lt:make-lisp-tools :root root :package "shop-ledger"
                                    :on-image-event callback
                                    :tools (append '("repl-spawn" "repl-eval" "repl-load-system" "debug-state"
                                                     "debug-backtrace" "inspect-condition" "debug-invoke-restart")
                                                   (and edit '("source-read" "edit-replace")))))
         (rules (list (agent:make-permission-rule "repl-spawn" "*" :deny :feedback "the image is already running")
                      (agent:make-permission-rule "*" "path:ledger.lisp" :allow)
                      (agent:make-permission-rule "debug-*" "*" :allow)
                      (agent:make-permission-rule "inspect-condition" "*" :allow)
                      (agent:make-permission-rule "*" "lisp:pure" :allow)
                      (agent:make-permission-rule "*" "lisp:reads *" :allow)
                      (agent:make-permission-rule "*" "lisp:calls shop-ledger:*" :allow)
                      (agent:make-permission-rule "*" "lisp:defines shop-ledger:*" :allow)
                      (agent:make-permission-rule "*" "lisp:package shop-ledger" :allow)
                      (agent:make-permission-rule "*" "lisp:mutates cl:*package*" :allow)
                      (agent:make-permission-rule "debug-invoke-restart" "restart:retry-order" :allow)
                      (agent:make-permission-rule "debug-invoke-restart" "restart:use-decimal" :allow)
                      (agent:make-permission-rule "debug-invoke-restart" "restart:skip-order" :allow)
                      (agent:make-permission-rule "debug-invoke-restart" "restart:abort" :deny
                                                  :feedback "aborting kills the worker: every amount must count")))
         (asked (list nil))
         (ok nil))
    (unwind-protect
         (progn
           (stage (format nil "~a: setup (not the agent): spawn, load shop-ledger, start the worker" label))
           (tool tools "repl-spawn" "backend" "swank")
           (tool tools "repl-eval" "code" (format nil "(asdf:load-asd (uiop:parse-native-namestring ~s))"
                                                  (%native (merge-pathnames "shop-ledger.asd" root))))
           (tool tools "repl-load-system" "system" "shop-ledger")
           (tool tools "repl-eval" "code" "(shop-ledger:start-worker)")
           (unless (wait-until (lambda () (lt:lisp-tools-pending-events tools)))
             (error "no image event arrived"))
           (let* ((event (first (lt:take-image-events tools)))
                  (agent (agent:make-ai-agent
                          :name "ledger-keeper" :backend backend :model model
                          :instructions (format nil "~a~%~%~a" (if edit +ledger-instructions+ +ledger-restart-instructions+)
                                                (lt:lisp-tools-instructions tools))
                          :tools (list tools) :permissions rules
                          :settings (agent:make-agent-settings :max-steps 30))))
             (show-event "the event (taken by the demo; it becomes the run's first message)" event)
             (show "the agent's tools" (lt:lisp-tools-names tools))
             (show "rules" (mapcar #'princ-to-string rules))
             (stage (format nil "~a: the run, its message the event's turn" label))
             (let* ((hp:*http-backend* (http-backend-async:make-async-backend))
                    (agent:*off-loop-specials* (union '(hp:*http-backend* http-backend-async:*event-backend-maker*)
                                                      agent:*off-loop-specials*))
                    (run (with-agent-loop
                           (handler-bind ((agent:agent-approval-required (%headless-deny asked tools)))
                             (let ((run (agent:run-ai-agent agent (list (lt:image-event-turn event))
                                                            :on-event (capture-run run-box))))
                               ;; As cl-stack-code's idle "turn" policy does: when the run
                               ;; ended with a debugger still waiting, a follow-up turn
                               ;; (a queued event's turn, else a reminder), at most twice.
                               (loop repeat 2
                                     while (search "DEBUGGER level"
                                                   (lt:call-lisp-tool tools "debug-state" "{}"))
                                     do (let ((queued (lt:take-image-events tools)))
                                          (note "  (the run ended with the worker still in the debugger: a follow-up turn~:[ with a reminder~; with the queued event~])"
                                                queued)
                                          (agent:continue-ai-agent
                                           run (if queued
                                                   (lt:image-event-turn (first queued))
                                                   "The worker still waits in the debugger (see the last <lisp-image-event>). Answer it with debug-invoke-restart."))))
                               run)))))
               (%print-run run :width 500)
               (show "answer" (or (ignore-errors (agent:agent-run-text run)) ""))
               (show "event kinds the callback saw" (%kinds (funcall seen)))
               (show "event turns in the run (the first message included)"
                     (count-if #'lt:image-event-turn-p (agent:agent-run-turns run)))
               (when (car asked) (show "asks the demo denied" (reverse (car asked))))))
           (stage (format nil "~a: checked by the demo after the run" label))
           (wait-until (lambda () (search "=> (T " (lt:call-lisp-tool tools "repl-eval" (json-args "code" +ledger-check+))))
                       :timeout 5)
           (let ((value (tool tools "repl-eval" "code" +ledger-check+)))
             (tool tools "debug-state")
             (show "ledger.lisp parse-amount now"
                   (let* ((text (read-file root "ledger.lisp"))
                          (at (search "(defun parse-amount" text)))
                     (subseq text at (search "(defun process-order" text))))
             (setf ok (and (stringp value) (search "=> (T 4275)" value) t))
             (show "worker done, total 4275 cents (12 + 7 + 3.50 + 20.25)" ok)))
      (lt:close-lisp-tools tools))
    ok))

(defun demo-image-events-real ()
  (banner "Demo 10: an agent answers a background debugger: a seeded bug in a loaded system")
  (let ((real (and *lm-model*
                   (%ledger-run (format nil "LM Studio ~a (real model, varies run to run; debug tools only: it answers with a restart)" *lm-model*)
                                "real" (lm-backend) *lm-model* :edit nil))))
    (cond (real t)
          (t (if *lm-model*
                 (note "~%  the real model did not fix it this run; the same event and tools on the scripted mock:")
                 (note "  no LM Studio model reachable: the real-model run is skipped; the scripted mock:"))
             (%ledger-run "scripted mock" "mock" (scripted-backend (%ledger-mock-steps)) nil)))))
