(in-package #:harness-demos-supervisor)

;;; Demos 1-5: supervisor-protocol 0.2.0, H8a. One supervisor for Demos 1-3 (a
;;; session › agent › tool-source tree with a subagent scope), then fresh ones
;;; for the caps and the stop escalation.

;;; Demo 1: scopes and units

(defun subagent-body (sup)
  "A subagent thread: starts its own shell job (owned by its scope through
*CURRENT-SCOPE*), then runs until a stop is requested."
  (lambda (u)
    (sp:start-unit sup :shell-job :command (shell-sleep-argv 60) :labels '(:purpose "test"))
    (loop until (sp:unit-stop-requested-p u) do (sleep 0.02))
    :done))

(defun demo-scopes ()
  (banner "Demo 1: supervisor-protocol 0.2.0 (H8a) — scopes and units")
  (with-checks ()
    (let* ((sup (sp:make-supervisor :exit-hook nil))
           (events (make-collector sup :since 0))
           (session (sp:make-scope sup :session "session-1"))
           (agent (sp:make-scope sup :agent "coder" :parent session))
           (tools (sp:make-scope sup :tool-source "echo-tools" :parent agent))
           (subagent (sp:make-scope sup :agent "subagent-1" :parent agent)))
      (setf *shared* (list :sup sup :events events :session session :agent agent
                           :tools tools :subagent subagent))
      (stage "the scope tree")
      (dolist (s (list (sp:supervisor-root-scope sup) session agent tools subagent))
        (note "~14s ~a" (sp:scope-kind s) (scope-label s)))
      (show "supervisor-id (<pid>-<8 hex>)" (length (sp:supervisor-id sup)))

      (stage "start: a shell job (agent), an echo server (tool source, :restart :on-failure), a subagent thread")
      (let* ((job (sp:start-unit sup :shell-job :command (shell-sleep-argv 60) :owner agent
                                                :labels '(:purpose "build")))
             (srv (sp:start-unit sup :rpc :command (echo-server-argv) :owner tools
                                          :restart :on-failure :backoff '(0.5 2 30) :max-restarts 3
                                          :labels '(:server "echo")))
             (sub (sp:start-unit sup :subagent :function (subagent-body sup) :owner subagent)))
        (setf (getf *shared* :job) job (getf *shared* :srv) srv (getf *shared* :sub) sub)
        (check "the subagent started its own shell job"
               (wait-until (lambda () (= 4 (length (sp:supervisor-units sup))))))
        (show "restart policy of the server" (sp:unit-restart-policy srv))
        (show "kind-default-restart :rpc / :shell-job"
              (list (sp:kind-default-restart :rpc) (sp:kind-default-restart :shell-job)))
        (stage "the unit table (supervisor-units, start order)")
        (unit-table (sp:supervisor-units sup))
        (check "every process unit has a pid, the thread none"
               (and (integerp (sp:unit-pid job)) (integerp (sp:unit-pid srv)) (null (sp:unit-pid sub))))

        (stage "echo round trip: send-unit-input, then unit-output")
        (sp:send-unit-input srv (format nil "hello supervisor~%"))
        (check "the server echoed the line"
               (wait-until (lambda () (search "hello supervisor" (sp:unit-output srv)))))
        (show "unit-output" (sp:unit-output srv))))))

;;; Demo 2: queries, closing a scope

(defun demo-queries ()
  (banner "Demo 2: queries by kind / owner / label / state; close the subagent scope")
  (with-checks ()
    (let ((sup (shared :sup)) (agent (shared :agent)) (subagent (shared :subagent))
          (tools (shared :tools)))
      (stage "find-units")
      (flet ((q (label &rest keys)
               (let ((r (ids (apply #'sp:find-units sup keys))))
                 (show label r)
                 r)))
        (q ":kind :shell-job" :kind :shell-job)
        (q ":kind (:rpc :subagent)" :kind '(:rpc :subagent))
        (q ":owner coder (recursive)" :owner agent)
        (check ":owner coder :recursive nil → only the agent's own job"
               (equal '("shell-job-1") (q ":owner coder :recursive nil" :owner agent :recursive nil)))
        (q ":owner subagent-1" :owner subagent)
        (q ":owner echo-tools" :owner tools)
        (check ":labels (:server \"echo\") → the server"
               (equal '("rpc-1") (q ":labels (:server \"echo\")" :labels '(:server "echo"))))
        (q ":labels (:purpose \"test\")" :labels '(:purpose "test"))
        (q ":state :running" :state :running)
        (show "find-unit \"rpc-1\"" (sp:unit-id (sp:find-unit sup "rpc-1")))
        (show "scope-units subagent-1" (ids (sp:scope-units subagent))))

      (stage "close-scope subagent-1 → only its units stop (one stop wave)")
      (let ((start (get-internal-real-time)))
        (show "close-scope" (and (sp:close-scope subagent :grace 2) t))
        (show "seconds" (elapsed-since start)))
      (show "close-scope again (idempotent)" (sp:close-scope subagent))
      (show "scope-closed-p" (sp:scope-closed-p subagent))
      (unit-table (sp:supervisor-units sup))
      (check "the subagent's thread and its job are :killed, the rest :running"
             (equal '(:running :running :killed :killed)
                    (mapcar #'sp:unit-state (sp:supervisor-units sup))))
      (show "the thread's result (cooperative stop)" (sp:unit-result (shared :sub)))
      (handler-case (sp:start-unit sup :shell-job :command (shell-sleep-argv 1) :owner subagent)
        (sp:scope-closed (c) (show "start-unit in the closed scope → signalled" (type-of c)))))))

;;; Demo 3: a killed server restarts once with backoff; the event stream

(defun demo-restart ()
  (banner "Demo 3: kill the server's process → one restart with backoff; the event stream")
  (with-checks ()
    (let* ((sup (shared :sup)) (srv (shared :srv)) (events (shared :events))
           (id (sp:unit-id srv))
           (pid1 (sp:unit-pid srv))
           (since (sp:supervisor-last-seq sup)))
      (stage "kill -9 / taskkill /F the server (pid ~d) from outside" pid1)
      (show "last seq before the kill" since)
      (os-kill pid1)
      (check "restarted: :running again with restart-count 1"
             (wait-until (lambda () (and (eq :running (sp:unit-state srv))
                                         (= 1 (sp:unit-restart-count srv))))
                         :timeout 20))
      (let ((pid2 (sp:unit-pid srv)))
        (show "same unit, same id" (sp:unit-id srv))
        (check (format nil "a new pid (~d → ~d)" pid1 pid2) (and pid2 (/= pid1 pid2)))
        (sp:send-unit-input srv (format nil "after the restart~%"))
        (check "the new process is wired to the same unit"
               (wait-until (lambda () (search "after the restart" (sp:unit-output srv))))))
      (sleep 0.3)
      (stage "the server's events (subscribe :since 0, from Demo 1 on)")
      (let* ((mine (remove id (collected events) :key #'sp:event-unit-id :test-not #'equal))
             (t0 (sp:event-monotonic (first mine))))
        (dolist (e mine) (print-event e t0))
        (let* ((failed (find :failed mine :key #'sp:event-to))
               (sched (find :restart-scheduled mine :key #'sp:event-type))
               (relaunch (find-if (lambda (e) (and (eq :state (sp:event-type e))
                                                   (eq :failed (sp:event-from e))))
                                  mine))
               (waited (- (sp:event-monotonic relaunch) (sp:event-monotonic failed))))
          (check "transitions: nil→starting→running→failed, restart-scheduled, failed→starting→running"
                 (equal '((:state nil :starting) (:state :starting :running) (:state :running :failed)
                          (:restart-scheduled :failed :starting) (:state :failed :starting)
                          (:state :starting :running))
                        (mapcar (lambda (e) (list (sp:event-type e) (sp:event-from e) (sp:event-to e)))
                                mine)))
          (show "failed detail (external kill = non-zero exit, no signal recorded)" (sp:event-detail failed))
          (show "restart-scheduled detail" (sp:event-detail sched))
          (show "restart-delay (0.5 2 30) for attempts 1..5"
                (loop for a from 1 to 5 collect (sp:restart-delay '(0.5 2 30) a)))
          (check (format nil "waited the backoff: ~,3f s between :failed and the relaunch (≥ 0.5)" waited)
                 (>= waited 0.49))
          (check "seq numbers have no gaps"
                 (let ((all (mapcar #'sp:event-seq (collected events))))
                   (equal all (loop for i from 1 to (length all) collect i))))

          (stage "replay: subscribe :since ~d (the kill's predecessor), then live events" since)
          (let ((replay (make-collector sup :since since)))
            (wait-until (lambda () (>= (length (collected replay)) 4)) :timeout 5)
            (let ((replayed (collected replay)))
              (note "replayed (~d events):" (length replayed))
              (dolist (e replayed) (print-event e))
              (check "the replay starts right after SINCE"
                     (= (1+ since) (sp:event-seq (first replayed)))))
            (multiple-value-bind (evs complete) (sp:events-since sup since)
              (show "events-since → (count complete-p)" (list (length evs) complete)))
            (note "now stop the server: its events reach the replaying subscriber live")
            (let ((before (sp:supervisor-last-seq sup)))
              (sp:stop-unit srv :grace 2)
              (let ((live (remove-if (lambda (e) (<= (sp:event-seq e) before))
                                     (wait-collected replay
                                                     (lambda (es) (find :killed es :key #'sp:event-to))))))
                (dolist (e live) (print-event e))
                (let ((seqs (mapcar #'sp:event-seq (collected replay))))
                  (check "replay + live: contiguous, no gap, no duplicate"
                         (equal seqs (loop for i from (1+ since) repeat (length seqs) collect i))))))
            (show "unsubscribe" (sp:unsubscribe (collector-subscription replay)))
            (show "subscription-active-p after unsubscribe"
                  (sp:subscription-active-p (collector-subscription replay)))))))))

;;; Demo 4: caps + LRU eviction

(defun demo-caps ()
  (banner "Demo 4: caps + LRU eviction (:kind-caps (:shell-job 2))")
  (with-checks ()
    (with-supervisor (sup :kind-caps '(:shell-job 2))
      (let* ((events (make-collector sup))
             (a (sp:start-unit sup :shell-job :command (shell-argv "exit 0")))
             (b (sp:start-unit sup :shell-job :command (shell-sleep-argv 60))))
        (sp:wait-unit a :timeout 10)
        (stage "at the cap: ~a finished, ~a running" (sp:unit-id a) (sp:unit-id b))
        (unit-table (sp:supervisor-units sup))
        (stage "start a third → the least recently used FINISHED unit is evicted silently")
        (let ((c (sp:start-unit sup :shell-job :command (shell-sleep-argv 60))))
          (show "started" (sp:unit-id c))
          (show "find-unit of the evicted one" (sp:find-unit sup (sp:unit-id a)))
          (let ((ev (find :evicted (wait-collected events (lambda (es) (find :evicted es :key #'sp:event-type)))
                          :key #'sp:event-type)))
            (print-event ev)
            (check "the finished unit was evicted" (equal (sp:unit-id a) (sp:event-unit-id ev))))
          (unit-table (sp:supervisor-units sup))
          (sp:touch-unit c)
          (stage "start a fourth with both live → unit-limit-reached; take EVICT-LRU")
          (let ((d (handler-bind ((sp:unit-limit-reached
                                    (lambda (cnd)
                                      (show "signalled" (type-of cnd))
                                      (show "  kind / limit" (list (sp:unit-limit-reached-kind cnd)
                                                                   (sp:unit-limit-reached-limit cnd)))
                                      (show "  victim (LRU live unit; c was touched)"
                                            (sp:unit-id (sp:unit-limit-reached-victim cnd)))
                                      (show "  restarts" (restart-names cnd))
                                      (note "→ EVICT-LRU")
                                      (invoke-restart 'sp:evict-lru))))
                     (sp:start-unit sup :shell-job :command (shell-sleep-argv 60)))))
            (show "started" (sp:unit-id d))
            (show "victim's state" (sp:unit-state b))
            (check "the victim was stopped and evicted"
                   (and (eq :killed (sp:unit-state b)) (null (sp:find-unit sup (sp:unit-id b)))))
            (unit-table (sp:supervisor-units sup))
            (stage "start a fifth → unit-limit-reached; take ABORT")
            (let ((e (handler-bind ((sp:unit-limit-reached
                                      (lambda (cnd)
                                        (show "  restarts" (restart-names cnd))
                                        (note "→ ABORT")
                                        (invoke-restart 'abort))))
                       (sp:start-unit sup :shell-job :command (shell-sleep-argv 60)))))
              (show "start-unit returned" e)
              (check "ABORT: nothing started, units unchanged"
                     (and (null e) (equal (ids (list c d)) (ids (sp:supervisor-units sup))))))
            (stage "interacting units are never evicted: no EVICT-LRU without a victim")
            (setf (sp:unit-interacting-p c) t (sp:unit-interacting-p d) t)
            (handler-bind ((sp:unit-limit-reached
                             (lambda (cnd)
                               (show "victim" (sp:unit-limit-reached-victim cnd))
                               (show "restarts" (restart-names cnd))
                               (check "restarts: only ABORT" (equal '(abort) (restart-names cnd)))
                               (invoke-restart 'abort))))
              (sp:start-unit sup :shell-job :command (shell-sleep-argv 60)))
            (setf (sp:unit-interacting-p c) nil (sp:unit-interacting-p d) nil)
            (let ((f (handler-bind ((sp:unit-limit-reached (lambda (cnd) (declare (ignore cnd))
                                                             (invoke-restart 'sp:evict-lru))))
                       (sp:start-unit sup :shell-job :command (shell-sleep-argv 60)))))
              (check (format nil "refused starts took no id: the next one is ~a" (sp:unit-id f))
                     (equal "shell-job-5" (sp:unit-id f))))))))))

;;; Demo 5: stop escalation

(defun term-ignoring-argv (seconds)
  "Unix: a child that ignores SIGTERM, says so, and sleeps SECONDS."
  (lisp-argv (format nil "(progn (sb-sys:enable-interrupt sb-unix:sigterm :ignore) (write-line \"ignoring TERM\") (finish-output) (sleep ~D))" seconds)))

(defun demo-stop ()
  (banner "Demo 5: stop escalation — graceful → stop-timeout → FORCE")
  (with-checks ()
    (with-supervisor (sup)
      (stage "a process that ignores SIGTERM (stop-unit :grace 1)")
      (if (windows-p)
          (note "skipped on Windows: a graceful process stop there is always forced (job object), so it never times out")
          (let ((u (sp:start-unit sup :shell-job :command (term-ignoring-argv 60))))
            (wait-until (lambda () (search "ignoring TERM" (sp:unit-output u))))
            (let* ((start (get-internal-real-time))
                   (state (handler-bind ((sp:stop-timeout
                                           (lambda (c)
                                             (show "signalled (not an error)" (type-of c))
                                             (show "  phase / waited" (list (sp:stop-timeout-phase c)
                                                                            (sp:stop-timeout-waited c)))
                                             (show "  restarts" (restart-names c))
                                             (note "→ FORCE")
                                             (invoke-restart 'sp:force))))
                            (sp:stop-unit u :grace 1))))
              (show "final state" state)
              (show "unit-signal (the last stop sent)" (sp:unit-signal u))
              (show "seconds" (elapsed-since start))
              (check "forced after the grace: :killed by :kill" (and (eq :killed state) (eq :kill (sp:unit-signal u)))))))

      (stage "a thread that never polls unit-stop-requested-p (stop-unit :grace 0.5): KEEP-WAITING once, then FORCE")
      (let* ((cleanup nil)
             (u (sp:start-unit sup :subagent
                                   :function (lambda (u)
                                               (declare (ignore u))
                                               (unwind-protect (loop (sleep 0.05))
                                                 (setf cleanup t)))))
             (timeouts 0)
             (start (get-internal-real-time))
             (state (handler-bind ((sp:stop-timeout
                                     (lambda (c)
                                       (incf timeouts)
                                       (show (format nil "stop-timeout #~d waited" timeouts) (sp:stop-timeout-waited c))
                                       (if (= timeouts 1)
                                           (progn (note "→ KEEP-WAITING") (invoke-restart 'sp:keep-waiting))
                                           (progn (note "→ FORCE (bt2:interrupt-thread unwinds the body)")
                                                  (invoke-restart 'sp:force))))))
                      (sp:stop-unit u :grace 0.5))))
        (show "final state" state)
        (show "seconds" (elapsed-since start))
        (show "unit-condition (a forced stop is not a failure)" (sp:unit-condition u))
        (check "two stop-timeouts, :killed, the UNWIND-PROTECT cleanup ran"
               (and (= 2 timeouts) (eq :killed state) cleanup)))

      (stage "unhandled stop-timeout → forced anyway")
      (let* ((u (sp:start-unit sup :subagent :function (lambda (u) (declare (ignore u)) (loop (sleep 0.05)))))
             (state (sp:stop-unit u :grace 0.3)))
        (check (format nil "stop-unit returned ~s" state) (eq :killed state))))))
