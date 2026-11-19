(in-package #:harness-demos-agent)

;;; Demo 1: ai-agent-protocol 0.10.0 permissions with the mock LLM backend.
;;; Approve "always" from a handler, the saved rule in an in-memory store,
;;; a policy deny that still wins, and reject-all-pending with feedback.

(defun %edit-call (path)
  (list "edit" (json-args "path" path)))

(defun %path-of (arguments)
  (gethash "path" (json:decode arguments)))

(defun %edit-agent (steps ran &key store (permissions '(("edit" "*" :ask))) counter)
  "Agent with one edit tool that asks by default and reports its path as the
permission resource. RAN's car collects the paths actually edited."
  (let ((agent (agent:make-ai-agent
                :name "coder"
                :backend (scripted-backend steps :counter (or counter (list 0)))
                :permissions permissions
                :permission-store store)))
    (agent:register-agent-tool
     agent
     (agent:make-function-tool
      :name "edit"
      :description "Edit a file"
      :permission-resources (lambda (args) (list (%path-of args)))
      :handler (lambda (args)
                 (push (%path-of args) (car ran))
                 (format nil "edited ~a" (%path-of args)))))
    agent))

(defun demo-permissions ()
  (banner "Demo 1: ai-agent-protocol 0.10.0 permissions (mock LLM)")
  (with-agent-loop
    (let* ((store (agent:make-in-memory-permission-rule-store))
           (ran (list nil))
           (asked '())
           (agent (%edit-agent (list (list (%edit-call "src/a.lisp"))
                                     (list (%edit-call "src/a.lisp"))
                                     "done editing")
                               ran :store store)))
      (stage "1a. model calls edit(src/a.lisp) twice; agent rule (\"edit\" \"*\" :ask)")
      (show "store rules before" (agent:list-permission-rules store))
      (let ((run (handler-bind ((agent:agent-approval-required
                                  (lambda (c)
                                    (let ((inv (agent:agent-approval-required-invocation c)))
                                      (push (agent:agent-invocation-id inv) asked)
                                      (note "  [handler] ~a" c)
                                      (note "  [handler] call ~a args ~a → answering \"always\" (invoke-approve-always)"
                                            (agent:agent-invocation-id inv)
                                            (agent:agent-invocation-arguments inv))
                                      (agent:invoke-approve-always c)))))
                   (agent:run-ai-agent agent "edit src/a.lisp twice"))))
        (show "finish reason" (agent:agent-run-finish-reason run))
        (show "approval prompts (call ids)" (reverse asked))
        (show "paths edited (in order)" (reverse (car ran)))
        (note "invocations:")
        (mapc #'print-invocation (agent:agent-run-invocations run))
        (note "run's approved permissions (agent-run-approved-permissions):")
        (mapc #'print-rule (agent:agent-run-approved-permissions run))
        (note "saved rules in the in-memory-permission-rule-store:")
        (mapc #'print-rule (agent:list-permission-rules store))
        (show "final text" (agent:agent-run-text run)))

      (stage "1b. a later run with the same store: no prompt at all")
      (setf asked '() (car ran) nil)
      (let* ((agent2 (%edit-agent (list (list (%edit-call "src/a.lisp")) "ok") ran :store store))
             (run (handler-bind ((agent:agent-approval-required
                                   (lambda (c) (push :asked asked) (agent:invoke-approve c))))
                    (agent:run-ai-agent agent2 "again"))))
        (show "finish reason" (agent:agent-run-finish-reason run))
        (show "prompts" (length asked))
        (mapc #'print-invocation (agent:agent-run-invocations run)))

      (stage "1c. same store, plus a run policy (\"edit\" \"*\" :deny): the deny still wins")
      (setf asked '() (car ran) nil)
      (let* ((agent3 (%edit-agent (list (list (%edit-call "src/a.lisp")) "understood") ran
                                  :store store))
             (run (handler-bind ((agent:agent-approval-required
                                   (lambda (c) (push :asked asked) (agent:invoke-approve c))))
                    (agent:run-ai-agent agent3 "edit again"
                                        :settings (agent:make-agent-settings
                                                   :extra (list :permission-policy
                                                                '(("edit" "*" :deny))))))))
        (show "finish reason" (agent:agent-run-finish-reason run))
        (show "prompts" (length asked))
        (show "paths edited" (car ran))
        (mapc #'print-invocation (agent:agent-run-invocations run)))

      (stage "1d. agent-layer deny (\"edit\" \"secret/*\" :deny) vs a saved allow-everything store")
      (setf (car ran) nil)
      (let* ((wide (agent:make-in-memory-permission-rule-store
                    :rules (list (agent:make-permission-rule "edit" "*" :allow))))
             (agent4 (%edit-agent (list (list (%edit-call "secret/key.pem")) "ok") ran
                                  :store wide
                                  :permissions (list '("edit" "*" :ask)
                                                     (agent:make-permission-rule
                                                      "edit" "secret/*" :deny
                                                      :feedback "Never edit secrets."))))
             (run (agent:run-ai-agent agent4 "edit the key")))
        (show "finish reason" (agent:agent-run-finish-reason run))
        (show "paths edited" (car ran))
        (mapc #'print-invocation (agent:agent-run-invocations run)))

      (stage "1e. three pending calls; reject all (deny-invocation :all t) with feedback")
      (setf (car ran) nil)
      (let* ((seen (list nil))
             (agent5 (%edit-agent (list (list (%edit-call "src/a.lisp")
                                              (%edit-call "src/b.lisp")
                                              (%edit-call "src/c.lisp"))
                                        "OK, I will explain the plan first.")
                                  ran))
             (run (agent:run-ai-agent agent5 "refactor three files")))
        (declare (ignore seen))
        (show "finish reason" (agent:agent-run-finish-reason run))
        (show "pending" (mapcar #'agent:agent-invocation-id (agent:agent-run-pending run)))
        (agent:deny-invocation run (second (agent:agent-run-pending run))
                               :feedback "Stop editing; explain the plan first." :all t)
        (show "pending after deny :all t"
              (mapcar #'agent:agent-invocation-id (agent:agent-run-pending run)))
        (mapc #'print-invocation (agent:agent-run-invocations run))
        (let ((run2 (agent:resume-ai-agent run)))
          (show "resumed finish reason" (agent:agent-run-finish-reason run2))
          (show "paths edited" (car ran))
          (note "tool results the model saw:")
          (dolist (p (tool-results run2))
            (format t "  ~a error-p=~a: ~a~%" (llm:llm-tool-result-part-id p)
                    (llm:llm-tool-result-part-error-p p) (llm:llm-tool-result-part-content p)))
          (show "final text" (agent:agent-run-text run2)))))))
