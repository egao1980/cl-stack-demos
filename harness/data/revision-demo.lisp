(in-package #:harness-demos-data)

;;; Demo 2 — revision-protocol 0.2.0: graph, CAS refs, targets and plans,
;;; merge projections, composite revisions. Ids are caller-chosen and every
;;; commit runs under a fixed clock, so the output is deterministic.

(defun at (seconds) (dt:make-instant seconds))

(defun commit-at (store seconds content &rest keys)
  "COMMIT-REVISION with the store clock fixed at unix SECONDS."
  (dt:with-fixed-clock ((at seconds))
    (apply #'rev:commit-revision store content keys)))

(defun ids (revisions) (mapcar #'rev:revision-id revisions))

(defun rid (store designator) (rev:revision-id (rev:resolve-revision store designator)))

(defun show-revision (store r)
  (say "  ~A  parents ~S  recorded ~D  ~S  state ~S"
       (rev:revision-id r) (rev:revision-parent-ids r)
       (dt:instant-seconds (rev:revision-recorded-at r))
       (rev:revision-message r) (rev:revision-state store r)))

(defun restart-list (condition)
  (remove-if-not (lambda (name) (find-restart name condition))
                 '(rev:retry rev:use-current rev:force rev:capture-and-continue
                   rev:discard-drift rev:take-ours rev:take-theirs rev:use-merged
                   rev:retry-step rev:skip-step rev:rollback-to-start
                   rev:skip-part rev:rebuild-from-recipe abort use-value)))

(defun demo-store ()
  "main: m1 ← m2 ← m3; feature: m1 ← f1 ← f2; tag v1 at m2."
  (let ((s (rev:make-in-memory-store)))
    (commit-at s 100 '(:app "demo" :version 1 :flags ()) :id "m1-init" :ref "main" :message "init")
    (commit-at s 200 '(:app "demo" :version 2 :flags ()) :id "m2-bump" :ref "main" :message "bump to 2")
    (commit-at s 150 '(:app "demo" :version 1 :flags ("beta")) :id "f1-beta" :ref "feature"
                                                                 :parents '("m1-init") :message "beta flag")
    (commit-at s 300 '(:app "demo" :version 3 :flags ()) :id "m3-bump" :ref "main" :message "bump to 3")
    (commit-at s 350 '(:app "demo" :version 9 :flags ("beta") :owner "f-team") :id "f2-owner"
                                                                :ref "feature" :message "version 9 + owner")
    (rev:tag-revision s "v1" "m2-bump" :annotation "first release")
    s))

(defun demo-revision ()
  (banner "Demo 2: revision-protocol 0.2.0")
  (let ((s (demo-store)))

    (demo-step ("2a. in-memory store: two branches, a tag, walks, merge-base, as-of")
      (say "refs:")
      (dolist (ref (rev:list-refs s))
        (say "  ~A ~(~A~) -> ~A~@[  (~A)~]" (rev:ref-name ref) (rev:ref-kind ref)
             (rev:ref-target ref) (rev:ref-annotation ref)))
      (say "walk-revisions (main feature) :topo:")
      (dolist (r (rev:walk-revisions s (list (rev:rev-ref "main") (rev:rev-ref "feature")) :order :topo))
        (show-revision s r))
      (say "walk :time  => ~S" (ids (rev:walk-revisions s (list (rev:rev-ref "main") (rev:rev-ref "feature"))
                                                         :order :time)))
      (say "walk main :first-parent :reverse t (git log) => ~S"
           (ids (rev:walk-revisions s (rev:rev-ref "main") :order :first-parent :reverse t)))
      (say "walk feature :until main => ~S"
           (ids (rev:walk-revisions s (rev:rev-ref "feature") :until (rev:rev-ref "main"))))
      (say "walk main :footprint (\"sexp:/version\") => ~S"
           (ids (rev:walk-revisions s (rev:rev-ref "main") :footprint '("sexp:/version"))))
      (say "merge-base main feature => ~A" (rev:revision-id (rev:merge-base s (rev:rev-ref "main") (rev:rev-ref "feature"))))
      (say "ancestor-p v1 main => ~S" (rev:ancestor-p s (rev:rev-tag "v1") (rev:rev-ref "main")))
      (say "resolve (rev-tag \"v1\") => ~A" (rid s (rev:rev-tag "v1")))
      (say "resolve (rev-ancestor main 2) => ~A" (rid s (rev:rev-ancestor (rev:rev-ref "main") 2)))
      (say "resolve id prefix \"f2-o\" => ~A" (rid s "f2-o"))
      (dolist (sec '(120 250 1000))
        (say "rev-as-of ~D :on main => ~A" sec (rid s (rev:rev-as-of (at sec) :on "main"))))
      (say "heads => ~S" (sort (ids (rev:heads s)) #'string<)))

    (demo-step ("2b. CAS ref-conflict resolved by the RETRY restart")
      (let* ((seen-tip (rid s (rev:rev-ref "main"))))
        (say "we read main = ~A, then someone else commits to main first" seen-tip)
        (commit-at s 400 '(:app "demo" :version 4 :flags ()) :id "m4-other" :ref "main" :message "concurrent")
        (let ((r (handler-bind ((rev:ref-conflict
                                  (lambda (c)
                                    (say "handler: ~A ~S expected ~S actual ~S"
                                         (type-of c) (rev:ref-conflict-name c)
                                         (rev:ref-conflict-expected c) (rev:ref-conflict-actual c))
                                    (say "handler: restarts offered ~S" (restart-list c))
                                    (say "handler: choosing RETRY")
                                    (invoke-restart 'rev:retry))))
                   (commit-at s 410 '(:app "demo" :version 5 :flags ()) :id "m5-ours" :ref "main"
                                                                         :expected seen-tip :message "ours"))))
          (say "committed ~A with parents ~S" (rev:revision-id r) (rev:revision-parent-ids r))
          (check "main now points at m5-ours" (equal "m5-ours" (rid s (rev:rev-ref "main")))))))

    (demo-step ("2c. in-memory target: plan-transition, describe-plan, execute-plan, drift")
      (let* ((tg (rev:make-in-memory-target :state (rev:revision-state s "m2-bump")
                                            :revisions '("m2-bump")))
             (plan (rev:plan-transition s tg (rev:rev-ref "feature"))))
        (say "target at ~S, state ~S" (rev:target-revisions tg) (rev:target-state tg))
        (say "plan to feature: start ~S goal ~S base ~S" (rev:plan-start plan) (rev:plan-goal plan)
             (rev:plan-base plan))
        (show-text "describe-plan" (rev:describe-plan plan nil))
        (multiple-value-bind (revs skipped) (rev:execute-plan tg plan)
          (say "execute-plan => ~S, skipped ~S" revs skipped))
        (say "target state ~S" (rev:target-state tg))
        (say "target-drift => ~S" (rev:target-drift tg s))
        (say "someone edits the target by hand: (setf target-state)")
        (setf (rev:target-state tg) '(:app "demo" :version 9 :flags ("beta") :owner "f-team" :hotfix t))
        (let ((drift (rev:target-drift tg s)))
          (show-text "target-drift (render :summary)" (d:render-diff drift :summary nil)))
        (let ((plan2 (rev:plan-transition s tg "m2-bump")))
          (show-text "describe-plan back to m2-bump" (rev:describe-plan plan2 nil))
          (handler-bind ((rev:target-drift-detected
                           (lambda (c)
                             (say "handler: ~A, drift ~A" (type-of c)
                                  (string-trim '(#\Newline)
                                               (d:render-diff (rev:target-drift-detected-drift c) :summary nil)))
                             (say "handler: restarts offered ~S" (restart-list c))
                             (say "handler: choosing CAPTURE-AND-CONTINUE")
                             (invoke-restart 'rev:capture-and-continue))))
            (say "execute-plan => ~S"
                 (dt:with-fixed-clock ((at 450)) (multiple-value-list (rev:execute-plan tg plan2))))))
        (say "target now ~S, state ~S" (rev:target-revisions tg) (rev:target-state tg))
        (let ((captured (find "f2-owner" (rev:revision-children s "f2-owner")
                              :key (lambda (r) (first (rev:revision-parent-ids r))) :test #'equal)))
          (say "captured revision (store-generated id): parents ~S message ~S state ~S"
               (rev:revision-parent-ids captured) (rev:revision-message captured)
               (rev:revision-state s captured)))
        (say "heads now: ~D (m5-ours + the captured drift revision)" (length (rev:heads s)))))

    (demo-step ("2d. project-merge: conflict as data, then commit-projection with TAKE-OURS")
      (multiple-value-bind (p conflicts) (rev:project-merge s "m3-bump" "f2-owner")
        (say "parents ~S fidelity ~S" (rev:projection-parents p) (rev:projection-fidelity p))
        (say "projected state (base value kept at conflicts): ~S" (rev:projection-state p))
        (show-conflicts conflicts)
        (dolist (c conflicts)
          (say "  ours ~S -> ~S, theirs ~S -> ~S"
               (d:change-before (d:conflict-ours c)) (d:change-after (d:conflict-ours c))
               (d:change-before (d:conflict-theirs c)) (d:change-after (d:conflict-theirs c))))
        (let ((r (handler-bind ((rev:merge-conflict
                                  (lambda (c)
                                    (say "handler: ~A with ~D conflict(s); restarts ~S"
                                         (type-of c) (length (rev:merge-conflict-conflicts c))
                                         (restart-list c))
                                    (say "handler: choosing TAKE-OURS")
                                    (invoke-restart 'rev:take-ours))))
                   (dt:with-fixed-clock ((at 500))
                     (rev:commit-projection s p :message "merge feature" :ref "merged")))))
          (say "merge revision (store-generated id, ~D hex chars) parents ~S" (length (rev:revision-id r)) (rev:revision-parent-ids r))
          (say "merged state ~S" (rev:revision-state s r)))))

    (demo-step ("2e. composite revision: manifest over two stores, restored")
      (let ((files (rev:make-in-memory-store))
            (db (rev:make-in-memory-store))
            (cs (rev:make-in-memory-store)))
        (commit-at files 100 '(:files ("README")) :id "files-1" :ref "main")
        (commit-at files 200 '(:files ("README" "src/app.lisp")) :id "files-2" :ref "main")
        (commit-at db 100 '(:tables ("users")) :id "db-1" :ref "main")
        (commit-at db 200 '(:tables ("users" "orders")) :id "db-2" :ref "main")
        (let* ((checkpoint
                 (dt:with-fixed-clock ((at 300))
                   (rev:commit-composite cs (list (rev:make-manifest-part :files files (rev:rev-ref "main")
                                                                          :depends-on '(:db))
                                                  (rev:make-manifest-part :db db (rev:rev-ref "main"))) :ref "main" :message "checkpoint")))
               (ft (rev:make-in-memory-target :state (rev:revision-state files "files-1")
                                              :revisions '("files-1")))
               (dbt (rev:make-in-memory-target :state (rev:revision-state db "db-1")
                                               :revisions '("db-1")))
               (targets (list :files ft :db dbt)))
          (handler-case (progn (rev:commit-composite cs (list (rev:make-manifest-part :db db "db-2")) :id "ckpt-1")
                               (say "commit-composite :id accepted"))
            (error (e) (say "commit-composite ... :id \"ckpt-1\" => ~A: ~A  [revision-protocol#6]" (type-of e) e)))
          (say "composite revision (store-generated id; commit-composite takes no :id) manifest parts ~S"
               (rev:manifest-parts (rev:composite-manifest cs checkpoint)))
          (say "targets before: files ~S ~S / db ~S ~S"
               (rev:target-revisions ft) (rev:target-state ft) (rev:target-revisions dbt) (rev:target-state dbt))
          (loop for (role . plan) in (rev:plan-composite cs checkpoint targets)
                do (say "plan-composite ~S: ~A" role (string-trim '(#\Newline) (rev:describe-plan plan nil))))
          (say "restore-composite => ~S" (rev:restore-composite cs checkpoint targets))
          (say "targets after:  files ~S ~S / db ~S ~S"
               (rev:target-revisions ft) (rev:target-state ft) (rev:target-revisions dbt) (rev:target-state dbt))
          (check "both parts at the checkpoint's revisions"
                 (and (equal '("files-2") (rev:target-revisions ft))
                      (equal '("db-2") (rev:target-revisions dbt)))))))))
