(in-package #:harness-demos-data)

;;; Demo 4 — sql-migrate 0.2.0 on SQLite in memory: a three-revision script
;;; directory, upgrade / downgrade, plans and offline SQL, a failing step seen
;;; as both STEP-FAILED and SQLITE-ERROR, retry-step, and a fork.

(orm:defmodel widget-m1 ()
  (id :integer :primary-key t :autoincrement t)
  (name :text :not-null t)
  (:table widgets))

(orm:defmodel widget-m2 ()
  (id :integer :primary-key t :autoincrement t)
  (name :text :not-null t)
  (color :text)
  (:table widgets))

(orm:defmodel gizmo-m3 ()
  (id :integer :primary-key t :autoincrement t)
  (widget-id :integer :not-null t)
  (serial :text :unique t)
  (:table gizmos))

(defun m-snap-1 () (orm:schema-snapshot '(widget-m1)))
(defun m-snap-2 () (orm:schema-snapshot '(widget-m2)))
(defun m-snap-3 () (orm:schema-snapshot '(widget-m2 gizmo-m3)))

(defun demo-directory ()
  (let ((dir (mig:make-script-directory)))
    (mig:make-revision dir nil (m-snap-1) :id "0001" :name "create widgets")
    (mig:make-revision dir (m-snap-1) (m-snap-2) :id "0002" :name "add widgets.color")
    (mig:make-revision dir (m-snap-2) (m-snap-3) :id "0003" :name "create gizmos")
    dir))

(defun mig-names (migrations)
  (mapcar (lambda (m) (format nil "~A ~A" (orm:schema-migration-revision m) (orm:schema-migration-name m)))
          migrations))

(defun live-tables (c)
  (mapcar (lambda (entry)
            (format nil "~A(~{~A~^,~})" (car entry) (mapcar #'orm:column-info-name (cdr entry))))
          (orm:reflect-schema c)))

(defun reject-stamp (c dir revision)
  "The test-suite trick: a trigger makes the version table refuse REVISION,
so the step's DDL runs and its stamp fails inside the step transaction."
  (mig:ensure-version-table c dir)
  (sql:execute c (concatenate 'string
                              (format nil "CREATE TRIGGER reject_stamp BEFORE INSERT ON ~A "
                                      (mig:directory-version-table dir))
                              (format nil "WHEN NEW.version_num = '~A' " revision)
                              "BEGIN SELECT RAISE(ABORT, 'stamp rejected'); END")))

(defun has-column-p (c table column)
  (and (find column (cdr (assoc table (orm:reflect-schema c) :test #'string-equal))
             :key #'orm:column-info-name :test #'string-equal)
       t))

(defun demo-migrate ()
  (banner "Demo 4: sql-migrate 0.2.0 (SQLite in memory)")
  (let ((dir (demo-directory)))

    (demo-step ("4a. three migrations: upgrade, current-revision, history, downgrade :count 1")
      (say "history: ~S" (mig-names (mig:history dir)))
      (say "head-revision: ~S" (mig:head-revision dir))
      (orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
        (say "current-revision at base: ~S" (mig:current-revision c dir))
        (say "upgrade => ~S" (mig-names (mig:upgrade c dir)))
        (say "current-revision: ~S" (mig:current-revision c dir))
        (say "live tables: ~S" (live-tables c))
        (say "version table rows: ~S" (rows c "SELECT * FROM sql_migrate_version"))
        (say "downgrade :count 1 => ~S" (mig-names (mig:downgrade c dir :count 1)))
        (say "current-revision: ~S" (mig:current-revision c dir))
        (say "live tables: ~S" (live-tables c))
        (say "downgrade :target :base => ~S" (mig-names (mig:downgrade c dir :target :base)))
        (say "current-revision: ~S, live tables ~S" (mig:current-revision c dir) (live-tables c))
        (handler-case (mig:upgrade c dir :target "nope")
          (rev:revision-not-found (e)
            (say "upgrade :target \"nope\" caught as rev:revision-not-found (~A): ~A" (type-of e) e)))))

    (demo-step ("4b. the plan for an upgrade, and offline SQL with render-diff :sql")
      (orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
        (mig:upgrade c dir :target "0001")
        (let* ((target (mig:make-sql-target c :directory dir))
               (plan (rev:plan-transition dir target "0003")))
          (say "target-revisions ~S -> plan goal ~S" (rev:target-revisions target) (rev:plan-goal plan))
          (show-text "describe-plan" (rev:describe-plan plan nil))
          (dolist (st (rev:plan-steps plan))
            (say "step ~A -> ~A lossy ~S" (rev:step-from st) (rev:step-to st) (mig:step-lossy-p st))
            (show-text "  render-diff :sql (engine dialect: ANSI)" (d:render-diff (rev:step-diff st) :sql nil)))
          (let ((down (rev:plan-transition dir (rev:make-in-memory-target :revisions '("0003")) "0001")))
            (show-text "describe-plan 0003 -> 0001 (downgrade)" (rev:describe-plan down nil))
            (dolist (st (rev:plan-steps down))
              (say "down step ~A -> ~A lossy ~S" (rev:step-from st) (rev:step-to st) (mig:step-lossy-p st))
              (show-text "  render-diff :sql" (d:render-diff (rev:step-diff st) :sql nil))))
          (let ((sqlite-diff (mig:make-schema-diff (orm:diff-schema (m-snap-1) (m-snap-3) :complete t)
                                                   :engine (mig:make-schema-diff-engine :dialect :sqlite3))))
            (show-text "0001 -> 0003 as one schema-diff, render-diff :sql with the :sqlite3 dialect"
                       (d:render-diff sqlite-diff :sql nil))
            (show-text "render-diff :summary" (d:render-diff sqlite-diff :summary nil)))
          (say "the database was not touched: current-revision ~S" (mig:current-revision c dir)))))

    (demo-step ("4c. a failing step: one condition, two classes; retry-step")
      (orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
        (mig:upgrade c dir :target "0001")
        (reject-stamp c dir "0002")
        (say "trigger installed: stamping 0002 raises 'stamp rejected'")
        (let ((e (handler-case (mig:upgrade c dir) (error (e) e))))
          (say "condition class: ~A" (type-of e))
          (check "(typep e 'revision-protocol:step-failed)" (typep e 'rev:step-failed))
          (check "(typep e 'sqlite:sqlite-error)" (typep e 'sqlite:sqlite-error))
          (say "step ~S attempt ~S, cause ~A: ~A"
               (rev:step-failed-step e) (rev:step-failed-attempt e)
               (type-of (rev:step-failed-cause e)) (rev:step-failed-cause e))
          (say "report (Code/Database/SQL block printed twice, sql-migrate#5): ~A" e))
        (say "after the failure: current-revision ~S, widgets.color present ~S"
             (mig:current-revision c dir) (has-column-p c "widgets" "color"))
        (say "0.1-style (handler-case (upgrade c dir) (sqlite:sqlite-error () :caught)) => ~S"
             (handler-case (mig:upgrade c dir) (sqlite:sqlite-error () :caught)))
        (let ((offered nil) (attempts '()))
          (let ((applied
                  (handler-bind ((rev:step-failed
                                   (lambda (s)
                                     (push (rev:step-failed-attempt s) attempts)
                                     (setf offered (remove-if-not (lambda (r) (find-restart r s))
                                                                  '(rev:retry-step rev:skip-step rev:rollback-to-start)))
                                     (say "handler-bind: step ~A failed (attempt ~D); restarts ~S"
                                          (rev:step-to (rev:step-failed-step s)) (rev:step-failed-attempt s) offered)
                                     (say "handler-bind: DROP TRIGGER reject_stamp, then RETRY-STEP")
                                     (sql:execute c "DROP TRIGGER reject_stamp")
                                     (invoke-restart 'rev:retry-step))))
                    (mig:upgrade c dir))))
            (say "upgrade => ~S" (mig-names applied))
            (say "attempts seen by the handler: ~S" (reverse attempts))
            (say "current-revision ~S, widgets.color present ~S, live tables ~S"
                 (mig:current-revision c dir) (has-column-p c "widgets" "color") (live-tables c))))))

    (demo-step ("4d. a fork: head-revision signals multiple-heads")
      (let ((fork (mig:make-script-directory)))
        (mig:register-revision fork (orm:make-migration nil (m-snap-1) :revision "a" :name "widgets"))
        (mig:register-revision fork (orm:make-migration (m-snap-1) (m-snap-2) :revision "b"
                                                                              :down-revision "a" :name "color on b"))
        (mig:register-revision fork (orm:make-migration (m-snap-1) (m-snap-2) :revision "c"
                                                                              :down-revision "a" :name "color on c"))
        (say "history (a forked directory is still listed): ~S" (mig-names (mig:history fork)))
        (handler-case (mig:head-revision fork)
          (mig:multiple-heads (e)
            (say "head-revision signalled ~A: heads ~S" (type-of e) (mig:multiple-heads-heads e))
            (say "  also a rev:revision-error? ~S; message ~S" (typep e 'rev:revision-error) (mig:migrate-error-message e))))
        (say "with a USE-VALUE handler: head-revision => ~S"
             (handler-bind ((mig:multiple-heads (lambda (e) (use-value (first (mig:multiple-heads-heads e)) e))))
               (mig:head-revision fork)))
        (orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
          (handler-case (mig:upgrade c fork)
            (error (e) (say "upgrade on the fork signalled ~A: ~A" (type-of e) e)))
          (handler-case (mig:upgrade c fork :target "b")
            (error (e) (say "upgrade :target \"b\" on the fork signalled ~A: ~A" (type-of e) e))))))))
