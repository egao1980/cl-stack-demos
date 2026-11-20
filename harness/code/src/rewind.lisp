(in-package #:harness-demos-code)

;;; Demos j–m: cl-stack-code 0.5.0 checkpoints and rewind (H5b). Every turn
;;; that changes the project's files records a checkpoint (a composite
;;; revision: the turn's session revision + the files). The files part is a
;;; git commit on a private ref (git), the commit id of @ (jj), or a pathlib
;;; snapshot (a plain folder). A rewind restores the conversation, the files,
;;; or both; a files rewind is an H5a plan, drift-checked. Always on the
;;; scripted mock, so every value is exact.

;;; jj.

(defvar *jj-version* :unknown)

(defun jj-version ()
  "`jj --version` (trimmed), or NIL when jj does not run."
  (when (eq *jj-version* :unknown)
    (setf *jj-version*
          (handler-case
              (multiple-value-bind (out err code)
                  (uiop:run-program '("jj" "--version") :output :string :error-output :string
                                                        :ignore-error-status t)
                (declare (ignore err))
                (and (zerop code) (%trim out)))
            (error () nil))))
  *jj-version*)

(defun jj-usable-p ()
  "True when jj runs and is at least revision-backend-jj's minimum."
  (let ((v (jj-version)))
    (and v
         (let ((have (revision-backend-jj:parse-jj-version v))
               (need (revision-backend-jj:parse-jj-version revision-backend-jj:*minimum-jj-version*)))
           (and have
                (loop for i from 0 below (max (length have) (length need))
                      for x = (or (nth i have) 0) for y = (or (nth i need) 0)
                      do (cond ((> x y) (return t)) ((< x y) (return nil)))
                      finally (return t)))))))

(defun %jj (dir &rest args)
  "jj -R DIR ARGS with a fixed identity → stdout; a non-zero exit signals an
error with jj's stderr."
  (multiple-value-bind (out err code)
      (%run dir (list* "jj" "-R" (%native dir) "--no-pager" "--color=never"
                       "--config" "user.name=Rewind Demo" "--config" "user.email=rewind-demo@invalid"
                       args))
    (unless (zerop code)
      (error "jj ~{~a~^ ~} failed (~d): ~a" args code err))
    out))

(defun make-jj-project (name files)
  "A colocated jj repository NAME/: a change \"init\" holding FILES, @ an
empty change on it."
  (let ((root (scratch-dir name)))
    (multiple-value-bind (out err code)
        (%run root (list "jj" "--no-pager" "--color=never" "git" "init" "--colocate" (%native root)))
      (declare (ignore out))
      (unless (zerop code) (error "jj git init failed (~d): ~a" code err)))
    (loop for (relative text) in files do (write-file root relative text))
    (%jj root "describe" "-m" "init")
    (%jj root "new")
    root))

(defun %jj-at (project template)
  "`jj log -r @ -T TEMPLATE` (this snapshots the working copy, as jj does)."
  (%trim (%jj project "log" "--no-graph" "-r" "@" "-T" template)))

(defun %jj-quiet (project &rest args)
  "jj ARGS without snapshotting the working copy."
  (apply #'%jj project "--ignore-working-copy" args))

;;; The project, the turns, the CLI.

(defparameter *notes-md* (format nil "# notes~%~%- add() fixed in turn 1~%"))

(defparameter *readme-3* (format nil "# calc~%~%`sh test.sh` runs the tests (add and mul).~%"))

(defparameter *sub-fn* (format nil "~%sub() {~%  echo $(( $1 - $2 ))~%}~%"))

(defun demo-turns ()
  "Three headless turns: (prompt tool-calls answer)."
  `(("fix add() in calc.sh"
     (("edit" ,(json-args "file_path" "calc.sh" "old_string" "$(( $1 - $2 ))" "new_string" "$(( $1 + $2 ))")))
     "Fixed add() in calc.sh.")
    ("write NOTES.md"
     (("write" ,(json-args "file_path" "NOTES.md" "content" *notes-md*)))
     "Wrote NOTES.md.")
    ("mention add and mul in the README"
     (("edit" ,(json-args "file_path" "README.md" "old_string" "runs the tests."
                          "new_string" "runs the tests (add and mul).")))
     "Updated README.md.")))

(defun make-kind-project (kind name)
  (ecase kind
    (:git (make-git-project name (calc-files)))
    (:jj (make-jj-project name (calc-files)))
    (:copy (let ((root (scratch-dir name)))
             (loop for (relative text) in (calc-files) do (write-file root relative text))
             root))))

(defun %cli-show (project config argv &key (script '()) (quiet nil))
  "Run ARGV on the mock SCRIPT, print the argv, exit, stdout and stderr.
→ (values exit stdout stderr)."
  (setf *mock-script* script)
  (show-argv argv)
  (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
    (show "exit" exit)
    (unless quiet
      (show "stdout" (if (string= out "") "(empty)" out)))
    (show "stderr" (if (string= err "") "(empty)" err))
    (values exit out err)))

(defun %turn (project config session k &optional (turns (demo-turns)))
  "Headless turn K (1-based) of TURNS on the mock, accepting edits. → the
session id."
  (destructuring-bind (prompt calls answer) (nth (1- k) turns)
    (let ((argv (append (list "-p" "--output-format" "json" "--permission-mode" "acceptEdits")
                        (and session (list "--resume" session))
                        (list prompt))))
      (setf *mock-script* (list calls answer))
      (show-argv argv)
      (multiple-value-bind (exit out err) (cli argv :cwd project :config config)
        (let ((result (json:decode out)))
          (note "  exit ~a, result ~s, session ~a~:[~;, stderr: ~:*~a~]"
                exit (jget result "result") (subseq (jget result "session_id") 0 8)
                (and (plusp (length err)) err))
          (unless (eql exit 0) (error "turn ~d exited ~a: ~a" k exit err))
          (jget result "session_id"))))))

(defun %store (project session)
  (code:open-session (code:sessions-directory project :data-dir (data-dir)) session))

(defun %short (id) (subseq id 0 (min 12 (length id))))

(defun %checkpoints (project session &key all)
  "The session's checkpoints, one line each: ordinal reason turn kind files-id."
  (let ((store (%store project session)))
    (mapcar (lambda (k)
              (format nil "#~d ~(~a~) turn ~d ~(~a~) ~a~:[~; (expired)~]"
                      (code:checkpoint-ordinal k) (code:checkpoint-reason k) (code:checkpoint-turn k)
                      (code:checkpoint-files-kind k) (%short (code:checkpoint-files-id k))
                      (code:checkpoint-expired-p k)))
            (code:session-checkpoints store :all all))))

(defun %turn-count (project session)
  (length (code:session-line (%store project session))))

(defun %files (project)
  "calc.sh's add line, NOTES.md present?, README's test line, sub() present?"
  (let ((calc (read-file project "calc.sh"))
        (readme (read-file project "README.md")))
    (list :add (if (search "$1 + $2" (subseq calc 0 (search "mul" calc))) "+ (fixed)" "- (bug)")
          :notes (and (probe-file (merge-pathnames "NOTES.md" project)) t)
          :readme (if (search "(add and mul)" readme) "turn 3" "original")
          :sub (and (search "sub()" calc) t))))

(defun %list-checkpoints (project config session)
  (%cli-show project config (list "-p" "--resume" session "--list-checkpoints")))

;;; git / jj / copy evidence.

(defun %git-index-hash (project)
  "The index's entries (git ls-files --stage), not the file: git status
refreshes the file's stat cache."
  (%trim (%git project "ls-files" "--stage")))

(defun %git-refs (project)
  (let ((s (%trim (%git project "for-each-ref" "--format=%(refname) %(objectname:short)"
                        code:+checkpoint-ref-prefix+))))
    (if (string= s "") "(none)" s)))

(defun %store-blobs (project session)
  "The number of blob files under the session's checkpoints/store/ (copy kind)."
  (let ((dir (merge-pathnames "store/" (code:checkpoints-directory (%store project session)))))
    (if (probe-file dir)
        (length (remove-if-not (lambda (p) (pathname-name p))
                               (directory (merge-pathnames "**/*.*" dir))))
        0)))

(defun %kind-before (kind project)
  (ecase kind
    (:git (list :head (%head project) :index (%git-index-hash project) :status (%porcelain project)))
    (:jj (list :change (%jj-at project "change_id.short()")
               :bookmarks (%trim (%jj-quiet project "bookmark" "list"))
               :visible (length (lines (%jj-quiet project "log" "--no-graph" "-r" "all()"
                                                  "-T" "change_id.short() ++ \"\\n\"")))))
    (:copy nil)))

(defun %kind-evidence (kind project session before)
  "What the turns' checkpoints wrote, per kind."
  (let* ((store (%store project session))
         (turns (remove :turn (code:session-checkpoints store) :key #'code:checkpoint-reason
                                                               :test-not #'eq)))
    (ecase kind
      (:git
       (stage "git: the private refs; HEAD, the branch and the index untouched")
       (note "$ git for-each-ref ~a" code:+checkpoint-ref-prefix+)
       (show "refs" (%git-refs project))
       (let ((ref (format nil "~a~a/~d" code:+checkpoint-ref-prefix+ session
                          (code:checkpoint-ordinal (car (last turns))))))
         (show "the newest ref's commit: subject | parent"
               (%trim (%git project "log" "-1" "--format=%s | %p" ref)))
         (show "its parent is HEAD" (equal (%head project)
                                          (%trim (%git project "rev-parse" (concatenate 'string ref "^")))))
         (show "git diff --stat HEAD <ref> (the turns' files, untracked NOTES.md included)"
               (%trim (%git project "diff" "--stat" "HEAD" ref))))
       (show "HEAD unchanged" (equal (getf before :head) (%head project)))
       (show "branch" (%trim (%git project "branch" "--show-current")))
       (show "index unchanged (git ls-files --stage)" (equal (getf before :index) (%git-index-hash project)))
       (show "git status --porcelain (the agent's edits, uncommitted as always)" (%porcelain project))
       (show "git log --oneline (no checkpoint commit on the branch)" (%trim (%git project "log" "--oneline"))))
      (:jj
       (stage "jj: each checkpoint is a commit id of @; nothing is written")
       (let ((at-commit (%jj-at project "commit_id")))
         (show "@ commit id now" (%short at-commit))
         (show "the newest :turn checkpoint's files id = @'s commit id"
               (equal at-commit (code:checkpoint-files-id (car (last turns))))))
       (dolist (k turns)
         (show (format nil "checkpoint #~d (turn ~d) files ~a: change id, NOTES.md?"
                       (code:checkpoint-ordinal k) (code:checkpoint-turn k)
                       (%short (code:checkpoint-files-id k)))
               (list (%trim (%jj-quiet project "log" "--no-graph" "-r" (code:checkpoint-files-id k)
                                       "-T" "change_id.short()"))
                     (zerop (nth-value 2 (%run project (list "jj" "-R" (%native project) "--ignore-working-copy"
                                                             "file" "show" "-r" (code:checkpoint-files-id k)
                                                             "root:\"NOTES.md\"")))))))
       (show "@'s change id is the one before the turns" (equal (getf before :change)
                                                                 (%jj-at project "change_id.short()")))
       (show "jj bookmark list (before → now)" (list (getf before :bookmarks)
                                                      (%trim (%jj-quiet project "bookmark" "list"))))
       (show "visible commits (before → now)"
             (list (getf before :visible)
                   (length (lines (%jj-quiet project "log" "--no-graph" "-r" "all()"
                                             "-T" "change_id.short() ++ \"\\n\"")))))
       (show "git refs under refs/cl-stack-code (colocated .git)" (%git-refs project)))
      (:copy
       (stage "plain folder: each checkpoint is a pathlib snapshot in the session's checkpoints/store/")
       (show "files ids" (mapcar (lambda (k) (%short (code:checkpoint-files-id k)))
                                 (code:session-checkpoints store)))
       (show "blob files in store/" (%store-blobs project session))
       (show "no .git or .jj in the folder" (not (or (probe-file (merge-pathnames ".git/" project))
                                                     (probe-file (merge-pathnames ".jj/" project)))))))))

;;; j) one kind: three turns, list, rewind files, rewind both, drift.

(defun %drift-restart (kind)
  (ecase kind (:git 'code:rebase-plan) (:jj 'code:rebase-plan) (:copy 'code:overwrite-plan)))

(defun %j-kind (kind)
  (banner (format nil "Demo j (~(~a~)): checkpoints and rewind on a ~a"
                  kind (ecase kind (:git "git repository") (:jj "colocated jj repository")
                         (:copy "plain folder"))))
  (let* ((project (make-kind-project kind (format nil "rewind-~(~a~)" kind)))
         (config (scratch-dir (format nil "rewind-~(~a~)-config" kind)))
         (before (%kind-before kind project))
         (session nil))
    (show "checkpoint kind (resolve-checkpoint-kind :auto)" (code:resolve-checkpoint-kind project))
    (stage "three headless turns that edit files (mock: one tool call, then the answer)")
    (dotimes (k 3)
      (setf session (%turn project config session (1+ k)))
      (show (format nil "files after turn ~d" (1+ k)) (%files project)))
    (stage "--list-checkpoints (● on the conversation's current line)")
    (%list-checkpoints project config session)
    (show "checkpoints (ordinal reason turn kind files-id)" (%checkpoints project session))
    (%kind-evidence kind project session before)
    (stage "--rewind 1 --rewind-mode files: the files go back to turn 1, the conversation stays at turn 3")
    (%cli-show project config (list "-p" "--resume" session "--rewind" "1" "--rewind-mode" "files"))
    (show "files" (%files project))
    (show "turns on the conversation's line (kept)" (%turn-count project session))
    (stage "--rewind 2 (--rewind-mode both, the default), --output-format json")
    (multiple-value-bind (exit out)
        (%cli-show project config (list "-p" "--output-format" "json" "--resume" session "--rewind" "2")
                   :quiet t)
      (declare (ignore exit))
      (show "result.rewind" (json:encode (jget (json:decode out) "rewind"))))
    (show "files" (%files project))
    (show "turns on the conversation's line" (%turn-count project session))
    (%list-checkpoints project config session)
    (stage "drift: calc.sh edited by hand after the last turn, then --rewind 0 --rewind-mode files")
    (write-file project "calc.sh" (concatenate 'string (read-file project "calc.sh") *sub-fn*))
    (show "the user appended sub() to calc.sh" (%files project))
    (%cli-show project config (list "-p" "--resume" session "--rewind" "0" "--rewind-mode" "files"))
    (show "files (nothing restored)" (%files project))
    (stage (format nil "rewind-to in Lisp: plan-drift, its restarts, ~(~a~)" (%drift-restart kind)))
    (let ((cp (code:make-checkpointer project :data-dir (data-dir)))
          (store (%store project session)))
      (unwind-protect
           (let ((result (handler-bind
                             ((code:plan-drift
                                (lambda (c)
                                  (show "condition" (type-of c))
                                  (show "message" (princ-to-string c))
                                  (show "plan-drift-paths" (code:plan-drift-paths c))
                                  (show "the plan is the rewind plan (plan-rewind)"
                                        (let ((r (code:plan-rewind (code:plan-error-plan c))))
                                          (list :session (%short (getf r :session))
                                                :to (%short (getf r :to)) :from (%short (getf r :from)))))
                                  (show "restarts"
                                        (mapcar #'restart-name
                                                (remove-if-not (lambda (r)
                                                                 (member (restart-name r)
                                                                         '(code:rebase-plan code:overwrite-plan
                                                                           code:abort-approval)))
                                                               (compute-restarts c))))
                                  (invoke-restart (%drift-restart kind)))))
                           (code:rewind-to cp store "0" :parts '(:files)))))
             (show "rewind-to → result" (list :files (getf result :files) :changes (getf result :changes)
                                              :conflicts (getf result :conflicts) :drift (getf result :drift)))
             (show "rewind-description" (code:rewind-description result)))
        (code:close-checkpointer cp)))
    (show (format nil "files (~a)" (ecase (%drift-restart kind)
                                     (code:rebase-plan "turn 0 merged with the user's sub()")
                                     (code:overwrite-plan "turn 0; the user's sub() overwritten")))
          (%files project))
    (when (eq kind :git)
      (show "git status --porcelain" (%porcelain project)))
    (%list-checkpoints project config session)
    (when (eq kind :jj)
      (stage "jj: drift again, then --rewind 1 --rewind-mode files --on-drift overwrite from the command line")
      (write-file project "calc.sh" (concatenate 'string (read-file project "calc.sh") "# by hand" (string #\Newline)))
      (%cli-show project config (list "-p" "--resume" session "--rewind" "1" "--rewind-mode" "files"
                                      "--on-drift" "overwrite"))
      (show "files" (%files project)))
    (when (eq kind :git)
      (stage "git: the private refs after the rewinds")
      (show "refs" (%git-refs project))
      (show "HEAD, index unchanged" (list (equal (getf before :head) (%head project))
                                          (equal (getf before :index) (%git-index-hash project)))))
    t))

(defun demo-checkpoints ()
  (banner "Demo j: checkpoints and rewind (H5b) headless, git / jj / plain folder (scripted mock)")
  (let ((*model* '(:mock)))
    (show "git --version" (%trim (%git (uiop:getcwd) "--version")))
    (show "jj --version" (or (jj-version) "(not installed)"))
    (let ((ok (list (%j-kind :git)
                    (if (jj-usable-p)
                        (%j-kind :jj)
                        (progn (note "Demo j (jj) skipped: jj ~a is not installed or older than revision-backend-jj's minimum ~a"
                                     (or (jj-version) "") revision-backend-jj:*minimum-jj-version*)
                               t))
                    (%j-kind :copy))))
      (unless (every #'identity ok) (error "a checkpoint kind did not finish: ~s" ok))
      t)))

;;; k) plan approval; l) retention.

(defun demo-checkpoints-plan ()
  (banner "Demo k: an approved plan (H5a) records :plan-before and :plan-after; rewind undoes it")
  (let* ((*model* '(:mock))
         (project (make-git-project "rewind-plan" (calc-files)))
         (config (scratch-dir "rewind-plan-config")))
    (multiple-value-bind (exit events) (%plan-run project config '())
      (let ((session (jget (%event events "result") "session_id")))
        (show "plan run exit, plan status" (list exit (jget (%event events "result") "plan" "status")))
        (show "checkpoints after the plan turn (it edited the area, not the project)"
              (or (%checkpoints project session) "(none)"))
        (%cli-show project config (list "-p" "--resume" session "--apply"))
        (show "files" (%files project))
        (%list-checkpoints project config session)
        (show "checkpoints" (%checkpoints project session))
        (show "refs" (%git-refs project))
        (stage "--rewind 0 --rewind-mode files: undoes the plan")
        (%cli-show project config (list "-p" "--resume" session "--rewind" "0" "--rewind-mode" "files"))
        (show "files" (%files project))
        (show "git status --porcelain" (%porcelain project))
        t))))

(defun %settings-keep (project keep)
  (write-file project ".cl-stack-code/settings.json"
              (format nil "{\"checkpoints\": {\"keep\": ~d}}~%" keep)))

(defun %m-kind (kind)
  (stage (format nil "retention on a ~(~a~) project: checkpoints.keep 2, three turns" kind))
  (let* ((project (make-kind-project kind (format nil "keep-~(~a~)" kind)))
         (config (scratch-dir (format nil "keep-~(~a~)-config" kind)))
         (session nil))
    (%settings-keep project 2)
    (when (eq kind :git)
      (%git project "add" ".cl-stack-code/settings.json")
      (%git project "commit" "-q" "-m" "keep 2"))
    (show ".cl-stack-code/settings.json" (read-file project ".cl-stack-code/settings.json"))
    (dotimes (k 3)
      (setf session (%turn project config session (1+ k)))
      (when (eq kind :copy)
        (show (format nil "blob files in store/ after turn ~d" (1+ k)) (%store-blobs project session))))
    (show "every checkpoint (session-checkpoints :all t)" (%checkpoints project session :all t))
    (%list-checkpoints project config session)
    (let* ((store (%store project session))
           (expired (merge-pathnames "refs/expired" (code:checkpoints-directory store))))
      (show "refs/expired → checkpoint" (and (probe-file expired)
                                            (%short (%trim (uiop:read-file-string expired))))))
    (ecase kind
      (:git (show "git for-each-ref (only the retained checkpoints' refs)" (%git-refs project)))
      (:copy
       (let* ((store (%store project session))
              (blobs (cl-stack-pathlib:make-directory-snapshot-store
                      (%native (merge-pathnames "store/" (code:checkpoints-directory store))))))
         (show "the original calc.sh's blob is still in store/ (only expired checkpoints used it)"
               (cl-stack-pathlib:snapshot-store-contains-p
                blobs (cl-stack-pathlib:octets-hash (babel:string-to-octets *calc-sh* :encoding :utf-8)))))))
    (stage "--rewind 1 --rewind-mode files: turn 1's checkpoint was collected")
    (%cli-show project config (list "-p" "--resume" session "--rewind" "1" "--rewind-mode" "files"))
    (show "files (unchanged)" (%files project))
    t))

(defun demo-checkpoints-retention ()
  (banner "Demo l: retention: checkpoints.keep 2 collects the older refs (git) and blobs (plain folder)")
  (let ((*model* '(:mock)))
    (and (%m-kind :git) (%m-kind :copy))))

;;; m) interactive: /checkpoints, /tree, the rewind dialog.

(defun demo-checkpoints-interactive ()
  (banner "Demo m: /checkpoints, /tree and the rewind dialog on the headless terminal (scripted streaming mock)")
  (let* ((project (make-git-project "tui-rewind" (calc-files)))
         (backend (make-tui-mock
                   (list (list :tool "edit" (%edit-args "calc.sh" "$(( $1 - $2 ))" "$(( $1 + $2 ))"))
                         "Fixed add() in calc.sh."
                         (list :tool "write" (json-args "file_path" "NOTES.md" "content" *notes-md*))
                         "Wrote NOTES.md."))))
    (with-tui (tui program :argv '("--permission-mode" "acceptEdits") :cols 100 :rows 26)
        (project (scratch-dir "tui-rewind-config") backend)
      (%wait tui "Message, @file")
      (send-line tui "fix add() in calc.sh")
      (%wait tui "Fixed add()")
      (%wait-until tui (lambda () (%idle-p program)) "idle")
      (send-line tui "write NOTES.md")
      (%wait tui "Wrote NOTES.md.")
      (%wait-until tui (lambda () (%idle-p program)) "idle")
      (show "files after two turns" (%files project))
      (send-line tui "/checkpoints")
      (%wait tui "turn 2")
      (snap tui "/checkpoints")
      (send-line tui "/tree")
      (%wait tui "◆")
      (snap tui "/tree: turns with a retained checkpoint are marked ◆")
      (send-line tui "/rewind")
      (let ((ok (when (%wait tui "Rewind to turn")
                  (snap tui "/rewind: the rewind dialog")
                  (h:tui-type tui "1")
                  (%wait tui "rewound to turn"))))
        (snap tui "Conversation and files (1)")
        (show "files after the rewind" (%files project))
        (show "git status --porcelain" (%porcelain project))
        (show "refs" (%git-refs project))
        (unless ok (error "the rewind dialog did not finish"))
        t))))
