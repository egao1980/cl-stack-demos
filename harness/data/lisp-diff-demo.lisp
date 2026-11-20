(in-package #:harness-demos-data)

;;; Demo 5 — diff-backend-lisp-source 0.1.0 (R3): the :lisp engine on a real file,
;;; process-protocol's src/protocol.lisp at 0.3.0 (4a470f2) and 0.4.0 (3f77a73),
;;; committed byte for byte under fixtures/.

(defun fixture (name)
  (uiop:read-file-string (merge-pathnames (concatenate 'string "fixtures/" name) (project-dir))
                         :external-format :utf-8))

(defun lisp-engine () (d:find-diff-engine :lisp))

(defun replace-once (text old new)
  "TEXT with the single occurrence of OLD replaced by NEW (error when OLD is not there once)."
  (let ((pos (search old text)))
    (unless (and pos (null (search old text :start2 (1+ pos))))
      (error "~S does not occur exactly once" old))
    (concatenate 'string (subseq text 0 pos) new (subseq text (+ pos (length old))))))

(defun show-footprints (label diff)
  (say "~A (~D change~:P, level ~S):" label (length (d:diff-changes diff)) (d:diff-level diff))
  (dolist (c (d:diff-changes diff))
    (say "  ~(~7A~) ~{~A~^ ~}" (d:change-kind c) (d:change-footprint c))))

(defun unit-text (plist) (getf plist :text))

(defparameter *kill-0.3*
  "(defun kill (handle &key force)
  (process-kill handle :force force))")

(defparameter *stdin-0.3*
  "(defun stdin (handle) (process-stdin handle))")

(defun demo-lisp-diff ()
  (banner "Demo 5: diff-backend-lisp-source 0.1.0 (form-level diff of Lisp source)")
  (let ((v3 (fixture "protocol-0.3.0.lisp"))
        (v4 (fixture "protocol-0.4.0.lisp")))
    (say "fixtures: process-protocol src/protocol.lisp 0.3.0 (~D chars), 0.4.0 (~D chars)"
         (length v3) (length v4))
    (say "engine: ~S, name ~S, detect-renames ~S" (type-of (lisp-engine))
         (d:engine-name (lisp-engine)) (dl:lisp-engine-detect-renames (lisp-engine)))

    (demo-step ("5a. source-units: the file cut into top-level forms (comments go with the next form)")
      (let ((units (dl:source-units v3)))
        (say "~D units; keys: ~{~A~^, ~}" (length units) (mapcar (lambda (u) (getf u :key)) units))
        (show-text "unit 2 text (the header comment travels with its form)" (unit-text (second units)))
        (check "units concatenate to the file exactly"
               (string= v3 (apply #'concatenate 'string (mapcar #'unit-text units))))))

    (demo-step ("5b. compute-diff 0.3.0 -> 0.4.0 at every level")
      (let ((summary (d:compute-diff (lisp-engine) v3 v4 :level :summary))
            (changes (d:compute-diff (lisp-engine) v3 v4 :level :changes))
            (content (d:compute-diff (lisp-engine) v3 v4 :level :content)))
        (show-text "render :summary (form level)" (d:render-diff summary :summary nil))
        (show-footprints ":changes (nested leaves in place of their form)" changes)
        (say ":content: ~D form-level change~:P, ~D with nested children"
             (length (d:diff-changes content))
             (count-if #'d:change-children (d:diff-changes content)))
        (let ((leaf (find "lisp:/defgeneric process-kill/2" (d:diff-changes changes)
                          :key (lambda (c) (first (d:change-footprint c))) :test #'string=)))
          (when leaf
            (say "leaf ~S: ~S -> ~S" (d:change-path leaf) (d:change-before leaf) (d:change-after leaf))))
        (show-text "unified-patch v3 diff :path \"src/protocol.lisp\" :context 1"
                   (dl:unified-patch v3 changes :path "src/protocol.lisp" :context 1))
        (check "apply-diff v3 (:changes) is v4 byte for byte (comments and layout kept)"
               (string= v4 (d:apply-diff v3 changes)))
        (check "apply-diff v3 (:summary) is v4" (string= v4 (d:apply-diff v3 summary)))
        (check "apply-diff v4 (invert-diff) is v3" (string= v3 (d:apply-diff v4 (d:invert-diff changes))))
        (check "parse-diff :sexp of render :sexp applies the same"
               (string= v4 (d:apply-diff v3 (d:parse-diff :sexp (d:render-diff changes :sexp nil)))))
        (check "the patch applies as text: parse-diff :unified (no path) on v3 = v4"
               (string= v4 (d:apply-diff v3 (d:parse-diff :unified (dl:unified-patch v3 changes)))))))

    (demo-step ("5c. a comment-only edit, a rename and a move are form-level changes")
      (let* ((edited (replace-once v3 ";;; RPC framing lives in rpc-protocol."
                                   ";;; RPC framing lives in rpc-protocol (see rpc.md)."))
             (renamed (replace-once edited "(defun alive-p (handle)" "(defun live-p (handle)"))
             (moved (replace-once (replace-once renamed (format nil "~A~%" *stdin-0.3*) "")
                                  "(defun exit-code" (format nil "~A~%(defun exit-code" *stdin-0.3*)))
             (diff (d:compute-diff (lisp-engine) v3 moved)))
        (show-footprints "diff" diff)
        (dolist (c (d:diff-changes diff))
          (when (eq (d:change-kind c) :move)
            (say "  move from ~S to ~S" (d:change-from c) (d:change-path c))))
        (show-text "unified-patch" (dl:unified-patch v3 diff :context 0))
        (check "apply-diff gives the edited file" (string= moved (d:apply-diff v3 diff)))))

    (demo-step ("5d. three-way merge, edits to different forms: clean")
      ;; ours: the real 0.4.0 change; theirs: a docstring on STDIN and a new form at the end.
      (let* ((theirs (concatenate 'string
                                  (replace-once v3 *stdin-0.3*
                                                "(defun stdin (handle)
  \"The child's standard input stream.\"
  (process-stdin handle))")
                                  (format nil "~%;; Convenience for scripts.~%(defun run-string (command)~%  (nth-value 1 (run command :output :string)))~%"))))
        (multiple-value-bind (merged conflicts) (d:three-way-diff (lisp-engine) v3 v4 theirs)
          (let ((text (d:apply-diff v3 merged)))
            (say "conflicts: ~D" (length conflicts))
            (show-text "unified-patch base -> merged (:context 0)"
                       (dl:unified-patch v3 merged :path "src/protocol.lisp" :context 0))
            (check "merged has ours (defun pid) and theirs (docstring, run-string)"
                   (and (search "(defun pid (handle)" text)
                        (search "The child's standard input stream." text)
                        (search "(defun run-string" text) t))
            (check "merged = merge in the other order (symmetric)"
                   (string= text (d:apply-diff v3 (d:three-way-diff (lisp-engine) v3 theirs v4))))))))

    (demo-step ("5e. three-way merge, both sides edit DEFUN KILL: a conflict, rendered")
      (let ((theirs (replace-once v3 *kill-0.3*
                                  "(defun kill (handle &key (force t))
  ;; Default to SIGKILL: callers never want a survivor.
  (process-kill handle :force force))")))
        (multiple-value-bind (merged conflicts) (d:three-way-diff (lisp-engine) v3 v4 theirs)
          (show-conflicts conflicts)
          (dolist (c conflicts)
            (let ((o (unit-text (d:change-after (d:conflict-ours c))))
                  (th (unit-text (d:change-after (d:conflict-theirs c)))))
              (show-text "ours vs theirs for the conflicting form (:text engine, :unified)"
                         (d:render-diff (d:compute-diff (text-engine) o th :context 3) :unified nil))))
          (let ((text (d:apply-diff v3 merged)))
            (check "the conflicting form keeps its base text" (and (search *kill-0.3* text) t))
            (check "the rest of ours is merged (defun pid present)" (and (search "(defun pid (handle)" text) t))))))

    (demo-step ("5f. a :lisp file inside a tree diff, rendered :unified")
      (let* ((tree-e (make-instance 'd:tree-diff-engine
                                    :leaf-engines '((:text . :text) (:lisp . :lisp))))
             (a (d:make-tree "src" (d:make-tree "protocol.lisp" (d:make-tree-file v3 :type :lisp))))
             (b (d:make-tree "src" (d:make-tree "protocol.lisp" (d:make-tree-file v4 :type :lisp))))
             (diff (d:compute-diff tree-e a b)))
        (say "first leaf footprints: ~{~A~^, ~}"
             (subseq (d:diff-footprint diff) 0 (min 3 (length (d:diff-footprint diff)))))
        (show-text "render :unified" (d:render-diff diff :unified nil))
        (check "apply-diff a diff state-equal b" (d:state-equal tree-e b (d:apply-diff a diff)))))

    (demo-step ("5g. a side that does not read clean: lisp-source-unreadable, use-text-diff restart")
      (let ((broken (replace-once v3 *kill-0.3* "(defun kill (handle &key force)
  (process-kill handle :force force)")))
        (let ((diff (handler-bind ((dl:lisp-source-unreadable
                                     (lambda (c)
                                       (say "signalled ~S: ~A" (type-of c) c)
                                       (say "diagnostics: ~D" (length (dl:lisp-source-unreadable-diagnostics c)))
                                       (say "restarts: ~{~(~A~)~^ ~}"
                                            (remove nil (mapcar #'restart-name (compute-restarts c))))
                                       (dl:use-text-diff c))))
                      (d:compute-diff (lisp-engine) v3 broken))))
          (say "fell back to engine ~S" (d:engine-name (d:diff-engine diff)))
          (show-text "render :unified" (d:render-diff diff :unified nil)))))))
