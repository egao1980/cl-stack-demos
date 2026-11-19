(in-package #:harness-demos-data)

;;; Demo 1 — diff-protocol 0.2.0: text, diff3 merge, trees, git unified, sexp.

(defun lines (&rest ls)
  "String of LS, each followed by a newline."
  (format nil "~{~A~%~}" ls))

(defun change-shape (c)
  (list (d:change-kind c) (d:change-path c) (d:change-from c) (d:change-footprint c)))

(defun show-changes (label diff)
  (say "~A (~D change~:P, level ~S, engine ~(~A~)):" label
       (length (d:diff-changes diff))
       (d:diff-level diff) (and (d:diff-engine diff) (d:engine-name (d:diff-engine diff))))
  (dolist (c (d:diff-changes diff))
    (say "  ~S" (change-shape c))))

(defun show-conflicts (conflicts)
  (say "conflicts: ~D" (length conflicts))
  (dolist (c conflicts)
    (say "  conflict footprint ~S: ours ~S / theirs ~S" (d:conflict-footprint c)
         (d:change-kind (d:conflict-ours c)) (d:change-kind (d:conflict-theirs c)))))

;;; Trees from (path content &key mode) specs, "/"-separated paths.

(defun split-path (path)
  (loop with start = 0
        for pos = (position #\/ path :start start)
        collect (subseq path start pos)
        while pos do (setf start (1+ pos))))

(defun spec-tree (specs)
  "→ a diff-protocol tree from SPECS, a list of (path content &key mode)."
  (labels ((build (items)
             (let ((names (remove-duplicates (mapcar (lambda (it) (first (first it))) items)
                                             :test #'string= :from-end t)))
               (apply #'d:make-tree
                      (loop for name in names
                            for mine = (remove name items :key (lambda (it) (first (first it)))
                                                          :test-not #'string=)
                            for leaf = (find-if (lambda (it) (null (rest (first it)))) mine)
                            nconc (list name
                                        (if leaf
                                            (apply #'d:make-tree-file (second leaf) (cddr leaf))
                                            (build (mapcar (lambda (it)
                                                             (cons (rest (first it)) (rest it)))
                                                           mine)))))))))
    (build (mapcar (lambda (s) (cons (split-path (first s)) (rest s))) specs))))

(defun tree-engine () (d:find-diff-engine :tree))
(defun text-engine () (d:find-diff-engine :text))
(defun sexp-engine () (d:find-diff-engine :sexp))

;;; git helpers (the demo drives the real git CLI)

(defun project-dir ()
  (asdf:system-source-directory "harness-demos-data"))

(defun git (dir &rest args)
  "Run git in DIR → its standard output. Signals on a non-zero exit."
  (uiop:run-program (list* "git" "-C" (uiop:native-namestring dir)
                           "-c" "user.name=demo" "-c" "user.email=demo@example.invalid"
                           "-c" "commit.gpgsign=false" "-c" "core.autocrlf=false"
                           args)
                    :output :string :error-output :string))

(defun write-spec-files (dir specs)
  (dolist (s specs)
    (destructuring-bind (path content &key mode) s
      (let ((file (merge-pathnames path dir)))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :supersede
                                  :external-format :utf-8)
          (write-string content out))
        (uiop:run-program (list "chmod" (if (eql mode #o100755) "755" "644")
                                (uiop:native-namestring file)))))))

(defparameter *git-before*
  `(("README" ,(lines "Demo repo" "" "Line three."))
    ("src/util.lisp" ,(lines "(defun one () 1)" "(defun two () 2)" "(defun three () 3)"
                             "(defun four () 4)"))
    ("run.sh" ,(lines "#!/bin/sh" "echo run"))
    ("old.txt" ,(lines "obsolete"))))

(defparameter *git-after*
  `(("README" ,(lines "Demo repo" "" "Line three, edited."))
    ("lib/util.lisp" ,(lines "(defun one () 1)" "(defun two () 2)" "(defun three () 3)"
                             "(defun four () 44)"))
    ("run.sh" ,(lines "#!/bin/sh" "echo run") :mode #o100755)
    ("docs/new.md" ,(lines "# New" "" "Added file."))))

(defun demo-diff ()
  (banner "Demo 1: diff-protocol 0.2.0")

  (demo-step ("1a. text diff of two files at :changes, rendered :unified")
    (let* ((a (lines "alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota"))
           (b (lines "alpha" "BETA" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa"))
           (diff (d:compute-diff (text-engine) a b :level :changes))
           (ctx (d:compute-diff (text-engine) a b :level :changes :context 3)))
      (show-changes "context 0 hunks" diff)
      (show-text "render :unified (context 0)" (d:render-diff diff :unified nil))
      (show-text "render :unified (:context 3, git-style hunk merge)" (d:render-diff ctx :unified nil))
      (check "apply-diff a diff = b" (string= b (d:apply-diff a diff)))
      (check "apply-diff b (invert-diff diff) = a" (string= a (d:apply-diff b (d:invert-diff diff))))))

  (demo-step ("1b. three-way merge with a real conflict: merge-texts")
    (let ((base (lines "title: Demo" "owner: alice" "port: 8080" "debug: false" "end"))
          (ours (lines "title: Demo" "owner: bob" "port: 8080" "debug: false" "end"))
          (theirs (lines "title: Demo" "owner: carol" "port: 8080" "debug: true" "end")))
      (multiple-value-bind (merged conflicts) (d:merge-texts base ours theirs)
        (show-text "merge-texts (default :merge style)" merged)
        (show-conflicts conflicts))
      (multiple-value-bind (merged conflicts)
          (d:merge-texts base ours theirs :style :diff3 :ours-label "HEAD" :theirs-label "feature")
        (show-text "merge-texts :style :diff3 (custom labels)" merged)
        (say "conflicts: ~D" (length conflicts)))
      (multiple-value-bind (diff conflicts) (d:three-way-diff (text-engine) base ours theirs)
        (show-changes "three-way-diff merged diff (non-conflicting part)" diff)
        (show-text "base + merged diff (conflict region keeps base)" (d:apply-diff base diff))
        (show-conflicts conflicts))))

  (demo-step ("1c. tree diff: rename detection and a mode change")
    (let* ((a (spec-tree `(("src/x.lisp" ,(lines "(a)" "(b)" "(c)" "(d)"))
                           ("run.sh" ,(lines "#!/bin/sh" "echo hi"))
                           ("keep.txt" ,(lines "same")))))
           (b (spec-tree `(("lib/x.lisp" ,(lines "(a)" "(b)" "(c)" "(D)"))
                           ("run.sh" ,(lines "#!/bin/sh" "echo hi") :mode #o100755)
                           ("keep.txt" ,(lines "same")))))
           (diff (d:compute-diff (tree-engine) a b)))
      (say "rename threshold ~S, detect renames ~S"
           (d:tree-engine-rename-threshold (tree-engine)) (d:tree-engine-detect-renames (tree-engine)))
      (show-changes "tree diff" diff)
      (show-text "render :summary" (d:render-diff diff :summary nil))
      (show-text "render :unified" (d:render-diff diff :unified nil))
      (check "apply-diff a diff state-equal b"
             (d:state-equal (tree-engine) b (d:apply-diff a diff)))))

  (demo-step ("1d. parse-diff :unified on real `git diff` output, then apply-diff")
    (let ((dir (merge-pathnames "work/git-demo/" (project-dir))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (git dir "init" "-q" "-b" "main")
             (write-spec-files dir *git-before*)
             (git dir "add" "-A")
             (git dir "commit" "-q" "-m" "before")
             (delete-file (merge-pathnames "src/util.lisp" dir))
             (delete-file (merge-pathnames "old.txt" dir))
             (write-spec-files dir *git-after*)
             (git dir "add" "-A")
             (git dir "commit" "-q" "-m" "after")
             (let* ((text (git dir "diff" "-M" "HEAD~1" "HEAD"))
                    (parsed (d:parse-diff :unified text))
                    (before (spec-tree *git-before*))
                    (after (spec-tree *git-after*)))
               (say "git version: ~A" (string-trim '(#\Newline) (uiop:run-program '("git" "--version") :output :string)))
               (show-text "git diff -M HEAD~1 HEAD" text)
               (say "parsed engine: ~(~A~)" (d:engine-name (d:diff-engine parsed)))
               (show-changes "parsed diff" parsed)
               (let ((applied (d:apply-diff before parsed)))
                 (check "apply-diff before parsed state-equal after (the new tree)"
                        (d:state-equal (tree-engine) after applied))
                 (check "apply-diff after (invert-diff parsed) state-equal before"
                        (d:state-equal (tree-engine) before
                                       (d:apply-diff after (d:invert-diff parsed)))))
               (show-text "re-render of the parsed diff :unified"
                          (d:render-diff parsed :unified nil))))
        (uiop:delete-directory-tree (merge-pathnames "work/" (project-dir))
                                    :validate t :if-does-not-exist :ignore))))

  (demo-step ("1e. sexp diff, invert-diff, apply-diff round trip")
    (let* ((a '(:name "svc" :replicas 2 :ports (80 443) :env (:mode "prod")))
           (b '(:name "svc" :replicas 3 :ports (80 443 8443) :env (:mode "prod" :trace t)))
           (diff (d:compute-diff (sexp-engine) a b))
           (inverse (d:invert-diff diff)))
      (show-changes "sexp diff" diff)
      (show-text "render :summary" (d:render-diff diff :summary nil))
      (say "render :sexp => ~A" (d:render-diff diff :sexp nil))
      (show-changes "invert-diff" inverse)
      (say "apply-diff a diff => ~S" (d:apply-diff a diff))
      (say "apply-diff b inverse => ~S" (d:apply-diff b inverse))
      (check "round trip a -> b -> a state-equal"
             (d:state-equal (sexp-engine) a (d:apply-diff (d:apply-diff a diff) inverse)))
      (check "parse-diff :sexp of render :sexp applies the same"
             (d:state-equal (sexp-engine) b
                            (d:apply-diff a (d:parse-diff :sexp (d:render-diff diff :sexp nil)))))))

  (demo-step ("1f. diff-protocol#2: list index shift merges cleanly")
    (let ((base '("x" "y")) (ours '("x")) (theirs '("x" "y" "z")))
      (say "base ~S, ours ~S (remove y), theirs ~S (append z)" base ours theirs)
      (multiple-value-bind (diff conflicts) (d:three-way-diff (sexp-engine) base ours theirs)
        (show-changes "merged diff" diff)
        (show-conflicts conflicts)
        (let ((merged (d:apply-diff base diff)))
          (say "apply-diff base merged => ~S" merged)
          (check "merged = (\"x\" \"z\") with no conflicts"
                 (and (null conflicts) (equal merged '("x" "z")))))))
    (let ((base '(:l ("x" "y") :n 1)) (ours '(:l ("x") :n 1)) (theirs '(:l ("x" "y" "z") :n 2)))
      (say "nested: base ~S, ours ~S, theirs ~S" base ours theirs)
      (multiple-value-bind (diff conflicts) (d:three-way-diff (sexp-engine) base ours theirs)
        (say "merged => ~S, conflicts ~D" (d:apply-diff base diff) (length conflicts))))))
