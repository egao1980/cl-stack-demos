(in-package #:harness-demos-fs)

;;; Demo 5: cl-stack-pathlib 0.6.x — one fnmatch glob matcher on every backend
;;; (local and memory filesystems agree), and probes that follow the final symlink.

(defparameter *glob-files*
  '("file1.txt" "file2.txt" "fileA.txt" "file10.txt" "[x].txt" "km.txt" "m.lisp"
    "notes.md" "src/a.lisp" "src/main.lisp" "src/m/b.lisp" "src/m/deep/c.lisp"))

(defparameter *glob-patterns*
  '(("glob" "file?.txt") ("glob" "file[0-9].txt") ("glob" "file[!0-9].txt") ("glob" "[km]*")
    ("glob" "\\[x\\].txt") ("glob" "[x].txt") ("glob" "*.l*") ("glob" "**/*.lisp")
    ("glob" "src/*/*.lisp") ("rglob" "m*.lisp") ("rglob" "*.lisp")))

(defun posix-under (base path)
  "PATH's POSIX string relative to BASE's (both PATH designators)."
  (let ((b (pl:as-posix base)) (p (pl:as-posix path)))
    (string-left-trim "/" (if (uiop:string-prefix-p b p) (subseq p (length b)) p))))

(defun glob-names (kind base pattern)
  (sort (mapcar (lambda (p) (posix-under base p))
                (if (string= kind "rglob") (pl:rglob base pattern) (pl:glob base pattern)))
        #'string<))

(defun populate (base)
  (dolist (f *glob-files*)
    (let ((p (pl:join base f)))
      (pl:mkdir (pl:parent p) :parents t :exist-ok t)
      (pl:write-text p "x"))))

(defun demo-pathlib-globs (root)
  (stage "glob / rglob: the same results on the local and the memory filesystem")
  (let ((local (pl:join (native root) "globs"))
        (results '()))
    (populate local)
    (pl:with-filesystem ((pl:make-memory-filesystem))
      (let ((mem (pl:ensure-path "/globs")))
        (populate mem)
        (loop for (kind pattern) in *glob-patterns*
              do (push (list kind pattern
                             (glob-names kind mem pattern))
                       results))))
    (loop for (kind pattern mem) in (reverse results)
          for loc = (glob-names kind local pattern)
          do (note "~5a ~18s → ~s~:[   LOCAL/MEMORY DIFFER: memory ~s~;~]"
                   kind pattern loc (equal loc mem) mem)))
  (stage "match-p (PurePath.match: trailing segments, or the whole path when absolute)")
  (dolist (case '(("a/b/c.lisp" "b/*.lisp") ("a/b/c.lisp" "*.lisp") ("a/b/c.lisp" "a/*.lisp")
                  ("/a/b/c.lisp" "/a/*/c.lisp") ("/a/b/c.lisp" "/b/c.lisp") ("x/[x].txt" "\\[x\\].txt")))
    (note "match-p ~14s ~14s → ~s" (first case) (second case) (pl:match-p (first case) (second case)))))

(defun probe-row (p)
  "One probe line: exists-p / file-p / directory-p / symlink-p, then size / stat or the condition."
  (flet ((try (fn)
           (handler-case (funcall fn)
             (error (c) (format nil "~a" (type-of c))))))
    (list :exists (pl:exists-p p) :file (pl:file-p p) :dir (pl:directory-p p) :link (pl:symlink-p p)
          :size (try (lambda () (pl:file-size p)))
          :stat (try (lambda () (pl:file-stat-kind (pl:stat p))))
          :lstat (try (lambda () (pl:file-stat-kind (pl:stat p :follow-symlinks nil)))))))

(defun make-links (base)
  (pl:write-text (pl:join base "target.txt") "12345")
  (pl:mkdir (pl:join base "dir") :parents t :exist-ok t)
  (pl:create-symlink (pl:join base "to-file") (pl:join base "target.txt"))
  (pl:create-symlink (pl:join base "to-dir") (pl:join base "dir"))
  (pl:create-symlink (pl:join base "dangling") (pl:join base "missing.txt"))
  (pl:create-symlink (pl:join base "loop-a") (pl:join base "loop-b"))
  (pl:create-symlink (pl:join base "loop-b") (pl:join base "loop-a")))

(defun demo-pathlib-links (root)
  (stage "probes follow the final symlink (stat(2)); symlink-p and :follow-symlinks nil are lstat(2)")
  (let ((local (pl:join (native root) "links"))
        (mem-rows '()))
    (pl:mkdir local :parents t :exist-ok t)
    (make-links local)
    (pl:with-filesystem ((pl:make-memory-filesystem))
      (let ((mem (pl:ensure-path "/links")))
        (pl:mkdir mem :parents t :exist-ok t)
        (make-links mem)
        (dolist (name '("target.txt" "dir" "to-file" "to-dir" "dangling" "loop-a"))
          (push (probe-row (pl:join mem name)) mem-rows))))
    (loop for name in '("target.txt" "dir" "to-file" "to-dir" "dangling" "loop-a")
          for mem in (reverse mem-rows)
          for loc = (probe-row (pl:join local name))
          do (note "~10a ~s" name loc)
             (unless (equal loc mem)
               (note "~10a   MEMORY DIFFERS: ~s" "" mem)))
    (stage "symlink-loop is a path-error")
    (handler-case (pl:file-size (pl:join local "loop-a"))
      (pl:symlink-loop (c)
        (show "signalled" (type-of c))
        (show "  path-error-p" (typep c 'pl:path-error))))))

(defun demo-pathlib-06 ()
  (banner "Demo 5: cl-stack-pathlib 0.6.1 (fnmatch globs on every backend, final-symlink probes)")
  (with-temp-dir (root "hd-pathlib06")
    (demo-pathlib-globs root)
    (demo-pathlib-links root)))
