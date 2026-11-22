(in-package #:harness-demos-isolation)

;;; Demo 1: R4c copy-on-write clones. cl-stack-pathlib 0.10.0 FS-CLONE-TREE
;;; with the mechanism fs-backend-darwin 0.2.0 registers (:CLONEFILE, APFS
;;; clonefile(2)) and with the portable :COPY rung, on the same work tree:
;;; the same tree comes out, the same edits give the same change set, and the
;;; original is never touched. Then areas, which are built on FS-CLONE-TREE.

(defparameter *blob-size* (* 32 1024 1024)
  "Size of data/blob.bin in the work tree: big enough for the clone/copy timings to differ.")

(defparameter *small-files* 300)

(defun blob-octets ()
  (let ((v (make-array *blob-size* :element-type '(unsigned-byte 8))))
    (dotimes (i *blob-size* v)
      (setf (aref v i) (mod (* i 31) 251)))))

(defun write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
    (write-sequence octets s))
  pathname)

(defun make-work-tree (fs base)
  "A small project: 300 source files, a 32 MiB data file, an executable script,
a symlink (not on Windows) and an ignored build directory."
  (dotimes (i *small-files*)
    (write-file (merge-pathnames (format nil "src/m~3,'0d.lisp" i) base)
                (lines (format nil "(defun m~3,'0d () ~d)" i i))))
  (write-octets (merge-pathnames "data/blob.bin" base) (blob-octets))
  (write-file (merge-pathnames "tools/gen.sh" base) (lines "#!/bin/sh" "echo generated"))
  (pl:fs-chmod fs (merge-pathnames "tools/gen.sh" base) #o755)
  (write-file (merge-pathnames "README.md" base) (lines "# work tree"))
  (write-file (merge-pathnames ".gitignore" base) (lines "build/"))
  (write-file (merge-pathnames "build/out.o" base) (lines "object code"))
  (unless (windows-p)
    (pl:fs-create-symlink fs (merge-pathnames "readme-link" base) "README.md")))

(defun edit-tree (fs dir)
  "The same five edits, applied to a clone or an area: a source edit, a delete,
a new file, an execute bit, and one byte changed in the middle of the 32 MiB
file (on a clone, only that block stops being shared)."
  (write-file (merge-pathnames "src/m001.lisp" dir) (lines "(defun m001 () :edited)"))
  (pl:fs-unlink fs (merge-pathnames "src/m002.lisp" dir))
  (write-file (merge-pathnames "NOTES.md" dir) (lines "added in the clone"))
  (pl:fs-chmod fs (merge-pathnames "src/m003.lisp" dir) #o755)
  (with-open-file (s (merge-pathnames "data/blob.bin" dir) :direction :io :if-exists :overwrite
                                                           :element-type '(unsigned-byte 8))
    (file-position s (floor *blob-size* 2))
    (write-byte 0 s)))

(defun change-key (c)
  "Everything an FS-CHANGE says, as a list (for EQUAL across mechanisms)."
  (list (pl:fs-change-kind c) (pl:fs-change-path c)
        (pl:fs-change-old-mode c) (pl:fs-change-new-mode c)
        (pl:fs-change-old-hash c) (pl:fs-change-new-hash c)))

(defun show-changes (changes)
  (dolist (c changes)
    (note "  ~13a ~16a ~@[~a~]~@[ -> ~a~]~@[  mode ~a~]~@[ -> ~a~]"
          (pl:fs-change-kind c) (pl:fs-change-path c)
          (short (pl:fs-change-old-hash c) 15) (short (pl:fs-change-new-hash c) 15)
          (and (eq (pl:fs-change-kind c) :mode-changed) (pl:fs-change-old-mode c))
          (and (eq (pl:fs-change-kind c) :mode-changed) (pl:fs-change-new-mode c)))))

(defun show-report (label report ms)
  (note "~a: ~s in ~d ms" label report ms)
  (note "  mechanism ~s, reason ~s, cloned ~d, copied ~d, fallbacks ~s"
        (pl:clone-report-mechanism report) (pl:clone-report-reason report)
        (pl:clone-report-cloned report) (pl:clone-report-copied report)
        (pl:clone-report-fallbacks report)))

(defun clone-into (fs base root name mechanism &key ignore)
  "FS-CLONE-TREE BASE → ROOT/NAME/ with MECHANISM, timed. → (values dir report ms)."
  (let* ((dst (merge-pathnames (format nil "~a/" name) root))
         (start (get-internal-real-time))
         (report (pl:fs-clone-tree fs base dst :mechanism mechanism :ignore ignore))
         (ms (ms-since start)))
    (values dst report ms)))

(defun demo-clone ()
  (banner "Demo 1: R4c COW clones — cl-stack-pathlib 0.10.0 fs-clone-tree, fs-backend-darwin 0.2.0 :clonefile vs the :copy rung")
  (with-temp-dir (root "hd-clone")
    (let* ((fs (pl:make-local-filesystem))
           (base (merge-pathnames "work/" root)))
      (make-work-tree fs base)
      (stage "mechanisms")
      (show "registered, best first (+ :copy)" (pl:clone-mechanisms))
      (show "available for this temp directory" (pl:clone-mechanisms fs base root))
      (multiple-value-bind (m reason) (pl:fs-clone-supported-p fs base)
        (show "fs-clone-supported-p (local, temp dir)" (list m reason)))
      (multiple-value-bind (m reason) (pl:fs-clone-supported-p (pl:make-memory-filesystem) #p"/")
        (show "fs-clone-supported-p (memory filesystem)" (list m reason)))
      (let* ((native-mechanism (pl:fs-clone-supported-p fs base))
             (snap0 (pl:snapshot base))
             (runs '()))
        (note (concatenate 'string "work tree: ~d source files, data/blob.bin ~d MiB, tools/gen.sh #o755, "
                           "~:[readme-link -> README.md, ~;~].gitignore with build/")
              *small-files* (floor *blob-size* (* 1024 1024)) (windows-p))
        (show "snapshot of the original" (short snap0 23))

        (stage "fs-clone-tree with each mechanism (timed; DESTINATION absent, created)")
        (if native-mechanism
            (multiple-value-bind (dir report ms) (clone-into fs base root "clone" native-mechanism)
              (show-report (format nil "~s" native-mechanism) report ms)
              (push (list native-mechanism dir) runs))
            (note "no clone mechanism here (~s): only the :copy rung runs"
                  (nth-value 1 (pl:fs-clone-supported-p fs base))))
        (multiple-value-bind (dir report ms) (clone-into fs base root "copy" :copy)
          (show-report ":copy" report ms)
          (push (list :copy dir) runs))
        (setf runs (nreverse runs))
        (multiple-value-bind (dir report ms)
            (clone-into fs base root "auto-ignore" :auto :ignore :auto)
          (show-report ":auto :ignore :auto" report ms)
          (show "  build/ left out" (not (probe-file (merge-pathnames "build/out.o" dir))))
          (show "  tree = snapshot of the original with :ignore :auto"
                (equal (pl:snapshot dir) (pl:snapshot base :ignore :auto))))
        (handler-case (pl:fs-clone-tree fs base (merge-pathnames "strict/" root) :mechanism :ficlone)
          (pl:unsupported-operation (c)
            (note ":mechanism :ficlone (strict, Linux only) → ~s" (type-of c))
            (note "  ~a" c)
            (show "  strict/ created" (and (probe-file (merge-pathnames "strict/" root)) t))))

        (stage "each clone is the original tree (content, modes, symlinks: same snapshot id)")
        (dolist (run runs)
          (show (format nil "~s clone snapshot = original" (first run))
                (equal (pl:snapshot (second run)) snap0)))
        (let ((f "data/blob.bin"))
          (show "mtime kept (clone vs original, data/blob.bin)"
                (loop for run in runs
                      collect (= (file-write-date (merge-pathnames f (second run)))
                                 (file-write-date (merge-pathnames f base))))))

        (stage "the same edits in each clone")
        (let ((change-sets '()))
          (dolist (run runs)
            (let ((start (get-internal-real-time)))
              (edit-tree fs (second run))
              (note "~s clone edited in ~d ms" (first run) (ms-since start)))
            (let ((changes (pl:snapshot-changes snap0 (pl:snapshot (second run)))))
              (note "changes in the ~s clone (snapshot-changes original → clone):" (first run))
              (show-changes changes)
              (push (mapcar #'change-key changes) change-sets)))
          (show "same change set for every mechanism"
                (every (lambda (s) (equal s (first change-sets))) change-sets)))

        (stage "the original is untouched")
        (show "snapshot of the original now = before" (equal (pl:snapshot base) snap0))
        (show "original src/m001.lisp" (read-file (merge-pathnames "src/m001.lisp" base)))
        (show "original src/m002.lisp still there" (and (probe-file (merge-pathnames "src/m002.lisp" base)) t))
        (show "original NOTES.md absent" (not (probe-file (merge-pathnames "NOTES.md" base))))

        (stage "areas are built with fs-clone-tree (open-area :mechanism :auto vs :copy)")
        (let ((change-sets '()))
          (dolist (mechanism '(:auto :copy))
            (let* ((start (get-internal-real-time))
                   (a (pl:fs-open-area fs base :mechanism mechanism :ignore :auto))
                   (ms (ms-since start)))
              (unwind-protect
                   (let ((report (pl:area-clone-report a)))
                     (note "open-area ~s → area-mechanism ~s (~s, cloned ~d, copied ~d) in ~d ms"
                           mechanism (pl:area-mechanism a) (pl:clone-report-mechanism report)
                           (pl:clone-report-cloned report) (pl:clone-report-copied report) ms)
                     (edit-tree fs (pl:path-pathname (pl:area-root a)))
                     (let ((changes (pl:area-changes a)))
                       (note "  area-changes: ~{~a~^, ~}"
                             (mapcar (lambda (c) (format nil "~(~a~) ~a" (pl:fs-change-kind c)
                                                         (pl:fs-change-path c)))
                                     changes))
                       (push (mapcar #'change-key changes) change-sets)))
                (pl:close-area a :action :discard))))
          (show "same fs-area-changes for :clone and :copy areas"
                (equal (first change-sets) (second change-sets)))
          (show "original unchanged after both areas" (equal (pl:snapshot base) snap0)))))))
