(in-package #:harness-demos-fs)

;;; Demo 7: R4b change journals — cl-stack-pathlib 0.9.0 (fs-watch, journal-drain,
;;; areas with :journal, the journal suite) over fs-backend-darwin 0.1.0 (:fsevents)
;;; and fs-backend-linux 0.1.0 (:inotify). Both backends are loaded on every OS;
;;; each is available only on its own, so :auto gives :fsevents on macOS,
;;; :inotify on Linux and :poll elsewhere (Windows until fs-backend-windows).

(defun ms-since (start)
  "Milliseconds since the internal real time START, to 0.1 ms."
  (/ (round (* 10000 (- (get-internal-real-time) start)) internal-time-units-per-second) 10.0))

(defmacro timed ((ms-var) form &body body)
  "Evaluate FORM, then BODY with IT bound to its value and MS-VAR to its wall
time in ms."
  (let ((start (gensym "START")))
    `(let* ((,start (get-internal-real-time))
            (it ,form)
            (,ms-var (ms-since ,start)))
       (declare (ignorable it))
       ,@body)))

(defun change-details (changes)
  "Every field of each fs-change (the law compares them all)."
  (mapcar (lambda (c) (list (pl:fs-change-kind c) (pl:fs-change-path c)
                            (pl:fs-change-old-mode c) (pl:fs-change-new-mode c)
                            (pl:fs-change-old-hash c) (pl:fs-change-new-hash c)))
          changes))

(defvar *law-breaks* nil
  "Steps of this demo where the journal answer differed from the full rescan.")

(defun law-check (label area)
  "Full rescan first (it leaves the journal and the area state alone), then the
journal call; print both timings and whether the results are EQUAL.
→ (values changes rescan-ms journal-ms)."
  (timed (rescan-ms) (pl:area-changes area :rescan t)
    (let ((rescan it))
      (timed (journal-ms) (pl:area-changes area)
        (let ((equal-p (equal (change-details it) (change-details rescan))))
          (unless equal-p (push label *law-breaks*))
          (note "~a: ~d change~:p; full rescan ~,1f ms, journal ~,1f ms; EQUAL: ~a"
                label (length it) rescan-ms journal-ms equal-p)
          (values it rescan-ms journal-ms))))))

(defun inject-incomplete (journal)
  "Deliver an 'events were lost' event to JOURNAL through its backend's test
hook (FSEvents MustScanSubDirs, inotify IN_Q_OVERFLOW). → T, or NIL when the
mechanism has none (:poll is incomplete on every drain anyway)."
  (case (pl:journal-mechanism journal)
    (:fsevents
     (fs-backend-darwin:fsevents-journal-inject
      journal (list (native (pl:path-pathname (pl:journal-root journal))))
      (list fs-backend-darwin:+event-flag-must-scan-sub-dirs+))
     t)
    (:inotify
     (fs-backend-linux:inject-inotify-event journal fs-backend-linux:+in-q-overflow+ :wd -1)
     t)
    (t nil)))

(defun os-name ()
  (cond ((uiop:os-macosx-p) :macos) ((uiop:os-windows-p) :windows)
        ((uiop:featurep :linux) :linux) (t :other)))

;;; 7a: which mechanism :auto picks.

(defun demo-journal-mechanisms (root)
  (stage "the mechanism registry: what :auto picks on this OS")
  (let ((fs pl:*filesystem*))
    (show "OS" (os-name))
    (show "registered ranking, best first (watch-mechanisms)" (pl:watch-mechanisms))
    (show "available for a local temp directory" (pl:watch-mechanisms fs root))
    (show "available on a memory filesystem" (pl:watch-mechanisms (pl:make-memory-filesystem) "/"))
    (let ((j (pl:fs-watch fs root)))
      (unwind-protect
           (progn (show "fs-watch :auto → journal-mechanism" (pl:journal-mechanism j))
                  (show "journal-open-p" (pl:journal-open-p j)))
        (pl:journal-close j))
      (show "journal-open-p after journal-close" (pl:journal-open-p j))
      (show "journal-close again (idempotent)" (pl:journal-close j))
      (handler-case (pl:journal-drain j)
        (pl:journal-error (c)
          (show "journal-drain after close" (type-of c)))))
    (dolist (m '(:fsevents :inotify))
      (unless (member m (pl:watch-mechanisms fs root))
        (handler-case (progn (pl:journal-close (pl:fs-watch fs root :mechanism m))
                             (note "UNEXPECTED: fs-watch :mechanism ~s worked here" m))
          (pl:unsupported-operation (c)
            (show (format nil "fs-watch :mechanism ~s here (unavailable)" m) (type-of c))))))))

;;; 7b: a watched tree, a scripted edit, one drain.

(defun demo-journal-drain (root)
  (stage "fs-watch a tree, edit it (pathlib, CL rename, a child process), journal-drain")
  (let ((dir (merge-pathnames "watched/" root)))
    (write-file (merge-pathnames "a/one.txt" dir) (lines "one"))
    (write-file (merge-pathnames "a/two.txt" dir) (lines "two"))
    (write-file (merge-pathnames "a/untouched.txt" dir) (lines "never edited"))
    (write-file (merge-pathnames "keep.txt" dir) (lines "to be deleted"))
    (ensure-directories-exist (merge-pathnames "b/" dir))
    (let ((j (pl:fs-watch pl:*filesystem* dir)))
      (unwind-protect
           (progn
             (show "journal-mechanism" (pl:journal-mechanism j))
             (note "edits: create new.txt; modify a/one.txt; rename a/two.txt → b/two.txt;")
             (note "       delete keep.txt; mkdir c/d/ with c/d/x.txt; a child sh (process-protocol)")
             (note "       writes child.txt and makes e/ with e/f.txt")
             (pl:write-text (merge-pathnames "new.txt" dir) (lines "new"))
             (pl:write-text (merge-pathnames "a/one.txt" dir) (lines "one, edited"))
             (rename-file (merge-pathnames "a/two.txt" dir) (merge-pathnames "b/two.txt" dir))
             (delete-file (merge-pathnames "keep.txt" dir))
             (write-file (merge-pathnames "c/d/x.txt" dir) (lines "x"))
             (sh dir "printf child > child.txt && mkdir e && printf f > e/f.txt")
             (timed (ms) (multiple-value-list (pl:journal-drain j))
               (destructuring-bind (paths complete-p) it
                 (show "drained paths" paths)
                 (show "complete-p" complete-p)
                 (show "drain wall time, ms" ms)
                 (if complete-p
                     (show "a/untouched.txt reported (expected NIL)"
                           (and (member "a/untouched.txt" paths :test #'string=) t))
                     (note "complete-p NIL: this mechanism cannot tell; the caller rescans"))))
             (show "second drain with no edits in between (paths complete-p)"
                   (multiple-value-list (pl:journal-drain j))))
        (pl:journal-close j)))))

;;; 7c: the law on ~2000 files — journal vs full rescan.

(defparameter *tree-dirs* 20)
(defparameter *tree-files-per-dir* 100)

(defun make-big-tree (base)
  (dotimes (d *tree-dirs*)
    (dotimes (f *tree-files-per-dir*)
      (write-file (merge-pathnames (format nil "d~2,'0d/f~3,'0d.txt" d f) base)
                  (format nil "dir ~d file ~d~%" d f)))))

(defun demo-journal-area (root)
  (stage "an area with :journal :auto over ~d files: fs-area-changes vs :rescan t"
         (* *tree-dirs* *tree-files-per-dir*))
  (let ((base (merge-pathnames "big/" root)))
    (make-big-tree base)
    (let ((floor (drain-floor-ms base)))
      (show "drain floor: an idle journal-drain on this mechanism, ms (min of 3)" floor)
      (demo-journal-area-rounds base floor))))

(defun drain-floor-ms (dir)
  "Wall time of an empty journal-drain on DIR with the :auto mechanism (the
mechanism's fixed wait, e.g. FSEvents' flush window), min of 3, in ms."
  (let ((j (pl:fs-watch pl:*filesystem* dir)))
    (unwind-protect
         (loop repeat 3
               minimize (timed (ms) (pl:journal-drain j) ms))
      (pl:journal-close j))))

(defun demo-journal-area-rounds (base floor)
  (progn
    (let ((area (pl:open-area base :journal :auto)))
      (unwind-protect
           (let ((aroot (pl:path-pathname (pl:area-root area)))
                 (j (pl:area-journal area))
                 (rescan-total 0) (journal-total 0))
             (show "area-journal mechanism" (and j (pl:journal-mechanism j)))
             (unless j
               (note "no journal attached (only :poll here): fs-area-changes is the R4a rescan"))
             (law-check "first call (absorbs the copy's own events)" area)
             (loop for round from 1 to 3
                   do (pl:write-text (merge-pathnames (format nil "d~2,'0d/f007.txt" round) aroot)
                                     (format nil "edited in round ~d~%" round))
                      (pl:write-text (merge-pathnames (format nil "d~2,'0d/new-~d.txt" (+ 10 round) round) aroot)
                                     "new")
                      (delete-file (merge-pathnames (format nil "d~2,'0d/f050.txt" (+ 5 round)) aroot))
                      (rename-file (merge-pathnames (format nil "d~2,'0d/f060.txt" (+ 15 round)) aroot)
                                   (merge-pathnames (format nil "d00/moved-~d.txt" round) aroot))
                      (sh aroot (format nil "printf child >> d19/f0~d0.txt" round))
                      (multiple-value-bind (changes rescan-ms journal-ms)
                          (law-check (format nil "round ~d (5 edits, cumulative)" round) area)
                        (declare (ignore changes))
                        (incf rescan-total rescan-ms)
                        (incf journal-total journal-ms)))
             (law-check "no edits since the last call" area)
             (note "rounds 1-3 total: full rescan ~,1f ms, journal ~,1f ms (~,1fx)"
                   rescan-total journal-total
                   (if (plusp journal-total) (/ rescan-total journal-total) 0))
             (when j
               (note "journal minus 3 drain floors (the narrowed re-examination itself): ~,1f ms vs ~,1f ms"
                     (- journal-total (* 3 floor)) rescan-total))
             (note "final changes:")
             (mapc #'show-change (pl:area-changes area)))
        (pl:close-area area :action :discard))
      (show "journal closed with the area"
            (let ((j (pl:area-journal area))) (if j (not (pl:journal-open-p j)) :no-journal))))))

;;; 7d: incomplete answers — :poll, and an injected overflow on the active mechanism.

(defun demo-journal-incomplete (root)
  (stage "incomplete drains (complete-p NIL) → a full rescan, the same answer")
  (let ((base (merge-pathnames "small/" root)))
    (write-file (merge-pathnames "x.txt" base) (lines "x"))
    (write-file (merge-pathnames "sub/y.txt" base) (lines "y"))
    (let ((j (pl:fs-watch pl:*filesystem* base :mechanism :poll)))
      (unwind-protect
           (progn
             (pl:write-text (merge-pathnames "z.txt" base) "z")
             (show "fs-watch :mechanism :poll, after an edit: (paths complete-p)"
                   (multiple-value-list (pl:journal-drain j))))
        (pl:journal-close j)))
    (let ((area (pl:open-area base :journal :poll)))
      (unwind-protect
           (let ((aroot (pl:path-pathname (pl:area-root area))))
             (show "area-journal mechanism" (pl:journal-mechanism (pl:area-journal area)))
             (pl:write-text (merge-pathnames "sub/y.txt" aroot) "y edited")
             (pl:write-text (merge-pathnames "w.txt" aroot) "w")
             (mapc #'show-change (law-check ":poll area" area)))
        (pl:close-area area)))
    (let* ((area (pl:open-area base :journal :auto))
           (j (pl:area-journal area)))
      (unwind-protect
           (let ((aroot (pl:path-pathname (pl:area-root area))))
             (cond
               ((not j) (note "no OS journal here: nothing to inject into"))
               (t
                (pl:area-changes area)
                (pl:write-text (merge-pathnames "x.txt" aroot) "x edited")
                (show (format nil "inject an overflow into the ~s journal" (pl:journal-mechanism j))
                      (inject-incomplete j))
                (pl:write-text (merge-pathnames "sub/v.txt" aroot) "v")
                (mapc #'show-change (law-check "after the injected overflow" area))
                (pl:write-text (merge-pathnames "sub/u.txt" aroot) "u")
                (law-check "next call (complete again)" area)
                (show "a raw drain on a fresh journal after an injected overflow"
                      (let ((j2 (pl:fs-watch pl:*filesystem* aroot)))
                        (unwind-protect
                             (progn (inject-incomplete j2)
                                    (list (multiple-value-list (pl:journal-drain j2))
                                          (multiple-value-list (pl:journal-drain j2))))
                          (pl:journal-close j2)))))))
        (pl:close-area area)))))

;;; 7e: the journal suite for the active mechanism.

(defun demo-journal-suite ()
  (let ((mechanism (first (pl:watch-mechanisms pl:*filesystem* (uiop:temporary-directory)))))
    (stage "cl-stack-pathlib/journal-suite: run-journal-suite ~s" mechanism)
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (failures steps)
          (cl-stack-pathlib/journal-suite:run-journal-suite mechanism)
        (show "steps run" (length steps))
        (show "steps" steps)
        (show "failures" failures)
        (show "elapsed seconds" (elapsed-since start))
        (note "suite ~:[FAILED~;passed~] for ~s: ~d/~d steps keep the law"
              (null failures) mechanism
              (- (length steps) (length (remove-duplicates (mapcar (lambda (f) (getf f :step)) failures))))
              (length steps))
        (when failures (push :journal-suite *law-breaks*))))))

(defun demo-journal ()
  (banner "Demo 7: cl-stack-pathlib 0.9.0 change journals (R4b) on fs-backend-darwin / fs-backend-linux 0.1.0")
  (setf *law-breaks* nil)
  (with-temp-dir (root "hd-journal")
    (demo-journal-mechanisms root)
    (demo-journal-drain root)
    (demo-journal-area root)
    (demo-journal-incomplete root))
  (demo-journal-suite)
  (stage "verdict")
  (show "law breaks (journal ≠ full rescan)" (reverse *law-breaks*))
  (when *law-breaks*
    (error "the journal law broke at ~{~a~^, ~}" (reverse *law-breaks*))))
