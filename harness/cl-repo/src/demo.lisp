;;;; Before and after for the cl-repo client: the published 0.27.35 (the fixes
;;;; of cl-repository PR 174) against 0.27.34, each run the way a user runs
;;;; it, through the launcher of its OCI-published client tree, in a fresh
;;;; HOME, against GHCR. The 0.27.35 results with a known answer are checked;
;;;; RUN-DEMOS returns NIL when one differs or a section stops on an error.
;;;; The 0.27.34 runs are printed, not checked: they show the bug as it was.

(in-package #:harness-demos-cl-repo)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results of the fixed client compared with their expected value.")
(defvar *mismatches* 0 "Those that differed.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args)
  (finish-output))

(defun banner (title)
  (out "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (ok label)
  "Count one check of the fixed client and print it. Returns OK."
  (incf *checks*)
  (unless ok (incf *mismatches*))
  (out "  ~:[!! FAILED~;ok~]  ~a~%" ok label)
  ok)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

;;; Places ---------------------------------------------------------------------

(defvar *root* nil "This run's scratch directory (printed as <tmp>).")
(defvar *cache* nil "XDG_CACHE_HOME the runs share, so each client compiles once.")
(defvar *clients-dir* nil "Where scripts/setup.sh put the client trees (printed as <clients>).")
(defvar *roswell-home* nil "Roswell's home: the fresh HOMEs keep using its SBCL.")
(defvar *counter* 0)

(defparameter +new+ "0.27.35" "The client with the fixes.")
(defparameter +old+ "0.27.34" "The client before them.")

(defun native (pathname)
  (uiop:native-namestring pathname))

(defun replace-all (text old new)
  (if (or (null old) (zerop (length old)))
      text
      (with-output-to-string (s)
        (loop with start = 0
              for at = (search old text :start2 start)
              do (write-string text s :start start :end (or at (length text)))
                 (if at
                     (progn (write-string new s) (setf start (+ at (length old))))
                     (return))))))

(defun scrub (text)
  "TEXT with this run's paths replaced: the scratch root by <tmp>/, the client
   trees by <clients>/, the home by ~/ (each also in its /private form)."
  (let ((text text))
    (loop for (dir . name) in (list (cons *root* "<tmp>/")
                                    (cons *clients-dir* "<clients>/")
                                    (cons (user-homedir-pathname) "~/"))
          when dir
            do (let ((plain (native dir))
                     (true (ignore-errors (native (truename dir)))))
                 (dolist (form (remove-duplicates (remove nil (list true plain))
                                                  :test #'string=))
                   (setf text (replace-all text form name)))))
    text))

(defun clean-output (text)
  "TEXT scrubbed (SCRUB), without the note SBCL prints while it builds its
   Unicode names: it has no newline of its own, so it sticks to the start of
   the client's next line."
  (let ((note ";;; Computing Hangul syllable names"))
    (replace-all (replace-all (scrub text) (format nil "~a~%" note) "") note "")))

(defun fresh-dir (name)
  "A new empty directory under *ROOT*."
  (let ((dir (merge-pathnames (format nil "~a-~d/" name (incf *counter*)) *root*)))
    (ensure-directories-exist dir)
    dir))

(defun write-file (pathname text)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (write-string text s))
  pathname)

(defun read-file (pathname)
  (uiop:read-file-string pathname :external-format :utf-8))

(defun text-lines (text)
  (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline)))

;;; The clients ----------------------------------------------------------------

(defstruct client version dest dir)

(defun find-client (version)
  "Client VERSION as scripts/setup.sh installed it (<clients>/VERSION/ holds
   its tree, cl-oci-VERSION/ the client), or NIL."
  (let* ((dest (merge-pathnames (format nil "~a/" version) *clients-dir*))
         (dir (merge-pathnames (format nil "cl-oci-~a/" version) dest)))
    (when (probe-file (merge-pathnames "roswell/cl-repo" dir))
      (make-client :version version :dest dest :dir dir))))

(defun new-p (client)
  (string= (client-version client) +new+))

(defun client-env (client home &key (cache *cache*) extra)
  "A user with HOME and nothing else: XDG directories under it, the client of
   CLIENT, no source registry or output translations from this image."
  (append extra
          (list (cons "HOME" (native home))
                (cons "ROSWELL_HOME" (native *roswell-home*))
                (cons "XDG_CACHE_HOME" (native cache))
                (cons "XDG_DATA_HOME" (native (merge-pathnames ".local/share/" home)))
                (cons "XDG_CONFIG_HOME" (native (merge-pathnames ".config/" home)))
                (cons "CL_REPOSITORY_DEST" (string-right-trim "/" (native (client-dest client))))
                (cons "CL_REPOSITORY_CLIENT_DIR" (native (client-dir client)))
                (cons "CL_SOURCE_REGISTRY" nil)
                (cons "ASDF_OUTPUT_TRANSLATIONS" nil)
                (cons "CL_REPO_DYNAMIC_SPACE_MB" nil))))

(defun launcher-command (client args &key log)
  "/bin/sh running CLIENT's launcher (roswell/cl-repo) with ARGS, stderr into
   stdout, stdin from /dev/null; with LOG, all output into the file whose
   name is in $DEMO_LOG."
  (list* "/bin/sh" "-c"
         (if log
             "exec \"$@\" </dev/null >\"$DEMO_LOG\" 2>&1"
             "exec \"$@\" </dev/null 2>&1")
         "sh" "/bin/sh" (native (merge-pathnames "roswell/cl-repo" (client-dir client)))
         args))

(defun cl-repo (client home project &rest args)
  "Run `cl-repo ARGS` with CLIENT for HOME in PROJECT → (values exit-code
   output), stdout and stderr in one string, paths scrubbed (CLEAN-OUTPUT)."
  (multiple-value-bind (code output)
      (proc:run (launcher-command client args)
                :directory project :env (client-env client home)
                :output :string :error-output :string :timeout 1800)
    (values code (clean-output output))))

(defparameter +noise+
  '("WARNING: System definition file" ";;; Computing Hangul" "  Recorded file manifest")
  "Line starts not worth printing: ASDF's secondary-system warning about a
   dependency's .asd, SBCL building its Unicode names.")

(defun shell-word (word)
  (if (every (lambda (c) (or (alphanumericp c) (find c "-_./:=+@,"))) word)
      word
      (format nil "'~a'" (replace-all word "'" "'\\''"))))

(defun shown-line-p (line)
  "Whether LINE of a client's output is printed: not blank, not a compiler
   line (\"; compiling file …\"; cl-repo's own \"; cl-repo: …\" lines stay),
   not a download line (Pulling, Symlink), not +NOISE+."
  (and (plusp (length (string-trim " " line)))
       (or (not (uiop:string-prefix-p ";" line))
           (uiop:string-prefix-p "; cl-repo" line))
       (not (uiop:string-prefix-p "Pulling " line))
       (not (uiop:string-prefix-p "  Symlink " line))
       (notany (lambda (n) (uiop:string-prefix-p n line)) +noise+)))

(defun show (args code output &key (max 16))
  (let* ((lines (remove-if-not #'shown-line-p (text-lines output)))
         (shown (subseq lines 0 (min max (length lines)))))
    (out "~&  $ cl-repo~{ ~a~}~%" (mapcar #'shell-word args))
    (dolist (line shown) (out "    ~a~%" line))
    (when (> (length lines) max)
      (out "    … ~d more lines~%" (- (length lines) max)))
    (out "    [exit ~a]~%" code)))

(defun run-step (client home project &rest args)
  "Run cl-repo ARGS and print the command and its output → (values code output)."
  (multiple-value-bind (code output) (apply #'cl-repo client home project args)
    (show args code output)
    (values code output)))

(defun lock-entries (project)
  "The entries of PROJECT's cl-repo.lock, plists, or NIL."
  (let ((file (merge-pathnames "cl-repo.lock" project)))
    (when (probe-file file)
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (read-from-string (read-file file) nil nil))))))

(defun lock-entry (project system)
  (find system (lock-entries project)
        :key (lambda (e) (getf e :system)) :test #'string-equal))

(defun show-lock (project)
  (let ((file (merge-pathnames "cl-repo.lock" project)))
    (if (probe-file file)
        (progn (out "  cl-repo.lock:~%")
               (dolist (line (text-lines (read-file file))) (out "    ~a~%" line)))
        (out "  (no cl-repo.lock)~%"))))

(defun installed-versions (home system)
  "Version directories of SYSTEM in HOME's install tree."
  (mapcar (lambda (d) (car (last (pathname-directory d))))
          (ignore-errors
            (uiop:subdirectories
             (merge-pathnames (format nil ".local/share/cl-repository/systems/~a/" system) home)))))

(defun installed-systems (home)
  (mapcar (lambda (d) (car (last (pathname-directory d))))
          (ignore-errors
            (uiop:subdirectories (merge-pathnames ".local/share/cl-repository/systems/" home)))))

(defun each-client (clients fn)
  "Call FN with each of CLIENTS (old first) under a sub-banner."
  (dolist (client clients)
    (out "~&~%-- cl-repo ~a~:[ (before)~; (after)~] --~%"
         (client-version client) (new-p client))
    (funcall fn client)))

;;; #166: cl-repo ros starts the image without Quicklisp -------------------------

(defun demo-quicklisp (client)
  (let ((home (fresh-dir "home"))
        (project (fresh-dir "p166"))
        (form "(format t \"QL=~a~%\" (and (find-package \"QL\") t))"))
    (write-file (merge-pathnames "demo166.asd" project) (format nil "(defsystem \"demo166\")~%"))
    (multiple-value-bind (code output) (run-step client home project "ros" "-e" form "-q")
      (declare (ignore code))
      (when (new-p client)
        (check (search "QL=NIL" output)
               "`cl-repo ros` starts Roswell with +Q: no Quicklisp in the image")
        (if (probe-file (merge-pathnames "lisp/quicklisp/setup.lisp" *roswell-home*))
            (multiple-value-bind (code output)
                (run-step client home project "ros" "-Q" "-e" form "-q")
              (declare (ignore code))
              (check (search "QL=T" output) "a -Q from the user turns Roswell's Quicklisp back on"))
            (out "  (this Roswell has no Quicklisp: the -Q run is skipped)~%"))))))

;;; #163: a lock entry pinned by digest --------------------------------------------

(defun demo-lock-digest (client)
  (let ((home (fresh-dir "home"))
        (project (fresh-dir "p163")))
    (write-file (merge-pathnames "demo163.asd" project)
                (format nil "(defsystem \"demo163\" :depends-on (\"cl-utilities\"))~%"))
    (out "  cl-utilities is published with a commit tag only (cl-stack-systems#80), so it installs as \"latest\".~%")
    (run-step client home project "install" "cl-utilities")
    (run-step client home project "sync")
    (show-lock project)
    (let ((entry (lock-entry project "cl-utilities")))
      (when (new-p client)
        (check (and entry
                    (equal (getf entry :version) "latest")
                    (uiop:string-prefix-p "sha256:" (getf entry :index-digest ""))
                    (equal (getf entry :registry) "https://ghcr.io"))
               "the lock records cl-utilities latest with its digest and registry")))
    (out "~&  A fresh machine: a new HOME, the project and its lock.~%")
    (let ((home-b (fresh-dir "home"))
          (clone (fresh-dir "p163-clone")))
      (dolist (name '("demo163.asd" "cl-repo.lock"))
        (when (probe-file (merge-pathnames name project))
          (write-file (merge-pathnames name clone) (read-file (merge-pathnames name project)))))
      (multiple-value-bind (code output) (run-step client home-b clone "sync" "--locked")
        (when (new-p client)
          (check (eql code 0) "`cl-repo sync --locked` succeeds on the fresh machine")
          (check (search "pulling sha256:" output) "it pulls cl-utilities latest by the lock's digest")
          (check (equal (installed-versions home-b "cl-utilities") '("latest"))
                 "cl-utilities latest is installed there"))))))

;;; #165: a missing minimum is reported as one -------------------------------------

(defun demo-minimum (client)
  (let ((home (fresh-dir "home"))
        (project (fresh-dir "p165")))
    (write-file (merge-pathnames "demo165.asd" project)
                (format nil "(defsystem \"demo165\" :depends-on ((:version \"csv-protocol\" \"0.9.0\")))~%"))
    (out "  demo165 needs csv-protocol 0.9.0 or later; GHCR has older versions only.~%")
    (multiple-value-bind (code output) (run-step client home project "sync")
      (let ((installed (installed-systems home)))
        (out "  installed afterwards: ~:[nothing~;~:*~{~a~^, ~}~]~%" installed)
        (when (new-p client)
          (check (not (eql code 0)) "the sync fails")
          (check (search "csv-protocol 0.9.0 or later is required; the registry has" output)
                 "and says which minimum is missing and what the registry has")
          (check (not (search "not in the registry" output))
                 "not that csv-protocol is missing from the registry")
          (check (null installed) "nothing of the rejected version is installed"))))))

;;; #167: resolution to the newest published version --------------------------------

(defun published-newest (output)
  "The version in \"schema-protocol 0.2.0 is installed and X is published\", or NIL."
  (let* ((key "is installed and ")
         (at (search key output)))
    (when at
      (let* ((start (+ at (length key)))
             (end (position #\Space output :start start)))
        (subseq output start end)))))

(defun demo-newest (client)
  (let ((home (fresh-dir "home"))
        (project (fresh-dir "p167")))
    (write-file (merge-pathnames "demo167.asd" project)
                (format nil "(defsystem \"demo167\" :depends-on (\"schema-protocol\"))~%"))
    (out "  An older schema-protocol is installed first; the project has no lock.~%")
    (run-step client home project "install" "schema-protocol:0.2.0")
    (multiple-value-bind (code output) (run-step client home project "sync")
      (declare (ignore code))
      (let* ((entry (lock-entry project "schema-protocol"))
             (locked (getf entry :version))
             (newest (published-newest output)))
        (out "  locked: schema-protocol ~a; installed: ~{~a~^, ~}~%"
             (or locked "(none)") (installed-versions home "schema-protocol"))
        (when (new-p client)
          (check (and newest (string/= newest "0.2.0"))
                 (format nil "the sync sees that ~a is published" newest))
          (check (and locked (equal locked newest))
                 "and locks that newest published version, not the installed 0.2.0"))))))

;;; #170: a qlfile-only project gets the closure of its entries ------------------------

(defun demo-qlfile (client)
  (let ((home (fresh-dir "home"))
        (project (fresh-dir "p170")))
    (write-file (merge-pathnames "qlfile" project) (format nil "ql schema-protocol-json~%"))
    (out "  The project is one qlfile line, `ql schema-protocol-json`, and no .asd.~%")
    (multiple-value-bind (code output)
        (run-step client home project "ros"
                  "-e" "(asdf:load-system \"schema-protocol-json\")"
                  "-e" "(format t \"LOADED schema-protocol-json ~a~%\" (asdf:component-version (asdf:find-system \"schema-protocol-json\")))"
                  "-q")
      (let ((locked (mapcar (lambda (e) (getf e :system)) (lock-entries project))))
        (out "  locked: ~:[nothing~;~:*~{~a~^, ~}~]~%" locked)
        (when (new-p client)
          (check (and (eql code 0) (search "LOADED schema-protocol-json" output))
                 "schema-protocol-json loads")
          (check (and (member "schema-protocol" locked :test #'string=)
                      (member "closer-mop" locked :test #'string=)
                      (member "schema-protocol-json" locked :test #'string=))
                 "the lock holds the entry and its dependencies (schema-protocol, closer-mop)"))))))

;;; #162: six images, one fasl root --------------------------------------------------

(defun run-parallel (client home project cache n)
  "Start N `cl-repo ros` images loading dexador at once over CACHE → a list
   of (exit-code . log text), one per run (its client process and its
   project image)."
  (let ((handles
         (loop for i from 1 to n
               for log = (merge-pathnames (format nil "image-~d.log" i) project)
               collect (cons log
                             (proc:launch
                              (launcher-command
                               client
                               (list "ros" "-e" "(asdf:load-system \"dexador\")"
                                     "-e" "(format t \"~&LOADED~%\")" "-q")
                               :log t)
                              :directory project
                              :env (client-env client home :cache cache
                                                           :extra (list (cons "DEMO_LOG" (native log)))))))))
    (loop for (log . handle) in handles
          for code = (or (proc:wait handle :timeout 1800)
                         (progn (proc:kill handle :force t) :timeout))
          collect (cons code (if (probe-file log) (read-file log) "")))))

(defun first-error-line (text)
  "The line of TEXT (a run's log) that says why it failed: the first line
   with one of the markers below, in their order, that is neither a compiler
   line nor a backtrace frame. A short line (\"Couldn't load\") gets the next
   one appended."
  (let ((lines (text-lines (clean-output text))))
    (flet ((candidate-p (line)
             (not (or (uiop:string-prefix-p ";" line)
                      (let ((colon (position #\: line)))
                        (and colon (plusp colon)
                             (every #'digit-char-p (string-trim " " (subseq line 0 colon)))))))))
      (loop for marker in '("Failed to find the TRUENAME" "Couldn't load" "Unhandled"
                            "debugger invoked on" "Error" "error")
            do (loop for (line next) on lines
                     when (and (candidate-p line) (search marker line))
                       do (return-from first-error-line
                            (string-trim " " (if (and next (< (length (string-trim " " line)) 40))
                                                 (format nil "~a ~a" (string-trim " " line)
                                                         (string-trim " " next))
                                                 line))))))
    nil))

(defun compiled-files (log)
  "The files LOG says were compiled, one per `; compiling file \"…\"` line
   (SBCL's *COMPILE-VERBOSE* output, from the client process and from the
   project image)."
  (loop with key = "; compiling file \""
        for line in (text-lines (clean-output log))
        when (uiop:string-prefix-p key line)
          collect (let ((end (position #\" line :start (length key))))
                    (subseq line (length key) end))))

(defun demo-parallel (client &key (images 6) (rounds 2))
  (let ((home (fresh-dir "home"))
        (project (fresh-dir "p162"))
        (cache (fresh-dir "cache162")))
    (write-file (merge-pathnames "demo162.asd" project)
                (format nil "(defsystem \"demo162\" :depends-on (\"dexador\"))~%"))
    (out "  demo162 depends on dexador. One sync first; then each round empties the fasl root~%  ($XDG_CACHE_HOME/common-lisp/) and starts ~d `cl-repo ros` runs at once. Each run's~%  client process compiles the client (dexador included) into that root, then its project~%  image loads dexador. A round counts the `; compiling file` lines of all ~:*~d runs.~%" images)
    (multiple-value-bind (code output) (cl-repo client home project "sync")
      (out "  $ cl-repo sync~%    [exit ~a] ~a~%" code
           (or (find-if (lambda (l) (search "cl-repo.lock" l)) (text-lines output)) "")))
    (out "  dexador is loaded from ~:[the client's tree~;the install tree~]~%"
         (installed-versions home "dexador"))
    (dotimes (round rounds)
      (uiop:delete-directory-tree (merge-pathnames "common-lisp/" cache)
                                  :validate t :if-does-not-exist :ignore)
      (let* ((start (get-internal-real-time))
             (results (run-parallel client home project cache images))
             (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second))
             (compiled (loop for (nil . log) in results append (compiled-files log)))
             (distinct (remove-duplicates compiled :test #'string=))
             (loaded (count-if (lambda (r) (and (eql (car r) 0) (search "LOADED" (cdr r)))) results)))
        (out "~&  round ~d: ~d of ~d runs loaded dexador; ~d compiles for ~d distinct files (~,1f s)~%"
             (1+ round) loaded images (length compiled) (length distinct) seconds)
        (loop for (code . log) in results
              for i from 1
              unless (and (eql code 0) (search "LOADED" log))
                do (out "    run ~d: exit ~a: ~a~%" i code (or (first-error-line log) "(no error line)")))
        (let ((repeated (- (length compiled) (length distinct))))
          (when (plusp repeated)
            (out "    ~d compiles redid a file that another run (or the same one) had compiled~%" repeated)))
        (when (new-p client)
          (check (= loaded images) (format nil "round ~d: every run loads dexador" (1+ round)))
          (check (and (plusp (length compiled)) (= (length compiled) (length distinct)))
                 (format nil "round ~d: each file is compiled once" (1+ round))))))))

;;; Entry point ----------------------------------------------------------------------

(defun warm-up (client)
  "Run CLIENT once (`cl-repo list`), so it compiles itself into the shared
   cache before the first section and its compiler output stays out of them."
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (code output) (cl-repo client (fresh-dir "home") (fresh-dir "warm") "list")
      (declare (ignore output))
      (out "  cl-repo ~a compiled into the shared fasl cache: exit ~a, ~,1f s~%"
           (client-version client) code
           (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))

(defun run-demos ()
  "Run every section. → T when every check of the fixed client held and no
   section stopped on an error."
  (setf *checks* 0 *mismatches* 0 *failed-sections* '() *counter* 0)
  (let* ((*clients-dir* (uiop:ensure-directory-pathname
                         (or (uiop:getenvp "HARNESS_CL_REPO_CLIENTS")
                             (asdf:system-relative-pathname "harness-demos-cl-repo" ".clients/"))))
         (*roswell-home* (uiop:ensure-directory-pathname
                          (or (uiop:getenvp "ROSWELL_HOME")
                              (merge-pathnames ".roswell/" (user-homedir-pathname)))))
         (*root* (uiop:ensure-directory-pathname
                  (format nil "~a/harness-cl-repo-~36r/"
                          (string-right-trim "/" (native (uiop:temporary-directory)))
                          (random (expt 36 6) (make-random-state t)))))
         (*cache* (merge-pathnames "cache/" *root*))
         (new (find-client +new+))
         (old (find-client +old+))
         (clients (remove nil (list old new)))
         (start (get-internal-real-time)))
    (ensure-directories-exist *cache*)
    (unwind-protect
        (progn
          (banner "The clients")
          (out "  cl-repo ~a: ~:[missing~;~:*~a~]~%" +new+ (and new (scrub (native (client-dir new)))))
          (out "  cl-repo ~a: ~:[missing (the before runs are skipped)~;~:*~a~]~%" +old+
               (and old (scrub (native (client-dir old)))))
          (out "  Each command runs the client's own launcher (roswell/cl-repo) in a fresh HOME with~%  ROSWELL_HOME kept, so Roswell's SBCL is used and nothing of this user's cl-repo state.~%  Compiler and download lines are left out of the listings.~%")
          (if (null new)
              (progn
                (push "The clients" *failed-sections*)
                (out "!! no cl-repo ~a client: run scripts/setup.sh first~%" +new+))
              (progn
                (mapc #'warm-up clients)
                (section "#166: `cl-repo ros` starts the image without Quicklisp"
                         (each-client clients #'demo-quicklisp))
                (section "#163: a lock entry for an untagged install is pinned by digest"
                         (each-client clients #'demo-lock-digest))
                (section "#165: a missing minimum version is reported as one"
                         (each-client clients #'demo-minimum))
                (section "#167: a sync resolves to the newest published version"
                         (each-client clients #'demo-newest))
                (section "#170: a qlfile-only project gets the closure of its entries"
                         (each-client clients #'demo-qlfile))
                (section "#162: six images share one fasl root"
                         (each-client clients #'demo-parallel))))
          (banner "Summary")
          (out "  ~d checks of cl-repo ~a, ~d failed~@[; sections stopped: ~{~a~^, ~}~] (~d s)~%"
               *checks* +new+ *mismatches* (reverse *failed-sections*)
               (round (- (get-internal-real-time) start) internal-time-units-per-second))
          (and (zerop *mismatches*) (null *failed-sections*)))
      (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore))))
