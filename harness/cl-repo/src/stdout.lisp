;;;; V=$(cl-repo --version) on a cold fasl cache: the published client 0.27.39
;;;; (cl-repository#194, #193; PR 195) against 0.27.38. The 0.27.39 results
;;;; are checked; the 0.27.38 runs are printed, as the "before" picture.

(in-package #:harness-demos-cl-repo)

(defparameter +stdout-new+ "0.27.39"
  "The client that keeps the output of its own build back (cl-repository#194)
   and whose SAT-IMPLY predicate is not a slot accessor (#193).")

(defparameter +stdout-old+ "0.27.38"
  "The release before it.")

(defparameter +capture-script+
  "V=$(/bin/sh \"$0\" --version 2>\"$DEMO_ERR\"); code=$?; printf '%s' \"$V\" >\"$DEMO_V\"; exit $code"
  "What a script does: the client's version into V, stderr into a file. $0 is
   the client's launcher; the shell writes V to $DEMO_V for the demo to read.")

(defun stdout-new-p (client)
  (string= (client-version client) +stdout-new+))

(defun capture-version (client cache &key env)
  "Run +CAPTURE-SCRIPT+ with CLIENT for a fresh HOME and the fasl root CACHE,
   ENV ((name . value) ...) added. → a plist: :CODE, :SECONDS, :V (what V
   held) and :ERR (stderr), paths scrubbed."
  (let* ((home (fresh-dir "home"))
         (v-file (merge-pathnames "v.txt" home))
         (err-file (merge-pathnames "stderr.txt" home))
         (start (get-internal-real-time))
         (code (proc:run (list "/bin/sh" "-c" +capture-script+
                               (native (merge-pathnames "roswell/cl-repo" (client-dir client))))
                         :directory home
                         :env (client-env client home
                                          :cache cache
                                          :extra (append (list (cons "DEMO_V" (native v-file))
                                                               (cons "DEMO_ERR" (native err-file)))
                                                         (or env (list (cons "CL_REPO_VERBOSE" nil)))))
                         :output nil :error-output nil :timeout 1800)))
    (list :code code
          :seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)
          :v (scrub (if (probe-file v-file) (read-file v-file) ""))
          :err (scrub (if (probe-file err-file) (read-file err-file) "")))))

(defun nonempty-lines (text)
  (if (string= text "") '() (text-lines text)))

(defun show-capture (note run)
  "Print the command, what V held and what stderr got."
  (let ((v (nonempty-lines (getf run :v)))
        (err (nonempty-lines (getf run :err))))
    (out "  $ V=$(cl-repo --version)   # ~a~%" note)
    (out "    exit ~a, ~,1f s. V holds ~d line~:p:~%" (getf run :code) (getf run :seconds) (length v))
    (dolist (line (subseq v 0 (min 3 (length v))))
      (out "      ~a~%" line))
    (when (> (length v) 3)
      (out "      … ~d more lines~%" (- (length v) 3)))
    (out "    stderr: ~d line~:p; `; compiling file` ~d, `;;; Computing Hangul` ~d, `type predicate` ~d~%"
         (length err)
         (count-occurrences "; compiling file" (getf run :err))
         (count-occurrences ";;; Computing Hangul" (getf run :err))
         (count-occurrences "type predicate" (getf run :err)))
    (when (<= 1 (length err) 3)
      (dolist (line err)
        (out "      ~a~%" (if (> (length line) 140) (format nil "~a…" (subseq line 0 140)) line))))))

(defun version-line-alone-p (run)
  "V holds one line, cl-repo +STDOUT-NEW+ and its directory."
  (let ((v (nonempty-lines (getf run :v))))
    (and (= 1 (length v))
         (uiop:string-prefix-p (format nil "cl-repo ~a (" +stdout-new+) (first v))
         (search (format nil "cl-oci-~a" +stdout-new+) (first v)))))

(defun demo-cold-and-warm (client)
  "A cold start (a fresh fasl root: the client compiles itself, as on the
   first run after an upgrade), then the next run over the same root."
  (let* ((cache (fresh-dir "cold-cache"))
         (cold (capture-version client cache))
         (warm (capture-version client cache)))
    (show-capture "first run, empty fasl root" cold)
    (when (stdout-new-p client)
      (check (eql 0 (getf cold :code)) "the first run exits 0")
      (check (version-line-alone-p cold) "V holds the version line alone")
      (check (zerop (count-occurrences "; compiling file" (getf cold :err)))
             "stderr gets no compiler output")
      (check (zerop (count-occurrences "type predicate" (getf cold :err)))
             "and no type-predicate warning (#193)"))
    (show-capture "next run, same fasl root" warm)
    (when (stdout-new-p client)
      (check (version-line-alone-p warm) "V holds the version line alone")
      (check (zerop (count-occurrences ";;; Computing Hangul" (getf warm :err)))
             "stderr gets no cl-unicode note"))))

(defun demo-verbose (client)
  "CL_REPO_VERBOSE=1 on a cold start: the compile on stderr, stdout as before."
  (let ((run (capture-version client (fresh-dir "cold-cache")
                              :env (list (cons "CL_REPO_VERBOSE" "1")))))
    (show-capture "CL_REPO_VERBOSE=1, empty fasl root" run)
    (check (version-line-alone-p run) "V still holds the version line alone")
    (check (plusp (count-occurrences "; compiling file" (getf run :err)))
           "stderr gets the compile")
    (check (zerop (count-occurrences "type predicate" (getf run :err)))
           "with no structure predicate overwritten by a slot accessor (#193)")))

(defun run-stdout-demos ()
  "cl-repository#194 and #193: V=$(cl-repo --version) with client 0.27.39
   against 0.27.38, each through its OCI-published launcher in fresh HOMEs
   on an empty fasl root. → T when every check of 0.27.39 held and no
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
         (new (find-client +stdout-new+))
         (old (find-client +stdout-old+))
         (clients (remove nil (list old new)))
         (start (get-internal-real-time)))
    (ensure-directories-exist *root*)
    (unwind-protect
         (progn
           (banner "The clients")
           (out "  cl-repo ~a: ~:[missing~;~:*~a~]~%" +stdout-new+
                (and new (scrub (native (client-dir new)))))
           (out "  cl-repo ~a: ~:[missing (the before runs are skipped)~;~:*~a~]~%" +stdout-old+
                (and old (scrub (native (client-dir old)))))
           (out "  Each command is a shell running V=$(cl-repo --version), the client's own launcher~%  (roswell/cl-repo), in a fresh HOME. Its stderr goes to a file; V is what the script gets.~%  An empty fasl root (XDG_CACHE_HOME) makes the client compile itself and its dependencies,~%  as on the first run after an upgrade or on a CI runner without a fasl cache.~%")
           (if (null new)
               (progn
                 (push "The clients" *failed-sections*)
                 (out "!! no cl-repo ~a client: run scripts/setup.sh ~a ~a first~%"
                      +stdout-new+ +stdout-old+ +stdout-new+))
               (progn
                 (section "#194: V=$(cl-repo --version) on an empty fasl root, then again"
                          (dolist (client clients)
                            (out "~&~%-- cl-repo ~a~:[ (before)~; (after)~] --~%"
                                 (client-version client) (stdout-new-p client))
                            (demo-cold-and-warm client)))
                 (section "#194, #193: CL_REPO_VERBOSE=1 on an empty fasl root"
                          (out "~&~%-- cl-repo ~a --~%" (client-version new))
                          (demo-verbose new))))
           (banner "Summary")
           (out "  ~d checks of cl-repo ~a, ~d failed~@[; sections stopped: ~{~a~^, ~}~] (~d s)~%"
                *checks* +stdout-new+ *mismatches* (reverse *failed-sections*)
                (round (- (get-internal-real-time) start) internal-time-units-per-second))
           (and (zerop *mismatches*) (null *failed-sections*)))
      (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore))))
