(in-package #:harness-demos-cl-repo)

(defparameter +offline-new+ "0.27.38"
  "The client that fails fast against an unreachable registry
   (cl-repository#179) and has --version (#189).")

(defparameter +offline-old+ "0.27.37"
  "The release before it.")

(defparameter +closed-port+ "http://127.0.0.1:9"
  "A loopback port nothing listens on: every connection is refused at once.
   As HTTPS_PROXY it cuts every registry off; as --registry it is a dead
   registry beside GHCR.")

(defparameter +black-hole+ "http://192.0.2.1:9"
  "An address in TEST-NET-1 (RFC 5737): routed nowhere, so a connection gets
   no answer at all, not even a refusal, until the client gives up.")

(defparameter *before-cap* 600
  "Seconds a run may take before it is stopped (its whole process group).")

(defparameter *black-hole-before-cap* 180
  "The cap for the 0.27.37 black-hole run, which takes far longer (it was
   stopped unfinished after 919 s in cl-repository#191).")

(defun offline-new-p (client)
  (string= (client-version client) +offline-new+))

(defun each-offline-client (clients fn)
  "Call FN with each of CLIENTS (old first) under a sub-banner."
  (dolist (client clients)
    (out "~&~%-- cl-repo ~a~:[ (before)~; (after)~] --~%"
         (client-version client) (offline-new-p client))
    (funcall fn client)))

(defun trace-line-p (line)
  "A CL_REPO_TRACE_HTTP line: one request that went out."
  (uiop:string-prefix-p "; cl-repo-http " line))

(defun count-occurrences (needle haystack)
  (loop for start = 0 then (+ pos (length needle))
        for pos = (search needle haystack :start2 start)
        while pos
        count t))

(defun proxy-env (proxy)
  "The environment of a user whose HTTPS (and HTTP) traffic goes through
   PROXY, with no proxy exceptions."
  (loop for name in '("HTTPS_PROXY" "https_proxy" "HTTP_PROXY" "http_proxy")
        collect (cons name proxy) into env
        finally (return (append env (list (cons "NO_PROXY" nil) (cons "no_proxy" nil))))))

(defun timed-cl-repo (client home project args &key env (cap *before-cap*))
  "Run `cl-repo ARGS` with CLIENT for HOME in PROJECT, ENV ((name . value)
   ...) added and CL_REPO_TRACE_HTTP on. After CAP seconds the run is
   stopped, its whole process group. → a plist: :CODE (NIL when stopped),
   :SECONDS, :REQUESTS (the trace lines: requests that went out) and
   :OUTPUT (stdout and stderr, trace lines removed, paths scrubbed)."
  (let ((log (merge-pathnames (format nil "run-~d.log" (incf *counter*)) *root*))
        (start (get-internal-real-time))
        (code nil))
    (handler-case
        (setf code (proc:run (launcher-command client args :log t)
                             :directory project
                             :env (client-env client home
                                              :extra (append (list (cons "DEMO_LOG" (native log))
                                                                   (cons "CL_REPO_TRACE_HTTP" "1"))
                                                             env))
                             :output nil :error-output nil
                             :timeout cap :process-group t))
      (proc:process-timeout-error () (setf code nil)))
    (let ((lines (text-lines (clean-output (if (probe-file log) (read-file log) "")))))
      (list :code code
            :seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)
            :requests (count-if #'trace-line-p lines)
            :output (format nil "~{~a~%~}" (remove-if #'trace-line-p lines))))))

(defun show-run (args run &key (max 6) note)
  "Print `cl-repo ARGS`, the run's output (SHOW), its time and request count."
  (show args (or (getf run :code) "none: stopped") (getf run :output) :max max)
  (out "    ~,1f s, ~d request~:p sent~@[ ~a~]~:[ (stopped at the cap: it was still waiting)~;~]~%"
       (getf run :seconds) (getf run :requests) note (getf run :code)))

(defun offline-project (name &rest depends-on)
  "A new project directory with NAME.asd depending on DEPENDS-ON."
  (let ((project (fresh-dir name)))
    (write-file (merge-pathnames (format nil "~a.asd" name) project)
                (format nil "(defsystem ~s :depends-on ~s)~%" name depends-on))
    project))

(defun demo-client-version (client)
  (let ((run (timed-cl-repo client (fresh-dir "home") (fresh-dir "v189") '("--version"))))
    (show-run '("--version") run :max 3)
    (when (offline-new-p client)
      (let ((line (find-if (lambda (l) (uiop:string-prefix-p "cl-repo " l))
                           (text-lines (getf run :output)))))
        (check (eql 0 (getf run :code)) "`cl-repo --version` exits 0")
        (check (and line
                    (uiop:string-prefix-p (format nil "cl-repo ~a (" +offline-new+) line)
                    (search (format nil "cl-oci-~a" +offline-new+) line))
               (format nil "and prints the release and the client's directory: ~a" line))))))

(defun demo-offline-installed (client)
  (let* ((project (offline-project "demo179a" "dexador"))
         (run (timed-cl-repo client (fresh-dir "home") project '("sync" "--no-lock")
                             :env (proxy-env +closed-port+))))
    (show-run '("sync" "--no-lock") run)
    (when (offline-new-p client)
      (let ((output (getf run :output)))
        (check (eql 0 (getf run :code)) "the sync succeeds")
        (check (< (getf run :seconds) 15) (format nil "in ~,1f s" (getf run :seconds)))
        (check (search "keeping installed versions" output) "and keeps the installed versions")
        (check (search "Registry https://ghcr.io is unreachable" output)
               "saying that the registry is unreachable")))))

(defun demo-dead-registry (client)
  (let* ((project (offline-project "demo179b" "dexador"))
         (args (list "sync" "--no-lock" "--registry" +closed-port+))
         (run (timed-cl-repo client (fresh-dir "home") project args))
         (named (count-occurrences (format nil "lookup in ~a failed" +closed-port+)
                                   (getf run :output))))
    (show-run args run :max 4)
    (out "    lines that name the dead registry: ~d~%" named)
    (when (offline-new-p client)
      (check (eql 0 (getf run :code)) "the sync succeeds")
      (check (< (getf run :seconds) 15) (format nil "in ~,1f s" (getf run :seconds)))
      (check (= 1 named) "and names the dead registry once"))))

(defun demo-black-hole (client)
  (let* ((project (offline-project "demo179c" "dexador"))
         (run (timed-cl-repo client (fresh-dir "home") project '("sync" "--no-lock")
                             :env (proxy-env +black-hole+)
                             :cap (if (offline-new-p client) *before-cap* *black-hole-before-cap*))))
    (show-run '("sync" "--no-lock") run :max 4)
    (when (offline-new-p client)
      (check (eql 0 (getf run :code)) "the sync succeeds")
      (check (< (getf run :seconds) 30)
             (format nil "in ~,1f s: one connect timeout (10 s), not one per attempt and package"
                     (getf run :seconds))))))

(defun demo-offline-missing (client)
  (let* ((project (offline-project "demo179d" "dexador" "parse-number"))
         (run (timed-cl-repo client (fresh-dir "home") project '("sync" "--no-lock")
                             :env (proxy-env +closed-port+))))
    (show-run '("sync" "--no-lock") run :max 10)
    (when (offline-new-p client)
      (let ((output (getf run :output)))
        (check (not (eql 0 (getf run :code))) "the sync fails")
        (check (< (getf run :seconds) 15) (format nil "in ~,1f s" (getf run :seconds)))
        (check (search "could not look up or pull parse-number" output) "naming parse-number")
        (check (search "Registry https://ghcr.io is unreachable" output)
               "and the unreachable registry")))))

(defun demo-offline-lock (client)
  (let* ((home (fresh-dir "home"))
         (project (offline-project "demo179e" "dexador" "parse-number")))
    (out "  Online first: the sync installs parse-number and writes cl-repo.lock.~%")
    (show-run '("sync") (timed-cl-repo client home project '("sync")) :max 3)
    (out "  Offline, the lock current and its entries installed:~%")
    (let ((run (timed-cl-repo client home project '("sync") :env (proxy-env +closed-port+))))
      (show-run '("sync") run :max 3)
      (when (offline-new-p client)
        (check (and (eql 0 (getf run :code)) (zerop (getf run :requests)))
               "the sync succeeds and sends no request")))
    (out "  Offline, parse-number removed from the install tree:~%")
    (uiop:delete-directory-tree
     (merge-pathnames ".local/share/cl-repository/systems/parse-number/" home)
     :validate t :if-does-not-exist :ignore)
    (let ((run (timed-cl-repo client home project '("sync") :env (proxy-env +closed-port+))))
      (show-run '("sync") run :max 4)
      (when (offline-new-p client)
        (check (not (eql 0 (getf run :code))) "the sync fails")
        (check (< (getf run :seconds) 15) (format nil "in ~,1f s" (getf run :seconds)))
        (check (search "(cl-repo.lock) is not installed" (getf run :output))
               "naming the lock entry it cannot fetch")))))

(defun run-offline-demos ()
  "cl-repository#179 and #189: client 0.27.38 against 0.27.37, each run
   through its OCI-published launcher in fresh HOMEs, with the registry
   unreachable: a proxy on a closed port, a dead --registry beside GHCR, a
   black-holed proxy. → T when every check of 0.27.38 held and no section
   stopped on an error."
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
         (new (find-client +offline-new+))
         (old (find-client +offline-old+))
         (clients (remove nil (list old new)))
         (start (get-internal-real-time)))
    (ensure-directories-exist *cache*)
    (unwind-protect
         (progn
           (banner "The clients")
           (out "  cl-repo ~a: ~:[missing~;~:*~a~]~%" +offline-new+
                (and new (scrub (native (client-dir new)))))
           (out "  cl-repo ~a: ~:[missing (the before runs are skipped)~;~:*~a~]~%" +offline-old+
                (and old (scrub (native (client-dir old)))))
           (out "  Each command runs the client's own launcher (roswell/cl-repo) in a fresh HOME, so every~%  dependency of dexador comes from the client's own tree. Offline is a proxy (HTTPS_PROXY) or a~%  registry (--registry) on ~a, where nothing listens, or a proxy on ~a,~%  which answers nothing. The machine's network is not touched. CL_REPO_TRACE_HTTP counts the~%  requests that went out. A run is stopped at a cap (~d s; ~d s for the 0.27.37 black hole).~%"
                +closed-port+ +black-hole+ *before-cap* *black-hole-before-cap*)
           (if (null new)
               (progn
                 (push "The clients" *failed-sections*)
                 (out "!! no cl-repo ~a client: run scripts/setup.sh ~a ~a first~%"
                      +offline-new+ +offline-old+ +offline-new+))
               (progn
                 (mapc #'warm-up clients)
                 (section "#189: cl-repo --version"
                          (each-offline-client clients #'demo-client-version))
                 (section "#179: offline, every dependency installed (proxy on a closed port)"
                          (each-offline-client clients #'demo-offline-installed))
                 (section "#179: a dead registry beside GHCR (--registry on a closed port)"
                          (each-offline-client clients #'demo-dead-registry))
                 (section "#179: a black-holed network (proxy that never answers)"
                          (each-offline-client clients #'demo-black-hole))
                 (section "#179: offline, and a package is not installed"
                          (each-offline-client clients #'demo-offline-missing))
                 (section "#179: offline with cl-repo.lock"
                          (each-offline-client clients #'demo-offline-lock))))
           (banner "Summary")
           (out "  ~d checks of cl-repo ~a, ~d failed~@[; sections stopped: ~{~a~^, ~}~] (~d s)~%"
                *checks* +offline-new+ *mismatches* (reverse *failed-sections*)
                (round (- (get-internal-real-time) start) internal-time-units-per-second))
           (and (zerop *mismatches*) (null *failed-sections*)))
      (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore))))
