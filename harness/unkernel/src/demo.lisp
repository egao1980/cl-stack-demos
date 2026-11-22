(in-package #:harness-demos-unkernel)

;;; compute-backend-unkernel 0.1.0: commands run in a Linux guest compiled to wasm,
;;; under Node. COMPUTE_UNKERNEL_NODE / COMPUTE_UNKERNEL_HOME pick Node and the HOME
;;; that scripts/setup.sh filled.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defmacro timed ((ms it) form &body body)
  "Bind IT to FORM's value and MS to its wall time in milliseconds around BODY."
  (let ((start (gensym)))
    `(let* ((,start (get-internal-real-time))
            (,it ,form)
            (,ms (round (* 1000 (- (get-internal-real-time) ,start)) internal-time-units-per-second)))
       (declare (ignorable ,it))
       ,@body)))

(defmacro show (form)
  "Print FORM's source, then evaluate it."
  `(progn (let ((*package* (find-package :harness-demos-unkernel)) (*print-right-margin* 90))
            (format t "~(~s~)~%" ',form))
          ,form))

(defun sh (session script &key (wall-clock 60))          ; → (values exit stdout stderr)
  (uk:session-exec session (list "sh" "-c" script) :wall-clock wall-clock))

(defun line (text) (string-trim '(#\Space #\Newline #\Return) text))

(defun one-line (text)                                  ; lines joined with " | ", or NIL
  (let ((l (line text)))
    (and (plusp (length l)) (format nil "~{~a~^ | ~}" (uiop:split-string l :separator '(#\Newline))))))

(defun host (&rest argv) (multiple-value-list (pp:run argv :output :string :error-output :string)))
(defun alive-p (pid) (eql 0 (first (host "kill" "-0" (princ-to-string pid)))))
(defun ns (p) (uiop:native-namestring (sc:physical-path p)))
(defun hosts-running ()                                 ; unkernel-host.mjs processes on the host
  (count-if (lambda (l) (plusp (length (line l))))
            (uiop:split-string (second (host "pgrep" "-f" "unkernel-host.mjs")) :separator '(#\Newline))))

(defun demo-availability (backend)
  (banner "1. Availability: confinement-available-p and its reason")
  (format t "Node: ~a (COMPUTE_UNKERNEL_NODE)~%HOME: ~a (COMPUTE_UNKERNEL_HOME)~%"
          (uk:unkernel-backend-node backend) (ns (uk:unkernel-backend-home backend)))
  (format t "Node probe: ~a~%Features: ~(~s~)~%"
          (nth-value 1 (uk:probe-node (uk:unkernel-backend-node backend))) (sc:confinement-features backend))
  (timed (ms it) (multiple-value-list (sc:confinement-available-p backend))
    (format t "configured backend  → ~s ~s (~d ms, probed once, then cached)~%" (first it) (second it) ms))
  (let ((empty (ensure-directories-exist (merge-pathnames "uk-empty-home/" (uiop:temporary-directory)))))
    (dolist (case `(("Node missing" :node "/nonexistent/bin/node")
                    ("guest HOME not set up" :home ,empty)))
      (let ((b (apply #'uk:make-unkernel-backend (rest case))))
        (format t "~a~%  available-p → ~{~s~%  reason: ~a~}~%" (first case)
                (multiple-value-list (sc:confinement-available-p b)))
        (handler-case (sc:run-sandboxed b '("true"))
          (sc:confinement-unavailable (c)
            (format t "  run-sandboxed → ~s: fails closed, nothing ran unconfined~%" (type-of c))))))
    (uiop:delete-empty-directory empty)))

(defun demo-one-shot (backend)
  (banner "2. One-shot run-sandboxed: Linux inside, whatever the host")
  (format t "host: ~a~%" (line (second (host "uname" "-sm"))))
  (let (cold)
    (dolist (cmd '(("uname" "-a") ("cat" "/etc/os-release")))
      (timed (ms it) (sc:run-sandboxed backend (sc:make-sandbox-spec :command cmd))
        (setf cold (or cold ms))
        (format t "~%$ ~{~a~^ ~}   (exit ~d, ~d ms: boot + run + power-off)~%~a"
                cmd (sc:sandbox-result-exit-code it) ms (sc:sandbox-result-stdout it))))
    cold))

(defun demo-isolation (backend)
  (banner "3. Isolation: mounts, network, TMPDIR")
  (destructuring-bind (root area ro outside)
      (let ((root (merge-pathnames (format nil "uk-demo-~36r/" (random (expt 36 6)))
                                   (sc:physical-path (uiop:temporary-directory)))))
        (cons root (mapcar (lambda (d) (ensure-directories-exist (merge-pathnames d root)))
                           '("area/" "ro/" "outside/"))))
   (let ((spec (sc:make-sandbox-spec
                :confinement (sc:make-confinement :writable (list area) :readable (list ro)
                                                  :read :system :deny-read '()))))
    (with-open-file (s (merge-pathnames "secret.txt" outside) :direction :output) (write-line "host secret" s))
    (with-open-file (s (merge-pathnames "notes.txt" ro) :direction :output) (write-line "read me" s))
    (unwind-protect
         (sc:with-sandbox (s backend spec)
           (flet ((try (label script)
                    (multiple-value-bind (code out err) (sh s script)
                      (format t "~a~%  $ ~a~%  exit ~d~@[  ~a~]~@[  ~a~]~%" label script code
                              (one-line out) (one-line err)))))
             (format t "session: boot ~d ms, writable ~a, read-only ~a, outside/ not mounted~%"
                     (uk:unkernel-session-boot-ms s) (ns area) (ns ro))
             (try "a) unmounted host path is invisible:" (format nil "cat ~asecret.txt" (ns outside)))
             (try "   its parent holds only the mount points:" (format nil "ls ~a" (ns root)))
             (try "b) read-only mount: readable, writes refused:"
                  (format nil "cat ~anotes.txt && echo x > ~anew.txt" (ns ro) (ns ro)))
             (format t "   on the host: ro/new.txt exists? ~s~%" (and (probe-file (merge-pathnames "new.txt" ro)) t))
             (try "c) the writable worktree (the session's cwd):" "pwd; echo written-in-guest > from-guest.txt")
             (format t "   on the host: area/from-guest.txt = ~s~%"
                     (line (uiop:read-file-string (merge-pathnames "from-guest.txt" area))))
             (try "d) network is off by default (DNS):" "nslookup example.com")
             (try "   (connect):" "nc -z -w 3 1.1.1.1 443; echo nc=$?; ip -o link | cut -d: -f2")
             (let ((tmp (uk:plan-tmp (uk:unkernel-session-plan s))))
               (try "e) TMPDIR is private, a fresh host dir mounted alone:"
                    "echo $TMPDIR; echo scratch > $TMPDIR/t.txt; ls $(dirname $TMPDIR)")
               (format t "   (the guest sees only the mount points; the host temp dir holds ~d entries)~%"
                       (length (uiop:directory* (merge-pathnames "*.*" (sc:physical-path (uiop:temporary-directory))))))
               (format t "   on the host: ~at.txt exists? ~s~%" tmp (and (probe-file (merge-pathnames "t.txt" tmp)) t))
               (sc:close-sandbox s)
               (format t "   after close-sandbox: ~a exists? ~s~%" tmp (and (uiop:directory-exists-p tmp) t)))))
      (uiop:delete-directory-tree root :validate t)))))

(defun demo-session (backend cold-ms)
  (banner "4. A session: several execs in one warm guest")
  (timed (open-ms it) (sc:open-sandbox backend nil)
    (let ((s it) (total 0))
      (unwind-protect
           (progn
             (format t "open-sandbox: ~d ms (kernel boot ~d ms)~%" open-ms (uk:unkernel-session-boot-ms s))
             (dolist (script '("uname -r" "echo $((6 * 7))" "cat /proc/cpuinfo | grep -c processor"
                               "ls /usr/bin | wc -l" "date -u +%Y"))
               (timed (ms it) (multiple-value-list (sh s script))
                 (incf total ms)
                 (format t "  ~38a → ~6a ~4d ms~%" script (line (second it)) ms)))
             (format t "5 warm execs: ~d ms in all (~d ms each); one cold run-sandboxed: ~d ms (~,1fx one warm exec)~%"
                     total (round total 5) cold-ms (/ cold-ms (max 1 (/ total 5)))))
        (sc:close-sandbox s)))))

(defun demo-deadline (backend)
  (banner "5. Deadline: a 2 s wall-clock on sleep 30")
  (let* ((s (sc:open-sandbox backend nil))
         (pid (uk:unkernel-session-pid s))
         (start (get-internal-real-time)))
    (format t "host process pid ~d, alive: ~s; unkernel hosts running: ~d~%The handler:~%"
            pid (alive-p pid) (hosts-running))
    (let ((result
            (show (handler-bind ((sc:sandbox-timeout
                                   (lambda (c)
                                     (format t "  signalled ~s (limit ~a s): ~a~%"
                                             (type-of c) (sc:sandbox-timeout-limit c) c)
                                     (invoke-restart 'sc:abort-execution))))
                    (uk:session-exec s '("sleep" "30") :wall-clock 2)))))
      (format t "  → ~s after ~,1f s; session alive: ~s~%" result
              (/ (- (get-internal-real-time) start) internal-time-units-per-second)
              (uk:unkernel-session-alive-p s)))
    (sleep 0.5)
    (format t "host pid ~d alive: ~s; unkernel hosts running: ~d; close-sandbox → ~s~%"
            pid (alive-p pid)
            (hosts-running) (sc:close-sandbox s))))

(defparameter *loops*
  '(("generic arithmetic" "(loop for i below 100000000 sum (mod (* i i) 7))")
    ("fixnum, (speed 3)"  "(locally (declare (optimize speed (safety 0))) (loop for i fixnum below 100000000 sum (the fixnum (mod (* i i) 7)) fixnum))")
    ("consing, sort, GC"  "(length (sort (loop for i below 3000000 collect (mod (* i 7919) 1000003)) (function <)))")))

(defun ms-of (output)                                   ; "<value> <n> ms" → n
  (let ((l (string-right-trim " ms" (line output))))
    (parse-integer l :start (1+ (position #\Space l :from-end t)))))

(defun demo-sbcl (backend)
  (banner "6. SBCL inside the guest")
  (sc:with-sandbox (s backend nil)
    (flet ((guest-sbcl (code)                           ; → (values exit stdout host-clock-ms)
             (timed (ms it) (multiple-value-list
                             (uk:session-exec s (list "sbcl" "--noinform" "--non-interactive" "--no-sysinit"
                                                      "--no-userinit" "--eval" code) :wall-clock 300))
               (values (first it) (second it) ms))))
      (guest-sbcl "(prin1 0)")                          ; warm the page cache
      (let ((start-ms (nth-value 2 (guest-sbcl "(prin1 0)"))))
        (format t "guest: ~a (start-up alone: ~d ms); host: SBCL ~a~%Each form is compiled and timed the same way in the guest and on the host:~%"
                (line (nth-value 1 (sh s "sbcl --version"))) start-ms (lisp-implementation-version))
        (loop for (label form) in *loops*
              for code = (format nil "(let ((s (get-internal-real-time))) (prin1 (funcall (compile nil '(lambda () ~a)))) (format t \" ~~d ms\" (round (* 1000 (- (get-internal-real-time) s)) internal-time-units-per-second)))" form)
              for (exit out ms) = (multiple-value-list (guest-sbcl code))
              for host = (with-output-to-string (*standard-output*) (eval (read-from-string code)))
              for ratio = (/ (- ms start-ms) (max 1 (ms-of host)))
              do (format t "~%  ~a: ~a~%    guest (exit ~d): ~a by its own clock, ~d ms by the host's (less start-up)~%    host: ~a   guest / host: ~,1fx (guest clock), ~,1fx (host clock)~%"
                         label form exit (line out) (- ms start-ms) host (/ (ms-of out) (max 1 (ms-of host))) ratio)
              minimize ratio into lo maximize ratio into hi
              finally (format t "~%~{~a~%~}" (list "The tradeoff: CPU-bound work in the guest is slower than native. The backend documents"
                                                  (format nil "15-40x (26-34x in its README); these loops measured ~,1f-~,1fx on this host (~a ~a)." lo hi (software-type) (machine-type))
                                                  "Either way it is the price of the portable tier: Linux in wasm under Node, no"
                                                  "hypervisor, no root. Prefer Seatbelt / Landlock or a microVM where they exist;"
                                                  "unkernel is the fallback, never the default.")))))))

(defun run-demos ()
  (let ((backend (uk:make-unkernel-backend))
        (*random-state* (make-random-state t)))
    (format t "compute-backend-unkernel ~a (unkernel ~a, guest ~a, ~{~a~^ ~})~%"
            (asdf:component-version (asdf:find-system "compute-backend-unkernel"))
            uk:+unkernel-version+ uk:+alpine-image+ uk:+guest-packages+)
    (demo-availability backend)
    (unless (sc:confinement-available-p backend)
      (format t "~%Backend unavailable: run scripts/setup.sh (see README).~%")
      (return-from run-demos nil))
    (let ((cold (demo-one-shot backend)))
      (demo-isolation backend)
      (demo-session backend cold))
    (demo-deadline backend)
    (demo-sbcl backend)
    (finish-output)
    t))
