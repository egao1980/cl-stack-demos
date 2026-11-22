(in-package #:harness-demos-isolation)

;;; Demos 2-3: R4d confinement of one command. compute-protocol 0.3.2's
;;; CONFINEMENT, run through the confining backend of this host:
;;; compute-backend-seatbelt 0.1.1 (macOS, sandbox-exec) or
;;; compute-backend-landlock 0.1.0 (Linux, bwrap / Landlock). Both load on
;;; every OS; the one that is unavailable here says why and is skipped.

(defun host-backends ()
  "(label . backend) for both confining backends, seatbelt first."
  (list (cons "compute-backend-seatbelt 0.1.1" (seatbelt:make-seatbelt-backend))
        (cons "compute-backend-landlock 0.1.0" (landlock:make-landlock-backend))))

(defun show-backends (backends)
  (dolist (b backends)
    (multiple-value-bind (ok reason) (sc:confinement-available-p (cdr b))
      (note "~a: available ~s~@[ (~a)~]" (car b) ok reason)
      (note "  features: ~s" (sc:confinement-features (cdr b))))))

(defun available-backend (backends)
  (find-if (lambda (b) (values (sc:confinement-available-p (cdr b)))) backends))

;;; A loopback listener the confined command tries to reach.

(defun open-listener ()
  "A TCP socket listening on 127.0.0.1 (port chosen by the OS). → (values socket port)."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen s 5)
    (values s (nth-value 1 (sb-bsd-sockets:socket-name s)))))

(defun first-line (string)
  (let ((s (string-right-trim '(#\Newline #\Return) (or string ""))))
    (subseq s 0 (position #\Newline s))))

(defun confined (backend confinement script)
  "Run /bin/sh -c SCRIPT under CONFINEMENT → (values exit-code stdout stderr)."
  (let ((r (sc:run-confined backend confinement (list "/bin/sh" "-c" script))))
    (values (sc:sandbox-result-exit-code r)
            (sc:sandbox-result-stdout r)
            (sc:sandbox-result-stderr r))))

(defun step-line (label model script backend confinement)
  "Run SCRIPT confined and print exit code, output, and the reference model's
decision (MODEL: :ALLOW or :DENY) next to it. → (values code stdout)."
  (multiple-value-bind (code out err) (confined backend confinement script)
    (note "~a" label)
    (note "  $ ~a" script)
    (note "  model says ~s; exit ~d~@[, stdout ~s~]~@[, stderr ~s~]"
          model code (let ((l (first-line out))) (and (plusp (length l)) l))
          (let ((l (first-line err))) (and (plusp (length l)) l)))
    (values code out)))

(defun demo-confine ()
  (banner "Demo 2: R4d confinement — compute-protocol 0.3.2 run-confined on this host's confining backend")
  (let* ((backends (host-backends))
         (chosen (available-backend backends)))
    (stage "the confining backends (both load on every OS)")
    (show-backends backends)
    (unless chosen
      (note "skipped: no confining backend is available on this host (~a)" (software-type))
      (return-from demo-confine))
    (note "using ~a" (car chosen))
    (with-temp-dir (root "hd-confine")
      (let* ((backend (cdr chosen))
             (area (merge-pathnames "area/" root))
             (victim (merge-pathnames "outside/victim.txt" root))
             (secret (merge-pathnames "outside/secret.txt" root))
             (fresh (merge-pathnames "outside/new.txt" root))
             (c (sc:make-confinement :writable (list area) :read :system :network :none)))
        (ensure-directories-exist area)
        (write-file victim (lines "original victim"))
        (write-file secret (lines "hunter2"))
        (stage "the confinement: area writable, READ :SYSTEM, network :NONE")
        (show "confinement-requirements" (sc:confinement-requirements c))
        (show "check-confinement" (and (sc:check-confinement backend c) :ok))
        (let ((summary (sc:confinement-summary c)))
          (show "writable" (getf summary :writable))
          (show "read / network / env-mode / tmp"
                (list (getf summary :read) (getf summary :network)
                      (getf summary :env-mode) (getf summary :tmp)))
          (show "deny-read (default credential dirs)" (length (getf summary :deny-read))))
        (sc:with-confined-launch (l backend c (list "/bin/sh" "-c" "true"))
          (let ((argv (sc:confined-launch-argv l)))
            (show "launch argv[0]" (first argv))
            (show "launch argv length" (length argv))
            (show "launch directory = the area"
                  (equal (native (sc:confined-launch-directory l))
                         (native (sc:physical-path area))))
            (show "launch env names" (mapcar #'car (sc:confined-launch-env l)))
            (when (search "sandbox-exec" (first argv))
              (let ((profile (third argv)))
                (show "SBPL profile lines" (count #\Newline profile))
                (note "profile lines naming the area:")
                (dolist (line (uiop:split-string profile :separator '(#\Newline)))
                  (when (or (search "(deny default)" line)
                            (search (string-right-trim "/" (native (sc:physical-path area))) line))
                    (note "  ~a" (string-trim " " line))))))))

        (stage "confined commands")
        (let* ((rc (sc:resolve-confinement c))
               (phys-victim (native victim))   ; ROOT is a truename: already physical
               (phys-secret (native secret)))
          (multiple-value-bind (code out)
              (step-line "1. write inside the area, then cat it"
                         (sc:confinement-decide rc :write (merge-pathnames "inside.txt" area))
                         "echo hello > inside.txt && cat inside.txt" backend c)
            (declare (ignore out))
            (show "   area/inside.txt on disk" (ignore-errors (read-file (merge-pathnames "inside.txt" area))))
            (unless (zerop code)
              (note "   KNOWN BUG egao1980/compute-backend-seatbelt#2: under READ :SYSTEM a confined cat")
              (note "   cannot write to run-confined's captured stdout (a temp file outside the area)")))
          (step-line "2. append to a file outside the area"
                     (sc:confinement-decide rc :write victim)
                     (format nil "echo pwned >> '~a'" phys-victim) backend c)
          (show "   outside/victim.txt unchanged" (equal (read-file victim) (lines "original victim")))
          (step-line "3. create a file outside the area"
                     (sc:confinement-decide rc :write fresh)
                     (format nil "echo x > '~a'" (native fresh)) backend c)
          (show "   outside/new.txt absent" (not (probe-file fresh)))
          (multiple-value-bind (code out)
              (step-line "4. read a secret outside the area"
                         (sc:confinement-decide rc :read secret)
                         (format nil "cat '~a'" phys-secret) backend c)
            (declare (ignore code))
            (show "   secret leaked" (and (search "hunter2" out) t)))
          (multiple-value-bind (listener port) (open-listener)
            (unwind-protect
                 (let ((script (format nil "/bin/bash -c 'exec 3<>/dev/tcp/127.0.0.1/~d && echo connected'" port)))
                   (multiple-value-bind (code out)
                       (proc:run (list "/bin/sh" "-c" script) :output :string :error-output :string)
                     (note "5. connect to a loopback listener")
                     (note "  unconfined baseline: exit ~d, stdout ~s" code (first-line out)))
                   (step-line "   confined:"
                              (sc:confinement-decide rc :connect (cons "127.0.0.1" port))
                              script backend c))
              (sb-bsd-sockets:socket-close listener)))
          (sb-posix:setenv "HD_DEMO_SECRET" "leak" 1)
          (unwind-protect
               (multiple-value-bind (code out)
                   (confined backend c
                             "echo \"HD_DEMO_SECRET=${HD_DEMO_SECRET-unset}\"; echo \"$TMPDIR\"; touch \"$TMPDIR/scratch\" && echo tmp-writable")
                 (let ((ls (uiop:split-string (string-right-trim '(#\Newline) out) :separator '(#\Newline))))
                   (note "6. environment scrub and private TMPDIR (HD_DEMO_SECRET=leak in the image)")
                   (note "  exit ~d, ~a" code (first ls))
                   (show "   TMPDIR is a private stack-compute-<hex>/ dir" (and (search "stack-compute-" (second ls)) t))
                   (show "   TMPDIR writable" (equal (third ls) "tmp-writable"))
                   (show "   TMPDIR gone after the launch" (not (probe-file (second ls))))))
            (sb-posix:unsetenv "HD_DEMO_SECRET")))))))

(defun demo-suite ()
  (banner "Demo 3: compute-protocol/confinement-suite 0.3.2 (21 laws) on this host's confining backend")
  (let ((backends (host-backends)))
    (show "suite cases" (length (suite:suite-case-names)))
    (dolist (b backends)
      (stage "~a" (car b))
      (multiple-value-bind (ok reason) (sc:confinement-available-p (cdr b))
        (if (not ok)
            (note "skipped: unavailable here (~a)" reason)
            (let ((start (get-internal-real-time)))
              (multiple-value-bind (failures ran skipped) (suite:run-confinement-suite (cdr b))
                (note "ran ~d, skipped ~d, failures ~d in ~,1f s"
                      (length ran) (length skipped) (length failures)
                      (/ (ms-since start) 1000.0))
                (show "skipped (features the backend does not enforce)" skipped)
                (if failures
                    (dolist (f failures)
                      (note "  FAILED ~s: ~a" (car f) (cdr f)))
                    (note "  no failures")))))))))
