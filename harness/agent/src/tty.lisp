(in-package #:harness-demos-agent)

;;; Demo 4: tui-backend-tty 0.1.0. In this (non-tty) run: TTY-AVAILABLE-P
;;; and the TTY-NOT-A-TERMINAL condition. Then a child SBCL on a real pty
;;; (pty-protocol / pty-backend-native) under sh, which prints `stty -g`
;;; before and after; the child enters raw mode, reads one key and exits.

(defun %asdf-load-form ()
  (let* ((home (ignore-errors (sb-int:sbcl-homedir-pathname)))
         (fasl (and home (probe-file (merge-pathnames "contrib/asdf.fasl" home)))))
    (if fasl (format nil "(load ~s)" (uiop:native-namestring fasl)) "(require :asdf)")))

(defun %registry-form ()
  "Source registry naming the directory of every system this image loaded."
  (let ((dirs '()))
    (dolist (name (asdf:already-loaded-systems))
      (let ((dir (ignore-errors (asdf:system-source-directory name))))
        (when dir (pushnew (uiop:native-namestring dir) dirs :test #'string=))))
    `(:source-registry ,@(mapcar (lambda (d) (list :directory d)) dirs)
                       :ignore-inherited-configuration)))

(defun %child-argv (report)
  (let ((child (uiop:native-namestring
                (asdf:system-relative-pathname "harness-demos-agent" "src/tty-child.lisp"))))
    (append
     (list (uiop:native-namestring sb-ext:*runtime-pathname*)
           "--core" (uiop:native-namestring sb-ext:*core-pathname*) "--noinform"
           "--non-interactive" "--no-sysinit" "--no-userinit")
     (loop for form in (list (%asdf-load-form)
                             (format nil "(uiop:symbol-call :asdf :initialize-source-registry '~s)"
                                     (%registry-form))
                             "(uiop:symbol-call :asdf :load-system \"tui-backend-tty\")"
                             (format nil "(load ~s)" child)
                             (format nil "(uiop:symbol-call :harness-demos-agent/tty-child :main ~s)"
                                     report))
           append (list "--eval" form)))))

(defun %strip-escapes (string)
  "Drop CSI / OSC / other ESC sequences and CRs for printing."
  (with-output-to-string (out)
    (let ((i 0) (n (length string)))
      (loop while (< i n)
            do (let ((c (char string i)))
                 (cond ((char= c (code-char 27))
                        (incf i)
                        (when (< i n)
                          (case (char string i)
                            (#\[ (incf i)
                             (loop while (and (< i n) (not (<= #x40 (char-code (char string i)) #x7e)))
                                   do (incf i))
                             (incf i))
                            (#\] (loop while (and (< i n) (not (member (char-code (char string i)) '(7 27))))
                                       do (incf i))
                             (incf i))
                            (t (incf i)))))
                       ((char= c #\Return) (incf i))
                       (t (write-char c out) (incf i))))))))

(defun %read-forms (path)
  (when (probe-file path)
    (with-open-file (s path :external-format :utf-8)
      (let ((*read-eval* nil) (*package* (find-package :keyword)))
        (loop for form = (handler-case (read s nil s) (error () s))
              until (eq form s) collect form)))))

(defun demo-tty ()
  (banner "Demo 4: tui-backend-tty 0.1.0 (raw mode on a real pty)")
  (stage "4a. this process (stdout is a file)")
  (show "tty-available-p" (tty:tty-available-p))
  (show "interactive-stream-p *standard-output*" (interactive-stream-p *standard-output*))
  (handler-case (tui:run-tui (make-instance 'standard-object) :backend (tty:make-tty-backend))
    (tty:tty-not-a-terminal (c)
      (show "run-tui signalled" (type-of c))
      (show "  stream" (tty:tty-not-a-terminal-stream c))
      (show "  message" (princ-to-string c))
      (show "  a tui-backend-error?" (typep c 'tui:tui-backend-error))))

  (stage "4b. child SBCL on an 80x24 pty under sh: stty -g before / during / after")
  (let* ((report (uiop:native-namestring
                  (merge-pathnames (format nil "harness-tty-~36r.sexp"
                                           (random (expt 36 6) (make-random-state t)))
                                   (uiop:temporary-directory))))
         (command (list* "sh" "-c"
                         "echo \"SH-BEFORE $(stty -g)\"; \"$@\"; echo \"SH-EXIT $?\"; echo \"SH-AFTER $(stty -g)\""
                         "sh" (%child-argv report)))
         (pty (pp:spawn-pty command :cols 80 :rows 24))
         (output (make-string-output-stream))
         (seen ""))
    (unwind-protect
         (labels ((drain (timeout)
                  (let ((chunk (pp:pty-read pty :timeout timeout)))
                    (when (and chunk (plusp (length chunk)))
                      (write-string (babel:octets-to-string chunk :encoding :utf-8
                                                                  :errorp nil)
                                    output)
                      (setf seen (concatenate 'string seen (get-output-stream-string output))))
                    chunk))
                (wait-for (needle timeout)
                  (let ((deadline (+ (get-internal-real-time)
                                     (* timeout internal-time-units-per-second))))
                    (loop
                      (when (search needle seen) (return t))
                      (when (> (get-internal-real-time) deadline) (return nil))
                      (unless (drain 0.1) (return (and (search needle seen) t)))))))
           (show "child pid" (pp:pty-pid pty))
           (show "READY drawn by the child" (wait-for "READY" 240))
           (pp:pty-write pty (babel:string-to-octets "k" :encoding :utf-8))
           (show "sh finished" (wait-for "SH-AFTER" 60))
           (loop repeat 5 while (drain 0.1))
           (multiple-value-bind (code signal) (pp:pty-wait pty :timeout 5)
             (show "pty exit code / signal" (list code signal)))
           (note "sh lines from the pty:")
           (dolist (line (uiop:split-string (%strip-escapes seen) :separator '(#\Newline)))
             (when (or (search "SH-" line) (search "READY" line))
               (note "  ~a" (string-trim " " line))))
           (note "child report (~a):" report)
           (dolist (form (%read-forms report))
             (note "  ~s" form))
           (let* ((forms (%read-forms report))
                  (lines (uiop:split-string (%strip-escapes seen) :separator '(#\Newline)))
                  (before (some (lambda (l) (let ((p (search "SH-BEFORE " l)))
                                              (and p (string-trim " " (subseq l (+ p 10))))))
                                lines))
                  (after (some (lambda (l) (let ((p (search "SH-AFTER " l)))
                                             (and p (string-trim " " (subseq l (+ p 9))))))
                               lines))
                  (raw (getf (rest (assoc :raw forms)) :stty-g)))
             (show "stty -g before (sh)" before)
             (show "stty -g while raw (child)" raw)
             (show "stty -g after (sh)" after)
             (show "raw differs from before" (and raw before (not (string= raw before))))
             (show "restored exactly (before = after)" (and before after (string= before after)))))
      (pp:pty-close pty)
      (uiop:delete-file-if-exists report))))
