(in-package #:harness-demos-fs)

;;; Output helpers: one banner per demo, one line per step, values printed as is.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun stage (fmt &rest args)
  (format t "~&~%-- ~?~%" fmt args))

(defun show (label value)
  (format t "~&   ~a: ~s~%" label value))

(defun note (fmt &rest args)
  (format t "~&   ~?~%" fmt args))

(defun short (id &optional (n 12))
  "First N characters of an id string (for readable output)."
  (if (and (stringp id) (> (length id) n)) (subseq id 0 n) id))

(defvar *outer-restarts* nil
  "Restarts established outside the demos (toplevel ABORT, EXIT …), left out of listings.")

(defun restart-names (condition)
  "Names of the restarts CONDITION offers, without the toplevel ones."
  (loop for r in (compute-restarts condition)
        for name = (restart-name r)
        when (and name (not (member r *outer-restarts*))) collect name))

(defmacro expecting ((var type) form &body on-signal)
  "Evaluate FORM with a HANDLER-BIND for TYPE; on a signal run ON-SIGNAL with VAR
bound (it may invoke a restart). If FORM returns without signalling, say so:
the demo expected a condition."
  (let ((signalled (gensym "SIGNALLED")))
    `(let ((,signalled nil))
       (multiple-value-prog1
           (handler-bind ((,type (lambda (,var)
                                   (setf ,signalled t)
                                   ,@on-signal)))
             ,form)
         (unless ,signalled
           (note "UNEXPECTED: ~s was not signalled" ',type))))))

;;; Temp directories (created and deleted by each demo).

(defun make-temp-dir (prefix)
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~a-~36r" prefix (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defun delete-temp-dir (dir)
  "Delete the temp directory DIR. On Windows, first clear the read-only
attribute (git writes its objects read-only, and DELETE-FILE cannot remove
those there)."
  (when (and (windows-p) (probe-file dir))
    (proc:run (list "attrib" "-R" (concatenate (quote string) (native dir) "*") "/S" "/D")))
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(defmacro with-temp-dir ((var prefix) &body body)
  `(let ((,var (make-temp-dir ,prefix)))
     (unwind-protect (progn ,@body)
       (delete-temp-dir ,var))))

(defun native (pathname) (uiop:native-namestring pathname))

;;; Windows. Git for Windows puts its POSIX tools (sh, env, printf, cat, sleep)
;;; on PATH, so most steps run unchanged through them; steps that need what
;;; those tools cannot give there (symlinks, signals, process groups) are
;;; skipped with a printed reason.

(defun windows-p () (uiop:os-windows-p))

(defun skipped-on-windows (what reason)
  "On Windows print that WHAT is skipped and why → T; elsewhere NIL."
  (when (windows-p)
    (note "skipped on Windows: ~a — ~a" what reason)
    t))

(defun find-on-path (name)
  "Absolute native namestring of the program NAME (NAME.exe on Windows) in a
PATH directory, or NIL."
  (let ((file (if (windows-p) (concatenate (quote string) name ".exe") name)))
    (dolist (dir (uiop:split-string (or (uiop:getenv "PATH") "") :separator (if (windows-p) ";" ":")))
      (unless (string= dir "")
        (let ((p (probe-file (merge-pathnames file (uiop:ensure-directory-pathname
                                                    (uiop:parse-native-namestring dir))))))
          (when p (return (uiop:native-namestring p))))))))

(defun posix-tool (unix-path)
  "UNIX-PATH (\"/bin/sh\") on Unix; on Windows the program of that name found on
PATH (Git for Windows' copy), as an absolute path."
  (if (windows-p)
      (or (find-on-path (pathname-name unix-path))
          (error "~a not found on PATH" (pathname-name unix-path)))
      unix-path))

;;; Running programs through process-protocol (the stack's own subprocess API).

(defun sh (directory script &key env)
  "Run /bin/sh -c SCRIPT (sh from PATH on Windows) in DIRECTORY → stdout string. Non-zero exit → error."
  (multiple-value-bind (code out err)
      (proc:run (list (posix-tool "/bin/sh") "-c" script) :directory directory :env env
                                             :output :string :error-output :string)
    (unless (zerop code)
      (error "sh ~s exited ~a: ~a" script code err))
    out))

(defun tool (directory env &rest argv)
  "Run ARGV in DIRECTORY with ENV merged → stdout string; non-zero exit → error."
  (multiple-value-bind (code out err)
      (proc:run argv :directory directory :env env :output :string :error-output :string)
    (unless (zerop code)
      (error "~{~a~^ ~} exited ~a: ~a" argv code err))
    out))

(defun isolated-env (home)
  "Environment for git / jj test repositories: own HOME, empty global config,
fixed identity, never the user's configuration."
  (let ((global (merge-pathnames ".gitconfig" home)))
    (ensure-directories-exist home)
    (with-open-file (s global :direction :output :if-exists :supersede)
      (write-string "" s))
    (list (cons "HOME" (native home))
          (cons "XDG_CONFIG_HOME" (native (merge-pathnames ".config/" home)))
          (cons "GIT_CONFIG_GLOBAL" (native global))
          (cons "GIT_CONFIG_NOSYSTEM" "1")
          (cons "GIT_AUTHOR_NAME" "Demo User")
          (cons "GIT_AUTHOR_EMAIL" "demo@example.invalid")
          (cons "GIT_COMMITTER_NAME" "Demo User")
          (cons "GIT_COMMITTER_EMAIL" "demo@example.invalid")
          (cons "JJ_USER" "Demo User")
          (cons "JJ_EMAIL" "demo@example.invalid")
          (cons "JJ_CONFIG" (native (merge-pathnames "jj-config.toml" home))))))

(defun write-file (pathname text)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (write-string text s))
  pathname)

(defun read-file (pathname)
  (uiop:read-file-string pathname :external-format :utf-8))

(defun lines (&rest strings)
  (format nil "~{~a~%~}" strings))
