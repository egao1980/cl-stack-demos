(in-package #:harness-demos-code)

;;; Output helpers, a temporary project, and RUN-CLI with captured streams.

(defvar *redactions* '()
  "(from . to) pairs applied to everything the demo prints: the temporary
project and config directories become <tmp>/…, the home directory ~/.")

(defun redact (text)
  (let ((text (princ-to-string text)))
    (loop for (from . to) in *redactions*
          do (loop for p = (search from text)
                   while p
                   do (setf text (concatenate 'string (subseq text 0 p) to
                                              (subseq text (+ p (length from)))))))
    text))

(defun banner (title)
  (format t "~&~%~a~%== ~a~%~a~%" (make-string 72 :initial-element #\=)
          title (make-string 72 :initial-element #\=))
  (finish-output))

(defun stage (title)
  (format t "~&~%--- ~a~%" title)
  (finish-output))

(defun note (control &rest args)
  (format t "~&~a~%" (redact (apply #'format nil control args)))
  (finish-output))

(defun show (label value)
  "LABEL: VALUE; a multi-line string goes on indented lines."
  (let ((text (if (stringp value) value (prin1-to-string value))))
    (if (find #\Newline text)
        (format t "~&~a:~%~{  | ~a~%~}" label
                (uiop:split-string (redact (string-right-trim '(#\Newline) text))
                                   :separator '(#\Newline)))
        (format t "~&~a: ~a~%" label (redact text))))
  (finish-output)
  value)

(defun clip (text &optional (n 240))
  "TEXT redacted, then cut to N characters (redacting first, so a path cut short
is never printed)."
  (setf text (redact text))
  (if (> (length text) n)
      (format nil "~a… [~d chars]" (subseq text 0 n) (length text))
      text))

(defun lines (text)
  (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=))

;;; Temporary trees.

(defun %native (pathname) (uiop:native-namestring pathname))

(defun write-file (root relative text)
  (let ((path (merge-pathnames relative root)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-string text s))
    path))

(defun read-file (root relative)
  (uiop:read-file-string (merge-pathnames relative root) :external-format :utf-8))

(defvar *scratch* nil "This run's temporary directory (truename).")

(defun scratch-dir (name)
  "A fresh directory NAME/ under this run's scratch directory."
  (let ((dir (merge-pathnames (format nil "~a/" name) *scratch*)))
    (when (probe-file dir)
      (uiop:delete-directory-tree dir :validate t))
    (ensure-directories-exist dir)
    (truename dir)))

(defun scratch-redactions (scratch more)
  "The (from . to) pairs for SCRATCH: its path (also without the leading
slash, as a model may write it), the project key of sessions and plans
(cl-stack-code 0.3.0+: the path with / turned into -), the home directory, the
run directory's own name (the status line cuts a path in the middle) and the
login name (ls -l output)."
  (let* ((native (%native scratch))
         (name (car (last (pathname-directory scratch))))
         (user (or (uiop:getenv "USER") (uiop:getenv "USERNAME"))))
    (append (list (cons native "<tmp>/")
                  (cons (subseq native 1) "<tmp-without-slash>/"))
            ;; macOS: /private/var/… is also reached as /var/…
            (and (uiop:string-prefix-p "/private/" native)
                 (list (cons (subseq native 8) "<tmp>/")))
            (list (cons (substitute #\- #\/ (string-right-trim "/" native)) "<tmp-key>")
                  (cons (%native (user-homedir-pathname)) "~/")
                  (cons name "<run>"))
            (and user (>= (length user) 4) (list (cons user "<user>")))
            more)))

(defmacro with-scratch (() &body body)
  "Bind *SCRATCH* to a fresh temporary directory, redacted as <tmp>/, and
delete it afterwards."
  `(let* ((*scratch* (let ((d (merge-pathnames
                               (format nil "cl-stack-code-demo-~36r/" (random (expt 36 6)
                                                                              (make-random-state t)))
                               (uiop:temporary-directory))))
                       (ensure-directories-exist d)
                       (truename d)))
          (*redactions* (scratch-redactions *scratch* *redactions*)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree *scratch* :validate t :if-does-not-exist :ignore))))

(defun make-project (name files)
  "A git-rooted project NAME/ (a bare .git/ directory is enough for
cl-stack-code's project root) holding FILES, ((relative text) …)."
  (let ((root (scratch-dir name)))
    (ensure-directories-exist (merge-pathnames ".git/" root))
    (loop for (relative text) in files do (write-file root relative text))
    root))

;;; Running the CLI.

(defun data-dir ()
  "This run's private data directory (sessions and plans; cl-stack-code 0.3.0+),
never the user's ~/.local/share."
  (ensure-directories-exist (merge-pathnames "data/" *scratch*)))

(defun cli (argv &key cwd config (input ""))
  "RUN-CLI with ARGV (prefixed by --cwd CWD and the current model's --model),
the private CONFIG directory and the current model's providers and
environment. → (values exit stdout stderr)."
  (let* ((argv (append (and cwd (list "--cwd" (%native cwd)))
                       (list "--model" (model-designator))
                       argv))
         (err (make-string-output-stream))
         (exit nil)
         (out (with-output-to-string (o)
                (setf exit (code:run-cli argv :output o :error-output err
                                              :input (make-string-input-stream input)
                                              :providers (model-providers)
                                              :user-config-dir config
                                              :getenv (model-getenv)
                                              :data-dir (data-dir))))))
    (values exit out (get-output-stream-string err))))

(defun show-argv (argv)
  (note "$ cl-stack-code --model ~a ~{~a~^ ~}" (model-designator)
        (mapcar (lambda (a) (if (find #\Space a) (format nil "'~a'" a) a)) argv)))

(defun decode-lines (out)
  "stream-json OUT → list of hash tables."
  (mapcar #'json:decode (lines out)))

(defun jget (object &rest keys)
  (dolist (k keys object)
    (setf object (and (hash-table-p object) (gethash k object)))))

