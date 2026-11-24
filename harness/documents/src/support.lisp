;;;; Output, self-checks and the Python cross-reads shared by every section.

(in-package #:harness-demos-documents)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")
(defvar *skipped* '() "Cross-reads skipped because uv is not on PATH.")

(defun out (control &rest args)
  (apply #'format t control args))

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (actual expected label &key (test #'equal))
  "Count one self-check; print it only when it fails. Returns ACTUAL."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (let ((*print-pretty* nil))
      (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected)))
  actual)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

(defun signalled (thunk)
  "The ERROR THUNK signals, or NIL when it returns."
  (handler-case (progn (funcall thunk) nil)
    (error (c) c)))

(defun one-line (string &optional (max 150))
  "STRING on one line (newlines as \\n), cut to MAX characters."
  (let ((flat (with-output-to-string (s)
                (loop for c across string
                      do (case c
                           (#\Newline (write-string "\\n" s))
                           (#\Tab (write-string "\\t" s))
                           (t (write-char c s)))))))
    (if (> (length flat) max)
        (concatenate 'string (subseq flat 0 (- max 3)) "...")
        flat)))

(defun show-lines (text &key (prefix "  | "))
  "Print TEXT line by line, each line ended by $ so trailing blanks show."
  (dolist (line (uiop:split-string text :separator '(#\Newline)))
    (out "~a~a$~%" prefix line)))

(defun words (string)
  "STRING split at whitespace, empty pieces dropped."
  (remove "" (uiop:split-string string :separator '(#\Space #\Tab #\Newline #\Return))
          :test #'string=))

;;; Files ---------------------------------------------------------------------

(defun here (relative)
  (asdf:system-relative-pathname "harness-demos-documents" relative))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun write-octets (octets path)
  (with-open-file (o path :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence octets o))
  path)

(defun tidy (namestring)
  "NAMESTRING with the home directory written as ~/ and the temp directory as <tmp>/."
  (flet ((swap (s prefix replacement)
           (let ((p (search prefix s)))
             (if p
                 (concatenate 'string (subseq s 0 p) replacement (subseq s (+ p (length prefix))))
                 s))))
    (let ((s (swap namestring (namestring (uiop:temporary-directory)) "<tmp>/")))
      (swap s (namestring (user-homedir-pathname)) "~/"))))

(defun tmp-file (name)
  (merge-pathnames (format nil "harness-demos-documents-~a" name)
                   (uiop:temporary-directory)))

(defun version-of (system)
  (asdf:component-version (asdf:find-system system)))

(defun sha256-hex (octets)
  "SHA-256 of OCTETS through crypto-protocol, lower-case Base16 through encoding-protocol."
  (string-downcase (stack-encoding:encode (crypto:digest octets :algorithm :sha256)
                                          :encoding :base16)))

;;; Python cross-reads through uv -------------------------------------------------

(defvar *uv* :unknown)

(defun uv-available-p ()
  (when (eq *uv* :unknown)
    (setf *uv* (handler-case (zerop (proc:run '("uv" "--version") :output nil
                                                                   :error-output nil :timeout 60))
                 (error () nil))))
  *uv*)

(defun python (script packages &rest args)
  "Run crosscheck/SCRIPT under `uv run --no-project --with P ... python -I`
with ARGS, and decode the JSON it prints (yaml-protocol reads JSON: JSON is
YAML). NIL, with a note, when uv is not on PATH."
  (cond
    ((not (uv-available-p))
     (push script *skipped*)
     (out "  (uv is not on PATH: the ~a cross-read is skipped)~%" script)
     nil)
    (t
     (multiple-value-bind (code stdout stderr)
         (proc:run (append (list "uv" "run" "--no-project")
                           (loop for p in packages append (list "--with" p))
                           (list "python" "-I" (namestring (here (format nil "crosscheck/~a" script))))
                           (mapcar (lambda (a) (if (pathnamep a) (namestring a) a)) args))
                   :output :string :error-output :string :timeout 900
                   :env '(("PYTHONIOENCODING" . "utf-8")))
       (check code 0 (format nil "uv run ~a exit code" script))
       (if (zerop code)
           (yaml:decode stdout)
           (progn (out "~a~%" stderr) nil))))))

(defun jget (object &rest keys)
  "Walk OBJECT (hash-tables with string keys, vectors with integer indices)."
  (dolist (k keys object)
    (setf object (if (integerp k) (aref object k) (gethash k object)))))

(defun python-line (label)
  (out "~&  ~a~%" label))
