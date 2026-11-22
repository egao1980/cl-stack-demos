(in-package #:harness-demos-acp)

;;; The JSON-RPC wire log. The agent's transport reads and writes through
;;; these two Gray streams, which copy every whole line (one ACP message) to
;;; a log file: "-> " = client to agent (stdin), "<- " = agent to client
;;; (stdout). The log is written by the agent process, so the same code logs
;;; the headless run and the Zed run. Redaction: the project directory
;;; becomes <project>, the home directory ~ (in that order, longest first).

(defvar *wire-log-lock* (bt2:make-lock :name "acp wire log"))

(defclass wire-log ()
  ((stream :initarg :stream :reader wire-log-stream)
   (redactions :initarg :redactions :initform '() :reader wire-log-redactions
               :documentation "((from . to) …), applied in order.")
   (count :initform 0 :accessor wire-log-count)))

(defun %replace-all (string from to)
  (if (zerop (length from))
      string
      (with-output-to-string (out)
        (loop with start = 0
              for pos = (search from string :start2 start)
              do (write-string string out :start start :end (or pos (length string)))
                 (if pos
                     (progn (write-string to out) (setf start (+ pos (length from))))
                     (return))))))

(defun redact (log line)
  (reduce (lambda (s pair) (%replace-all s (car pair) (cdr pair)))
          (wire-log-redactions log) :initial-value line))

(defun log-line (log direction line)
  (bt2:with-lock-held (*wire-log-lock*)
    (let ((out (wire-log-stream log)))
      (format out "~3,'0D ~A ~A~%" (incf (wire-log-count log)) direction (redact log line))
      (finish-output out))))

(defun open-wire-log (path &key project)
  "A WIRE-LOG appending to PATH (created). PROJECT, a directory, is redacted."
  (let ((home (string-right-trim "/" (uiop:native-namestring (user-homedir-pathname))))
        (project (and project (string-right-trim "/" (uiop:native-namestring project)))))
    (make-instance 'wire-log
                   :stream (open (ensure-directories-exist path) :direction :output
                                 :if-exists :append :if-does-not-exist :create
                                 :external-format :utf-8)
                   :redactions (append (and project (list (cons project "<project>")))
                                       (list (cons home "~"))))))

;;; Input: lines are read whole by rpc-backend-stdio (READ-LINE).
(defclass logging-input (trivial-gray-streams:fundamental-character-input-stream)
  ((target :initarg :target :reader %target)
   (log :initarg :log :reader %log)))

(defmethod trivial-gray-streams:stream-read-line ((s logging-input))
  (multiple-value-bind (line missing-newline-p) (read-line (%target s) nil nil)
    (cond ((null line) (values "" t))
          (t (when (plusp (length (string-trim '(#\Space #\Return #\Tab) line)))
               (log-line (%log s) "->" (string-right-trim '(#\Return) line)))
             (values line missing-newline-p)))))

(defmethod trivial-gray-streams:stream-read-char ((s logging-input))
  (read-char (%target s) nil :eof))
(defmethod trivial-gray-streams:stream-unread-char ((s logging-input) char)
  (unread-char char (%target s)))
(defmethod trivial-gray-streams:stream-peek-char ((s logging-input))
  (peek-char nil (%target s) nil :eof))
(defmethod trivial-gray-streams:stream-listen ((s logging-input))
  (listen (%target s)))
(defmethod close ((s logging-input) &key abort)
  (close (%target s) :abort abort)
  (call-next-method))

;;; Output: characters are collected until a newline, then the line is logged.
(defclass logging-output (trivial-gray-streams:fundamental-character-output-stream)
  ((target :initarg :target :reader %target)
   (log :initarg :log :reader %log)
   (pending :initform (make-string-output-stream) :reader %pending)))

(defun %note-char (s char)
  (if (char= char #\Newline)
      (log-line (%log s) "<-" (get-output-stream-string (%pending s)))
      (write-char char (%pending s))))

(defmethod trivial-gray-streams:stream-write-char ((s logging-output) char)
  (%note-char s char)
  (write-char char (%target s)))
(defmethod trivial-gray-streams:stream-write-string ((s logging-output) string
                                                     &optional (start 0) end)
  (loop for i from start below (or end (length string))
        do (%note-char s (char string i)))
  (write-string string (%target s) :start start :end end)
  string)
(defmethod trivial-gray-streams:stream-line-column ((s logging-output)) nil)
(defmethod trivial-gray-streams:stream-force-output ((s logging-output))
  (force-output (%target s)))
(defmethod trivial-gray-streams:stream-finish-output ((s logging-output))
  (finish-output (%target s)))
(defmethod close ((s logging-output) &key abort)
  (when (open-stream-p (%target s)) (finish-output (%target s)))
  (close (%target s) :abort abort)
  (call-next-method))

(defun stdio-streams ()
  "UTF-8 character streams on fd 0 / 1 (SBCL)."
  (values (sb-sys:make-fd-stream 0 :input t :external-format :utf-8 :buffering :full)
          (sb-sys:make-fd-stream 1 :output t :external-format :utf-8 :buffering :full)))
