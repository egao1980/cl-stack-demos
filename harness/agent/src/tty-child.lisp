;;; Loaded by the child Lisp that demo 4 starts on a pty (not compiled into
;;; harness-demos-agent). Runs a one-key TUI on tui-backend-tty and appends
;;; one readable form per line to a report file.

(defpackage #:harness-demos-agent/tty-child
  (:use #:cl)
  (:local-nicknames (#:tui #:tui-protocol) (#:tty #:tui-backend-tty))
  (:export #:main))
(in-package #:harness-demos-agent/tty-child)

(defvar *report* nil)

(defun report (key &rest plist)
  (with-open-file (s *report* :direction :output :if-exists :append
                              :if-does-not-exist :create :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (prin1 (cons key plist) s)
        (terpri s)))))

(defun stty-g ()
  (string-trim '(#\Space #\Newline #\Return)
               (uiop:run-program '("stty" "-g") :input :interactive :output :string
                                                :error-output nil :ignore-error-status t)))

(defun stty-flags ()
  "icanon / echo / isig as stty -a shows them."
  (let ((a (uiop:run-program '("stty" "-a") :input :interactive :output :string
                                            :error-output nil :ignore-error-status t)))
    (loop for flag in '("icanon" "echo" "isig")
          collect (if (search (concatenate 'string "-" flag " ") a) (concatenate 'string "-" flag) flag))))

(defclass one-key () ())

(defmethod tui:tui-init ((p one-key))
  (report :raw :stty-g (stty-g) :flags (stty-flags))
  (values nil nil))

(defmethod tui:tui-update ((p one-key) model (m tui:key-message))
  (declare (ignore model))
  (report :key :key (tui:key-message-key m) :text (tui:key-message-text m))
  (values m :quit))

(defmethod tui:tui-update ((p one-key) model m)
  (declare (ignore m))
  (values model nil))

(defmethod tui:tui-view ((p one-key) model canvas)
  (declare (ignore model))
  (tui:draw-text canvas 0 0 (format nil "READY ~dx~d press a key"
                                    (tui:canvas-cols canvas) (tui:canvas-rows canvas))))

(defun main (report)
  (setf *report* report)
  (report :before :stty-g (stty-g) :flags (stty-flags) :available (tty:tty-available-p))
  (handler-case
      (let ((final (tui:run-tui (make-instance 'one-key) :backend (tty:make-tty-backend))))
        (report :returned :final-key (and final (tui:key-message-key final))))
    (error (c) (report :error :type (string (type-of c)) :message (princ-to-string c))))
  (report :after :stty-g (stty-g) :flags (stty-flags)))
