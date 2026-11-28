(defpackage #:harness-demos-data
  (:use #:cl)
  (:local-nicknames (#:d #:diff-protocol)
                    (#:dl #:diff-backend-lisp-source)
                    (#:rev #:revision-protocol)
                    (#:dt #:datetime-protocol)
                    (#:orm #:sql-orm)
                    (#:mig #:sql-migrate)
                    (#:sql #:sql-protocol)
                    (#:q #:sql-query)
                    (#:qd #:sql-query-duckdb)
                    (#:ddb #:cl-stack-duckdb)
                    (#:dda #:cl-stack-duckdb/arrow)
                    (#:arrow #:arrow-protocol))
  (:export #:run-demos))

(in-package #:harness-demos-data)

;;; Demo scaffolding: banners, labelled output, and one HANDLER-CASE per step
;;; so a failing step prints its condition and the demo carries on.

(defvar *indent* "    ")

(defun banner (title)
  (format t "~&~%~A~%== ~A~%~A~%" (make-string 72 :initial-element #\=)
          title (make-string 72 :initial-element #\=)))

(defun say (fmt &rest args)
  "One labelled output line."
  (format t "~&~A~?~%" *indent* fmt args))

(defun show-text (label text)
  "LABEL, then TEXT verbatim, each line indented and framed by |."
  (format t "~&~A~A:~%" *indent* label)
  (with-input-from-string (s text)
    (loop for line = (read-line s nil)
          while line do (format t "~A  | ~A~%" *indent* line)))
  (when (and (plusp (length text))
             (char/= #\Newline (char text (1- (length text)))))
    (format t "~A  (no newline at end)~%" *indent*)))

(defun check (label value)
  "Print LABEL => VALUE, flagging NIL as a failed expectation."
  (format t "~&~A~A => ~S~:[  <-- UNEXPECTED~;~]~%" *indent* label value value)
  value)

(defmacro demo-step ((title) &body body)
  "Print TITLE; run BODY; print and swallow an ERROR that escapes it."
  `(progn
     (format t "~&~%--- ~A~%" ,title)
     (handler-case (progn ,@body)
       (error (e)
         (format t "~&~A!! STEP FAILED: ~A: ~A~%" *indent* (type-of e) e)))))
