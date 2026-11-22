(in-package #:harness-demos-css)

;;; Output helpers: one banner per demo, one line per step, values printed as is.

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun stage (fmt &rest args)
  (format t "~&~%-- ~?~%" fmt args))

(defun show (label value)
  (format t "~&   ~a: ~s~%" label value))

(defun note (fmt &rest args)
  (format t "~&   ~?~%" fmt args))

(defun block-text (text &key (indent 3) (max-lines nil))
  "TEXT indented, optionally cut to MAX-LINES lines (the cut is said)."
  (let* ((lines (uiop:split-string (string-right-trim '(#\Newline) text)
                                   :separator '(#\Newline)))
         (n (length lines))
         (pad (make-string indent :initial-element #\Space)))
    (loop for line in lines
          for i from 1
          while (or (null max-lines) (<= i max-lines))
          do (format t "~&~a~a~%" pad line))
    (when (and max-lines (> n max-lines))
      (format t "~&~a... (~d more lines)~%" pad (- n max-lines)))))

(defun pretty (sheet) (css:write-stylesheet sheet :pretty t))
(defun mini (sheet) (css:write-stylesheet sheet :pretty nil))

(defun utf-8-length (string)
  (length (sb-ext:string-to-octets string :external-format :utf-8)))

(defun rule-label (node)
  "A one-line name for a top-level stylesheet node."
  (typecase node
    (css:rule (format nil "~{~a~^, ~}"
                      (mapcar (lambda (s) (css:selector-text s :pretty nil))
                              (css:rule-selectors node))))
    (css:media-rule (format nil "@media ~a (~d rule~:p)" (css:media-rule-query node)
                            (length (css:at-rule-rules node))))
    (css:supports-rule (format nil "@supports ~a (~d rule~:p)" (css:supports-rule-condition node)
                               (length (css:at-rule-rules node))))
    (css:keyframes-rule (format nil "@keyframes ~a" (css:keyframes-rule-name node)))
    (css:at-rule (format nil "@~a" (css:at-rule-name node)))
    (t (princ-to-string node))))

(defun count-style-rules (sheet)
  "Style rules in SHEET, counting those inside grouping at-rules."
  (labels ((walk (nodes)
             (loop for n in nodes
                   sum (typecase n
                         (css:rule 1)
                         (css:keyframes-rule 0)
                         (css:at-rule (walk (ignore-errors (css:at-rule-rules n))))
                         (t 0)))))
    (walk (css:stylesheet-rules sheet))))

(defun write-file (pathname text)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (write-string text s))
  pathname)
