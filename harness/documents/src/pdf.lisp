;;;; 1. PDF text with no password (doc-extract-backend-pdf#9).

(in-package #:harness-demos-documents)

(defparameter *pdf-words*
  '("Quarterly" "report" "Sales" "grew" "in" "both" "regions." "North" "120" "135,"
    "South" "98" "110." "Notes" "R&D" "grew" "too.")
  "Every word make-fixtures.py draws, in page order.")

(defparameter *pdf-password* "open sesame" "The user password of report-locked.pdf.")

(defun demo-pdf ()
  (let* ((path (here "fixtures/report.pdf"))
         (octets (read-octets path))
         (locked (here "fixtures/report-locked.pdf"))
         (backend (pdf:make-pdf-doc-extract-backend)))
    (out "libpdfium loaded: ~s~%" (pdf:pdfium-available-p))
    (check (pdf:pdfium-available-p) t "libpdfium from the package's native overlay")
    (out "fixtures/report.pdf: ~:d bytes, two A5 pages written by reportlab, not encrypted.~%~
          (make-pdf-doc-extract-backend) has :password NIL, so libpdfium gets a NULL password.~%~
          0.1.4 passed NIL to a :string argument: every unencrypted PDF failed with~%~
          \"NIL is not a Lisp string or pointer.\"~%~%" (length octets))
    (let ((from-path (dx:extract-text backend path :format :pdf))
          (from-octets (dx:extract-text backend octets :format :pdf)))
      (out "extract-text, pathname source:~%")
      (show-lines from-path)
      (check (words from-path) *pdf-words* "every word of report.pdf")
      (check from-octets from-path "octet source = pathname source")
      (out "extract-text, octet source: the same ~d characters.~%" (length from-octets))
      (let ((meta (dx:extract-metadata backend path :format :pdf)))
        (out "extract-metadata: ~s~%" meta)
        (check (getf meta :title) "Quarterly report" ":title")
        (check (getf meta :page-count) 2 ":page-count"))
      (let ((empty (dx:extract-text (pdf:make-pdf-doc-extract-backend :password "")
                                    path :format :pdf)))
        (check empty from-path ":password \"\" gives the same text")
        (out ":password \"\" gives the same text: ~s~%" (string= empty from-path)))
      (out "~%fixtures/report-locked.pdf: the same pages, AES-256, user password ~s.~%" *pdf-password*)
      (dolist (password (list nil "wrong"))
        (let ((c (signalled (lambda ()
                              (dx:extract-text (pdf:make-pdf-doc-extract-backend :password password)
                                               locked :format :pdf)))))
          (out "  :password ~s => ~a: ~a~%" password (and c (type-of c)) (and c (one-line (princ-to-string c))))
          (check (typep c 'dx:doc-extract-error) t (format nil ":password ~s signals doc-extract-error" password))))
      (let ((unlocked (dx:extract-text (pdf:make-pdf-doc-extract-backend :password *pdf-password*)
                                       locked :format :pdf)))
        (out "  :password ~s => the text of report.pdf: ~s~%" *pdf-password* (string= unlocked from-path))
        (check unlocked from-path "the right password opens the locked copy"))
      (let ((c (signalled (lambda () (pdf:make-pdf-doc-extract-backend :password 42)))))
        (out "  :password 42 => ~a (check-type)~%" (and c (type-of c)))
        (check (typep c 'type-error) t ":password 42 is a type-error"))
      (out "~%pypdf's view of the same files:~%")
      (let ((plain (python "pypdf_text.py" '("pypdf==6.1.1") path))
            (no-password (python "pypdf_text.py" '("pypdf==6.1.1" "cryptography==46.0.3") locked))
            (right (python "pypdf_text.py" '("pypdf==6.1.1" "cryptography==46.0.3") locked *pdf-password*)))
        (when plain
          (let ((pages (coerce (jget plain "pages") 'list)))
            (out "  pypdf ~a, report.pdf: ~d pages, title ~s~%" (jget plain "version")
                 (length pages) (jget plain "title"))
            (dolist (page pages) (show-lines page :prefix "  pypdf | "))
            (check (loop for p in pages append (words p)) (words from-path)
                   "pypdf and libpdfium give the same words")))
        (when no-password
          (out "  pypdf, report-locked.pdf with no password: ~a (~a)~%"
               (jget no-password "error") (jget no-password "message"))
          (check (jget no-password "error") "pypdf.errors.FileNotDecryptedError"
                 "pypdf refuses without the password"))
        (when right
          (check (loop for p across (jget right "pages") append (words p)) (words from-path)
                 "pypdf with the password: the same words")
          (out "  pypdf, report-locked.pdf with ~s: the same words as libpdfium~%" *pdf-password*))))))
