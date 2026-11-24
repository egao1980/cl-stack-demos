;;;; The tour.

(in-package #:harness-demos-documents)

(defun run-demos (&key (dir (here "output/")))
  "Print the tour and write the image section's PNGs to DIR. Returns T when
every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '()) (*skipped* '())
        (*print-right-margin* 110)
        (crypto:*crypto-backend* (crypto-backend-ironclad:make-ironclad-crypto-backend)))
    (ensure-directories-exist dir)
    (format t "~&Documents and data formats (~a ~a, ~a ~a)~%~
               ~{~a ~a~^, ~}~%"
            (software-type) (machine-type) (lisp-implementation-type) (lisp-implementation-version)
            (loop for s in '("doc-extract-backend-pdf" "doc-extract-protocol" "compression-protocol"
                             "yaml-protocol" "cl-stack-config" "image-protocol" "image-backend-stb")
                  collect s collect (version-of s)))
    (section "1. PDF text with no password (doc-extract-backend-pdf#9)" (demo-pdf))
    (section "2. DOCX and XLSX (doc-extract-protocol#6, compression-protocol#10)" (demo-office))
    (section "3. YAML block output with no trailing blank (yaml-protocol#7)" (demo-yaml))
    (section "4. .env files and typed settings (cl-stack-config#8)" (demo-dotenv))
    (section "5. Image editing operations (image-protocol#2)" (demo-image dir))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%~
               ~:[~;Cross-reads skipped (no uv): ~:*~{~a~^, ~}.~%~]"
            *checks* *mismatches* (reverse *failed-sections*)
            (remove-duplicates (reverse *skipped*) :test #'string= :from-end t))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
