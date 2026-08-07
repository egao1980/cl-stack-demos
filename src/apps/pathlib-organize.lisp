;;;; Inspired by tfeldmann/organize — extension-bucket file rules (pathlib walk).
(in-package #:cl-stack-demos)

(defun %ext-bucket (path)
  (let* ((ext (string-downcase (or (stack-pathlib:extension path) "")))
         (ext (string-left-trim "." ext)))
    (cond ((member ext '("txt" "md" "rst") :test #'string=) "docs")
          ((member ext '("png" "jpg" "jpeg" "gif") :test #'string=) "images")
          ((member ext '("pdf") :test #'string=) "pdf")
          ((member ext '("py" "lisp" "js") :test #'string=) "code")
          (t "other"))))

(defun run-pathlib-organize ()
  (let* ((inbox (merge-pathnames "organize-inbox/" (fixtures-dir)))
         (fs (stack-pathlib:make-memory-filesystem))
         (counts (make-hash-table :test #'equal)))
    (stack-pathlib:with-filesystem (fs)
      ;; seed memory FS from fixture names (content optional)
      (dolist (p (uiop:directory-files inbox))
        (let* ((name (file-namestring p))
               (dest (stack-pathlib:join "/inbox" name)))
          (stack-pathlib:write-text dest (uiop:read-file-string p))))
      (dolist (p (stack-pathlib:iterdir "/inbox"))
        (when (stack-pathlib:file-p p)
          (let* ((bucket (%ext-bucket p))
                 (target (stack-pathlib:join "/" bucket (stack-pathlib:name p))))
            (stack-pathlib:with-auto-create-parents
              (stack-pathlib:mkdir (stack-pathlib:parent target) :parents t))
            (stack-pathlib:write-text target (stack-pathlib:read-text p))
            (incf (gethash bucket counts 0)))))
      (format t "~&; buckets:")
      (maphash (lambda (k v) (format t " ~A=~A" k v)) counts)
      (terpri)
      (assert (>= (hash-table-count counts) 3))
      (assert (stack-pathlib:exists-p "/docs/note.txt"))
      (assert (stack-pathlib:exists-p "/images/pic.png"))
      (assert (stack-pathlib:exists-p "/pdf/doc.pdf"))
      (assert (stack-pathlib:exists-p "/code/script.py"))
      t)))

(register-app "pathlib-organize"
              :title "Extension-bucket organizer (memory FS)"
              :upstream "https://github.com/tfeldmann/organize"
              :fn #'run-pathlib-organize)
