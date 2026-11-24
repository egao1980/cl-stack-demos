;;;; 2. DOCX and XLSX (doc-extract-protocol#6, compression-protocol#10).

(in-package #:harness-demos-documents)

(defun block-outline (blocks &optional (depth 0))
  "Print BLOCKS (an extracted-document's tree) one line per block."
  (let ((indent (make-string (* 2 depth) :initial-element #\Space)))
    (dolist (b blocks)
      (typecase b
        (dx:section
         (out "  ~asection ~s (level ~d)~%" indent (dx:section-title b) (dx:section-level b))
         (block-outline (dx:section-children b) (1+ depth)))
        (dx:text-block
         (out "  ~atext ~(~a~): ~s~%" indent (dx:text-block-kind b) (dx:text-block-text b)))
        (dx:table-block
         (out "  ~atable ~d x ~d:~%" indent (dx:table-block-rows b) (dx:table-block-cols b))
         (dolist (line (uiop:split-string (string-right-trim '(#\Newline) (dx:render-table b))
                                          :separator '(#\Newline)))
           (out "  ~a  ~a~%" indent line)))
        (t (out "  ~a~(~a~)~%" indent (dx:block-node-type b)))))))

(defun all-blocks (blocks)
  "BLOCKS and every block under a section, depth first."
  (loop for b in blocks
        collect b
        when (typep b 'dx:section)
          append (all-blocks (dx:section-children b))))

(defun table-grid (table)
  "TABLE as a list of rows of cell strings (a cell's blocks' plain text)."
  (let ((grid (make-array (list (dx:table-block-rows table) (dx:table-block-cols table))
                          :initial-element "")))
    (dolist (cell (dx:table-block-cells table))
      (setf (aref grid (dx:table-cell-row cell) (dx:table-cell-col cell))
            (format nil "~{~a~^ ~}" (mapcar #'dx:block-plain-text (dx:table-cell-content cell)))))
    (loop for r below (array-dimension grid 0)
          collect (loop for c below (array-dimension grid 1) collect (aref grid r c)))))

(defun python-cell (value)
  "An openpyxl cell value (decoded from JSON) as the text the Lisp table holds."
  (cond ((eq value :null) "")
        ((stringp value) value)
        ((integerp value) (format nil "~d" value))
        ((floatp value) (let ((*read-default-float-format* 'double-float))
                          (prin1-to-string value)))
        (t (princ-to-string value))))

(defun archive-files (octets)
  "((name octets) ...) of every entry of the zip OCTETS, in archive order."
  (let ((archive (zip:open-archive octets :format :zip)))
    (unwind-protect
         (loop for e in (zip:archive-entries archive)
               unless (zip:archive-entry-directory-p e)
                 collect (list (zip:archive-entry-name e) (zip:read-entry archive e)))
      (zip:close-archive archive))))

(defun count-octets (needle octets)
  (loop with n = (length needle)
        for start = 0 then (+ p n)
        for p = (search needle octets :start2 start)
        while p
        count t))

(defparameter +eocd+ (coerce #(#x50 #x4b #x05 #x06) '(vector (unsigned-byte 8)))
  "PK\\5\\6, the zip end-of-central-directory signature.")

(defun demo-office ()
  (let* ((backend (dx:make-default-doc-extract-backend))
         (docx-path (here "fixtures/report.docx"))
         (xlsx-path (here "fixtures/report.xlsx"))
         (docx (read-octets docx-path))
         (xlsx (read-octets xlsx-path))
         (docx-text (dx:extract-text backend docx-path :format :docx))
         (xlsx-text (dx:extract-text backend xlsx-path :format :xlsx))
         (docx-doc (dx:extract-document backend docx-path :format :docx))
         (xlsx-doc (dx:extract-document backend xlsx-path :format :xlsx)))
    (out "0.3.1 read the openpyxl workbook as \"\" and ran the python-docx table cells together~%~
          (\"regionq2North135\"), split runs mid-word and kept \"&amp;\".~%~%")
    (out "fixtures/report.docx (python-docx, ~:d bytes): extract-text~%" (length docx))
    (show-lines docx-text)
    (out "extract-sections: ~s~%"
         (mapcar (lambda (s) (list (dx:section-title s) (dx:section-level s)))
                 (dx:extract-sections backend docx-path :format :docx)))
    (out "extract-document:~%")
    (block-outline (dx:extracted-document-blocks docx-doc))
    (check (first (uiop:split-string docx-text :separator '(#\Newline))) "Quarterly report" "first line")
    (check (and (search "Sales grew in both regions." docx-text) t) t "runs joined without a break")
    (check (and (search "R&D grew too." docx-text) t) t "&amp; decoded")
    (check (and (search (format nil "North~a120~a135" #\Tab #\Tab) docx-text) t) t "cells separated by Tab")
    (out "~%fixtures/report.xlsx (openpyxl, ~:d bytes): extract-text~%" (length xlsx))
    (show-lines xlsx-text)
    (out "extract-document:~%")
    (block-outline (dx:extracted-document-blocks xlsx-doc))
    (check (mapcar #'dx:section-title (dx:extracted-document-blocks xlsx-doc)) '("Totals" "Notes")
           "one section per sheet, in workbook order")
    ;; A package that stores another zip-based file uncompressed (compression-protocol#10).
    (let* ((files (append (archive-files docx)
                          (list (list "word/embeddings/Microsoft_Excel_Worksheet.xlsx" xlsx))))
           (stored (zip:write-archive-bytes files))
           (stored-path (write-octets stored (tmp-file "embedded.docx"))))
      (out "~%report.docx re-zipped by compression-protocol's writer (stored, like zipfile's~%~
            default ZIP_STORED) with report.xlsx added as word/embeddings/Microsoft_Excel_Worksheet.xlsx:~%~
            ~:d bytes, ~d entries, ~d end-of-central-directory signatures (the inner one first).~%"
           (length stored) (length files) (count-octets +eocd+ stored))
      (check (count-octets +eocd+ stored) 2 "two end records")
      (let ((text (dx:extract-text backend stored :format :docx)))
        (out "extract-text gives the text of report.docx: ~s~%~
              (compression-protocol 0.5.0 took the inner record first: \"not a docx package (zip):~%~
              compression error: corrupt zip central directory\")~%" (string= text docx-text))
        (check text docx-text "the stored package reads like the original"))
      ;; Damaged uploads.
      (let* ((truncated (subseq docx 0 (floor (length docx) 2)))
             (flipped (let ((copy (copy-seq docx))
                            (at (+ (search (encoding-protocol:encode "word/document.xml") docx) 60)))
                        (setf (aref copy at) (logxor #xff (aref copy at)))
                        copy))
             (truncated-path (write-octets truncated (tmp-file "truncated.docx")))
             (flipped-path (write-octets flipped (tmp-file "flipped.docx"))))
        (out "~%Damaged uploads signal doc-extract-error, with the zip or XML condition as its cause:~%")
        (loop for (label source) in (list (list "the first half of report.docx" truncated)
                                          (list "report.docx with one byte of word/document.xml's data inverted" flipped)
                                          (list "a text file named .docx" (encoding-protocol:encode "just text")))
              do (let ((c (signalled (lambda () (dx:extract-text backend source :format :docx)))))
                   (out "  ~a =>~%    ~a: ~a~%    cause: ~a~%" label (type-of c)
                        (one-line (princ-to-string c) 120)
                        (let ((cause (and (typep c 'dx:doc-extract-error) (dx:doc-extract-error-cause c))))
                          (if cause (type-of cause) "none")))
                   (check (typep c 'dx:doc-extract-error) t (format nil "~a: doc-extract-error" label))))
        (out "~%python-docx's and openpyxl's view of the same files:~%")
        (let ((py (python "office.py" '("python-docx==1.2.0" "openpyxl==3.1.5")
                          docx-path xlsx-path stored-path truncated-path flipped-path)))
          (when py
            (destructuring-bind (pdocx pxlsx pstored ptruncated pflipped) (coerce (jget py "files") 'list)
              (out "  python-docx ~a, openpyxl ~a~%" (jget py "python-docx") (jget py "openpyxl"))
              (let* ((paragraphs (map 'list (lambda (p) (aref p 0)) (jget pdocx "paragraphs")))
                     (rows (map 'list (lambda (row) (format nil (format nil "~~{~~a~~^~a~~}" #\Tab)
                                                            (coerce row 'list)))
                                (jget pdocx "tables" 0)))
                     (lines (uiop:split-string docx-text :separator '(#\Newline))))
                (out "  report.docx paragraphs: ~s~%  table rows: ~s~%" paragraphs
                     (map 'list (lambda (row) (coerce row 'list)) (jget pdocx "tables" 0)))
                (check (remove-if (lambda (l) (member l rows :test #'string=)) lines) paragraphs
                       "the paragraphs are python-docx's")
                (check (remove-if-not (lambda (l) (member l rows :test #'string=)) lines) rows
                       "the table rows are python-docx's cells"))
              (let ((sheets (coerce (jget pxlsx "sheets") 'list))
                    (ours (mapcar (lambda (s) (table-grid (find-if (lambda (b) (typep b 'dx:table-block))
                                                                   (dx:section-children s))))
                                  (dx:extracted-document-blocks xlsx-doc))))
                (loop for sheet in sheets
                      for grid in ours
                      do (let ((theirs (map 'list (lambda (row) (map 'list #'python-cell row))
                                            (aref sheet 1))))
                           (out "  report.xlsx sheet ~s (data_only=True): ~s~%" (aref sheet 0) theirs)
                           (check grid theirs (format nil "sheet ~a: openpyxl's values" (aref sheet 0))))))
              (out "  the stored package: ~d paragraphs, same as report.docx: ~s~%"
                   (length (jget pstored "paragraphs"))
                   (equalp (jget pstored "paragraphs") (jget pdocx "paragraphs")))
              (check (jget pstored "paragraphs") (jget pdocx "paragraphs") "python-docx reads the stored package"
                     :test #'equalp)
              (out "  the first half: ~a~%  the inverted byte: ~a~%"
                   (jget ptruncated "error")
                   (or (jget pflipped "error") "read without an error"))
              (check (jget ptruncated "error") "docx.opc.exceptions.PackageNotFoundError"
                     "python-docx: PackageNotFoundError (its wrapper of zipfile.BadZipFile)"))))
        (mapc #'uiop:delete-file-if-exists (list stored-path truncated-path flipped-path))))))
