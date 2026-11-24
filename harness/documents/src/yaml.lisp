;;;; 3. YAML block output with no trailing space (yaml-protocol#7).

(in-package #:harness-demos-documents)

(defparameter *yaml-flow* "{name: Ada, langs: [lisp, ml]}"
  "The issue's document, in flow style.")

(defparameter *yaml-deep*
  "service:
  name: reports
  ports: [8080, 8443]
  db: {host: db.internal, pool: {min: 1, max: 8}}
  tags: []
  owner: ~
matrix:
  - [1, 2]
  - [3, 4]
")

(defun trailing-blank-lines (text)
  "1-based numbers of TEXT's lines that end in a space or a tab."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for n from 1
        when (and (plusp (length line))
                  (member (char line (1- (length line))) '(#\Space #\Tab)))
          collect n))

(defun shared-document ()
  "A mapping whose two keys hold the same vector, and a sequence holding the
same mapping twice: encode writes &id1 the first time, *id1 after."
  (let ((langs (vector "lisp" "ml"))
        (person (make-hash-table :test #'equal))
        (root (make-hash-table :test #'equal)))
    (setf (gethash "name" person) "Ada"
          (gethash "langs" person) langs
          (gethash "teaches" person) langs)
    (setf (gethash "people" root) (vector person person))
    root))

(defun yaml-case (label value)
  "Encode VALUE, print it with line ends marked, check it. Returns the text."
  (let* ((text (yaml:encode value))
         (trailing (trailing-blank-lines text)))
    (out "~a~%  (yaml:encode ...) => ~d characters, lines ending in a blank: ~s~%"
         label (length text) trailing)
    (show-lines text)
    (check trailing '() (format nil "~a: no trailing blank" label))
    (check (yaml:decode text) value (format nil "~a: decodes back EQUALP" label) :test #'equalp)
    text))

(defun write-text (text path)
  (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text o))
  path)

(defun demo-yaml ()
  (out "Each line below ends with $, so a trailing space would show as \" $\".~%~
        0.2.1 wrote \"langs: \" for the first one: a space before the newline of~%~
        every key whose value is a nested mapping or sequence.~%~%")
  (let* ((flow (yaml-case (format nil "(yaml:decode ~s):" *yaml-flow*) (yaml:decode *yaml-flow*)))
         (deep (progn (out "~%")
                 (yaml-case "A deeper document (nested mappings and sequences, [], null):"
                            (yaml:decode *yaml-deep*))))
         (shared (progn (out "~%")
                   (yaml-case "Shared objects (one vector under two keys, one mapping twice):"
                              (shared-document))))
         (texts (list flow deep shared)))
    (check (string-right-trim '(#\Newline) flow) (format nil "langs:~%  - lisp~%  - ml~%name: Ada")
           "the issue's document, exactly")
    (check (and (search "&id" shared) (search "*id" shared) t) t "anchor and alias written")
    (let ((back (yaml:decode shared)))
      (check (eq (jget back "people" 0) (jget back "people" 1)) t
             "the alias decodes to the same object (EQ)"))
    (out "~%The text ~:[ends without a final newline, where PyYAML's dump ends with one~%~
          (yaml-protocol#11, found by this demo)~;ends with a newline~].~%"
         (char= (char flow (1- (length flow))) #\Newline))
    (out "~%PyYAML's view of the three outputs:~%")
    (let* ((paths (loop for text in texts
                        for i from 1
                        collect (write-text text (tmp-file (format nil "~d.yaml" i)))))
           (py (apply #'python "pyyaml_load.py" '("PyYAML==6.0.3") paths)))
      (when py
        (out "  PyYAML ~a~%" (jget py "version"))
        (loop for text in texts
              for file across (jget py "files")
              for i from 1
              do (let ((same (equalp (jget file "data") (yaml:decode text))))
                   (out "  ~d. yaml.safe_load gives the same data: ~s; lines ending in a blank: ~s~%~
                         ~5@tPyYAML's own safe_dump of it:~%"
                        i same (coerce (jget file "trailing") 'list))
                   (show-lines (string-right-trim '(#\Newline) (jget file "dump"))
                               :prefix "     py | ")
                   (check same t (format nil "PyYAML reads case ~d the same" i))
                   (check (coerce (jget file "trailing") 'list) '()
                          (format nil "PyYAML sees no trailing blank in case ~d" i)))))
      (mapc #'uiop:delete-file-if-exists paths))))
