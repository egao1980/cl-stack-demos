(in-package #:harness-demos-sample)

;;; cl-stack-sample's README, section 4 ("Work on it with the cl-stack
;;; harness"), followed by a newcomer: fresh clone, launcher downloaded,
;;; executable built into a fresh cache, the -p session on a live model, then
;;; an independent `cl-repo test`. Each step is a command; the demo prints what
;;; it ran and a trimmed view of the output. The API key goes to the child in
;;; its environment only and is never printed.

(defun env (name &optional default)
  (let ((v (uiop:getenv name)))
    (if (and v (plusp (length v))) v default)))

(defparameter *sample-repo*
  (env "HARNESS_SAMPLE_REPO" "https://github.com/egao1980/cl-stack-sample.git"))
(defparameter *sample-ref* (env "HARNESS_SAMPLE_REF" "main"))
(defparameter *launcher-url*
  (env "HARNESS_SAMPLE_LAUNCHER_URL"
       "https://raw.githubusercontent.com/egao1980/lisp-workspace/main/scripts/cl-stack-code.sh"))
(defparameter *prompt* "Add a w (week) unit to parse-duration, with tests. Run them.")

;;; ---- Output ----------------------------------------------------------------

(defvar *scrub* '() "(text . replacement), longest first.")

(defun add-scrub (text replacement)
  (when (and text (plusp (length text)))
    (push (cons text replacement) *scrub*)
    (setf *scrub* (sort *scrub* #'> :key (lambda (p) (length (car p)))))))

(defun scrub (text)
  (dolist (pair *scrub* text)
    (loop for p = (search (car pair) text)
          while p
          do (setf text (concatenate 'string (subseq text 0 p) (cdr pair)
                                     (subseq text (+ p (length (car pair)))))))))

(defun say (control &rest args)
  (write-string (scrub (apply #'format nil control args)))
  (terpri)
  (finish-output))

(defun lines (text)
  (uiop:split-string (string-right-trim '(#\Newline) text) :separator (string #\Newline)))

(defun print-block (text &key (max 40) tail)
  (let* ((all (lines (scrub text)))
         (n (length all))
         (shown (cond ((<= n max) all)
                      (tail (last all max))
                      (t (subseq all 0 max)))))
    (when (and tail (> n max)) (say "   | … (~d lines above)" (- n max)))
    (dolist (l shown) (say "   | ~a" l))
    (when (and (not tail) (> n max)) (say "   | … (~d more lines)" (- n max)))))

(defun banner (title)
  (say "")
  (say "== ~a ==" title))

;;; ---- Commands --------------------------------------------------------------

(defun run (argv &key directory env-alist (show t) (max 30) tail)
  "Run ARGV (strings), environment = ours + ENV-ALIST. → (values exit-code
output seconds); stdout and stderr merged."
  (let* ((start (get-internal-real-time))
         (full (append (mapcar (lambda (p) (format nil "~a=~a" (car p) (cdr p))) env-alist)
                       argv))
         (argv* (if env-alist (cons "/usr/bin/env" full) argv)))
    (say "$ ~{~a~^ ~}" (mapcar (lambda (a) (if (find #\Space a) (format nil "'~a'" a) a)) argv))
    (multiple-value-bind (out err code)
        (uiop:run-program argv* :directory directory :output :string
                                :error-output :output :ignore-error-status t)
      (declare (ignore err))
      (let ((seconds (round (- (get-internal-real-time) start) internal-time-units-per-second)))
        (when show (print-block out :max max :tail tail))
        (say "   exit ~d, ~d s" code seconds)
        (values code out seconds)))))

;;; ---- stream-json -----------------------------------------------------------

(defun field (object key)
  (and (hash-table-p object)
       (multiple-value-bind (v present) (gethash key object)
         (and present (not (eq v :null)) v))))

(defun short (text &optional (n 110))
  (let ((one (substitute #\Space #\Newline (princ-to-string text))))
    (if (> (length one) n) (concatenate 'string (subseq one 0 n) "…") one)))

(defun input-summary (input)
  (or (field input "command") (field input "file_path") (field input "path")
      (field input "pattern")
      (and (hash-table-p input) (plusp (hash-table-count input))
           (short (stack-json:encode input)))
      ""))

(defun summarize-events (text)
  "Print one line per tool call and result from stream-json TEXT. → the
result event (a hash table) or NIL."
  (let ((result nil) (steps 0))
    (dolist (line (lines text))
      (let ((event (and (plusp (length line)) (char= #\{ (char line 0))
                        (ignore-errors (stack-json:decode line)))))
        (when event
          (let ((type (field event "type")))
            (cond ((equal type "tool_use")
                   (say "   step ~2d  ~a ~a" (field event "step") (field event "name")
                        (short (input-summary (field event "input")) 90)))
                  ((equal type "tool_result")
                   (say "            → ~a~@[  ~a~]" (field event "status")
                        (and (not (equal (field event "status") "done"))
                             (short (field event "content") 80))))
                  ((equal type "assistant")
                   (setf steps (max steps (or (field event "step") 0))))
                  ((equal type "result") (setf result event)))))))
    (say "   model calls: ~d" steps)
    result))

;;; ---- The demo --------------------------------------------------------------

(defun model-env ()
  (let ((base (env "HARNESS_DEMOS_BASE_URL" "http://127.0.0.1:1234/v1"))
        (key (env "HARNESS_DEMOS_API_KEY" (env "LM_API_TOKEN")))
        (model (env "HARNESS_DEMOS_MODEL")))
    (unless model (error "set HARNESS_DEMOS_MODEL to the model id"))
    (add-scrub key "<key>")
    (values `(("OPENAI_BASE_URL" . ,base) ,@(and key `(("OPENAI_API_KEY" . ,key)))
              ("OPENAI_MODEL" . ,model))
            base model)))

(defun run-demos ()
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "harness-demos-sample-~d/" (random 1000000 (make-random-state t)))
                                 (uiop:temporary-directory))))
         (clone (merge-pathnames "cl-stack-sample/" root))
         (bin (merge-pathnames "bin/cl-stack-code" root))
         (cache (merge-pathnames "cache/" root))
         (fresh (not (equal (env "HARNESS_SAMPLE_FRESH_CACHE" "1") "0")))
         (verdict :failed))
    (ensure-directories-exist bin)
    (add-scrub (uiop:native-namestring root) "<tmp>/")
    (add-scrub (string-right-trim "/" (uiop:native-namestring (truename root))) "<tmp>")
    (add-scrub (string-right-trim "/" (uiop:native-namestring (user-homedir-pathname))) "~")
    (multiple-value-bind (menv base model) (model-env)
      (say "harness-demos-sample: cl-stack-sample README section 4, as a newcomer")
      (say "MODEL THAT RAN: ~a on ~a (openai provider)" model base)
      (banner "1. Clone the sample")
      (run (list "git" "clone" "--quiet" "--depth" "1" "--branch" *sample-ref* *sample-repo*
                 (uiop:native-namestring clone)))
      (run (list "git" "log" "--oneline" "-1") :directory clone)
      (run (list "cat" ".cl-stack-code/settings.json") :directory clone)
      (banner "2. Get the launcher (direct download)")
      (run (list "curl" "-fsSL" *launcher-url* "-o" (uiop:native-namestring bin)))
      (run (list "chmod" "+x" (uiop:native-namestring bin)))
      (run (list "grep" "-m1" "^VERSION=" (uiop:native-namestring bin)))
      (banner (format nil "3. Build the executable (~:[the existing cache~;a fresh cache~])" fresh))
      (let ((cache-env (and fresh `(("XDG_CACHE_HOME" . ,(uiop:native-namestring cache))))))
        (run (list (uiop:native-namestring bin) "--which") :env-alist cache-env :max 4 :tail t)
        (run (list (uiop:native-namestring bin) "--version") :env-alist cache-env)
        (run (list (uiop:native-namestring bin) "--acp") :env-alist cache-env)
        (banner "4. The -p session (README: \"Headless\")")
        (multiple-value-bind (code out seconds)
            (run (list (uiop:native-namestring bin) "--model" "openai" "-p" *prompt*
                       "--permission-mode" "acceptEdits" "--output-format" "stream-json")
                 :directory clone :env-alist (append cache-env menv) :show nil)
          (declare (ignore seconds))
          (let ((result (summarize-events out)))
            (say "   result: ~a, ~a turns, exit ~d"
                 (field result "subtype") (field result "num_turns") code)
            (when (field result "result")
              (say "   final answer:")
              (print-block (field result "result") :max 25))))
        (banner "5. What changed")
        (run (list "git" "status" "--short") :directory clone)
        (multiple-value-bind (c diff) (run (list "git" "diff") :directory clone :max 80)
          (declare (ignore c))
          (banner "6. Independent check: cl-repo test")
          (multiple-value-bind (code out) (run (list "cl-repo" "test") :directory clone :show nil)
            (let ((summary (remove-if-not (lambda (l) (or (search "passed" l) (search "failed" l)
                                                          (search "#\\w" l) (search "\"1w" l)
                                                          (search "\"2w" l)))
                                          (lines out))))
              (print-block (format nil "~{~a~%~}" summary) :max 15)
              (setf verdict (if (and (zerop code) (search "#\\w" diff) (search "w\"" diff))
                                :ok :failed)))))))
    (banner "Verdict")
    (say "~:[FAILED: the week unit or its tests are missing, or cl-repo test failed~;OK: w unit added, tests added, cl-repo test passes~]"
         (eq verdict :ok))
    (when (equal (env "HARNESS_SAMPLE_KEEP") "1") (say "kept ~a" (uiop:native-namestring root)))
    (unless (equal (env "HARNESS_SAMPLE_KEEP") "1")
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
    (uiop:quit (if (eq verdict :ok) 0 1))))
