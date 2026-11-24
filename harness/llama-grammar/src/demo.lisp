(in-package #:harness-demos-llama-grammar)

;;;; llama-cpp 0.2.1: grammar-constrained generation on a local GGUF, the
;;;; python-to-cl "llama-cpp-python / outlines" job. llama-cpp 0.2.0 aborted the
;;;; whole Lisp image on the first grammar-constrained token (llama-cpp#17) and
;;;; on exit with a live engine (#18). Every result with a known answer is
;;;; checked: RUN-DEMOS returns NIL when one differs or a section stops on an
;;;; error. The model is tinyllamas/stories260K.gguf (1 MB) in
;;;; HARNESS_LLAMA_MODEL.

(defvar *checks* 0)
(defvar *mismatches* 0)
(defvar *failed-sections* '())

(defun shown (text)
  "TEXT with the home and temporary directories replaced by ~ and <tmp>."
  (let ((text (princ-to-string text)))
    (flet ((swap (from to)
             (let ((from (string-right-trim "/" from)))
               (loop for at = (search from text)
                     while (and at (plusp (length from)))
                     do (setf text (concatenate 'string (subseq text 0 at) to
                                                (subseq text (+ at (length from)))))))))
      (swap (namestring (uiop:temporary-directory)) "<tmp>")
      (swap (namestring (user-homedir-pathname)) "~")
      text)))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  (format t "~&~{~a~%~}~%" lines))

(defun show (label value)
  (format t "~&  ~38a ~a~%" label (shown value)))

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (let ((ok (funcall test got expected)))
    (unless ok (incf *mismatches*))
    (format t "~&  ~:[MISMATCH~;ok      ~] ~a: ~a~:[ (want ~a)~;~*~]~%"
            ok label (shown (prin1-to-string got)) ok (prin1-to-string expected))
    ok))

(defmacro section (title &body body)
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (format t "~&  SECTION FAILED: ~a: ~a~%" (type-of c) (shown c))))))

(defparameter *yes-no* "root ::= (\"yes\" | \"no\")")

(defun model-path ()
  (let ((p (uiop:getenv "HARNESS_LLAMA_MODEL")))
    (and p (plusp (length p)) (probe-file p) (namestring (probe-file p)))))

(defun complete* (engine prompt &rest keys)
  "COMPLETE → (:ok text) or (:llama-error message)."
  (handler-case (list :ok (apply #'l:complete engine prompt keys))
    (l:llama-error (c) (list :llama-error (princ-to-string c)))))

(defun demo-plain (engine)
  (section "1. The engine and a plain completion"
    (show "llama-version" (l:llama-version))
    (show "libllamastack ABI" (l:llama-abi-version))
    (show "model" (model-path))
    (let ((text (l:complete engine "Once upon a time" :max-tokens 12)))
      (show "complete \"Once upon a time\" (12)" (prin1-to-string text))
      (check "some text" (plusp (length text)) t))))

(defun demo-grammar (engine)
  (section "2. Grammar-constrained COMPLETE (#17)"
    (para "llama-cpp 0.2.0 called llama_sampler_accept after llama_sampler_sample, which"
          "already accepts the token: the grammar saw every piece twice and threw"
          "\"Unexpected empty grammar stack after accepting piece: y\" on the first token."
          "The shim was C, so the C++ exception ran into std::terminate: SIGABRT, LDB."
          "0.2.1 accepts each token once; once the grammar is complete llama.cpp only allows"
          "end-of-generation, so the text is a sentence of the grammar.")
    (show "grammar" *yes-no*)
    (dolist (prompt '("The cat" "Is the sky blue?" "Once upon a time"))
      (let ((got (complete* engine prompt :max-tokens 8 :grammar *yes-no*)))
        (check (format nil "complete ~s :grammar" prompt)
               (and (eq (first got) :ok) (member (second got) '("yes" "no") :test #'string=) t)
               t)
        (show "  →" (prin1-to-string got))))
    (let ((g "root ::= \"The answer is \" (\"yes\" | \"no\") \".\""))
      (show "grammar" g)
      (check "complete \"Question:\""
             (second (complete* engine "Question:" :max-tokens 16 :grammar g))
             "The answer is yes."
             :test (lambda (got want)
                     (declare (ignore want))
                     (member got '("The answer is yes." "The answer is no.") :test #'equal))))))

(defun demo-parsed-and-streamed (engine)
  (section "3. PARSE-GRAMMAR reuse, :on-token streaming, temperature"
    (let ((g (l:parse-grammar engine *yes-no*)))
      (unwind-protect
           (check "one parse, three completions (COMPLETE clones the sampler)"
                  (loop repeat 3 collect (l:complete engine "The cat" :max-tokens 8 :parsed g))
                  '("yes" "yes" "yes"))
        (l:free-grammar g)))
    (let* ((pieces '())
           (text (l:complete engine "The cat" :max-tokens 8 :grammar *yes-no*
                                              :on-token (lambda (p) (push p pieces) nil))))
      (show ":on-token pieces" (prin1-to-string (reverse pieces)))
      (check "pieces join to the text" (apply #'concatenate 'string (reverse pieces)) text))
    (check "temperature 0.8: still a sentence"
           (loop repeat 5
                 always (member (l:complete engine "The cat" :max-tokens 8 :temperature 0.8
                                                             :grammar *yes-no*)
                                '("yes" "no") :test #'string=))
           t)))

(defun demo-cut (engine)
  (section "4. An open-ended grammar is cut at :max-tokens"
    (let ((g "root ::= \"{\" [a-z]+ \"}\""))
      (show "grammar" g)
      (dolist (n '(2 4 8))
        (let ((text (l:complete engine "The cat" :max-tokens n :grammar g)))
          (show (format nil ":max-tokens ~d" n) (prin1-to-string text))
          (check (format nil "prefix of a sentence (~d)" n)
                 (and (char= #\{ (char text 0))
                      (every (lambda (ch) (or (char<= #\a ch #\z) (char= ch #\}))) (subseq text 1)))
                 t))))))

(defun demo-errors (engine)
  (section "5. Dead ends and C++ exceptions become LLAMA-ERROR (#17)"
           (para "No token's piece starts with NUL, so this grammar rules out every candidate."
                 "llama_sampler_sample picks one anyway (greedy keeps candidate 0) and llama.cpp's"
                 "accept throws; 0.2.0 aborted here. The 0.2.1 shim applies the grammar itself,"
                 "drops what it rules out, and sees that nothing is left: a grammar dead end.")
           (let ((got (complete* engine "The cat" :max-tokens 4 :grammar "root ::= \"\\x00\"")))
             (show "complete :grammar \"root ::= \\\"\\\\x00\\\"\"" (second got))
             (check "a LLAMA-ERROR" (first got) :llama-error)
             (check "status RUNTIME, \"grammar dead end\""
                    (and (search "(RUNTIME)" (second got)) (search "grammar dead end" (second got)) t) t))
           (let ((got (complete* engine "The cat" :max-tokens 4 :grammar "root ::= \"a\" | missing-rule")))
             (show "undefined rule" (second got))
             (check "status INVALID" (and (search "(INVALID)" (second got)) t) t))
           (para "libllamastack is C++ behind the C ABI: every exported function catches what"
                 "llama.cpp throws and returns LLAMA_STACK_RUNTIME with the text."
                 "llama_stack_guard_selftest throws on purpose, inside that guard:")
           (check "std::runtime_error, std::bad_alloc, a non-std exception"
                  (loop for kind in '(1 2 3)
                        collect (list (l::%guard-selftest kind) (l::%last-error)))
                  '((:runtime "C++ exception: guard selftest")
                    (:runtime "out of memory")
                    (:runtime "unknown C++ exception")))
           (check "the image goes on: complete after the errors"
                  (second (complete* engine "The cat" :max-tokens 8 :grammar *yes-no*)) "yes")))

(defun chat-model-path ()
  (let ((p (uiop:getenv "HARNESS_LLAMA_CHAT_MODEL")))
    (and p (plusp (length p)) (probe-file p) (namestring (probe-file p)))))

(defun demo-chat-vocab ()
  (section "7. A vocabulary whose token 0 is end-of-generation (#17, round 2)"
           (para "SmolLM2's token 0 is <|endoftext|>. When the grammar rules out every token,"
                 "the forced pick is token 0, an end-of-generation token, and llama.cpp's grammar"
                 "accept calls GGML_ABORT (abort(), not an exception: no guard catches it)."
                 "A grammar that names the end-of-turn token gets there after one word:"
                 "llama.cpp masks EOG tokens until the grammar is complete, so <|im_end|> is never"
                 "allowed. The 0.2.1 shim stops at the dead end before it picks.")
           (if (null (chat-model-path))
               (format t "~&  (set HARNESS_LLAMA_CHAT_MODEL to SmolLM2-135M-Instruct-Q4_0.gguf; see README)~%")
               (let ((engine (l:load-engine :model-path (chat-model-path) :n-ctx 256)))
                 (unwind-protect
                     (progn
                       (show "model" (chat-model-path))
                       (check "yes/no grammar ends at EOG"
                              (complete* engine "The cat" :max-tokens 8 :grammar *yes-no*)
                              '(:ok "yes")
                              :test (lambda (got want)
                                      (declare (ignore want))
                                      (and (eq (first got) :ok)
                                           (member (second got) '("yes" "no") :test #'string=)
                                           t)))
                       (dolist (g '("root ::= (\"yes\" | \"no\") <|im_end|>" "root ::= \"\\x00\""))
                         (let ((got (complete* engine "The cat" :max-tokens 8 :grammar g)))
                           (show (format nil "grammar ~a" g) (second got))
                           (check "a grammar dead end, not SIGABRT"
                                  (and (eq (first got) :llama-error)
                                       (search "grammar dead end" (second got))
                                       t)
                                  t)))
                       (check "the image goes on"
                              (first (complete* engine "The cat" :max-tokens 8 :grammar *yes-no*))
                              :ok))
                   (l:free-engine engine))))))

(defun child-source-registry ()
  `(:source-registry
    ,@(remove-duplicates
       (loop for name in (asdf:already-loaded-systems)
             for dir = (ignore-errors (asdf:system-source-directory name))
             when dir collect (list :directory (namestring dir)))
       :test #'equal)
    :ignore-inherited-configuration))

#+sbcl
(defun exit-child (&rest evals)
  "A fresh SBCL that loads llama-cpp, keeps an engine in a global, then EVALS.
   → (values exit-code stderr)."
  (let ((setup (with-standard-io-syntax
                 (let ((*print-readably* nil))
                   (list (format nil "(progn (asdf:initialize-source-registry '~s) (asdf:load-system \"llama-cpp\"))"
                                 (child-source-registry))
                         (format nil "(defvar cl-user::*engine* (llama-cpp:load-engine :model-path ~s :n-ctx 256))"
                                 (model-path)))))))
    (multiple-value-bind (out err code)
        (uiop:run-program (append (list (namestring sb-ext:*runtime-pathname*)
                                        "--core" (namestring sb-ext:*core-pathname*)
                                        "--noinform" "--disable-ldb" "--lose-on-corruption"
                                        "--end-runtime-options"
                                        "--no-sysinit" "--no-userinit" "--non-interactive"
                                        "--eval" "(require :asdf)")
                                  (loop for e in (append setup evals) append (list "--eval" e)))
                          :input nil :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore out))
      (values code err))))

(defun demo-exit ()
  (section "6. Exit with a live engine (#18, #20)"
    (para "An engine kept in a global outlives the Lisp side: C exit() destroyed"
          "ggml-metal's device list under the model's residency sets and"
          "ggml_metal_device_free aborted (GGML_ASSERT, SIGABRT) instead of exiting with the"
          "program's code. The finalizers never helped: they closed over the engine, so it"
          "was never garbage (#20). 0.2.1 frees what is still live from sb-ext:*exit-hooks*"
          "(FREE-LIVE-HANDLES: grammars, then engines).")
    #-sbcl (format t "~&  (child-image section is SBCL-only)~%")
    #+sbcl
    (dolist (case '(("(sb-ext:exit :code 3)" 3) ("(error \"boom\")" 1)))
      (multiple-value-bind (code err) (exit-child (first case))
        (show (format nil "child: engine in a global, then ~a" (first case))
              (format nil "exit ~a~@[, ~a~]" code
                      (and (or (search "GGML_ASSERT" err) (search "SIGABRT" err)) "aborted")))
        (check "exit code, no abort"
               (list code (and (or (search "GGML_ASSERT" err) (search "SIGABRT" err)) t))
               (list (second case) nil))))
    (let* ((engine (l:load-engine :model-path (model-path) :n-ctx 256))
           (grammar (l:parse-grammar engine *yes-no*)))
      (declare (ignorable grammar))
      (check "free-live-handles (one engine, one grammar here)" (l:free-live-handles) 2)
      (check "again: nothing left" (l:free-live-handles) 0)
      (check "the handles read as freed" (list (l::engine-freed-p engine) (l::grammar-freed-p grammar))
             '(t t)))))

(defun run-demos ()
  "Run every section. → T when every check matched and no section failed."
  (setf *checks* 0 *mismatches* 0 *failed-sections* '())
  (cond
   ((not (l:llama-available-p))
    (format t "~&libllamastack is not loaded (LLAMA_CPP_NATIVE).~%")
    (push "no libllamastack" *failed-sections*))
   ((not (model-path))
    (format t "~&Set HARNESS_LLAMA_MODEL to tinyllamas/stories260K.gguf (see README).~%")
    (push "no model" *failed-sections*))
   (t
    (let ((engine (l:load-engine :model-path (model-path) :n-ctx 256)))
      (unwind-protect
          (progn
            (demo-plain engine)
            (demo-grammar engine)
            (demo-parsed-and-streamed engine)
            (demo-cut engine)
            (demo-errors engine))
        (l:free-engine engine)))
    (demo-exit)
    (demo-chat-vocab)))
  (format t "~&~%~d checks, ~d mismatched~@[; failed sections: ~{~a~^, ~}~]~%"
          *checks* *mismatches* (reverse *failed-sections*))
  (and (zerop *mismatches*) (null *failed-sections*)))
