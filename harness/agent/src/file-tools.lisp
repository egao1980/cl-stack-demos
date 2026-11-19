(in-package #:harness-demos-agent)

;;; Demo 2: agent-tools 0.1.0 on a temporary project directory.

(defparameter *project-files*
  '((".gitignore" "build/
*.log
")
    ("src/main.lisp" "(defpackage #:app (:use #:cl))
(in-package #:app)

(defun greet (name)
  ;; TODO: localize
  (format nil \"Hello, ~a\" name))

(defun main ()
  (print (greet \"world\")))
")
    ("src/util.lisp" "(in-package #:app)
;; TODO: remove
(defun twice (x) (* 2 x))
")
    ("README.md" "# app
TODO: write docs
")
    ("debug.log" "TODO in a log file (ignored)
")
    ("build/out.lisp" ";; TODO generated (ignored)
")))

(defun %write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(defun %make-project ()
  (let ((root (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "harness-demo-~36r/" (random (expt 36 6)
                                                                         (make-random-state t)))
                                (uiop:temporary-directory)))))
    (loop for (rel text) in *project-files*
          do (%write-text (merge-pathnames rel root) text))
    root))

(defun %call-tool (tools name &rest kv)
  "Call NAME with KV as JSON; print the result or the failure (type + message)."
  (let ((args (apply #'json-args kv)))
    (format t "~&> ~a ~a~%" name args)
    (handler-case (let ((r (at:call-file-tool tools name args)))
                    (show "result" r)
                    r)
      (error (c)
        (format t "~&FAILED with ~s:~%  ~a~%" (type-of c) c)
        (finish-output)
        nil))))

(defun %hash-line (result)
  (and result
       (let ((p (search "hash: " result :from-end t)))
         (and p (subseq result (+ p 6))))))

(defun demo-file-tools ()
  (banner "Demo 2: agent-tools 0.1.0 file tools")
  (let ((root (%make-project)))
    (unwind-protect
         (let ((tools (at:make-file-tools :root (namestring root))))
           (show "root" (fs:as-posix (at:file-tools-root tools)))
           (show "tools" (at:file-tools-names tools))

           (stage "2a. read with offset/limit")
           (%call-tool tools "read" "file_path" "src/main.lisp" "offset" 4 "limit" 3)

           (stage "2b. write a new file (parents created)")
           (%call-tool tools "write" "file_path" "notes/todo.txt"
                       "content" (format nil "- ship the demo~%"))

           (stage "2c. edit (unique match): unified-diff preview and the new hash")
           (let* ((before (%hash-line (%call-tool tools "read" "file_path" "src/main.lisp"
                                                  "limit" 1)))
                  (after (%hash-line (%call-tool tools "edit" "file_path" "src/main.lisp"
                                                 "old_string" "\"Hello, ~a\""
                                                 "new_string" "\"Hi there, ~a!\""))))
             (show "hash before" before)
             (show "hash after" after)

             (stage "2d. edit with a non-unique old_string")
             (%call-tool tools "edit" "file_path" "src/main.lisp"
                         "old_string" "(defun " "new_string" "(defun* ")

             (stage "2e. edit with a stale expected_hash (the pre-edit hash)")
             (%call-tool tools "edit" "file_path" "src/main.lisp"
                         "old_string" "world" "new_string" "there"
                         "expected_hash" before))

           (stage "2f. glob and grep respect .gitignore (build/, *.log)")
           (note "files on disk: ~{~a~^ ~}"
                 (mapcar (lambda (p) (enough-namestring p root))
                         (remove-if #'uiop:directory-pathname-p
                                    (uiop:directory-files root "**/*.*"))))
           (%call-tool tools "glob" "pattern" "**/*")
           (%call-tool tools "glob" "pattern" "**/*.log")
           (%call-tool tools "grep" "pattern" "TODO")
           (%call-tool tools "grep" "pattern" "todo" "case_insensitive" t "include" "*.lisp")
           (note "an explicit path is never refused for being ignored:")
           (%call-tool tools "read" "file_path" "debug.log")

           (stage "2g. root escapes are refused")
           (%call-tool tools "read" "file_path" "../x")
           (%call-tool tools "read" "file_path" "..\\x")
           (%call-tool tools "write" "file_path" "src/../../escape.txt" "content" "nope")
           (%call-tool tools "read" "file_path" "/etc/passwd")
           (show "escape.txt written outside root?"
                 (and (probe-file (merge-pathnames "../escape.txt" root)) t))

           (stage "2h. tool-permission-resources")
           (dolist (call (list (list "read" (json-args "file_path" "./src/main.lisp"))
                               (list "edit" (json-args "file_path" "src/../README.md"
                                                       "old_string" "a" "new_string" "b"))
                               (list "grep" (json-args "pattern" "x"))
                               (list "glob" (json-args "pattern" "*" "path" "src"))
                               (list "read" (json-args "file_path" "../x"))
                               (list "read" (json-args "file_path" "..\\x"))
                               (list "read" "not json")))
             (format t "  ~a ~a → ~s~%" (first call) (second call)
                     (agent:tool-permission-resources tools (first call) (second call))))

           (stage "2i. a mock-LLM agent run that uses edit then read")
           (with-agent-loop
             (let* ((agent (agent:make-ai-agent
                            :name "coder"
                            :backend (scripted-backend
                                      (list (list (list "edit" (json-args "file_path" "src/util.lisp"
                                                                          "old_string" "(* 2 x)"
                                                                          "new_string" "(+ x x)")))
                                            (list (list "read" (json-args "file_path" "src/util.lisp")))
                                            "Rewrote twice to use addition."))
                            :tools (list tools)))
                    (run (agent:run-ai-agent agent "make twice use +")))
               (show "finish reason" (agent:agent-run-finish-reason run))
               (show "tools the model was offered"
                     (mapcar #'llm:llm-tool-name (agent:collect-run-tools agent)))
               (dolist (p (tool-results run))
                 (format t "~&tool result ~a (error-p ~a):~%" (llm:llm-tool-result-part-id p)
                         (llm:llm-tool-result-part-error-p p))
                 (show "  content" (llm:llm-tool-result-part-content p)))
               (show "final text" (agent:agent-run-text run))
               (show "src/util.lisp on disk"
                     (uiop:read-file-string (merge-pathnames "src/util.lisp" root))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
