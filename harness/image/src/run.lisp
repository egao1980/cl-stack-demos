(in-package #:harness-demos-image)

(defparameter *demos*
  '(demo-scanner demo-image demo-observe demo-agent-rules demo-agent-real))

(defun run-demos (&key (demos *demos*))
  "Run each demo; an error or a false result is reported and the next demo
runs. → T when every demo returned true."
  (let ((failed '()))
    (with-scratch ()
      (banner "Loaded systems")
      (dolist (name '("lisp-mcp" "lisp-mcp/agent" "lisp-source-protocol" "repl-protocol"
                      "repl-backend-swank" "ai-agent-protocol" "llm-protocol" "llm-protocol-openai"))
        (let ((system (asdf:find-system name nil)))
          (note "  ~22a ~a" name (and system (asdf:component-version system)))))
      (detect-model)
      (dolist (demo demos)
        (handler-case (unless (funcall demo)
                        (push demo failed)
                        (format t "~&~%!! ~a did not show what it should (see above)~%" demo))
          (error (c)
            (push demo failed)
            (format t "~&~%!! ~a FAILED: ~s~%!! ~a~%" demo (type-of c) (redact c))
            (finish-output)))))
    (banner "Summary")
    (note "demos run: ~{~a~^, ~}" (mapcar #'string-downcase demos))
    (note "demos that failed: ~:[none~;~:*~{~a~^, ~}~]" (mapcar #'string-downcase (reverse failed)))
    (null failed)))
