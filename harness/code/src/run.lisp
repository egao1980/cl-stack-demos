(in-package #:harness-demos-code)

(defparameter *demos*
  '(demo-text demo-stream-json demo-headless-denial demo-settings demo-instructions
    demo-interactive))

(defun run-demos (&key (demos *demos*))
  "Run each demo; an error is printed and the next demo runs. → T when none
raised an error."
  (let ((failed '()))
    (with-scratch ()
      (banner "Loaded systems")
      (dolist (name '("cl-stack-code" "ai-agent-protocol" "agent-tools" "cli-protocol"
                      "cli-backend-clingon" "llm-protocol" "llm-protocol-openai"
                      "tui-protocol" "tui-markdown" "tui-backend-terminal" "tui-backend-tty"
                      "steer-protocol" "http-backend-async" "event-backend-libuv"))
        (let ((system (asdf:find-system name nil)))
          (note "  ~22a ~a" name (and system (asdf:component-version system)))))
      (detect-model)
      (dolist (demo demos)
        (handler-case (funcall demo)
          (error (c)
            (push demo failed)
            (format t "~&~%!! ~a FAILED: ~s~%!! ~a~%" demo (type-of c) (redact c))
            (finish-output)))))
    (banner "Summary")
    (note "model that ran: ~a" (if (real-model-p) (format nil "LM Studio ~a" (second *model*)) "scripted mock"))
    (note "demos run: ~{~a~^, ~}" (mapcar #'string-downcase demos))
    (note "demos that raised an error: ~:[none~;~:*~{~a~^, ~}~]"
          (mapcar #'string-downcase (reverse failed)))
    (null failed)))
