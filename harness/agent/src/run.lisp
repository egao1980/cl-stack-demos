(in-package #:harness-demos-agent)

(defparameter *demos*
  '(demo-permissions demo-file-tools demo-tui-widgets demo-tty demo-llm-cache))

(defun run-demos (&key (demos *demos*))
  "Run each demo; an error is printed and the next demo runs."
  (let ((failed '()))
    (banner "Loaded systems")
    (dolist (name '("ai-agent-protocol" "agent-tools" "tui-protocol" "tui-backend-terminal"
                    "tui-backend-tty" "terminal-backend-lisp" "pty-backend-native"
                    "llm-protocol" "llm-protocol-openai" "http-backend-async"
                    "event-backend-libuv" "cl-stack-pathlib" "diff-protocol"))
      (let ((system (asdf:find-system name nil)))
        (note "  ~22a ~a  ~a" name (and system (asdf:component-version system))
              (and system (uiop:native-namestring (asdf:system-source-directory system))))))
    (dolist (demo demos)
      (handler-case (funcall demo)
        (error (c)
          (push demo failed)
          (format t "~&~%!! ~a FAILED: ~s~%!! ~a~%" demo (type-of c) c)
          (finish-output))))
    (banner "Summary")
    (note "demos run: ~{~a~^, ~}" (mapcar #'string-downcase demos))
    (note "demos that raised an error: ~:[none~;~:*~{~a~^, ~}~]"
          (mapcar #'string-downcase (reverse failed)))
    (null failed)))
