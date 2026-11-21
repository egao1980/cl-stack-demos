(in-package #:harness-demos-agent)

(defparameter *demos*
  '(demo-permissions demo-file-tools demo-tui-widgets demo-tty demo-llm-cache demo-markdown demo-shell-tools))

(defun run-demos (&key (demos *demos*))
  "Run each demo; an error is printed and the next demo runs."
  (let ((failed '())
        (start (get-internal-real-time)))
    (banner "Loaded systems")
    (dolist (name '("ai-agent-protocol" "agent-tools" "tui-protocol" "tui-backend-terminal"
                    "tui-backend-tty" "terminal-backend-lisp" "pty-backend-native"
                    "llm-protocol" "llm-protocol-openai" "http-backend-async"
                    "event-backend-libuv" "cl-stack-pathlib" "diff-protocol"
                    "tui-markdown" "process-protocol" "process-backend-uiop" "pty-protocol"))
      (let ((system (asdf:find-system name nil)))
        (note "  ~22a ~a  ~a" name (and system (asdf:component-version system))
              (and system (uiop:native-namestring (asdf:system-source-directory system))))))
    (dolist (demo demos)
      (let ((t0 (get-internal-real-time)))
        (handler-case (funcall demo)
          (error (c)
            (push demo failed)
            (format t "~&~%!! ~a FAILED: ~s~%!! ~a~%" demo (type-of c) c)
            (finish-output)))
        (note "~&  (~(~a~): ~,1f s wall time)" demo
              (/ (- (get-internal-real-time) t0) internal-time-units-per-second))))
    (banner "Summary")
    (note "demos run: ~{~a~^, ~}" (mapcar #'string-downcase demos))
    (note "demos that raised an error: ~:[none~;~:*~{~a~^, ~}~]"
          (mapcar #'string-downcase (reverse failed)))
    (note "wall time: ~,1f s" (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (null failed)))
