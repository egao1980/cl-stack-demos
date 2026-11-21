(in-package #:harness-demos-image)

(defparameter *demos*
  '(demo-scanner demo-image demo-observe demo-agent-rules demo-agent-real
    demo-image-events demo-image-events-denied demo-image-job-events demo-image-events-observe
    demo-image-events-real))

(defun run-demos (&key (demos *demos*))
  "Run each demo; an error or a false result is reported and the next demo
runs. → T when every demo returned true."
  (let ((failed '())
        (start (get-internal-real-time)))
    (with-scratch ()
      (banner "Loaded systems")
      (dolist (name '("lisp-mcp" "lisp-mcp/agent" "lisp-source-protocol" "repl-protocol"
                      "repl-backend-swank" "ai-agent-protocol" "llm-protocol" "llm-protocol-openai"))
        (let ((system (asdf:find-system name nil)))
          (note "  ~22a ~a" name (and system (asdf:component-version system)))))
      (detect-model)
      (dolist (demo demos)
        (let ((t0 (get-internal-real-time)))
          (handler-case (unless (funcall demo)
                          (push demo failed)
                          (format t "~&~%!! ~a did not show what it should (see above)~%" demo))
            (error (c)
              (push demo failed)
              (format t "~&~%!! ~a FAILED: ~s~%!! ~a~%" demo (type-of c) (redact c))
              (finish-output)))
          ;; a run that failed before printing its usage
          (print-lm-usage (format nil "~(~a~) (calls not printed by the demo)" demo))
          (note "~&  (~(~a~): ~,1f s wall time)" demo
                (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))))
    (banner "Summary")
    (note "demos run: ~{~a~^, ~}" (mapcar #'string-downcase demos))
    (note "demos that failed: ~:[none~;~:*~{~a~^, ~}~]" (mapcar #'string-downcase (reverse failed)))
    (note "peak input tokens of a real-model request: ~:[none (no real model ran)~;~:*~d~]" *lm-peak-input*)
    (note "wall time: ~,1f s" (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (null failed)))
