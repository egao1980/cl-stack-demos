(defpackage #:harness-demos-agent
  (:use #:cl)
  (:local-nicknames (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:at #:agent-tools)
                    (#:fs #:cl-stack-pathlib)
                    (#:json #:json-protocol)
                    (#:ev #:event-protocol)
                    (#:hp #:http-protocol)
                    (#:oa #:llm-protocol-openai)
                    (#:tui #:tui-protocol)
                    (#:w #:tui-protocol/widgets)
                    (#:h #:tui-backend-terminal)
                    (#:tty #:tui-backend-tty)
                    (#:pp #:pty-protocol))
  (:export #:run-demos
           #:demo-permissions
           #:demo-file-tools
           #:demo-tui-widgets
           #:demo-tty
           #:demo-llm-cache))

(in-package #:harness-demos-agent)

;;; Output helpers shared by every demo.

(defun banner (title)
  (format t "~&~%~a~%== ~a~%~a~%" (make-string 72 :initial-element #\=)
          title (make-string 72 :initial-element #\=))
  (finish-output))

(defun stage (title)
  (format t "~&~%--- ~a~%" title)
  (finish-output))

(defun show (label value)
  "Print LABEL: VALUE; multi-line strings go on their own indented lines."
  (if (and (stringp value) (find #\Newline value))
      (format t "~&~a:~%~{  | ~a~%~}" label (uiop:split-string value :separator '(#\Newline)))
      (format t "~&~a: ~s~%" label value))
  (finish-output)
  value)

(defun note (control &rest args)
  (format t "~&~?~%" control args)
  (finish-output))

(defun json-args (&rest kv)
  "JSON object string from alternating keys and values (what a model sends)."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    (json:encode h)))

(defmacro with-agent-loop (&body body)
  "Bind a libuv event backend and loop, as run-ai-agent needs."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (ev:make-event-loop eb)))
     (ev:with-event-backend (eb)
       (ev:with-event-loop-var (el)
         ,@body))))

(defun scripted-backend (steps &key (counter (list 0)))
  "Mock LLM backend answering STEPS in turn. A step is a string (final
reply) or a list of tool calls (name json-arguments). Extra generates reuse
the last step. COUNTER's car counts generates."
  (llm:make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b turns))
              (let* ((i (car counter))
                     (step (nth (min i (1- (length steps))) steps)))
                (incf (car counter))
                (if (stringp step)
                    (llm:make-llm-response :parts (list (llm:make-llm-text-part :text step))
                                           :finish-reason :stop)
                    (llm:make-llm-response
                     :parts (loop for (name args) in step
                                  for k from 1
                                  collect (llm:make-llm-tool-call-part
                                           :id (format nil "g~d-c~d" (1+ i) k)
                                           :name name :arguments args))
                     :finish-reason :tool-use))))))

(defun tool-results (run)
  "Tool result parts of RUN's turns, oldest first."
  (loop for turn in (agent:agent-run-turns run)
        append (remove-if-not #'llm:llm-tool-result-part-p (llm:llm-turn-parts turn))))

(defun print-invocation (inv)
  (format t "~&  ~a ~a ~a → ~s~:[~; error-p~]~%~@[    result: ~a~%~]"
          (agent:agent-invocation-id inv) (agent:agent-invocation-name inv)
          (agent:agent-invocation-arguments inv) (agent:agent-invocation-status inv)
          (agent:agent-invocation-error-p inv) (agent:agent-invocation-result inv))
  (finish-output))

(defun print-rule (rule)
  (format t "~&  rule ~s ~s ~s~@[ feedback ~s~]~%"
          (agent:permission-rule-action rule) (agent:permission-rule-resource rule)
          (agent:permission-rule-effect rule) (ignore-errors (agent:permission-rule-feedback rule))))
