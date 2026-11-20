(in-package #:harness-demos-image)

;;; Demo 2: a spawned Swank image driven through LISP-TOOLS, the native tool
;;; source (CALL-LISP-TOOL runs lisp-mcp's own handlers in this image with
;;; its state bound; no MCP, no JSON-RPC). Demo 3: observe mode.

(defparameter +demo-file+
  (format nil "(defun half (x) (/ x 2))~%~%(defun quarter (x) (half (half x)))~%"))

(defparameter +seeded+
  "(defun seeded (x) (restart-case (error \"seeded bug ~a\" x) (return-default () :report \"Return 42\" 42)))")

(defun %has (needle text) (and (stringp text) (search needle text) t))

(defun %permission (tools rules name &rest kv)
  "The decision an agent with RULES takes on NAME (no model call)."
  (let ((a (agent:make-ai-agent :name "gate" :backend (llm:make-mock-llm-backend)
                                :tools (list tools) :permissions rules)))
    (agent:agent-permission a tools name (apply #'json-args kv))))

(defun demo-image ()
  (banner "Demo 2: a spawned Swank image through the native tool source (no MCP)")
  (let* ((root (make-project "image" `(("demo.lisp" ,+demo-file+))))
         (tools (lt:make-lisp-tools :root root))
         (ok t))
    (unwind-protect
         (progn
           (show "tools offered" (length (lt:lisp-tools-names tools)))
           (show "first ten" (subseq (lt:lisp-tools-names tools) 0 10))
           (show "instructions (first 3 lines)"
                 (format nil "~{~a~^~%~}"
                         (subseq (uiop:split-string (lt:lisp-tools-instructions tools)
                                                    :separator '(#\Newline))
                                 0 3)))
           (stage "spawn, eval, compile a located form")
           (setf ok (and (%has "SBCL" (tool tools "repl-spawn" "backend" "swank")) ok))
           (setf ok (and (%has "=> 3" (tool tools "repl-eval" "code" "(+ 1 2)")) ok))
           (tool tools "repl-compile" "path" "demo.lisp" "locator" "defun half")
           (setf ok (and (%has "=> 2" (tool tools "repl-eval" "code" "(half 4)")) ok))
           (stage "a seeded bug: the error comes back as the debugger state, not a hang")
           (tool tools "repl-eval" "code" +seeded+)
           (setf ok (and (%has "DEBUGGER level 1" (tool tools "repl-eval" "code" "(seeded 1)")) ok))
           (stage "the restarts as permission resources, and a rule gate over them")
           (loop for i in '(0 1 99)
                 do (show (format nil "resources of debug-invoke-restart ~d" i)
                          (agent:tool-permission-resources tools "debug-invoke-restart"
                                                           (json-args "restart" i))))
           (let ((rules (list (agent:make-permission-rule "debug-invoke-restart" "restart:return-default" :allow)
                              (agent:make-permission-rule "debug-invoke-restart" "restart:abort" :deny
                                                          :feedback "choose the restart that returns a value"))))
             (show "rules" (mapcar #'princ-to-string rules))
             (show "decision for restart 1 (abort)" (%permission tools rules "debug-invoke-restart" "restart" 1))
             (show "decision for restart 0 (return-default)" (%permission tools rules "debug-invoke-restart" "restart" 0))
             (show "approval asked without a rule (an +approval-tools+ entry)"
                   (agent:tool-needs-approval-p tools "debug-invoke-restart" (json-args "restart" 0))))
           (stage "the allowed restart")
           (setf ok (and (%has "=> 42" (tool tools "debug-invoke-restart" "restart" 0)) ok))
           (tool tools "debug-state")
           (stage "a background job and a bounded result")
           (let ((job (%token-after "as job " (tool tools "repl-eval" "code" "(progn (sleep 3) :done)" "timeout" 1))))
             (setf ok (and job (%has ":DONE" (tool tools "job-wait" "id" job "timeout" 30)) ok)))
           (let ((r (tool tools "repl-eval" "code" "(loop for i below 4000 collect i)")))
             (let ((id (%token-after "Stored as " r)))
               (show "lisp-mcp's cut marker" (find-if (lambda (l) (search "Stored as" l))
                                                   (uiop:split-string r :separator '(#\Newline))))
               (when id (tool tools "output-read" "id" id "offset"
                              (parse-integer (or (%token-after "offset=" r) "0") :junk-allowed t)
                              "limit" 1))))
           (stage "errors the model can act on")
           (tool tools "repl-compile" "path" "../outside.lisp")
           (tool tools "source-read" "path" "demo.lisp" "locator" "defun nope")
           (tool tools "repl-eval" "code" "(+ 1 2)" "connection" "nope"))
      (lt:close-lisp-tools tools))
    (stage "after close-lisp-tools")
    (show "closed-p" (lt:lisp-tools-closed-p tools))
    (let ((c (tool tools "repl-list")))
      (setf ok (and (typep c 'agent:tool-unavailable) ok)))
    ok))

(defun demo-observe ()
  (banner "Demo 3: observe mode on an attached image refuses a mutating form")
  (let* ((root (make-project "observe" '()))
         (full (lt:make-lisp-tools :root root))
         (watch nil)
         (ok t))
    (unwind-protect
         (let* ((r (progn (tool full "repl-spawn" "backend" "swank")
                          (tool full "repl-eval" "code" "(swank:create-server :port 0 :dont-close t)")))
                (port (and (stringp r) (search "=> " r)
                           (parse-integer r :start (+ 3 (search "=> " r)) :junk-allowed t))))
           (setf watch (lt:make-lisp-tools :root root :mode :observe))
           (show "observe catalog" (lt:lisp-tools-names watch))
           (show "instructions, OBSERVE line"
                 (find-if (lambda (l) (uiop:string-prefix-p "OBSERVE MODE" l))
                          (uiop:split-string (lt:lisp-tools-instructions watch) :separator '(#\Newline))))
           (stage "attach and read")
           (note "  (the port of the image's second Swank server is not printed; it varies)")
           (handler-case (lt:call-lisp-tool watch "repl-connect" (json-args "port" port))
             (error (c) (setf ok nil) (note "  repl-connect failed: ~a" c)))
           (setf ok (and (%has "=> 3" (tool watch "repl-eval" "code" "(+ 1 2)")) ok))
           (tool watch "repl-eval" "code" "(length *features*)")
           (stage "refused before the handler runs")
           (dolist (code '("(defparameter *observed* 1)" "(progn (print :hi) (+ 1 2))"
                           "(asdf:load-system \"shop\")"))
             (let ((c (handler-case (progn (lt:call-lisp-tool watch "repl-eval" (json-args "code" code)) nil)
                        (error (c) c))))
               (note "> repl-eval ~a" code)
               (if (typep c 'lt:observe-mode-refused)
                   (progn
                     (show "  condition" (format nil "~(~s~), a ~(~s~)" (type-of c) 'agent:tool-permission-denied))
                     (show "  resources" (agent:tool-permission-denied-resources c))
                     (show "  feedback" (agent:tool-permission-denied-feedback c)))
                   (progn (setf ok nil) (show "  NOT refused" c)))))
           (let ((c (handler-case (progn (lt:call-lisp-tool watch "repl-spawn" "{}") nil) (error (c) c))))
             (note "> repl-spawn (not in the observe catalog)")
             (show "  condition" (type-of c))
             (when (typep c 'agent:tool-permission-denied)
               (show "  feedback" (agent:tool-permission-denied-feedback c))))
           (stage "the full source sees that nothing reached the image")
           (setf ok (and (%has "=> NIL" (tool full "repl-eval" "code" "(boundp '*observed*)")) ok))
           (lt:close-lisp-tools watch)
           (setf ok (and (%has "=> 3" (tool full "repl-eval" "code" "(+ 1 2)")) ok))
           (note "  closing the observer left the image running"))
      (when watch (lt:close-lisp-tools watch))
      (lt:close-lisp-tools full))
    ok))
