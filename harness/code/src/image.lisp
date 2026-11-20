(in-package #:harness-demos-code)

;;; cl-stack-code 0.6.0 (H4b): the Lisp image in the product. Demo n: -p
;;; --lisp-image spawn with stream-json: the lisp_image init key, a
;;; background debugger as an image_event line and an inbox turn, a restart
;;; denied headless, one allowed by a restart:<name> rule, a detached job's
;;; event. Demo o: the interactive mode, onDebugger "turn" and "notice": the
;;; banner, the status line marker, the notice, the restart dialog. Demo p:
;;; observe mode against an attached image. All on scripted mocks; the
;;; real-model H4b run is harness/image Demo 10.

(defparameter +bg-error+
  "(progn (sb-thread:make-thread (lambda () (restart-case (error \"bg boom: the worker lost its queue\") (retry-later () :report \"Retry later\" 7))) :name \"bg-worker\") (sleep 1) :started)"
  "Starts a thread that enters the debugger at once; returns :STARTED a
second later, so the event arrives while this repl-eval runs.")

(defparameter +late-error+
  "(progn (sb-thread:make-thread (lambda () (sleep 1.5) (restart-case (error \"late boom: the cache refresh failed\") (retry-later () :report \"Retry later\" 7))) :name \"late-worker\") :started)"
  "Starts a thread that enters the debugger 1.5 s later; returns at once.")

(defun %check (what ok)
  "OK, or an error naming WHAT (run-demos reports errors, not false results)."
  (or ok (error "~a did not show what it should (see above)" what)))

(defparameter +image-tag+ "<lisp-image-event")

(defun %image-turn-p (turn)
  (and (eq :user (llm:llm-turn-role turn))
       (let ((text (llm:turn-text turn)))
         (and (stringp text) (uiop:string-prefix-p +image-tag+ text)))))

(defun %image-turns (turns) (remove-if-not #'%image-turn-p (llm:coerce-turns turns)))

(defun %events-of (events type)
  (remove type events :key (lambda (e) (jget e "type")) :test-not #'equal))

(defun %tool-result-named (events name &optional (n 0))
  (nth n (remove name (%tool-results events) :key (lambda (e) (jget e "name")) :test-not #'equal)))

(defun %print-image-events (events)
  (dolist (e (%events-of events "image_event"))
    (note "  {type: image_event, kind: ~a, connection: ~a, thread: ~a, level: ~a, job: ~a}"
          (jget e "kind") (jget e "connection") (jget e "thread") (jget e "level") (jget e "job"))
    (show "    text" (jget e "text"))))

(defun %print-tool-lines (events &key (width 300))
  (dolist (e events)
    (when (member (jget e "type") '("tool_use" "tool_result") :test #'equal)
      (note "  ~a" (clip (json:encode e) width)))))

;;; Demo n.

(defun demo-image-headless ()
  (banner "Demo n: -p --lisp-image spawn --output-format stream-json: image events in a headless run")
  (let* ((*model* '(:mock))
         (project (make-project "image-headless" '(("AGENTS.md" "Look after the worker image."))))
         (requests '())
         (argv (list "-p" "--output-format" "stream-json" "--lisp-image" "spawn" "--max-turns" "12"
                     "--allowed-tools" "repl-eval"
                     "--allowed-tools" "debug-state"
                     "--allowed-tools" "debug-invoke-restart(restart:retry-later)"
                     "start the worker and look after it")))
    (flet ((saw (turns)
             (push (length (%image-turns turns)) requests)))
      (setf *mock-script*
            (list (lambda (turns) (saw turns) (list (list "repl-eval" (json-args "code" +bg-error+))))
                  (lambda (turns) (saw turns) (list (list "debug-state" (json-args))))
                  (lambda (turns) (saw turns) (list (list "debug-invoke-restart" (json-args "restart" 1))))
                  (lambda (turns) (saw turns) (list (list "debug-invoke-restart" (json-args "restart" 0))))
                  (lambda (turns) (saw turns)
                    (list (list "repl-eval" (json-args "code" "(progn (sleep 2) :report-done)" "timeout" 1))))
                  (lambda (turns) (saw turns)
                    (list (list "repl-eval" (json-args "code" "(progn (sleep 2.5) :waited)"))))
                  (lambda (turns) (saw turns)
                    (format nil "The worker hit bg boom; abort was denied, retry-later ran. The report job ended: ~a"
                            (if (search "kind=\"job\"" (llm:turn-text (car (last (%image-turns turns)))))
                                "its event reached me." "no event yet."))))))
    (show-argv argv)
    (multiple-value-bind (exit out err) (cli argv :cwd project :config (scratch-dir "image-headless-config"))
      (let* ((events (decode-lines out))
             (init (first (%events-of events "system")))
             (result (%result events)))
        (show "exit code" exit)
        (show "stderr" (if (string= err "") "(empty)" err))
        (show "event types in order"
              (remove-duplicates (mapcar (lambda (e) (jget e "type")) events) :test #'equal :from-end t))
        (stage "the system init event: lisp_image, and the lisp tools after the file and shell tools")
        (show "lisp_image" (json:encode (jget init "lisp_image")))
        (let ((tools (coerce (jget init "tools") 'list)))
          (show "tools" (length tools))
          (show "first lisp tool, after bash"
                (list (position "bash" tools :test #'string=) (position "repl-eval" tools :test #'string=)))
          (show "tools (from bash on)" (subseq tools (or (position "bash" tools :test #'string=) 0))))
        (stage "tool_use / tool_result lines")
        (%print-tool-lines events)
        (stage "image_event lines (one per queued event, written on the loop thread)")
        (%print-image-events events)
        (stage "what the model saw: image event turns per generate")
        (show "per generate" (reverse requests))
        (stage "verdict")
        (let ((abort (%tool-result-named events "debug-invoke-restart" 0))
              (retry (%tool-result-named events "debug-invoke-restart" 1)))
          (show "restart 1 (abort), no rule" (list (jget abort "status") (clip (jget abort "content") 160)))
          (show "restart 0 (retry-later), allowed by the rule" (list (jget retry "status") (jget retry "content")))
          (show "permission_denials"
                (map 'list (lambda (d) (list (jget d "name") (jget d "reason")))
                     (or (jget result "permission_denials") #())))
          (show "result subtype" (jget result "subtype"))
          (show "result text" (jget result "result"))
          (let ((kinds (mapcar (lambda (e) (jget e "kind")) (%events-of events "image_event"))))
            (%check "Demo n"
             (and (eql 0 exit)
                 (equal "denied" (jget abort "status"))
                 (search "Invoked restart 0 [RETRY-LATER]" (jget retry "content"))
                 (equal '("debugger" "job") kinds)))))))))

;;; Demo o.

(defparameter *image-ui-timeout* 40 "Seconds for image waits (a spawn, then a turn).")

(defun %status-row (tui) (car (last (h:tui-screen tui))))

(defun %o1-turn ()
  (stage "o1: onDebugger \"turn\" (the default): an idle event starts a turn; the restart asks in a dialog")
  (let* ((project (make-project "image-tui-turn" '()))
         (backend (make-tui-mock
                   (list (list :tool "repl-eval" (json-args "code" +late-error+))
                         "Started the worker."
                         (list :tool "debug-invoke-restart" (json-args "restart" 0))
                         "The cache refresh failed in late-worker; I chose retry-later."))))
    (with-tui (tui program :rows 22 :argv '("--lisp-image" "spawn" "--allowed-tools" "repl-eval"))
        (project (scratch-dir "image-tui-turn-config") backend)
      (%wait tui "Message, @file" :timeout *image-ui-timeout*)
      (%wait-until tui (lambda () (search "λ image" (%status-row tui))) "the image marker")
      (snap tui "start: the status line marker")
      (show "state-image" (code:state-image (code:code-program-state program)))
      (send-line tui "start the worker")
      (%wait tui "Started the worker." :timeout *image-ui-timeout*)
      (%wait tui "λ image is in the debugger" :timeout *image-ui-timeout*)
      (let ((ok (%wait tui "Permission" :timeout *image-ui-timeout*)))
        (snap tui "the event started a turn: banner above the rule, marker (debugger), the restart dialog")
        (let ((turns (%image-turns (third (tui-mock-requests backend)))))
          (show "image event turns in generate 3" (length turns))
          (when turns (show "its text (first 2 lines)"
                            (format nil "~{~a~^~%~}" (subseq (lines (llm:turn-text (first turns))) 0 2)))))
        (h:tui-press tui :enter)
        (and ok
             (%wait tui "I chose retry-later" :timeout *image-ui-timeout*)
             (%wait-until tui (lambda () (not (search "λ image is in the debugger" (%screen-text tui))))
                          "the banner to go")
             (progn (%wait-until tui (lambda () (%idle-p program)) "idle")
                    (snap tui "after the restart: no banner, the marker is back to λ image")
                    (show "last tool result"
                          (%turns-text (last (llm:coerce-turns (fourth (tui-mock-requests backend))))))
                    t))))))

(defun %o2-notice ()
  (stage "o2: onDebugger \"notice\" (project settings): no turn starts; the event follows the next message")
  (let* ((project (make-project "image-tui-notice" '()))
         (backend (make-tui-mock
                   (list (list :tool "repl-eval" (json-args "code" +late-error+))
                         "Started the worker."
                         "late-worker failed (the cache refresh); restart 0, retry-later, would retry it."))))
    (write-file project ".cl-stack-code/settings.json"
                "{\"lispImage\": {\"image\": \"spawn\", \"onDebugger\": \"notice\"}}")
    (show ".cl-stack-code/settings.json" (read-file project ".cl-stack-code/settings.json"))
    (with-tui (tui program :rows 22 :argv '("--allowed-tools" "repl-eval"))
        (project (scratch-dir "image-tui-notice-config") backend)
      (%wait tui "Message, @file" :timeout *image-ui-timeout*)
      (send-line tui "start the worker")
      (%wait tui "Started the worker." :timeout *image-ui-timeout*)
      (%wait tui "λ image is in the debugger" :timeout *image-ui-timeout*)
      (sleep 2)
      (snap tui "idle with a waiting debugger: the notice, the banner, the marker; no turn")
      (show "generates so far" (length (tui-mock-requests backend)))
      (show "busy" (not (%idle-p program)))
      (send-line tui "what happened?")
      (prog1 (%wait tui "would retry it" :timeout *image-ui-timeout*)
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "the user's message, then the queued event, in the same request")
        (let* ((turns (llm:coerce-turns (third (tui-mock-requests backend))))
               (asked (position-if (lambda (turn) (search "what happened?" (or (llm:turn-text turn) ""))) turns))
               (event (position-if #'%image-turn-p turns)))
          (show "generate 3: user message at, event turn at" (list asked event)))))))

(defun demo-image-interactive ()
  (banner "Demo o: --lisp-image in the interactive mode on the headless terminal (scripted streaming mock)")
  (let ((results (list (cons "o1" (handler-case (%o1-turn)
                                    (error (c) (note "  !! o1 raised ~s: ~a" (type-of c) (clip c 400)) nil)))
                       (cons "o2" (handler-case (%o2-notice)
                                    (error (c) (note "  !! o2 raised ~s: ~a" (type-of c) (clip c 400)) nil))))))
    (stage "o: steps that reached their last screen")
    (loop for (name . ok) in results do (note "  ~4a ~:[NO~;yes~]" name ok))
    (unless (every #'cdr results)
      (error "interactive image steps did not finish: ~{~a~^, ~}" (mapcar #'car (remove-if #'cdr results))))
    t))

;;; Demo p.

(defparameter +prod-error+
  "(progn (sb-thread:make-thread (lambda () (sleep 3) (restart-case (error \"prod boom: payment gateway timed out\") (retry-later () :report \"Retry later\" 7))) :name \"prod-worker\") :started)"
  "Run by the image's owner just before the observer attaches: the thread
fails 3 s later, so its debugger goes to the newest connection (the
observer's; egao1980/lisp-mcp#22).")

(defun demo-image-observe ()
  (banner "Demo p: -p --lisp-image attach:PORT --lisp-observe: the event arrives, the restart is refused")
  (let* ((*model* '(:mock))
         (project (make-project "image-observe" '()))
         (server (lt:make-lisp-tools :root project :image-events nil)))
    (unwind-protect
         (let* ((r (progn (lt:call-lisp-tool server "repl-spawn" "{}")
                          (lt:call-lisp-tool server "repl-eval"
                                             (json-args "code" "(swank:create-server :port 0 :dont-close t)"))))
                (port (parse-integer r :start (+ 3 (search "=> " r)) :junk-allowed t))
                (argv (list "-p" "--output-format" "stream-json" "--lisp-image" (format nil "attach:~d" port)
                            "--lisp-observe" "--permission-mode" "bypassPermissions"
                            "why did the payment worker stop?")))
           (note "  the image's owner spawned an image with a second Swank server (port shown as PORT)")
           (setf *mock-script*
                 (list (lambda (turns)
                         (declare (ignore turns))
                         ;; the "model" thinks for 5 s: the worker fails meanwhile
                         (sleep 5)
                         (list (list "debug-state" (json-args))))
                       (lambda (turns)
                         (declare (ignore turns))
                         (list (list "debug-invoke-restart" (json-args "restart" 0))))
                       (lambda (turns)
                         (declare (ignore turns))
                         (list (list "repl-eval" (json-args "code" "(defparameter *poked* t)"))))
                       (lambda (turns)
                         (format nil "~:[No event reached me.~;prod-worker is in the debugger: the payment gateway timed out.~] I am an observer, so I cannot restart it: I would choose 0, retry-later, since the gateway may answer again."
                                 (%image-turns turns)))))
           (note "> (owner) repl-eval ~a" +prod-error+)
           (lt:call-lisp-tool server "repl-eval" (json-args "code" +prod-error+))
           (note "$ cl-stack-code --model mock ~{~a~^ ~}"
                 (mapcar (lambda (a) (cond ((uiop:string-prefix-p "attach:" a) "attach:PORT")
                                           ((find #\Space a) (format nil "'~a'" a))
                                           (t a)))
                         argv))
           (multiple-value-bind (exit out err) (cli argv :cwd project :config (scratch-dir "image-observe-config"))
             (let* ((events (decode-lines out))
                    (init (first (%events-of events "system")))
                    (lisp-image (jget init "lisp_image")))
               (show "exit code" exit)
               (show "stderr" (if (string= err "") "(empty)" err))
               (when lisp-image (setf (gethash "port" lisp-image) "PORT"))
               (show "lisp_image" (json:encode lisp-image))
               (show "debug-invoke-restart offered"
                     (and (member "debug-invoke-restart" (coerce (jget init "tools") 'list) :test #'string=) t))
               (stage "image_event lines")
               (%print-image-events events)
               (stage "tool_use / tool_result lines")
               (%print-tool-lines events :width 400)
               (show "result text" (jget (%result events) "result"))
               (stage "the owner's view")
               (show "(boundp '*poked*)" (lt:call-lisp-tool server "repl-eval" (json-args "code" "(boundp '*poked*)")))
               (let ((event (first (%events-of events "image_event")))
                     (restart (%tool-result-named events "debug-invoke-restart")))
                 (%check "Demo p"
                  (and (eql 0 exit)
                      event
                      (search "observe mode: it cannot choose a restart" (jget event "text"))
                      restart (eq t (jget restart "is_error"))))))))
      (lt:close-lisp-tools server))))
