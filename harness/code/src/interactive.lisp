(in-package #:harness-demos-code)

;;; Demo f: cl-stack-code 0.2.0 interactive mode (H3b). The CODE-PROGRAM made
;;; by MAKE-CODE-PROGRAM, driven on tui-backend-terminal's headless driver
;;; over the pure-Lisp emulator, as cl-stack-code's program tests do. The
;;; model is a scripted streaming mock (provider "tui-mock") so every screen
;;; is deterministic; gates hold a generate until the demo opens them, which
;;; makes the interrupt and the queued follow-up exact. One extra turn runs
;;; on LM Studio when a model is there.

;;; A scripted streaming backend. Steps:
;;;   "text"                         streamed word by word, finish :stop
;;;   (:tool name json)              one tool call
;;;   (:tools (name json) …)         several tool calls
;;;   (:split "before" GATE "after") stream BEFORE, wait at GATE, stream AFTER
;;;   (:gate GATE STEP)              wait at GATE, then answer with STEP
;;; Usage: 10 in, 5 out per response.

(defstruct (gate (:constructor make-gate ()))
  (lock (bt:make-lock "gate"))
  (cv (bt:make-condition-variable))
  (open nil)
  (waiting nil))

(defun open-gate (gate)
  (bt:with-lock-held ((gate-lock gate))
    (setf (gate-open gate) t)
    (bt:condition-notify (gate-cv gate)))
  gate)

(defun gate-waiting-p (gate)
  (bt:with-lock-held ((gate-lock gate)) (gate-waiting gate)))

(defun %pass-gate (gate)
  (bt:with-lock-held ((gate-lock gate))
    (setf (gate-waiting gate) t)
    (loop until (gate-open gate)
          do (bt:condition-wait (gate-cv gate) (gate-lock gate) :timeout 1))))

(defclass tui-mock (llm:llm-backend)
  ((steps :initarg :steps :reader tui-mock-steps)
   (requests :initform '() :accessor %tui-mock-requests)
   (lock :initform (bt:make-lock "tui-mock") :reader %tui-mock-lock)))

(defun make-tui-mock (steps) (make-instance 'tui-mock :steps steps))

(defun tui-mock-requests (backend)
  "Each request's turns, oldest first."
  (bt:with-lock-held ((%tui-mock-lock backend))
    (reverse (%tui-mock-requests backend))))

(defmethod llm:backend-supports-p ((backend tui-mock) feature)
  (and (member feature '(:tools :stream)) t))

(defun %words (text)
  "TEXT cut after each run of spaces or newlines."
  (let ((out '()) (start 0) (n (length text)))
    (flet ((space-p (c) (member c '(#\Space #\Newline))))
      (loop while (< start n)
            do (let* ((sp (or (position-if #'space-p text :start start) n))
                      (end (or (position-if-not #'space-p text :start sp) n)))
                 (push (subseq text start end) out)
                 (setf start end))))
    (nreverse out)))

(defun %stream-text (text on-part)
  (when on-part
    (dolist (w (%words text))
      (funcall on-part (llm:make-llm-text-part :text w))
      (sleep 0.01))))

(defun %tui-mock-respond (backend turns on-part)
  (let* ((n (bt:with-lock-held ((%tui-mock-lock backend))
              (push (copy-list (llm:coerce-turns turns)) (%tui-mock-requests backend))
              (length (%tui-mock-requests backend))))
         (step (or (nth (1- n) (tui-mock-steps backend)) "(the mock script is out of steps)"))
         (text "") (calls '()))
    (loop while (and (consp step) (eq (first step) :gate))
          do (%pass-gate (second step))
             (setf step (third step)))
    (flet ((calls (specs)
             (loop for (name arguments) in specs
                   for i from 1
                   collect (llm:make-llm-tool-call-part
                            :id (format nil "call-~d-~d" n i) :name name :arguments arguments))))
      (etypecase step
        (string (setf text step) (%stream-text text on-part))
        (cons
         (ecase (first step)
           (:tool (setf calls (calls (list (rest step)))))
           (:tools (setf calls (calls (rest step))))
           (:split (destructuring-bind (before gate after) (rest step)
                     (%stream-text before on-part)
                     (%pass-gate gate)
                     (%stream-text after on-part)
                     (setf text (concatenate 'string before after))))))))
    (when on-part (dolist (c calls) (funcall on-part c)))
    (llm:make-llm-response
     :parts (append (and (plusp (length text)) (list (llm:make-llm-text-part :text text))) calls)
     :model "tui-mock"
     :finish-reason (if calls :tool-use :stop)
     :usage (llm:make-llm-usage :input-tokens 10 :output-tokens 5 :total-tokens 15))))

(defmethod llm:generate ((backend tui-mock) turns &key model settings tools tool-choice output)
  (declare (ignore model settings tools tool-choice output))
  (%tui-mock-respond backend turns nil))

(defmethod llm:stream-generate ((backend tui-mock) turns &key model settings tools tool-choice
                                                              on-part output)
  (declare (ignore model settings tools tool-choice output))
  (%tui-mock-respond backend turns on-part))

(defun %turns-text (turns)
  "All text and tool-result content of TURNS."
  (with-output-to-string (s)
    (dolist (turn turns)
      (dolist (part (llm:llm-turn-parts turn))
        (cond ((llm:llm-text-part-p part) (format s "~a~%" (llm:llm-text-part-text part)))
              ((llm:llm-tool-result-part-p part)
               (format s "[tool_result~:[~; error~]] ~a~%"
                       (llm:llm-tool-result-part-error-p part)
                       (llm:llm-tool-result-part-content part))))))))

;;; The headless TUI.

(defparameter *ui-timeout* 30)

(defun %screen-text (tui) (format nil "~{~a~^~%~}" (h:tui-screen tui)))

(defun %wait (tui needle &key (timeout *ui-timeout*))
  (or (h:tui-wait-for tui needle :timeout timeout)
      (progn (note "  (timed out after ~ds waiting for ~s on screen)" timeout needle) nil)))

(defun %wait-until (tui predicate what &key (timeout *ui-timeout*))
  (or (h:tui-wait-until tui predicate :timeout timeout)
      (progn (note "  (timed out after ~ds waiting for ~a)" timeout what) nil)))

(defun %settled (tui &key (quiet 0.2) (timeout 3))
  "The screen rows once they have not changed for QUIET seconds."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
        (last (h:tui-screen tui))
        (since (get-internal-real-time)))
    (loop
      (sleep 0.02)
      (let ((now (h:tui-screen tui)))
        (if (equal now last)
            (when (> (- (get-internal-real-time) since) (* quiet internal-time-units-per-second))
              (return now))
            (setf last now since (get-internal-real-time))))
      (when (> (get-internal-real-time) deadline) (return last)))))

(defun %redact-row (row)
  "REDACT, plus a scratch path the status line cut short (\"/private/var/…/cl-stack-c…\")."
  (let* ((row (redact row))
         (scratch (%native *scratch*))
         (cut (search "…" row)))
    (when cut
      (dotimes (start cut)
        (when (char= #\/ (char row start))
          (let ((prefix (subseq row start cut)))
            (when (and (>= (length prefix) 4) (uiop:string-prefix-p prefix scratch))
              (return-from %redact-row
                (concatenate 'string (subseq row 0 start) "<tmp>/" (subseq row cut))))))))
    row))

(defun snap (tui label &key (settle t))
  "Print the screen as rows between bars (paths redacted)."
  (let ((rows (if settle (%settled tui) (h:tui-screen tui))))
    (format t "~&  ┌ screen: ~a~%" label)
    (loop for row in rows for i from 0
          do (format t "  ~2d│~a│~%" i (%redact-row row)))
    (finish-output)
    rows))

(defun send-line (tui text)
  (h:tui-type tui text)
  (h:tui-press tui :enter))

(defun %idle-p (program) (not (code:state-busy-p (code:code-program-state program))))

(defun call-with-tui (project config backend fn &key argv (cols 80) (rows 24) gates
                                                     (model "tui-mock") providers getenv)
  (let* ((program (code:make-code-program
                   (code:parse-arguments (append (list "--cwd" (%native project) "--model" model)
                                                 argv))
                   :providers (or providers
                                  (list (cons "tui-mock" (lambda (m env)
                                                           (declare (ignore m env))
                                                           backend))))
                   :user-config-dir config
                   :getenv (or getenv (lambda (name) (declare (ignore name)) nil))))
         (tui nil))
    (unwind-protect
         (progn
           (setf tui (h:make-headless-tui program :cols cols :rows rows
                                                  :terminal-backend (make-instance 'tl:lisp-backend)))
           (funcall fn tui program))
      (dolist (g gates) (open-gate g))
      (when tui (ignore-errors (h:tui-stop tui)))
      (code:close-code-program program))))

(defmacro with-tui ((tui program &rest keys) (project config backend) &body body)
  `(call-with-tui ,project ,config ,backend
                  (lambda (,tui ,program) (declare (ignorable ,tui ,program)) ,@body)
                  ,@keys))

(defun %edit-args (path old new)
  (json-args "file_path" path "old_string" old "new_string" new))

;;; The scenarios. Each returns true when it reached its last screen.

(defun %f1-startup ()
  (stage "f1: the first screen (120 × 12): empty transcript, rule, input, status line")
  (let ((project (make-project "tui-start" '(("README.md" "# demo"))))
        (backend (make-tui-mock '())))
    (with-tui (tui program :cols 120 :rows 12) (project (scratch-dir "tui-start-config") backend)
      (prog1 (%wait tui "Message, @file, /help")
        (snap tui "start")
        (multiple-value-bind (left right) (code:status-line-text (code:code-program-state program))
          (show "status-line-text left" left)
          (show "status-line-text right" right))))))

(defun %f2-mention ()
  (stage "f2: an @ mention: the completion menu, Tab, and the file attached to the prompt")
  (let ((project (make-project "tui-mention"
                               `(("src/alpha.lisp" ,(format nil "(defun alpha () :alpha)~%"))
                                 ("src/alphabet.lisp" ,(format nil "(defun abc () \"abc\")~%"))
                                 ("README.md" "readme")
                                 (".gitignore" ,(format nil "build/~%"))
                                 ("build/alpha.fasl" "ignored"))))
        (backend (make-tui-mock '("Seen `alpha`: it returns `:alpha`."))))
    (show "project-files" (code:project-files project))
    (show "mention-candidates \"@alp\"" (code:mention-candidates (code:project-files project) "@alp"))
    (with-tui (tui program :rows 14) (project (scratch-dir "tui-mention-config") backend)
      (%wait tui "Message, @file")
      (h:tui-type tui "look at @alp")
      (%wait tui "@src/alpha.lisp")
      (snap tui "typing \"look at @alp\" (the menu sits under the input)")
      (h:tui-press tui :tab)
      (%wait-until tui (lambda () (equal "look at @src/alpha.lisp"
                                         (w:text-input-value (code:code-program-input program))))
                   "the completed input")
      (snap tui "Tab")
      (h:tui-press tui :enter)
      (prog1 (%wait tui "it returns")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "Enter and the answer")
        (show "what the model received (first request, user text)"
              (let ((text (%turns-text (first (tui-mock-requests backend)))))
                (subseq text (or (search "look at" text) 0))))))))

(defparameter *markdown-answer*
  (format nil "# Plan~%~%The bug is in `greet`: it **drops** the name.~%~%1. read `src/greet.lisp`~%2. fix the *format* call~%3. run the tests~%~%```lisp~%(defun greet (name)~%  (format nil \"Hello, ~~a!\" name))~%```~%~%| step | file |~%|------|------|~%| fix | src/greet.lisp |~%~%> Nothing is written until you approve.~%"))

(defun %f3-markdown ()
  (stage "f3: a streamed markdown answer: mid-stream (held at a gate) and finished")
  (let* ((gate (make-gate))
         (cut (search "2. fix" *markdown-answer*))
         (project (make-project "tui-md" '()))
         (backend (make-tui-mock (list (list :split (subseq *markdown-answer* 0 cut) gate
                                             (subseq *markdown-answer* cut))))))
    (with-tui (tui program :rows 28 :gates (list gate)) (project (scratch-dir "tui-md-config") backend)
      (%wait tui "Message, @file")
      (send-line tui "plan the fix")
      (%wait-until tui (lambda () (gate-waiting-p gate)) "the stream to reach the gate")
      (%wait tui "read src/greet.lisp")
      (snap tui "mid-stream: spinner, partial markdown")
      (open-gate gate)
      (prog1 (%wait tui "Nothing is written")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "finished: heading without #, no ** or backticks, code block, table, quote")))))

(defun %f4-approve-always (config)
  (stage "f4: an edit asks: the dialog with its diff preview, answered \"Always allow\"")
  (let* ((project (make-project "tui-always"
                                `(("src/greet.lisp"
                                   ,(format nil "(in-package :demo)~%~%(defun greet (name)~%  (format nil \"Hello!\"))~%~%(defun bye (name)~%  (format nil \"Bye!\"))~%")))))
         (backend (make-tui-mock
                   (list (list :tool "edit" (%edit-args "src/greet.lisp"
                                                        "(format nil \"Hello!\")"
                                                        "(format nil \"Hello, ~a!\" name)"))
                         (list :tool "edit" (%edit-args "src/greet.lisp"
                                                        "(format nil \"Bye!\")"
                                                        "(format nil \"Bye, ~a!\" name)"))
                         "Both functions now use `name`."))))
    (with-tui (tui program :rows 26) (project config backend)
      (%wait tui "Message, @file")
      (send-line tui "use the name in greet and bye")
      (%wait tui "Always allow")
      (snap tui "the approval dialog")
      (show "nothing written yet: src/greet.lisp has \"Hello!\""
            (and (search "\"Hello!\"" (read-file project "src/greet.lisp")) t))
      (h:tui-press tui :arrow-down)
      (h:tui-press tui :enter)
      (prog1 (%wait tui "Both functions now use name")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "after \"Always allow\": the second edit of the same file did not ask")
        (show "src/greet.lisp" (read-file project "src/greet.lisp"))
        (show ".cl-stack-code/settings.local.json"
              (read-file project ".cl-stack-code/settings.local.json"))
        (show "approval dialogs shown" 1)))))

(defun %f4b-next-start (config)
  (stage "f4b: the next start reads the saved rule: /permissions lists it, the edit runs without a dialog")
  (let* ((project (truename (merge-pathnames "tui-always/" *scratch*)))
         (backend (make-tui-mock
                   (list (list :tool "edit" (%edit-args "src/greet.lisp" "(in-package :demo)"
                                                        "(in-package #:demo)"))
                         "Done."))))
    (with-tui (tui program :rows 20) (project config backend)
      (%wait tui "Message, @file")
      (send-line tui "/permissions")
      (%wait tui "edit(src/greet.lisp)")
      (send-line tui "fix the in-package")
      (prog1 (%wait tui "Done.")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "/permissions, then an edit with no dialog")))))

(defun %f5-reject-feedback ()
  (stage "f5: Reject with feedback: the edit is not made and the model reads the feedback")
  (let* ((project (make-project "tui-reject" `(("a.txt" ,(format nil "alpha~%")))))
         (backend (make-tui-mock (list (list :tool "edit" (%edit-args "a.txt" "alpha" "omega"))
                                       "OK, I will put it in `b.txt` instead."))))
    (with-tui (tui program :rows 22) (project (scratch-dir "tui-reject-config") backend)
      (%wait tui "Message, @file")
      (send-line tui "change alpha to omega")
      (%wait tui "Reject all")
      (h:tui-type tui "3")
      (%wait tui "What should it do instead?")
      (h:tui-type tui "leave a.txt alone, use b.txt")
      (snap tui "3 = Reject, typing the feedback")
      (h:tui-press tui :enter)
      (prog1 (%wait tui "instead.")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "the model's answer to the feedback")
        (show "a.txt (unchanged)" (read-file project "a.txt"))
        (show "the tool result the model got"
              (string-trim '(#\Newline)
                           (let ((text (%turns-text (second (tui-mock-requests backend)))))
                             (subseq text (or (search "[tool_result" text) 0)))))))))

(defun %f6-escape ()
  (stage "f6: Esc interrupts a running turn")
  (let* ((gate (make-gate))
         (project (make-project "tui-esc" '()))
         (backend (make-tui-mock (list (list :gate gate "this answer arrives too late")
                                       "The conversation goes on."))))
    (with-tui (tui program :rows 14 :gates (list gate)) (project (scratch-dir "tui-esc-config") backend)
      (%wait tui "Message, @file")
      (send-line tui "count to a million")
      (%wait-until tui (lambda () (gate-waiting-p gate)) "the generate to wait at its gate")
      (%wait tui "Working… (Esc to interrupt)")
      (snap tui "busy: the spinner above the rule")
      (h:tui-press tui :escape)
      (open-gate gate)
      (%wait tui "interrupted")
      (%wait-until tui (lambda () (%idle-p program)) "idle")
      (snap tui "Esc")
      (send-line tui "then just say hi")
      (prog1 (%wait tui "The conversation goes on.")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "the next message")
        (show "\"too late\" text shown" (and (search "too late" (%screen-text tui)) t))))))

(defun %f7-queue ()
  (stage "f7: a follow-up typed while a turn runs is queued, then sent when the turn ends")
  (let* ((gate (make-gate))
         (project (make-project "tui-queue" '()))
         (backend (make-tui-mock (list (list :gate gate "First answer.")
                                       "Second answer, with the first still in the history."))))
    (with-tui (tui program :rows 16 :gates (list gate)) (project (scratch-dir "tui-queue-config") backend)
      (%wait tui "Message, @file")
      (send-line tui "one")
      (%wait-until tui (lambda () (gate-waiting-p gate)) "the generate to wait at its gate")
      (send-line tui "two")
      (%wait tui "queued: two")
      (snap tui "\"two\" typed while \"one\" runs")
      (show "state-queue" (code:state-queue (code:code-program-state program)))
      (open-gate gate)
      (prog1 (%wait tui "Second answer")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap tui "the queue drained: one, First answer, two, Second answer")
        (show "requests sent" (length (tui-mock-requests backend)))
        (show "the second request holds \"one\" and \"First answer.\""
              (let ((text (%turns-text (second (tui-mock-requests backend)))))
                (and (search "one" text) (search "First answer." text) t)))))))

(defun %f8-help ()
  (stage "f8: /help")
  (let ((project (make-project "tui-help" '()))
        (backend (make-tui-mock '())))
    (with-tui (tui program :rows 30) (project (scratch-dir "tui-help-config") backend)
      (%wait tui "Message, @file")
      (send-line tui "/help")
      (prog1 (%wait tui "/permissions [MODE]")
        (snap tui "/help")
        (show "requests sent to the model" (length (tui-mock-requests backend)))))))

(defun %f9-no-terminal ()
  (stage "f9: run-cli without -p on string streams: not a terminal, exit 2, nothing sent")
  (let ((project (make-project "tui-noterm" '())))
    (let ((*mock-script* '()) (*model* '(:mock)))
      (multiple-value-bind (exit out err) (cli '("hello") :cwd project :config (scratch-dir "tui-noterm-config"))
        (show "exit code" exit)
        (show "stdout" (if (string= out "") "(empty)" out))
        (show "stderr" err)
        (eql exit 2)))))

(defun %f10-real-model ()
  (stage "f10: one turn on the LM Studio model, same program, same headless terminal")
  (unless (real-model-p)
    (note "  skipped: no LM Studio model reachable (the mock steps above are the demo)")
    (return-from %f10-real-model t))
  (let ((project (make-project "tui-real"
                               `(("notes.txt" ,(format nil "Release notes~%Codename: Juniper~%Ships: 2026-12-01~%"))))))
    (with-tui (tui program :rows 24 :model (model-designator) :providers nil :getenv (model-getenv)
                   :argv (list "--append-system-prompt" *no-think* "--max-turns" "6"
                               "--allowed-tools" "read" "--disallowed-tools" "write,edit,multi_edit,bash"))
        (project (scratch-dir "tui-real-config") nil)
      (%wait tui "Message, @file")
      (send-line tui "Read @notes.txt and give the codename in one sentence, in **bold**.")
      (%wait-until tui (lambda () (not (%idle-p program))) "the turn to start")
      (let ((done (h:tui-wait-until tui (lambda () (%idle-p program)) :timeout 420)))
        (snap tui (format nil "the real model (~a) ~:[still working after 420 s~;answered~]"
                          (second *model*) done))
        (unless done
          (h:tui-press tui :escape)
          (%wait-until tui (lambda () (%idle-p program)) "the interrupt")
          (snap tui "Esc on the real model's turn")))
      (let ((kinds (mapcar #'code:entry-kind (code:state-transcript (code:code-program-state program)))))
        (show "transcript entry kinds" kinds)
        (when (member :error kinds)
          (note "  an error entry: a stack failure, not model variance"))
        ;; A slow or unhelpful small model is not a demo failure; an error entry is.
        (not (member :error kinds))))))

(defun demo-interactive ()
  (banner "Demo f: interactive mode (H3b) on the headless terminal, scripted streaming mock")
  (let ((config (scratch-dir "tui-always-config"))
        (results '()))
    (flet ((run-step (name thunk)
             (let ((ok (handler-case (funcall thunk)
                         (error (c)
                           (note "  !! ~a raised ~s: ~a" name (type-of c) (clip (princ-to-string c) 400))
                           nil))))
               (push (cons name (and ok t)) results))))
      (run-step "f1" #'%f1-startup)
      (run-step "f2" #'%f2-mention)
      (run-step "f3" #'%f3-markdown)
      (run-step "f4" (lambda () (%f4-approve-always config)))
      (run-step "f4b" (lambda () (%f4b-next-start config)))
      (run-step "f5" #'%f5-reject-feedback)
      (run-step "f6" #'%f6-escape)
      (run-step "f7" #'%f7-queue)
      (run-step "f8" #'%f8-help)
      (run-step "f9" #'%f9-no-terminal)
      (run-step "f10" #'%f10-real-model))
    (stage "f: steps that reached their last screen")
    (loop for (name . ok) in (reverse results) do (note "  ~4a ~:[NO~;yes~]" name ok))
    (unless (every #'cdr results)
      (error "interactive steps did not finish: ~{~a~^, ~}"
             (mapcar #'car (remove-if #'cdr (reverse results)))))))
