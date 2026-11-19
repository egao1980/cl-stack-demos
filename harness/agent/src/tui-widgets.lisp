(in-package #:harness-demos-agent)

;;; Demo 3: tui-protocol/widgets 0.2.0 on tui-backend-terminal's headless
;;; driver over the pure-Lisp terminal-protocol emulator.

(defun %emulator () (make-instance 'terminal-backend-lisp:lisp-backend))

(defun %settled-screen (tui &key (quiet 0.15) (timeout 3))
  "Wait until the screen has not changed for QUIET seconds. → the rows."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
        (last (h:tui-screen tui))
        (since (get-internal-real-time)))
    (loop
      (sleep 0.02)
      (let ((now (h:tui-screen tui)))
        (if (equal now last)
            (when (> (- (get-internal-real-time) since)
                     (* quiet internal-time-units-per-second))
              (return now))
            (setf last now since (get-internal-real-time))))
      (when (> (get-internal-real-time) deadline) (return last)))))

(defun %snap (tui label &key wait-for)
  "Print the emulator screen as text rows, after WAIT-FOR shows (or the
screen settles)."
  (when wait-for
    (unless (h:tui-wait-for tui wait-for :timeout 3)
      (note "  (timed out waiting for ~s on screen)" wait-for)))
  (let ((rows (%settled-screen tui)))
    (format t "~&  screen after ~a:~%" label)
    (loop for row in rows for i from 0
          do (format t "  ~2d |~a|~%" i row))
    (finish-output)
    rows))

(defun %headless (program cols rows)
  (h:make-headless-tui program :cols cols :rows rows :terminal-backend (%emulator)))

(defun %print-events (program)
  (dolist (e (w:widget-program-events program))
    (typecase e
      (w:submit-event (note "  event: submit-event text=~s" (w:submit-event-text e)))
      (w:confirm-event (note "  event: confirm-event choice=~s feedback=~s"
                             (w:confirm-event-choice e) (w:confirm-event-feedback e)))
      (w:cancel-event (note "  event: cancel-event"))
      (t (note "  event: ~s" e)))))

(defun %demo-text-input ()
  (stage "3a. text-input with history, a wide character, typing and paste")
  (let* ((input (make-instance 'w:text-input :prompt "> " :placeholder "Ask anything"
                                             :history (list "git status" "make test")))
         (program (w:make-widget-program input))
         (tui (%headless program 30 4)))
    (unwind-protect
         (progn
           (%snap tui "start (placeholder)" :wait-for "Ask")
           (h:tui-type tui "héllo 世界")
           (%snap tui "typing \"héllo 世界\"" :wait-for "界")
           (show "value" (w:text-input-value input))
           (show "cursor (row col)" (multiple-value-list (w:text-input-cursor input)))
           (show "cursor display column" (w:text-input-cursor-column input))
           (h:tui-paste tui (format nil "~%pasted line~c~cwith	tab" #\Return #\Newline))
           (%snap tui "pasting \"\\npasted line\\r\\nwith\\ttab\"" :wait-for "tab")
           (show "value" (w:text-input-value input))
           (show "lines" (w:text-input-lines input))
           (h:tui-press tui :enter)
           (%snap tui "Enter (submit)")
           (h:tui-press tui :arrow-up)
           (%snap tui "Up (history: newest)")
           (loop for i from 2 to 5
                 do (h:tui-press tui :arrow-up)
                    (%snap tui (format nil "Up #~d" i))
                    (show "  cursor (row col)" (multiple-value-list (w:text-input-cursor input))))
           (show "history" (w:text-input-history input))
           (h:tui-press tui :escape)
           (sleep 0.2)
           (%print-events program))
      (ignore-errors (h:tui-stop tui)))))

(defun %demo-confirm (label keys)
  "Run a confirm-dialog and drive it with KEYS: a list of (:press key) or
(:type string) or (:snap label)."
  (stage (format nil "3b. confirm-dialog: ~a" label))
  (let* ((dialog (make-instance 'w:confirm-dialog
                                :title "Permission"
                                :message "coder wants to edit src/a.lisp (3 lines)"))
         (program (w:make-widget-program dialog :quit-on '(w:confirm-event w:cancel-event)))
         (tui (%headless program 44 10)))
    (unwind-protect
         (progn
           (%snap tui "open" :wait-for "Deny")
           (dolist (k keys)
             (ecase (first k)
               (:press (h:tui-press tui (second k)))
               (:type (h:tui-type tui (second k)))
               (:snap (%snap tui (second k)))))
           (h:tui-result tui :timeout 3)
           (show "program finished" (not (h:tui-running-p tui)))
           (show "dialog mode" (w:confirm-dialog-mode dialog))
           (%print-events program))
      (ignore-errors (h:tui-stop tui)))))

;;; A small composite program: status-line on row 0, a follow-tail viewport,
;;; a spinner on the last row. Streamed text arrives as STREAM-CHUNK messages
;;; sent with TUI-SEND from the demo thread.

(defstruct stream-chunk (text "" :type string))
(defstruct stream-done)

(defclass agent-pane ()
  ((status :initform (make-instance 'w:status-line :left " coder · qwen3.5-2b"
                                                   :right "ctx 12% "
                                                   :style (terminal-protocol:make-style
                                                           :inverse t))
           :reader pane-status)
   (viewport :initform (make-instance 'w:viewport :follow t) :reader pane-viewport)
   (spinner :initform (make-instance 'w:spinner :label "thinking…" :interval 0.1)
            :reader pane-spinner)
   (run :initform nil :accessor pane-run)))

(defmethod tui:tui-init ((pane agent-pane))
  (setf (pane-run pane) tui:*tui*)
  (values pane (w:widget-init (pane-spinner pane))))

(defmethod tui:tui-update ((pane agent-pane) model message)
  (declare (ignore model))
  (cond ((stream-chunk-p message)
         (w:viewport-append-text (pane-viewport pane) (stream-chunk-text message))
         (values pane nil))
        ((stream-done-p message)
         (w:spinner-stop (pane-spinner pane))
         (setf (w:status-line-right (pane-status pane)) "done ")
         (values pane nil))
        ((tui:tick-message-p message)
         (multiple-value-bind (s command) (w:widget-update (pane-spinner pane) message)
           (declare (ignore s))
           (values pane command)))
        (t
         (w:widget-update (pane-viewport pane) message)
         (values pane nil))))

(defmethod tui:tui-view ((pane agent-pane) model canvas)
  (declare (ignore model))
  (let ((cols (tui:canvas-cols canvas)) (rows (tui:canvas-rows canvas)))
    (w:widget-view (pane-status pane) (tui:sub-canvas canvas 0 0 cols 1))
    (w:widget-view (pane-viewport pane) (tui:sub-canvas canvas 0 1 cols (- rows 2)))
    (w:widget-view (pane-spinner pane) (tui:sub-canvas canvas 0 (1- rows) cols 1))))

(defun %demo-pane ()
  (stage "3c. viewport (follow tail) receiving streamed text + spinner + status-line")
  (let* ((pane (make-instance 'agent-pane))
         (tui (%headless pane 36 7)))
    (unwind-protect
         (let ((vp (pane-viewport pane)))
           (h:tui-wait-until tui (lambda () (pane-run pane)))
           (%snap tui "start" :wait-for "thinking")
           (flet ((send (text) (tui:tui-send (make-stream-chunk :text text) (pane-run pane))))
             (dolist (chunk (list "I'll read src/a.lisp " (format nil "first.~%It defines ") (format nil "GREET and MAIN.~%")))
               (send chunk)
               (sleep 0.05))
             (%snap tui "3 streamed chunks" :wait-for "MAIN")
             (dolist (chunk (list (format nil "Plan:~%") (format nil "1. rename greet~%") (format nil "2. update callers~%") (format nil "3. run the tests~%") (format nil "4. commit~%") "Starting with step 1"))
               (send chunk)
               (sleep 0.05))
             (%snap tui "5 more chunks (tail followed)" :wait-for "step 1")
             (show "row count / max offset / offset / at bottom / follow"
                   (list (w:viewport-row-count vp) (w:viewport-max-offset vp)
                         (w:viewport-offset vp) (w:viewport-at-bottom-p vp)
                         (w:viewport-follow-p vp)))
             (h:tui-press tui :home)
             (%snap tui "Home (scrolled to top, follow off)")
             (show "offset / follow" (list (w:viewport-offset vp) (w:viewport-follow-p vp)))
             (send " … more text while scrolled up")
             (%snap tui "a chunk while scrolled up (view stays put)")
             (h:tui-press tui :end)
             (%snap tui "End (back at the tail, follow on)" :wait-for "scrolled up")
             (show "offset / follow" (list (w:viewport-offset vp) (w:viewport-follow-p vp)))
             (show "spinner frame index" (w:spinner-frame (pane-spinner pane)))
             (tui:tui-send (make-stream-done) (pane-run pane))
             (%snap tui "stream done (spinner stopped, status right = done)" :wait-for "done")))
      (ignore-errors (h:tui-stop tui)))))

(defun demo-tui-widgets ()
  (banner "Demo 3: tui-protocol/widgets 0.2.0 on the headless driver")
  (%demo-text-input)
  (let ((term (terminal-protocol:make-terminal :cols 10 :rows 2 :backend (%emulator))))
    (note "~&note: encode-key :digit-1 with no :text → ~s; with :text \"1\" → ~s (so the demo types digits)"
          (terminal-protocol:encode-key term :digit-1)
          (terminal-protocol:encode-key term :digit-1 :text "1"))
    (terminal-protocol:terminal-close term))
  (%demo-confirm "allow once (type 1)"
                 '((:type "1")))
  (%demo-confirm "always allow (Down, Enter)"
                 '((:press :arrow-down) (:snap "Down") (:press :enter)))
  (%demo-confirm "deny with feedback (type 3, type text, Enter)"
                 '((:type "3") (:snap "3 (feedback mode)")
                   (:type "use src/b.lisp instead") (:snap "typing feedback")
                   (:press :enter)))
  (%demo-pane))
