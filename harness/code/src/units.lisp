(in-package #:harness-demos-code)

;;; Demo t: cl-stack-code 0.8.0 (H8d), the session's units. Each program,
;;; SDK / --rpc session and -p run has its own supervisor (supervisor-protocol
;;; 0.3.1); every bash command is a :shell-job unit and the agent gets the
;;; unit tools (agent-tools 0.3.1). t1-t4 drive the code program on the
;;; headless terminal (120 x 30, the scripted streaming mock TUI-MOCK, as
;;; Demos f / h / m / o): the status line count, the Ctrl-P panel, follow and
;;; search, s under the three rule outcomes next to the agent's own
;;; unit_control call, a failure notice. t5 is the units/* methods of --rpc
;;; on in-memory pipes (raw lines, as Demo r). t6 SIGKILLs a child
;;; `cl-stack-code --rpc` (as Demo s starts it) that left two jobs running,
;;; and the next session in the same project sweeps them.
;;;
;;; Masked on the printed screens: the PID, UP, CPU% and RSS columns of a
;;; unit row become <pid> <up> <cpu> <rss> when they hold a value ("-" stays),
;;; and unit pids elsewhere <pid>; in JSON "pid", "started", "uptime_s",
;;; "time", "cpu_percent", "cpu_time_s", "rss_bytes" and "session_id".

(defun %pick (unix windows)
  "UNIX on macOS / Linux (/bin/sh -c), WINDOWS on Windows (cmd.exe /d /s /c:
agent-tools' default shells)."
  (if (uiop:os-windows-p) windows unix))

(defun %bg (command)
  "A mock step: bash COMMAND in the background (a :shell-job unit)."
  (list :tool "bash" (json-args "command" command "run_in_background" t)))

(defun %job-a ()
  (%pick "echo 'job-a: fetching deps'; echo 'job-a: waiting for the build lock'; sleep 300"
         "echo job-a: fetching deps& echo job-a: waiting for the build lock& ping -n 301 127.0.0.1 >nul"))

(defun %job-b ()
  (%pick "echo 'job-b: watching src/ for changes'; sleep 300"
         "echo job-b: watching src/ for changes& ping -n 301 127.0.0.1 >nul"))

(defparameter +server-late-line+ "GET /health 200 again"
  "The line the dev server prints once the demo creates .go (Unix), or after
3 s (Windows: cmd.exe has no one-line wait loop).")

(defun %server ()
  "A fake dev server: 16 request lines (log lines 2-17), then one more after
.go appears (line 18). The search index scores blocks of 16 log lines and
gives one hit per block, so the two /health lines (3 and 18) are two hits."
  (%pick (format nil "echo 'serving on 127.0.0.1:8765'; echo 'GET /health 200'; for i in 1 2 3 4 5 6 7 8 9 10 11 12; do echo \"GET /static/app.js?v=$i 304\"; done; echo 'GET /api/units 200'; echo 'GET /api/missing 404'; while [ ! -f .go ]; do sleep 0.1; done; echo '~a'; sleep 300"
                 +server-late-line+)
         (format nil "echo serving on 127.0.0.1:8765& echo GET /health 200& (for /L %i in (1,1,12) do @echo GET /static/app.js?v=%i 304)& echo GET /api/units 200& echo GET /api/missing 404& ping -n 4 127.0.0.1 >nul& echo ~a& ping -n 301 127.0.0.1 >nul"
                 +server-late-line+)))

(defun %failing-build ()
  (%pick "echo 'build: compiling greet.c'; sleep 1; echo 'greet.c:3: error: undefined symbol greet' >&2; exit 3"
         "echo build: compiling greet.c& ping -n 2 127.0.0.1 >nul& (echo greet.c:3: error: undefined symbol greet)1>&2& exit 3"))

;;; Screens with the clock-driven columns masked.

(defun %sup-units (sup) (coerce (sup:supervisor-units sup) 'list))

(defun %unit-ids (sup) (mapcar #'sup:unit-id (%sup-units sup)))

(defun %pid-redactions (sup)
  (loop for u in (%sup-units sup)
        for pid = (sup:unit-pid u)
        when pid collect (cons (princ-to-string pid) "<pid>")))

(defun %pad (string n)
  (if (< (length string) n)
      (concatenate 'string string (make-string (- n (length string)) :initial-element #\Space))
      string))

(defun %mask-field (row start width tag)
  (let ((field (string-trim " " (subseq row start (+ start width)))))
    (if (or (string= field "") (string= field "-"))
        row
        (concatenate 'string (subseq row 0 start) (format nil "~v@a" width tag)
                     (subseq row (+ start width))))))

(defun %mask-unit-row (row ids)
  "ROW with PID (43-48), UP (50-55), CPU% (57-61) and RSS (63-68) masked when
it is a unit row of the panel (its name column is one of IDS)."
  (if (and (>= (length row) 22)
           (member (string-trim " " (subseq row 2 22)) ids :test #'string=))
      (let ((r (%pad row 69)))
        (setf r (%mask-field r 43 6 "<pid>")
              r (%mask-field r 50 6 "<up>")
              r (%mask-field r 57 5 "<cpu>")
              r (%mask-field r 63 6 "<rss>"))
        (string-right-trim " " r))
      row))

(defun snap-units (tui sup label)
  "SNAP with the unit columns and the unit pids masked."
  (let* ((ids (%unit-ids sup))
         (*redactions* (append (%pid-redactions sup) *redactions*))
         (rows (%settled tui)))
    (format t "~&  ┌ screen: ~a~%" label)
    (loop for row in rows for i from 0
          do (format t "  ~2d│~a│~%" i (%redact-row (%mask-unit-row row ids))))
    (finish-output)
    rows))

(defun %last-row (tui) (car (last (h:tui-screen tui))))

(defun %cells (tui y)
  (aref (getf (h:tui-snapshot tui) :cells) y))

(defun %cell-string (cell)
  (let ((tx (and cell (tp:cell-text cell))))
    (cond ((stringp tx) tx) ((characterp tx) (string tx)) (t ""))))

(defun %inverse-runs (tui y)
  "The texts of the inverse runs on screen row Y."
  (let ((runs '()) (cur '()))
    (flet ((flush () (when cur (push (format nil "~{~a~}" (reverse cur)) runs) (setf cur '()))))
      (loop for cell across (%cells tui y)
            do (if (and cell (tp:cell-style cell) (tp:style-inverse (tp:cell-style cell)))
                   (push (%cell-string cell) cur)
                   (flush)))
      (flush))
    (nreverse runs)))

(defun %foreground-at (tui y x)
  (let* ((row (%cells tui y))
         (cell (and (< x (length row)) (aref row x))))
    (and cell (tp:cell-style cell) (tp:style-foreground (tp:cell-style cell)))))

(defun %row-index (tui needle)
  (position-if (lambda (row) (search needle row)) (h:tui-screen tui)))

(defun %selected-id (tui)
  (let ((row (find-if (lambda (r) (uiop:string-prefix-p "> " r)) (h:tui-screen tui))))
    (and row (>= (length row) 22) (string-trim " " (subseq row 2 22)))))

(defun %select-unit (tui id)
  "Down until ID is selected (scope rows are skipped). → true when it is."
  (loop repeat 10
        for before = (%selected-id tui)
        until (equal id before)
        do (h:tui-press tui :arrow-down)
           (h:tui-wait-until tui (lambda () (let ((now (%selected-id tui))) (and now (not (equal now before)))))
                             :timeout 2))
  (equal id (%selected-id tui)))

(defun %ctrl-p (tui) (h:tui-press tui :p :modifiers '(:ctrl)))

(defun %unit-row (tui id)
  (find-if (lambda (row) (and (>= (length row) 22) (string= id (string-trim " " (subseq row 2 22)))))
           (h:tui-screen tui)))

(defun %start-reviewer (sup gate)
  "A :subagent thread in scope (:agent \"reviewer\"), as the H8a fixture's: two
lines, then it waits until (CAR GATE) or a stop."
  (let ((scope (sup:make-scope sup :agent "reviewer")))
    (sup:start-unit sup :subagent
                    :owner scope
                    :function (lambda (u)
                                (declare (ignore u))
                                (write-line "reviewer: reading src/greet.lisp")
                                (write-line "reviewer: 2 comments so far")
                                (finish-output)
                                (loop until (or (car gate) (sup:unit-stop-requested-p sup:*current-unit*))
                                      do (sleep 0.05))
                                (write-line "reviewer: done")
                                (finish-output)
                                :done))))

(defun call-with-units-tui (name steps fn &key argv)
  "FN gets (tui program backend project gate): a code program on 120 x 30 in a
fresh project NAME with bash allowed (plus ARGV)."
  (let* ((project (make-project name '(("README.md" "# demo"))))
         (config (scratch-dir (format nil "~a-config" name)))
         (backend (make-tui-mock steps))
         (gate (list nil)))
    (unwind-protect
         (with-tui (tui program :cols 120 :rows 30 :argv (append '("--allowed-tools" "bash") argv))
             (project config backend)
           (%wait tui "Message, @file")
           (funcall fn tui program backend project gate))
      (setf (car gate) t))))

(defmacro with-units-tui ((tui program backend project gate &rest keys) (name steps) &body body)
  `(call-with-units-tui ,name ,steps
                        (lambda (,tui ,program ,backend ,project ,gate)
                          (declare (ignorable ,tui ,program ,backend ,project ,gate))
                          ,@body)
                        ,@keys))

(defun %last-line-is (sup id text)
  (lambda () (let ((u (sup:find-unit sup id))) (and u (equal text (code:unit-last-line u))))))

;;; t1 + t2: the panel, then follow and search.

(defun %t1-t2-panel ()
  (stage "t1: the agent starts two background jobs and a dev server; the status line counts them; Ctrl-P opens the panel")
  (with-units-tui (tui program backend project gate)
      ("units-panel" (list (%bg (%job-a)) (%bg (%job-b)) (%bg (%server))
                           "Started two jobs and the dev server."))
    (send-line tui "start the build jobs and the dev server")
    (%wait tui "Started two jobs")
    (%wait-until tui (lambda () (%idle-p program)) "idle")
    (let ((sup (code:code-program-supervisor program)))
      (%wait-until tui (%last-line-is sup "shell-job-3" "GET /api/missing 404") "the server's fourth line")
      (%wait-until tui (%last-line-is sup "shell-job-1" "job-a: waiting for the build lock") "job-a's lines")
      (note "  (the demo adds a subagent thread in scope reviewer/, as the H8a fixture does: a unit with no pid)")
      (%start-reviewer sup gate)
      (%wait-until tui (%last-line-is sup "subagent-1" "reviewer: 2 comments so far") "the reviewer's lines")
      (%wait-until tui (lambda () (search "4 running · " (%last-row tui))) "the status line count")
      (snap-units tui sup "the transcript; the status line says 4 running")
      (show "status-line-text right" (nth-value 1 (code:status-line-text (code:code-program-state program))))
      (show "state-units-running" (code:state-units-running (code:code-program-state program)))
      (%ctrl-p tui)
      (%wait tui "Units · ")
      ;; CPU% needs two samples (+unit-stats-interval+ apart)
      (%wait-until tui (lambda () (let ((row (%unit-row tui "shell-job-1")))
                                    (and row (>= (length row) 62)
                                         (string/= "-" (string-trim " " (subseq row 57 62))))))
                   "the CPU% column" :timeout 15)
      (snap-units tui sup "Ctrl-P: the owner tree (shell/ = the agent's bash, reviewer/ = the subagent scope)")
      (let ((row (%unit-row tui "shell-job-1")))
        (show "shell-job-1's PID / UP / CPU% / RSS hold values (masked above)"
              (and row (mapcar (lambda (r) (not (string= "-" (string-trim " " (subseq row (first r) (second r))))))
                               '((43 49) (50 56) (57 62) (63 69))))))
      (let ((row (%unit-row tui "subagent-1")))
        (show "subagent-1 (a thread): PID / CPU% / RSS"
              (and row (mapcar (lambda (r) (string-trim " " (subseq row (first r) (second r))))
                               '((43 49) (57 62) (63 69))))))
      (let* ((rows (h:tui-screen tui))
             (y (position-if (lambda (r) (uiop:string-prefix-p "> " r)) rows)))
        (show "the selected row (shell-job-1) is one inverse run; shell-job-2's row has none"
              (list (and y (equal (list (string-right-trim " " (nth y rows)))
                                  (mapcar (lambda (r) (string-right-trim " " r)) (%inverse-runs tui y))))
                    (and y (%inverse-runs tui (1+ y))))))
      (show "panel rows = unit-objects (id status kind owner)"
            (mapcar (lambda (o) (list (jget o "id") (jget o "status") (jget o "kind") (jget o "owner")))
                    (code:unit-objects sup)))

      (stage "t2: select the dev server, Enter follows its log; a new line arrives; / search, n, N")
      (%select-unit tui "shell-job-3")
      (h:tui-press tui :enter)
      (%wait tui "Log shell-job-3 (running) · following")
      (%wait tui "GET /api/missing 404")
      (snap-units tui sup "Enter: the log view, following")
      (write-file project ".go" "")
      (prog1
          (and (%wait tui +server-late-line+)
               (progn
                 (snap-units tui sup "following: the server's new line shows without a key")
                 (h:tui-type tui "/")
                 (%wait tui "search> ")
                 (h:tui-type tui "health")
                 (h:tui-press tui :enter)
                 (%wait tui "search \"health\": 1 of 2")
                 (snap-units tui sup "/ health Enter: hit lines marked >, every occurrence of the term inverse")
                 (loop for row in (butlast (h:tui-screen tui) 3) for y from 0
                       for runs = (%inverse-runs tui y)
                       when runs
                         do (note "  row ~2d ~:[ ~;>~] inverse runs: ~s" y (uiop:string-prefix-p ">" row) runs))
                 (show "unit-panel-hits (one per 16-line block of the index)"
                       (length (code:unit-panel-hits (code:code-program-panel program))))
                 (h:tui-type tui "n")
                 (%wait tui "search \"health\": 2 of 2")
                 (show "after n, title" (first (h:tui-screen tui)))
                 (h:tui-type tui "N")
                 (%wait tui "search \"health\": 1 of 2")
                 (show "after N, title" (first (h:tui-screen tui)))
                 (h:tui-type tui "N")
                 (%wait tui "search \"health\": 2 of 2")
                 (show "N again wraps to the last hit, title" (first (h:tui-screen tui)))
                 (h:tui-press tui :escape)
                 (h:tui-wait-until tui (lambda () (not (search "search \"" (first (h:tui-screen tui))))) :timeout 5)
                 (show "Esc clears the search, title" (first (h:tui-screen tui)))
                 (h:tui-press tui :escape)
                 (%wait tui "Units · ")
                 (show "Esc again: the list" (code:unit-panel-mode (code:code-program-panel program)))
                 (%ctrl-p tui)
                 (%wait tui "Started two jobs")
                 (show "Ctrl-P closes it; the transcript is back" (not (code:unit-panel-open-p (code:code-program-panel program))))
                 t))
        (setf (car gate) t)))))

;;; t3: s under the three rule outcomes, and the agent's own unit_control.

(defun %t3-run (label argv)
  "One session: the panel's s on shell-job-1, then the model's unit_control
stop on shell-job-2. → (panel-outcome agent-outcome)."
  (stage (format nil "t3 ~a" label))
  (with-units-tui (tui program backend project gate :argv argv)
      ((format nil "units-control-~(~a~)" (subseq label 0 (position #\Space label)))
       (list (%bg (%job-a)) (%bg (%job-b)) "Started two jobs."
             (list :tool "unit_control" (json-args "unit" "shell-job-2" "action" "stop"))
             "Done."))
    (send-line tui "start two jobs")
    (%wait tui "Started two jobs.")
    (%wait-until tui (lambda () (%idle-p program)) "idle")
    (let ((sup (code:code-program-supervisor program))
          panel agent)
      (%wait-until tui (%last-line-is sup "shell-job-2" "job-b: watching src/ for changes") "job-b's line")
      (%ctrl-p tui)
      (%wait tui "Units · ")
      (%select-unit tui "shell-job-1")
      (h:tui-type tui "s")
      (%wait-until tui (lambda () (let ((text (%screen-text tui)))
                                    (or (search "Unit control" text) (search "Stopped shell-job-1" text)
                                        (search "denied by" text))))
                   "the panel's answer to s")
      (cond ((search "Unit control" (%screen-text tui))
             (setf panel :ask)
             (snap-units tui sup "s: the confirm dialog (Yes selected)")
             (h:tui-press tui :enter)
             (%wait tui "Stopped shell-job-1: killed")
             (%wait-until tui (lambda () (search "killed" (or (%unit-row tui "shell-job-1") ""))) "the killed row")
             (snap-units tui sup "Yes: stopped; the help row has unit_control's text"))
            ((search "denied by" (%screen-text tui))
             (setf panel :deny)
             (snap-units tui sup "s: refused, the rule named on the help row; no dialog"))
            (t
             (setf panel :allow)
             (%wait-until tui (lambda () (search "killed" (or (%unit-row tui "shell-job-1") ""))) "the killed row")
             (snap-units tui sup "s: stopped at once, no dialog")))
      (show "shell-job-1 state" (sup:unit-state (sup:find-unit sup "shell-job-1")))
      (%ctrl-p tui)
      (%wait-until tui (lambda () (not (code:unit-panel-open-p (code:code-program-panel program)))) "the panel to close")
      (send-line tui "stop shell-job-2 too")
      (%wait-until tui (lambda () (let ((text (%screen-text tui))) (or (search "Allow once" text) (search "Done." text))))
                   "the agent's unit_control")
      (let ((asked (and (search "Allow once" (%screen-text tui)) t)))
        (when asked
          (snap-units tui sup "the agent's unit_control asks: the approval dialog")
          (h:tui-press tui :enter))
        (%wait tui "Done.")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (snap-units tui sup "the transcript after the agent's call")
        (let* ((text (%turns-text (car (last (tui-mock-requests backend)))))
               (result (string-trim '(#\Newline) (subseq text (or (search "[tool_result" text :from-end t) 0)))))
          (show "the tool result the model got" (clip result 300))
          (setf agent (cond ((search "Stopped shell-job-2" result) (if asked :ask :allow))
                            (t :deny)))))
      (show "shell-job-2 state" (sup:unit-state (sup:find-unit sup "shell-job-2")))
      (show "(panel agent)" (list panel agent))
      (list panel agent))))

(defun %t3-parity ()
  (let ((rows (list (cons "default rules (unit_control asks)" (%t3-run "default rules (unit_control asks)" '()))
                    (cons "allow unit_control(stop:shell-job-*)"
                          (%t3-run "allow rule: --allowed-tools 'unit_control(stop:shell-job-*)'"
                                   '("--allowed-tools" "unit_control(stop:shell-job-*)")))
                    (cons "deny unit_control(stop:shell-job-*)"
                          (%t3-run "deny rule: --disallowed-tools 'unit_control(stop:shell-job-*)'"
                                   '("--disallowed-tools" "unit_control(stop:shell-job-*)"))))))
    (stage "t3: permission parity, the panel's s and the agent's unit_control under the same rules")
    (loop for (label panel agent) in rows
          do (note "  ~38a panel ~6a agent ~6a ~:[DIFFERENT~;same~]" label panel agent (eq panel agent)))
    (%check "t3" (equal '((:ask :ask) (:allow :allow) (:deny :deny)) (mapcar #'cdr rows)))))

;;; t4: a failing unit.

(defun %t4-failure ()
  (stage "t4: a background build fails: the notice in the transcript, then the panel and its log")
  (with-units-tui (tui program backend project gate)
      ("units-fail" (list (%bg (%job-a)) (%bg (%failing-build)) "Started the watcher and the build."))
    (send-line tui "run the build in the background")
    (%wait tui "Started the watcher")
    (let ((sup (code:code-program-supervisor program)))
      (prog1 (%wait tui "unit shell-job-2 (shell-job) failed with exit code 3; /ps to inspect")
        (%wait-until tui (lambda () (%idle-p program)) "idle")
        (%wait-until tui (lambda () (search "1 running · " (%last-row tui))) "the count to drop")
        (snap-units tui sup "the failure notice; the status line count dropped to 1")
        (send-line tui "/ps")
        (%wait tui "Units · ")
        (%select-unit tui "shell-job-2")
        (snap-units tui sup "/ps: shell-job-2 failed 3, the stderr line as its last line")
        (h:tui-press tui :enter)
        (%wait tui "Log shell-job-2 (failed 3)")
        (%wait tui "undefined symbol")
        (snap-units tui sup "its log")
        (let ((y (%row-index tui "undefined symbol")))
          (when y
            (show "the stderr line's foreground (palette colour 1, red)"
                  (%foreground-at tui y (search "greet.c:3" (nth y (h:tui-screen tui)))))))))))

;;; t5: --rpc units/*.

(defun %mask-json (object)
  (cond ((hash-table-p object)
         (let ((h (make-hash-table :test #'equal)))
           (maphash (lambda (k v)
                      (setf (gethash k h)
                            (if (and (member k '("pid" "started" "uptime_s" "time" "cpu_percent" "cpu_time_s"
                                                 "rss_bytes" "session_id")
                                             :test #'equal)
                                     (not (eq v :null)))
                                (format nil "<~a>" k)
                                (%mask-json v))))
                    object)
           h))
        ((and (vectorp object) (not (stringp object))) (map 'vector #'%mask-json object))
        (t object)))

(defun %masked-line (line &optional (width 300))
  (clip (handler-case (json:encode (%mask-json (json:decode line))) (error () line)) width))

(defun %rpc-fault* (rpc method &rest kv)
  "→ (code message data-hash) of METHOD's error response, or :no-error."
  (handler-case (progn (apply #'%rpc-call rpc method kv) :no-error)
    (rpc:rpc-error (e) (list (rpc:rpc-error-code e) (rpc:rpc-error-message e) (rpc:rpc-error-data e)))))

(defun %show-fault (label fault)
  (if (eq fault :no-error)
      (show label "no error (!)")
      (destructuring-bind (code message data) fault
        (show label (format nil "~d ~a~@[ ~a~]" code (clip message 140)
                            (and (hash-table-p data) (json:encode (%mask-json data))))))))

(defun %unit-line (o)
  (format nil "~12a ~9a ~9a ~6a pid ~a  last ~s" (jget o "id") (jget o "kind") (jget o "status") (jget o "owner")
          (if (eq :null (jget o "pid")) "null" "<pid>")
          (let ((l (jget o "last_line"))) (if (eq l :null) nil l))))

(defun %unit-event-line (e)
  (format nil "seq ~3d ~a ~12a ~a → ~a~@[ ~a~]" (code:event-value e "seq") (code:event-value e "event")
          (code:event-value e "unit_id")
          (let ((f (code:event-value e "from"))) (if (or (null f) (eq f :null)) "null" f))
          (let ((f (code:event-value e "to"))) (if (or (null f) (eq f :null)) "null" f))
          (let ((d (code:event-value e "detail")))
            (and (hash-table-p d) (plusp (hash-table-count d)) (json:encode d)))))

(defun %t5-rpc ()
  (stage "t5: --rpc units/* (serve-rpc on in-memory pipes; bash allowed, unit_control(restart:*) denied)")
  (let* ((project (make-project "units-rpc" '(("README.md" "# demo"))))
         (config (scratch-dir "units-rpc-config"))
         (backend (make-tui-mock (list (%bg (%job-a)) (%bg (%server))
                                       (list :tool "bash" (json-args "command" "exit 3"))
                                       "Started two units; the check failed.")))
         (settings (code:make-settings :model "mock"
                                       :allow (list (code:parse-permission-rule "bash" :allow))
                                       :deny (list (code:parse-permission-rule "unit_control(restart:*)" :deny))))
         (to-server (make-string-output-stream))
         (to-client (make-string-output-stream))
         (cell (list :running))
         (data (data-dir))
         (ok nil))
    (multiple-value-bind (server-in client-out) (stdio:make-stdio-pipe)
      (multiple-value-bind (client-in server-out) (stdio:make-stdio-pipe)
        (let* ((server-tx (stdio:make-stdio-rpc-transport
                           :input server-in :output (make-broadcast-stream server-out to-client)))
               (client-tx (stdio:make-stdio-rpc-transport
                           :input client-in :output (make-broadcast-stream client-out to-server)))
               (thread (bt:make-thread
                        (lambda ()
                          (setf (car cell)
                                (handler-case
                                    (code:serve-rpc server-tx :cwd project :settings settings
                                                              :providers (%mock-providers backend)
                                                              :user-config-dir config :data-dir data
                                                              :getenv (lambda (n) (declare (ignore n)) nil))
                                  (error (e) (list :error (princ-to-string e))))))
                        :name "rpc server"))
               (rpc (code:connect-rpc-client :transport client-tx)))
          (unwind-protect
               (let* ((init (%rpc-call rpc "initialize"))
                      (id (code:event-value (%rpc-call rpc "session/new") "session_id")))
                 (show "initialize: the units/* methods"
                       (remove-if-not (lambda (m) (uiop:string-prefix-p "units/" m))
                                      (coerce (code:event-value init "methods") 'list)))
                 (show "initialize: \"unit\" is an event type"
                       (and (find "unit" (code:event-value init "events") :test #'equal) t))
                 (%rpc-call rpc "turn/start" "session_id" id "prompt" "start the job and the server, then run the check")
                 (let ((result (%wait-for (lambda () (first (code:rpc-client-events rpc :type "result" :session-id id)))
                                          "the turn's result")))
                   (show "turn result" (code:event-value result "result")))
                 (%wait-for (lambda () (search "404" (princ-to-string
                                                      (jget (first (coerce (jget (%rpc-call rpc "units/read" "session_id" id "unit" "shell-job-2" "tail" 1) "records") 'list)) "text"))))
                            "the server's fourth line")

                 (stage "t5a: units/list (the panel's rows: unit-objects)")
                 (let ((r (%rpc-call rpc "units/list" "session_id" id)))
                   (dolist (o (coerce (jget r "units") 'list)) (note "  ~a" (%unit-line o)))
                   (show "seq, running" (list (jget r "seq") (jget r "running")))
                   (show "keys of one unit" (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys))
                                                                       (first (coerce (jget r "units") 'list)))
                                                 (nreverse keys))))
                 (show "units/list state failed"
                       (map 'list (lambda (o) (jget o "id")) (jget (%rpc-call rpc "units/list" "session_id" id "state" "failed") "units")))
                 (let ((job (find "shell-job-1" (coerce (jget (%rpc-call rpc "units/list" "session_id" id "resources" t) "units") 'list)
                                  :key (lambda (o) (jget o "id")) :test #'equal)))
                   (show "units/list resources true: shell-job-1 rss_bytes integer, cpu_time_s number"
                         (list (integerp (jget job "rss_bytes")) (realp (jget job "cpu_time_s")))))

                 (stage "t5b: units/subscribe since 0: the replay, then live events, each seq once")
                 (let* ((sub (%rpc-call rpc "units/subscribe" "session_id" id "since" 0))
                        (seq (jget sub "seq")))
                   (show "units/subscribe → subscription, seq, complete"
                         (list (jget sub "subscription") seq (jget sub "complete")))
                   (%wait-for (lambda () (>= (length (code:rpc-client-events rpc :type "unit" :session-id id)) seq))
                              "the replay")
                   (dolist (e (code:rpc-client-events rpc :type "unit" :session-id id))
                     (note "  replay ~a" (%unit-event-line e)))

                   (stage "t5c: units/control: an ask is -32017, the client confirms with confirmed: true")
                   (%show-fault "units/control stop shell-job-1" (%rpc-fault* rpc "units/control" "session_id" id
                                                                              "unit" "shell-job-1" "action" "stop"))
                   (show "shell-job-1 after the -32017"
                         (jget (find "shell-job-1" (coerce (jget (%rpc-call rpc "units/list" "session_id" id) "units") 'list)
                                     :key (lambda (o) (jget o "id")) :test #'equal)
                               "status"))
                   (let ((r (%rpc-call rpc "units/control" "session_id" id "unit" "shell-job-1" "action" "stop" "confirmed" t)))
                     (show "units/control stop shell-job-1 confirmed: true → result, unit.status"
                           (list (jget r "result") (jget r "unit" "status"))))
                   (%wait-for (lambda () (find-if (lambda (e) (and (equal "shell-job-1" (code:event-value e "unit_id"))
                                                                   (equal "killed" (code:event-value e "to"))))
                                                  (code:rpc-client-events rpc :type "unit" :session-id id)))
                              "the live killed event")
                   (sleep 0.3)
                   (dolist (e (code:rpc-client-events rpc :type "unit" :session-id id))
                     (when (> (code:event-value e "seq") seq)
                       (note "  live   ~a" (%unit-event-line e))))
                   (let ((seqs (mapcar (lambda (e) (code:event-value e "seq"))
                                       (code:rpc-client-events rpc :type "unit" :session-id id))))
                     (show "seqs received" seqs)
                     (show "1..N with no gap and no duplicate" (equal seqs (loop for i from 1 to (length seqs) collect i)))
                     (setf ok (equal seqs (loop for i from 1 to (length seqs) collect i)))))

                 (stage "t5d: units/read and units/search")
                 (let ((r (%rpc-call rpc "units/read" "session_id" id "unit" "shell-job-2" "limit" 4)))
                   (show "units/read shell-job-2 limit 4 → status, next_line, last_line, ended"
                         (list (jget r "status") (jget r "next_line") (jget r "last_line") (jget r "ended")))
                   (dolist (rec (coerce (jget r "records") 'list))
                     (note "  ~3d ~6a ~a" (jget rec "line") (jget rec "stream") (jget rec "text"))))
                 (show "units/read tail 1"
                       (map 'list (lambda (rec) (jget rec "text"))
                            (jget (%rpc-call rpc "units/read" "session_id" id "unit" "shell-job-2" "tail" 1) "records")))
                 (let ((r (%rpc-call rpc "units/search" "session_id" id "query" "health")))
                   (show "units/search health → total" (jget r "total"))
                   (dolist (hit (coerce (jget r "hits") 'list))
                     (note "  ~a line ~d ~s" (jget hit "ref") (jget hit "line") (jget hit "text"))))
                 (show "units/search \"\" kinds [exit] → total (end records of finished units)"
                       (jget (%rpc-call rpc "units/search" "session_id" id "query" "" "kinds" #("exit")) "total"))

                 (stage "t5e: errors carry the condition name in data")
                 (%show-fault "units/read shell-job-9" (%rpc-fault* rpc "units/read" "session_id" id "unit" "shell-job-9"))
                 (%show-fault "units/control restart shell-job-2 confirmed (deny rule)"
                              (%rpc-fault* rpc "units/control" "session_id" id "unit" "shell-job-2"
                                           "action" "restart" "confirmed" t))
                 (%show-fault "units/control input to shell-job-3 (finished)"
                              (%rpc-fault* rpc "units/control" "session_id" id "unit" "shell-job-3"
                                           "action" "input" "input" "x" "confirmed" t))
                 (show "units/unsubscribe sub-1"
                       (json:encode (%rpc-call rpc "units/unsubscribe" "session_id" id "subscription" "sub-1")))
                 (%show-fault "units/unsubscribe sub-1 again" (%rpc-fault* rpc "units/unsubscribe" "session_id" id
                                                                           "subscription" "sub-1"))
                 (%show-fault "units/list state zzz" (%rpc-fault* rpc "units/list" "session_id" id "state" "zzz"))
                 (%show-fault "units/control kill" (%rpc-fault* rpc "units/control" "session_id" id
                                                                "unit" "shell-job-2" "action" "kill"))
                 (%rpc-call rpc "session/close" "session_id" id))
            (code:close-rpc-client rpc)
            (close client-out)
            (%wait-for (lambda () (not (bt:thread-alive-p thread))) "the server thread to end" :timeout 30))
          (stage "t5f: the wire (units/* only; JSON masked)")
          (let* ((sent (remove-if-not (lambda (l) (search "\"units/" l)) (%wire-lines to-server)))
                 (ids (mapcar (lambda (l) (jget (json:decode l) "id")) sent))
                 (received (%wire-lines to-client))
                 (unit-notes 0))
            (note "  client → server (~d units/* requests):" (length sent))
            (dolist (l sent) (note "    ~a" (%masked-line l)))
            (note "  server → client (the responses to those, and the first 3 unit notifications):")
            (dolist (l received)
              (let ((o (ignore-errors (json:decode l))))
                (cond ((and o (member (jget o "id") ids :test #'equal))
                       (note "    ~a" (%masked-line l)))
                      ((and o (equal "unit" (jget o "params" "type")))
                       (when (<= (incf unit-notes) 3) (note "    ~a" (%masked-line l)))))))
            (note "    … ~d unit notifications in all" unit-notes))
          (show "serve-rpc returned (EOF)" (car cell))
          (%check "t5" ok))))))

;;; t6: the crash sweep.

(defun %pid-alive-p (pid)
  (if (uiop:os-windows-p)
      (let ((out (uiop:run-program (list "tasklist" "/FI" (format nil "PID eq ~D" pid) "/NH" "/FO" "CSV")
                                   :output :string :ignore-error-status t)))
        (and (search (format nil "\"~D\"" pid) out) t))
      (let ((out (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (uiop:run-program (list "ps" "-o" "stat=" "-p" (princ-to-string pid))
                                                :output :string :ignore-error-status t))))
        (and (plusp (length out)) (char/= (char out 0) #\Z)))))

(defun %os-kill (pid)
  "SIGKILL (taskkill /F) PID from outside, as a crash or the OOM killer would."
  (uiop:run-program (if (uiop:os-windows-p)
                        (list "taskkill" "/F" "/PID" (princ-to-string pid))
                        (list "kill" "-9" (princ-to-string pid)))
                    :ignore-error-status t :output nil :error-output nil))

(defun %unit-files (state)
  (directory (merge-pathnames "*.unit" (sup:unit-file-directory state))))

(defun %t6-sweep ()
  (stage "t6: a cl-stack-code --rpc child starts two jobs and is SIGKILLed; the next session in the project sweeps them")
  (let* ((project (make-project "units-crash" `((".cl-stack-code/settings.json"
                                                 ,(format nil "{\"permissions\": {\"allow\": [\"bash\"]}}~%")))))
         (config (scratch-dir "units-crash-config"))
         (data (merge-pathnames "data/" config))
         (state (code:units-directory project :data-dir data))
         (argv (rpc-child-argv project config (list (%bg (%job-a)) (%bg (%job-b)) "Started two jobs.")))
         (rpc (code:connect-rpc-client :command argv))
         (child-pid nil)
         (pids '()))
    (note "  project .cl-stack-code/settings.json: {\"permissions\": {\"allow\": [\"bash\"]}}")
    (unwind-protect
         (progn
           (code:rpc-client-call rpc "initialize" nil :timeout 600)
           (let ((id (code:event-value (%rpc-call rpc "session/new") "session_id")))
             (%rpc-call rpc "turn/start" "session_id" id "prompt" "start two jobs")
             (code:rpc-client-wait-event rpc (lambda (e) (equal "result" (code:event-type e))) :timeout *rpc-timeout*)
             (%wait-for (lambda () (= 2 (length (%unit-files state)))) "the two unit files")
             (stage "t6a: before the crash")
             (let ((units (coerce (jget (%rpc-call rpc "units/list" "session_id" id) "units") 'list)))
               (dolist (o units) (note "  ~a" (%unit-line o)))
               (setf pids (mapcar (lambda (o) (cons (jget o "id") (jget o "pid"))) units))))
           (let ((plist (sup:read-unit-file (first (%unit-files state)))))
             (setf child-pid (getf plist :supervisor-pid))
             (show "unit files in <data>/units/<project key>/units/ (one per process unit)"
                   (sort (mapcar (lambda (p) (let* ((n (file-namestring p)) (k (search "--" n)))
                                               (if k (concatenate 'string "<supervisor>" (subseq n k)) n)))
                                 (%unit-files state))
                         #'string<))
             (show "a unit file's keys" (loop for k in plist by #'cddr collect k))
             (show "the unit file's supervisor pid is the child, not this image"
                   (and (integerp child-pid) (/= child-pid (sup:current-pid)))))
           (show "units alive" (mapcar (lambda (p) (list (car p) (%pid-alive-p (cdr p)))) pids))
           (stage "t6b: SIGKILL the child (taskkill /F on Windows): no exit hook, no shutdown")
           (%os-kill child-pid)
           (%wait-for (lambda () (not (%pid-alive-p child-pid))) "the child to die" :timeout 30)
           (show "child alive" (%pid-alive-p child-pid))
           (sleep 0.5)
           (show "units alive (orphans)" (mapcar (lambda (p) (list (car p) (%pid-alive-p (cdr p)))) pids))
           (show "unit files left behind" (length (%unit-files state)))
           (ignore-errors (code:close-rpc-client rpc :timeout 5))
           (stage "t6c: the next start in the same project: a new SDK session (make-code-supervisor sweeps first)")
           (code:with-client (client :cwd project :settings (code:make-settings :model "mock")
                                     :providers (%mock-providers (make-tui-mock '()))
                                     :user-config-dir config :data-dir data
                                     :getenv (lambda (n) (declare (ignore n)) nil))
             (let* ((s (code:client-new-session client))
                    (sup (code:code-session-supervisor s))
                    (report (sup:supervisor-sweep-report sup)))
               (dolist (r (sort (copy-list report) #'string< :key #'sup:sweep-record-unit-id))
                 (note "  sweep: ~12a ~s  killed ~d pid~:p (the sh and its sleep)" (sup:sweep-record-unit-id r) (sup:sweep-record-action r)
                       (length (sup:sweep-record-killed-pids r))))
               (show "the new session's own units" (code:client-units s))
               (%wait-for (lambda () (notany (lambda (p) (%pid-alive-p (cdr p))) pids)) "the orphans to die" :timeout 10)
               (stage "t6d: after the sweep")
               (show "units alive" (mapcar (lambda (p) (list (car p) (%pid-alive-p (cdr p)))) pids))
               (show "unit files left" (length (%unit-files state)))
               (%check "t6" (and (= 2 (length report))
                                 (every (lambda (r) (eq :killed (sup:sweep-record-action r))) report)
                                 (notany (lambda (p) (%pid-alive-p (cdr p))) pids)
                                 (zerop (length (%unit-files state))))))))
      (ignore-errors (code:close-rpc-client rpc :timeout 5))
      (dolist (p (cons child-pid (mapcar #'cdr pids)))
        (when (integerp p) (ignore-errors (%os-kill p)))))))

(defun demo-units ()
  (banner "Demo t: the session's units (cl-stack-code 0.8.0, H8d): the process panel, follow and search, unit control, --rpc units/*, the crash sweep")
  (let ((results '()))
    (flet ((run-step (name thunk)
             (let ((ok (handler-case (funcall thunk)
                         (error (c)
                           (note "  !! ~a raised ~s: ~a" name (type-of c) (clip (princ-to-string c) 400))
                           nil))))
               (push (cons name (and ok t)) results))))
      (run-step "t1-t2" #'%t1-t2-panel)
      (run-step "t3" #'%t3-parity)
      (run-step "t4" #'%t4-failure)
      (run-step "t5" #'%t5-rpc)
      (run-step "t6" #'%t6-sweep))
    (stage "t: steps that reached their last screen or check")
    (loop for (name . ok) in (reverse results) do (note "  ~6a ~:[NO~;yes~]" name ok))
    (unless (every #'cdr results)
      (error "units steps did not finish: ~{~a~^, ~}" (mapcar #'car (remove-if #'cdr (reverse results)))))))
