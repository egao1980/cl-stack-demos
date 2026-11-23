(in-package #:harness-demos-zed-sample)

;;; The headless rehearsal of the Zed demo. harness/acp's scripted client
;;; (DEMO-CLIENT: real files, terminals that run their command, permission
;;; answered with a restart) plays Zed. It spawns cl-stack-code-logged.sh
;;; --acp --model openai in ZED_SAMPLE_PROJECT (a cl-stack-sample clone at
;;; demo/zed-seeded-bug, synced), sends one prompt, approves every permission
;;; request (allow_once) and prints every client call and session/update
;;; with the seconds since the prompt. Then an independent `cl-repo test`
;;; in the clone gives the verdict.
;;;
;;; Env: ZED_SAMPLE_PROJECT (required), ZED_SAMPLE_WIRE_LOG (default
;;; rehearsal/wire-log.txt here), ZED_SAMPLE_PROMPT, ZED_SAMPLE_LAUNCHER
;;; (CL_STACK_CODE_LAUNCHER for the wrapper). The model comes from
;;; OPENAI_BASE_URL / OPENAI_API_KEY / OPENAI_MODEL in the environment.

(defvar *here* (asdf:system-source-directory "harness-demos-zed-sample"))

(defparameter +default-prompt+ "The tests fail. Find and fix the bug.")

(defvar *failures* 0)
(defvar *t0* 0)

(defun check (label ok)
  (format t "  [~:[FAIL~;ok~]] ~A~%" ok label)
  (unless ok (incf *failures*))
  ok)

(defun %secs () (/ (- (get-internal-real-time) *t0*) (float internal-time-units-per-second)))

(defun %env (name &optional default)
  (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))

(defun %clip (s n)
  (let ((s (coerce (substitute #\Space #\Newline (princ-to-string s)) (quote (simple-array character (*))))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 n) "…") s)))

;;; The client: DEMO-CLIENT with time stamps, chunk texts and tool results.

(defclass rehearsal-client (demo:demo-client)
  ((chunks :initform '() :accessor client-chunks
           :documentation "(seconds . text) of each agent_message_chunk, newest first.")
   (tools :initform '() :accessor client-tools
          :documentation "(id title kind) of each tool_call, newest first.")))

(defun %content-text (u)
  (and (slot-boundp u 'acp:content)
       (format nil "~{~A~^ | ~}"
               (map 'list (lambda (c)
                            (typecase c
                              (acp:diff (format nil "diff ~A" (acp:path c)))
                              (acp:terminal (format nil "terminal ~A" (acp:terminal-id c)))
                              (t (let ((inner (and (slot-exists-p c 'acp:content)
                                                   (slot-boundp c 'acp:content)
                                                   (acp:content c))))
                                   (if (typep inner 'acp:text-content)
                                       (acp:text inner)
                                       (type-of c))))))
                    (acp:content u)))))

(defmethod acp:acp-session-update ((client rehearsal-client) notification)
  (let ((u (acp:update notification))
        (s (%secs)))
    (typecase u
      (acp:update-agent-message-chunk
       (let ((text (acp:text (acp:content u))))
         (push (cons s text) (client-chunks client))
         (demo::%event client "[~7,1F] message chunk ~S" s (%clip text 200))))
      (acp:update-agent-thought-chunk
       (demo::%event client "[~7,1F] thought chunk ~S" s (%clip (acp:text (acp:content u)) 120)))
      (acp:update-tool-call
       (push (list (acp:tool-call-id u) (acp:title u) (acp:kind u)) (client-tools client))
       (demo::%event client "[~7,1F] ~A" s (demo::%describe-update u)))
      (acp:update-tool-call-update
       (demo::%event client "[~7,1F] tool_call_update ~A ~A~@[ → ~A~]" s
                     (acp:tool-call-id u) (acp:status u)
                     (let ((text (%content-text u))) (and text (%clip text 240)))))
      (t (demo::%event client "[~7,1F] ~A" s (demo::%describe-update u))))))

(defun %client-caps ()
  (acp:parse-acp-object 'acp:client-capabilities
                        (json-protocol:decode
                         "{\"fs\":{\"readTextFile\":true,\"writeTextFile\":true},\"terminal\":true}")))

(defun %has (client prefix)
  (some (lambda (e) (search prefix e)) (demo::client-events client)))

(defun %tool-titles (client)
  (mapcar #'second (reverse (client-tools client))))

(defun %titled (client &rest prefixes)
  (some (lambda (title) (some (lambda (p) (uiop:string-prefix-p p title)) prefixes))
        (%tool-titles client)))

(defun %independent-test (dir)
  (multiple-value-bind (out err code)
      (uiop:run-program '("cl-repo" "test") :directory dir :output :string
                                             :error-output :output :ignore-error-status t)
    (declare (ignore err))
    (let ((lines (uiop:split-string (string-right-trim '(#\Newline) out) :separator '(#\Newline))))
      (format t "~%  independent cl-repo test (exit ~A), last lines:~%~{    ~A~%~}"
              code (last lines 6)))
    (zerop code)))

(defun rehearse ()
  (let* ((*failures* 0)
         (project (uiop:ensure-directory-pathname
                   (or (%env "ZED_SAMPLE_PROJECT") (error "ZED_SAMPLE_PROJECT is required"))))
         (dir (string-right-trim "/" (uiop:native-namestring project)))
         (wire-log (or (%env "ZED_SAMPLE_WIRE_LOG")
                       (uiop:native-namestring (merge-pathnames "rehearsal/wire-log.txt" *here*))))
         (prompt (%env "ZED_SAMPLE_PROMPT" +default-prompt+))
         (client (make-instance 'rehearsal-client :permission 'acp:allow-once :redact dir))
         (stop nil))
    (when (probe-file wire-log) (delete-file wire-log))
    (format t "harness-demos-zed-sample 0.1.0: acp-protocol ~A, acp-backend-stdio ~A~%"
            (asdf:component-version (asdf:find-system "acp-protocol"))
            (asdf:component-version (asdf:find-system "acp-backend-stdio")))
    (format t "model ~A~%prompt ~S~%~72,,,'-A~%" (%env "OPENAI_MODEL") prompt "")
    (multiple-value-bind (connection agent)
        (stdio:spawn-acp-agent
         (list (uiop:native-namestring (merge-pathnames "cl-stack-code-logged.sh" *here*))
               "--acp" "--model" "openai")
         :client client :directory project
         :env (append (list (cons "ZED_SAMPLE_WIRE_LOG" wire-log)
                            (cons "ZED_SAMPLE_REDACT" dir))
                      (let ((l (%env "ZED_SAMPLE_LAUNCHER")))
                        (and l (list (cons "CL_STACK_CODE_LAUNCHER" l))))))
      (unwind-protect
           (let* ((started (get-internal-real-time))
                  (init (acp:acp-initialize
                         agent (acp:make-acp 'acp:initialize-request
                                             :protocol-version 1
                                             :client-capabilities (%client-caps)
                                             :client-info (acp:make-acp 'acp:implementation
                                                                        :name "harness-demos-zed-sample rehearsal"
                                                                        :version "0.1.0"))))
                  (info (acp:agent-info init)))
             (format t "  initialize → ~A ~A in ~,2F s~%" (acp:name info) (acp:version info)
                     (/ (- (get-internal-real-time) started) (float internal-time-units-per-second)))
             (let ((sid (acp:session-id
                         (acp:acp-new-session agent (acp:make-acp 'acp:new-session-request
                                                                  :cwd dir :mcp-servers (vector))))))
               (format t "  session/new → ~A~%" sid)
               (setf *t0* (get-internal-real-time))
               ;; An error answer (e.g. the model server dropping the
               ;; connection: "incomplete HTTP response") is reported, and
               ;; the checks still run on what happened.
               (handler-case
                   (let ((response (acp:acp-prompt
                                    agent (acp:make-acp 'acp:prompt-request
                                                        :session-id sid
                                                        :prompt (vector (acp:make-acp 'acp:text-content
                                                                                      :text prompt))))))
                     (setf stop (acp:stop-reason response))
                     (format t "  session/prompt → stopReason ~A after ~,1F s~%" stop (%secs)))
                 (error (e)
                   (format t "  session/prompt → error after ~,1F s: ~A~%" (%secs) e)))))
        (format t "  agent exit code: ~A~%" (stdio:close-acp-connection connection :timeout 10))))
    (let* ((chunks (reverse (client-chunks client)))
           (distinct-times (remove-duplicates (mapcar (lambda (c) (round (car c))) chunks))))
      (format t "~%Tool calls (~D):~%~{  ~A~%~}" (length (client-tools client)) (%tool-titles client))
      (format t "Message chunks: ~D, at ~D distinct seconds~%" (length chunks) (length distinct-times))
      (format t "~%Checks:~%")
      (check "stopReason end_turn" (equal stop "end_turn"))
      (check "files read through the client (fs/read_text_file)" (%has client "fs/read_text_file"))
      (check "lisp tools used (source-* / edit-* / repl-*)"
             (%titled client "source-" "edit-" "repl-" "debug-"))
      (check "the fix made with a lisp structural edit (edit-*)" (%titled client "edit-"))
      (check "session context shown: AGENTS.md and the duration-units skill"
             (%titled client "Context: AGENTS.md · skills: duration-units"))
      (check "the **/*.lisp rule loaded lazily (Rule loaded: .claude/rules/lisp.md)"
             (%titled client "Rule loaded: .claude/rules/lisp.md"))
      (check "the skill tool used" (%titled client "skill" "Skill"))
      (check "the tests ran in a client terminal (terminal/create)" (%has client "terminal/create"))
      (check "message text streamed (chunks at 3+ distinct seconds)" (>= (length distinct-times) 3))
      (let ((asked (count-if (lambda (e) (search "session/request_permission" e))
                             (demo::client-events client))))
        (check (format nil "permission prompts answered (~D session/request_permission)" asked)
               (plusp asked)))
      (check "independent cl-repo test passes" (%independent-test dir)))
    (let ((lines (with-open-file (in wire-log :if-does-not-exist nil)
                   (if in (loop while (read-line in nil) count t) 0))))
      (format t "~%Wire log: ~D messages~%" lines))
    (format t "~%~72,,,'=A~%Checks failed: ~D~%" "" *failures*)
    (finish-output)
    (zerop *failures*)))
