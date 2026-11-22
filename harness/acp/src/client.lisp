(in-package #:harness-demos-acp)

;;; The headless scripted ACP client (the editor's side, for the wire test).
;;; Files are the real files of a project directory; terminals run their
;;; command to completion with process-protocol (at terminal/create) and
;;; keep the output; permission is answered by PERMISSION (a restart name of
;;; acp-permission-required: acp:allow-once, acp:reject-once, …). Every
;;; client-side call and session/update is printed as it arrives (the
;;; client's GFs run on the transport's reader thread, in arrival order).

(defclass demo-client (acp:acp-client)
  ((lock :initform (bt2:make-lock :name "demo client") :reader %client-lock)
   (permission :initarg :permission :initform 'acp:allow-once :reader client-permission)
   (out :initarg :out :initform *standard-output* :reader client-out)
   (terminals :initform (make-hash-table :test 'equal) :reader %terminals
              :documentation "terminal id → (exit-code . output)")
   (redact :initarg :redact :initform nil :reader client-redact)
   (events :initform '() :accessor %events :documentation "Newest first.")))

(defun %event (client fmt &rest args)
  (let ((line (apply #'format nil fmt args)))
    (when (client-redact client)
      (setf line (%replace-all line (client-redact client) "<project>")))
    (bt2:with-lock-held ((%client-lock client))
      (push line (%events client))
      (format (client-out client) "  client: ~A~%" line)
      (finish-output (client-out client)))))

(defun client-events (client)
  (bt2:with-lock-held ((%client-lock client)) (reverse (%events client))))

(defun %describe-update (u)
  (typecase u
    (acp:update-agent-message-chunk
     (format nil "message chunk \"~A\"" (acp:text (acp:content u))))
    (acp:update-plan
     (format nil "plan [~{~A~^ | ~}]"
             (map 'list (lambda (e) (format nil "~A: ~A" (acp:status e) (acp:content e)))
                  (acp:entries u))))
    (acp:update-tool-call
     (format nil "tool_call ~A \"~A\" kind ~A status ~A~@[ content ~{~A~^, ~}~]"
             (acp:tool-call-id u) (acp:title u) (acp:kind u) (acp:status u)
             (and (slot-boundp u 'acp:content)
                  (map 'list (lambda (c)
                               (typecase c
                                 (acp:diff (format nil "diff of ~A (~D → ~D chars)"
                                                   (acp:path c) (length (acp:old-text c))
                                                   (length (acp:new-text c))))
                                 (acp:terminal (format nil "terminal ~A" (acp:terminal-id c)))
                                 (t (type-of c))))
                       (acp:content u)))))
    (acp:update-tool-call-update
     (format nil "tool_call_update ~A status ~A" (acp:tool-call-id u) (acp:status u)))
    (t (format nil "~A" (acp:session-update u)))))

(defmethod acp:acp-session-update ((client demo-client) notification)
  (%event client "session/update ~A" (%describe-update (acp:update notification))))

(defmethod acp:acp-request-permission :around ((client demo-client) request)
  (%event client "session/request_permission \"~A\" options [~{~A~^, ~}] → ~(~A~)"
          (acp:title (acp:tool-call request))
          (map 'list (lambda (o) (format nil "~A (~A)" (acp:option-id o) (acp:kind o)))
               (acp:options request))
          (client-permission client))
  (handler-bind ((acp:acp-permission-required
                   (lambda (c)
                     (let ((r (find-restart (client-permission client) c)))
                       (when r (invoke-restart r))))))
    (call-next-method)))

(defmethod acp:acp-read-text-file ((client demo-client) request)
  (let ((path (acp:path request)))
    (unless (probe-file path)
      (error 'acp:acp-resource-not-found :message (format nil "No such file: ~A" path)))
    (let ((text (uiop:read-file-string path :external-format :utf-8)))
      (%event client "fs/read_text_file ~A → ~D chars" path (length text))
      (acp:make-acp 'acp:read-text-file-response :content text))))

(defmethod acp:acp-write-text-file ((client demo-client) request)
  (let ((path (acp:path request)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string (acp:content request) out))
    (%event client "fs/write_text_file ~A ← ~D chars" path (length (acp:content request)))
    (acp:make-acp 'acp:write-text-file-response)))

(defmethod acp:acp-create-terminal ((client demo-client) request)
  (let* ((argv (cons (acp:command request)
                     (and (slot-boundp request 'acp:args) (coerce (acp:args request) 'list))))
         (id (format nil "term-~D" (1+ (hash-table-count (%terminals client))))))
    (multiple-value-bind (code out err)
        (process-protocol:run argv :output :string :error-output :string
                                   :directory (acp:cwd request) :timeout 120)
      (setf (gethash id (%terminals client)) (cons code (concatenate 'string out err)))
      (%event client "terminal/create ~{~A~^ ~} in ~A → ~A (exit ~A)"
              argv (acp:cwd request) id code)
      (acp:make-acp 'acp:create-terminal-response :terminal-id id))))

(defun %terminal (client request)
  (or (gethash (acp:terminal-id request) (%terminals client))
      (error 'acp:acp-resource-not-found :message "Unknown terminal")))

(defmethod acp:acp-wait-for-terminal-exit ((client demo-client) request)
  (let ((term (%terminal client request)))
    (%event client "terminal/wait_for_exit ~A → ~A" (acp:terminal-id request) (car term))
    (acp:make-acp 'acp:wait-for-terminal-exit-response :exit-code (car term))))

(defmethod acp:acp-terminal-output ((client demo-client) request)
  (let ((term (%terminal client request)))
    (%event client "terminal/output ~A → ~A" (acp:terminal-id request)
            (string-trim '(#\Newline) (cdr term)))
    ;; MAKE-ACP drops NIL initargs (the field is omitted); the required
    ;; boolean `truncated` is set to false through its slot.
    (let ((response (acp:make-acp 'acp:terminal-output-response
                                  :output (cdr term)
                                  :exit-status (acp:make-acp 'acp:terminal-exit-status
                                                             :exit-code (car term)))))
      (setf (acp:truncated response) nil)
      response)))

(defmethod acp:acp-release-terminal ((client demo-client) request)
  (%event client "terminal/release ~A" (acp:terminal-id request))
  (acp:make-acp 'acp:release-terminal-response))
