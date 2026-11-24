(in-package #:harness-demos-services)

;;; Section 3 and the mail lines of section 6. NOT in the build: they need
;;; mail-protocol / mail-backend-smtp 0.3.0 (egao1980/mail-protocol#6, merged,
;;; not yet on GHCR). To bring them back once 0.3.0 is published:
;;;   - harness-demos-services.asd: pin mail-protocol and mail-backend-smtp
;;;     0.3.0, add (:feature :sbcl (:require "sb-posix")) and (:file "mail")
;;;     after "package";
;;;   - package.lisp: the local nicknames mail -> mail-protocol and
;;;     smtp -> mail-backend-smtp;
;;;   - demo.lisp: call (demo-mail) after (demo-redis) in RUN-DEMOS and
;;;     (demo-mail-review-fixes) in DEMO-REVIEW-FIXES; start Mailpit
;;;     (README.md).

;;; --- 3. mail ---------------------------------------------------------------

(defun header-lines (wire)
  (let ((end (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline) wire)))
    (uiop:split-string (remove #\Return (subseq wire 0 end)) :separator '(#\Newline))))

(defun shape (entity &optional (depth 0))
  (let ((mt (mime:mime-content-type entity))
        (cd (mime:mime-content-disposition entity)))
    (format t "~&  ~a~(~a/~a~)~@[  attachment ~a~]~%"
            (make-string (* 2 depth) :initial-element #\Space)
            (mime:media-type-type mt) (mime:media-type-subtype mt)
            (and cd (mime:disposition-filename cd)))
    (dolist (p (mime:mime-parts entity))
      (shape p (1+ depth)))))

(defun compose ()
  (let ((m (mail:make-message :from "harness@ex.com" :to "team@ex.com"
                              :subject "Weekly report" :body "The report is attached.")))
    (mail:add-alternative m "<p>The <b>report</b> is attached.</p>")
    (mail:add-attachment m (coerce #(37 80 68 70 45 49 46 55) '(vector (unsigned-byte 8)))
                         :filename "report.pdf")
    m))

(defun demo-mail ()
  (banner "3. mail-protocol 0.3.0 + mail-backend-smtp 0.3.0: compose, AUTH, STARTTLS, TLS")
  (let* ((m (compose))
         (wire (mail:print-message m)))
    (format t "  headers:~%~{    ~a~%~}" (header-lines wire))
    (format t "  structure after print + parse:~%")
    (let ((back (mail:parse-message wire)))
      (shape (mail:message-entity back))
      (format t "  message-body -> ~s~%  :subtype html -> ~s~%  attachments -> ~s~%"
              (mail:message-body back) (mail:message-body back :subtype "html")
              (mapcar (lambda (e) (mime:disposition-filename (mime:mime-content-disposition e)))
                      (mail:message-attachments back)))))
  (let ((host (env "DEMO_SMTP_HOST" "127.0.0.1"))
        (user (env "DEMO_SMTP_USER" "evidence"))
        (password (env "DEMO_SMTP_PASSWORD" "s3cret")))
    (flet ((try (label backend)
             (format t "  ~a -> ~a~%" label
                     (handler-case (progn (mail:backend-send backend (compose)) "sent")
                       (mail:mail-send-error (e) (format nil "~a: ~a" (type-of e) e))))))
      (let ((port (parse-integer (env "DEMO_SMTP_AUTH_PORT" "11026"))))
        (live ("SMTP AUTH" host port)
          (try (format nil "port ~d, no login" port) (smtp:make-smtp-backend :host host :port port))
          (try (format nil "port ~d, :username ~a" port user)
               (smtp:make-smtp-backend :host host :port port :username user :password password))
          (try (format nil "port ~d, wrong password" port)
               (smtp:make-smtp-backend :host host :port port :username user :password "nope"))))
      (cond
        ((not (trust-demo-ca)) (format t "  SKIP STARTTLS / TLS: DEMO_CA not set~%"))
        (t
         (let ((port (parse-integer (env "DEMO_SMTP_STARTTLS_PORT" "11027"))))
           (live ("SMTP STARTTLS" host port)
             (try (format nil "port ~d (requires STARTTLS), plain" port)
                  (smtp:make-smtp-backend :host host :port port))
             (try (format nil "port ~d, :starttls t" port)
                  (smtp:make-smtp-backend :host host :port port :starttls t))))
         (let ((port (parse-integer (env "DEMO_SMTP_TLS_PORT" "11028"))))
           (live ("SMTPS" host port)
             (try (format nil "port ~d, :tls t" port)
                  (smtp:make-smtp-backend :host host :port port :tls t))))))
      (try "127.0.0.1:1 (refused)" (smtp:make-smtp-backend :host "127.0.0.1" :port 1)))))

;;; --- 6. review fixes: mail -------------------------------------------------

(defun visible (string)
  "STRING with CR and LF written as \\r and \\n."
  (with-output-to-string (o)
    (loop for c across string
          do (case c
               (#\Return (write-string "\\r" o))
               (#\Newline (write-string "\\n" o))
               (t (write-char c o))))))

(defun fork-message-ids ()
  "(values parent-id child-id): one Message-ID before a fork, then one on
each side of it. NIL where SBCL has no fork."
  #+(and sbcl unix)
  (let ((path (format nil "/tmp/demo-msgid-~d.txt" (sb-posix:getpid))))
    (mail:make-message-id :domain "ex.com")
    (let ((pid (sb-posix:fork)))
      (if (zerop pid)
          (unwind-protect
              (with-open-file (o path :direction :output :if-exists :supersede)
                (write-string (mail:make-message-id :domain "ex.com") o))
            (sb-ext:exit :code 0 :abort t))
          (let ((mine (mail:make-message-id :domain "ex.com")))
            (sb-posix:waitpid pid 0)
            (values mine (prog1 (uiop:read-file-string path) (delete-file path)))))))
  #-(and sbcl unix)
  (values nil nil))

(defun demo-mail-review-fixes ()
  ;; mail-protocol #3 / #7: Message-IDs across fork; a bare CR in DATA.
  (multiple-value-bind (parent child) (fork-message-ids)
    (if parent
        (format t "  mail: Message-ID after fork: parent ~a, child ~a~%" parent child)
        (format t "  mail: SKIP fork: not SBCL on Unix~%")))
  (format t "  mail: DATA for \"hi\\r.\\rMAIL FROM:<x@evil>\" -> ~a~%"
          (visible (smtp:dot-stuff
                    (format nil "hi~C.~CMAIL FROM:<x@evil>" #\Return #\Return)))))
