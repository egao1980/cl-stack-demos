;;;; 4. .env files and typed settings (cl-stack-config#8).

(in-package #:harness-demos-documents)

(schema:defschema db-settings ()
  (host string)
  (port (integer 1 65535))
  (:key-style :snake))

(schema:defschema app-settings ()
  (verbose boolean :default nil)
  (database db-settings)
  (max-connections (integer 1) :default 4)
  (greeting string :default "")
  (:key-style :snake)
  (:extra :ignore))

(defparameter *expected-dotenv*
  `(("APP_DATABASE__HOST" . "db.internal")
    ("APP_DATABASE__PORT" . "5432")
    ("APP_URL" . "postgres://db.internal:5432/reports")
    ("APP_BACKUP_URL" . "backup.internal/reports")
    ("APP_GREETING" . ,(format nil "hello~%world, it's \"quoted\""))
    ("APP_PEM" . ,(format nil "-----BEGIN KEY-----~%MIIB~%-----END KEY-----"))
    ("APP_VERBOSE" . "true")
    ("APP_COLOR" . "#336699")
    ("APP_MAX_CONNECTIONS" . "8")
    ("APP_EMPTY" . "")
    ("APP_FLAG"))
  "fixtures/app.env as python-dotenv 1.1.1 reads it, worked out by hand.")

(defun settings-plist (s)
  (let ((db (slot-value s 'database)))
    (list :verbose (slot-value s 'verbose) :host (slot-value db 'host) :port (slot-value db 'port)
          :max-connections (slot-value s 'max-connections))))

(defun demo-dotenv ()
  (let* ((env-path (here "fixtures/app.env"))
         (toml-path (here "fixtures/app.toml"))
         (pairs (cfg:read-dotenv env-path :environ '())))
    (out "fixtures/app.env:~%")
    (show-lines (string-right-trim '(#\Newline) (uiop:read-file-string env-path)))
    (out "~%(read-dotenv #p\"fixtures/app.env\" :environ '()) =>~%")
    (loop for (name . value) in pairs
          do (out "  ~22a ~a~%" name (one-line (prin1-to-string value))))
    (check pairs *expected-dotenv* "read-dotenv")
    (out "~%A bare NAME has the value NIL and is never set; ${NAME:-default} reads the file~%~
          first, then :environ (here empty), as dotenv_values does.~%")
    ;; Syntax warnings are skipped, as python-dotenv logs and skips.
    (let ((warnings '()))
      (let ((parsed (handler-bind ((cfg:dotenv-syntax-warning
                                     (lambda (w)
                                       (push (cfg:dotenv-syntax-warning-line w) warnings)
                                       (muffle-warning w))))
                      (cfg:parse-dotenv (format nil "A=1~%not a statement~%B=2~%") :environ '()))))
        (out "~%parse-dotenv of \"A=1\\nnot a statement\\nB=2\": ~s, dotenv-syntax-warning on line(s) ~s~%"
             parsed (reverse warnings))
        (check parsed '(("A" . "1") ("B" . "2")) "the bad line is skipped")
        (check (reverse warnings) '(2) "one warning, line 2")))
    ;; load-dotenv: the process environment.
    (let ((path (tmp-file "load.env")))
      (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
        (format o "DOCS_DEMO_KEEP=from-file~%DOCS_DEMO_NEW=from-file~%"))
      (setf (uiop:getenv "DOCS_DEMO_KEEP") "from-the-environment")
      (let ((set (cfg:load-dotenv path)))
        (out "~%load-dotenv with DOCS_DEMO_KEEP already set: set ~s; DOCS_DEMO_KEEP is ~s~%"
             set (uiop:getenv "DOCS_DEMO_KEEP"))
        (check (mapcar #'car set) '("DOCS_DEMO_NEW") "only the unset variable is set")
        (check (uiop:getenv "DOCS_DEMO_KEEP") "from-the-environment" "no override"))
      (cfg:load-dotenv path :override t)
      (out "load-dotenv :override t: DOCS_DEMO_KEEP is ~s~%" (uiop:getenv "DOCS_DEMO_KEEP"))
      (check (uiop:getenv "DOCS_DEMO_KEEP") "from-file" ":override t")
      (uiop:delete-file-if-exists path))
    ;; Layers and typed settings.
    (let* ((environ '(("APP_MAX_CONNECTIONS" . "16")))
           (config (cfg:load-config toml-path :prefix "APP" :dotenv env-path :environ environ))
           (settings (cfg:load-into 'app-settings config)))
      (out "~%(load-config #p\"fixtures/app.toml\" :prefix \"APP\" :dotenv #p\"fixtures/app.env\"~%~
            ~13t:environ '((\"APP_MAX_CONNECTIONS\" . \"16\"))), then (load-into 'app-settings config):~%  ~s~%~
            file < .env < environment: host and port come from .env (\"5432\" coerced to an~%~
            integer), max_connections from the environment, \"true\" is T.~%"
           (settings-plist settings))
      (check (settings-plist settings) '(:verbose t :host "db.internal" :port 5432 :max-connections 16)
             "load-into")
      (let ((c (signalled
                (lambda ()
                  (cfg:load-into 'app-settings
                                 (cfg:load-config toml-path :prefix "APP" :dotenv env-path
                                                            :environ '(("APP_MAX_CONNECTIONS" . "0")
                                                                       ("APP_DATABASE__PORT" . "x")
                                                                       ("APP_VERBOSE" . "maybe"))))))))
        (out "~%With APP_MAX_CONNECTIONS=0, APP_DATABASE__PORT=x and APP_VERBOSE=maybe:~%~a~%"
             (string-right-trim '(#\Newline) (princ-to-string c)))
        (check (typep c 'cfg:config-validation-error) t "config-validation-error")
        (check (and (typep c 'schema:schema-validation-error)
                    (length (schema:schema-validation-error-issues c)))
               3 "every field reported at once")))
    ;; Not UTF-8.
    (let ((latin1 (tmp-file "latin1.env")))
      (write-octets (coerce (append (map 'list #'char-code "APP_NAME=caf") '(#xe9 10))
                            '(vector (unsigned-byte 8)))
                    latin1)
      (let ((c (signalled (lambda () (cfg:read-dotenv latin1 :environ '())))))
        (out "~%A Latin-1 file (\"APP_NAME=caf\\xe9\"): ~a~%  ~a~%" (type-of c)
             (tidy (one-line (princ-to-string c) 200)))
        (check (typep c 'cfg:config-parse-error) t "not UTF-8: config-parse-error")
        (check (and c (cfg:config-error-path c) t) t "the error names the path"))
      (out "~%python-dotenv's view of the same files:~%")
      (let ((py (python "dotenv_values.py" '("python-dotenv==1.1.1") env-path latin1)))
        (when py
          (destructuring-bind (app bad) (coerce (jget py "files") 'list)
            (let ((theirs (map 'list (lambda (pair)
                                       (cons (aref pair 0)
                                             (if (eq (aref pair 1) :null) nil (aref pair 1))))
                               (jget app "pairs"))))
              (out "  python-dotenv ~a, dotenv_values(\"fixtures/app.env\") equals read-dotenv: ~s~%"
                   (jget py "version") (equal theirs pairs))
              (check theirs pairs "python-dotenv's values"))
            (out "  dotenv_values of the Latin-1 file: ~a~%" (jget bad "error"))
            (check (jget bad "error") "UnicodeDecodeError" "python-dotenv: UnicodeDecodeError"))))
      (uiop:delete-file-if-exists latin1))))
