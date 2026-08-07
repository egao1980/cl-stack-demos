;;;; Inspired by pydantic/pydantic-settings — env-prefixed settings + file defaults.
(in-package #:cl-stack-demos)

(defun run-settings-env ()
  (let* ((cfg-path (merge-pathnames "config.toml" (fixtures-dir)))
         ;; DEMO_DATABASE__HOST overlays database.host
         (prev (uiop:getenv "DEMO_DATABASE__HOST")))
    (setf (uiop:getenv "DEMO_DATABASE__HOST") "db.example")
    (unwind-protect
         (let ((cfg (stack-config:load cfg-path :prefix "DEMO" :env t)))
           (assert (string= (stack-config:get-string cfg "app_name") "demos"))
           (assert (eql (stack-config:get-boolean cfg "debug") nil))
           (assert (= (stack-config:get-integer cfg "database.port") 5432))
           (assert (string= (stack-config:get-string cfg "database.host") "db.example"))
           (format t "~&; app=~A host=~A port=~A~%"
                   (stack-config:get-string cfg "app_name")
                   (stack-config:get-string cfg "database.host")
                   (stack-config:get-integer cfg "database.port"))
           t)
      (if prev
          (setf (uiop:getenv "DEMO_DATABASE__HOST") prev)
          (setf (uiop:getenv "DEMO_DATABASE__HOST") nil)))))

(register-app "settings-env"
              :title "TOML + env overlay (pydantic-settings shape)"
              :upstream "https://github.com/pydantic/pydantic-settings"
              :fn #'run-settings-env)
