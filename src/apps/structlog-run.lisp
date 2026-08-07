;;;; Inspired by hynek/structlog show_off.py — structured fields + context.
(in-package #:cl-stack-demos)

(defun run-structlog-run ()
  (asdf:load-system "log-backend-log4cl")
  (asdf:load-system "sexp-protocol")
  (stack-log:configure :level :info :layout :structured :format :sexp)
  (stack-log:with-context (:request-id "demo-1")
    (stack-log:info "informative!" :some-key "some_value")
    (stack-log:warn "uh-uh!")
    (stack-log:log-error "omg" :a-dict '(:a 42 :b "foo")))
  (stack-log:info "boot" :version "0.1.0")
  (format t "~&; structlog-run emitted structured sexp logs~%")
  t)

(register-app "structlog-run"
              :title "structlog show_off-shaped structured logging"
              :upstream "https://github.com/hynek/structlog"
              :fn #'run-structlog-run)
