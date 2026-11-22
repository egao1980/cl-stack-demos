;;; The demo agent process: `sbcl --script agent-main.lisp`, started by
;;; acp-agent.sh (for Zed, and for the headless client of run-demos).
;;; ACP_DEMO_REGISTRY names a file holding the source registry form that
;;; cl-repo run computed (it is deleted once read). Loading prints only to
;;; stderr: stdout carries ACP messages only.

(require "asdf")
(let ((file (uiop:getenv "ACP_DEMO_REGISTRY")))
  (when (and file (probe-file file))
    (asdf:initialize-source-registry
     (with-open-file (in file) (with-standard-io-syntax (let ((*read-eval* nil)) (read in)))))
    (delete-file file)))
(let ((*standard-output* *error-output*))
  (asdf:load-system "harness-demos-acp"))
(funcall (intern "SERVE-DEMO-AGENT" "HARNESS-DEMOS-ACP"))
(uiop:quit 0)
