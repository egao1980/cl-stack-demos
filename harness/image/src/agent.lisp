(in-package #:harness-demos-image)

;;; Demo 4: an agent run on the scripted mock with permission rules over the
;;; scanner's resources. Demo 5: LM Studio finds and fixes a seeded bug in a
;;; small system loaded into a spawned image, using the tools.

(defun %tool-results (run)
  (loop for turn in (agent:agent-run-turns run)
        append (remove-if-not #'llm:llm-tool-result-part-p (llm:llm-turn-parts turn))))

(defun %print-run (run &key (width 300))
  (let ((results (%tool-results run)))
    (loop for inv in (agent:agent-run-invocations run)
          for k from 1
          do (let ((part (find (agent:agent-invocation-id inv) results
                               :key #'llm:llm-tool-result-part-id :test #'equal)))
               (note "  ~2d. ~a ~a" k (agent:agent-invocation-name inv)
                     (clip (agent:agent-invocation-arguments inv) 200))
               (note "      status ~(~a~)~:[~; error~]" (agent:agent-invocation-status inv)
                     (and part (llm:llm-tool-result-part-error-p part)))
               (when part
                 (show "      result" (clip (string-right-trim '(#\Newline)
                                                                (princ-to-string (llm:llm-tool-result-part-content part)))
                                           width)))))
    (show "finish reason" (agent:agent-run-finish-reason run))))

(defun demo-agent-rules ()
  (banner "Demo 4: an agent run (scripted mock): rules allow pure evals, deny io, an undecided mutation asks")
  (let* ((root (make-project "agent-rules" '()))
         (tools (lt:make-lisp-tools :root root :groups "image"))
         (rules (list (agent:make-permission-rule "repl-spawn" "image:spawn" :allow)
                      (agent:make-permission-rule "repl-eval" "lisp:pure" :allow)
                      (agent:make-permission-rule "repl-eval" "lisp:reads *" :allow)
                      (agent:make-permission-rule "repl-eval" "lisp:io" :deny
                                                  :feedback "no printing in this image; return the value instead")))
         (steps (list (cons "repl-spawn" (json-args "backend" "swank"))
                      (cons "repl-eval" (json-args "code" "(+ 1 2)"))
                      (cons "repl-eval" (json-args "code" "(length *features*)"))
                      (cons "repl-eval" (json-args "code" "(print (+ 1 2))"))
                      (cons "repl-eval" (json-args "code" "(* 6 7)"))
                      (cons "repl-eval" (json-args "code" "(setf *answer* 42)"))
                      "done"))
         (ok nil))
    (show "rules" (mapcar #'princ-to-string rules))
    (unwind-protect
         (with-agent-loop
           (let ((run (agent:run-ai-agent
                       (agent:make-ai-agent :name "lisper" :backend (scripted-backend steps)
                                            :tools (list tools) :permissions rules)
                       "compute some numbers in the image")))
             (%print-run run)
             (let ((pending (remove :proposed (agent:agent-run-invocations run)
                                    :key #'agent:agent-invocation-status :test-not #'eq)))
               (dolist (inv pending)
                 (show (format nil "waiting for approval: ~a ~a" (agent:agent-invocation-name inv)
                               (agent:agent-invocation-arguments inv))
                       (agent:tool-permission-resources tools (agent:agent-invocation-name inv)
                                                        (agent:agent-invocation-arguments inv))))
               (setf ok (and (eq :approval (agent:agent-run-finish-reason run)) (= 1 (length pending)))))))
      (lt:close-lisp-tools tools))
    (stage "the same catch-all allow on an observe source: the refusal is a tool result, the run goes on")
    (let ((watch (lt:make-lisp-tools :root root :mode :observe)))
      (unwind-protect
           (with-agent-loop
             (let ((run (agent:run-ai-agent
                         (agent:make-ai-agent
                          :name "watcher"
                          :backend (scripted-backend (list (cons "repl-eval" (json-args "code" "(setf *x* 1)"))
                                                           "ok, observing only"))
                          :tools (list watch)
                          :permissions (list (agent:make-permission-rule "*" "*" :allow)))
                         "set *x*")))
               (%print-run run)
               (setf ok (and ok (eq :stop (agent:agent-run-finish-reason run))))))
        (lt:close-lisp-tools watch)))
    ok))

;;; Demo 5.

(defparameter +shop-asd+
  (format nil "(defsystem \"shop\"~%  :components ((:file \"cart\")))~%"))

(defparameter +shop-cart+
  (format nil ";;; A shopping cart. An item is a plist (:price P :qty Q).~%~%(defpackage #:shop~%  (:use #:cl)~%  (:export #:line-total #:cart-total))~%~%(in-package #:shop)~%~%(defun line-total (item)~%  \"The price of ITEM times its quantity.\"~%  (getf item :price))~%~%(defun cart-total (items)~%  \"The sum of the line totals of ITEMS.\"~%  (reduce #'+ (mapcar #'line-total items)))~%"))

(defparameter +shop-check+ "(shop:cart-total '((:price 3 :qty 2) (:price 5 :qty 1)))")

(defparameter +shop-prompt+
  (format nil "The system \"shop\" (file cart.lisp) is loaded in the live Lisp image. ~a should return 11 but it does not. Find the bug: evaluate the call with repl-eval, read the functions of cart.lisp with source-read (locators look like \"defun NAME\"), then fix the faulty function in the file with edit-replace (path \"cart.lisp\", the defun as locator, the whole corrected defun as code, compile true). Finally evaluate the call again to check it returns 11, and say what you changed in one sentence." +shop-check+))

(defparameter +shop-fixed-line-total+
  (format nil "(defun line-total (item)~%  \"The price of ITEM times its quantity.\"~%  (* (getf item :price) (getf item :qty)))"))

(defun %shop-mock-steps ()
  "What a model that does it right calls: the same tools, scripted."
  (list (cons "repl-eval" (json-args "code" +shop-check+))
        (cons "source-read" (json-args "path" "cart.lisp" "locator" "defun line-total"))
        (cons "source-read" (json-args "path" "cart.lisp" "locator" "defun cart-total"))
        (cons "edit-replace" (json-args "path" "cart.lisp" "locator" "defun line-total"
                                        "code" +shop-fixed-line-total+ "compile" t))
        (cons "repl-eval" (json-args "code" +shop-check+))
        "line-total returned only the price; it now multiplies the price by :qty, and the cart total is 11."))

(defun %shop-run (label slug backend model)
  "Spawn an image, load shop, let BACKEND's agent fix it with the tools.
→ true when the image then answers 11."
  (let* ((root (make-project (format nil "shop-~a" slug)
                             `(("shop.asd" ,+shop-asd+) ("cart.lisp" ,+shop-cart+))))
         (tools (lt:make-lisp-tools :root root
                                    ;; edit-replace compile=true scans its code in this
                                    ;; package, not the file's (egao1980/lisp-mcp#17)
                                    :package "shop"
                                    :tools '("repl-connect" "repl-eval" "source-outline" "source-read"
                                             "edit-replace" "repl-compile")))
         (setup (lt:make-lisp-tools :root root))
         (rules (list (agent:make-permission-rule "*" "path:cart.lisp" :allow)
                      (agent:make-permission-rule "*" "lisp:pure" :allow)
                      (agent:make-permission-rule "*" "lisp:reads *" :allow)
                      (agent:make-permission-rule "*" "lisp:calls shop:*" :allow)
                      (agent:make-permission-rule "*" "lisp:defines shop:*" :allow)
                      (agent:make-permission-rule "*" "lisp:package shop" :allow)
                      (agent:make-permission-rule "*" "lisp:mutates cl:*package*" :allow)
                      (agent:make-permission-rule "repl-eval" "lisp:io" :deny
                                                  :feedback "no printing; return values instead")))
         (asked '())
         (ok nil))
    (unwind-protect
         (progn
           (stage (format nil "~a: setup (not the agent): spawn, load shop, show the bug" label))
           (tool setup "repl-spawn" "backend" "swank")
           (tool setup "repl-eval" "code" (format nil "(asdf:load-asd (uiop:parse-native-namestring ~s))" (%native (merge-pathnames "shop.asd" root))))
           (tool setup "repl-load-system" "system" "shop")
           (tool setup "repl-eval" "code" +shop-check+)
           ;; The agent's source attaches to the same image over a second Swank port.
           (let* ((r (lt:call-lisp-tool setup "repl-eval" (json-args "code" "(swank:create-server :port 0 :dont-close t)")))
                  (port (parse-integer r :start (+ 3 (search "=> " r)) :junk-allowed t)))
             (lt:call-lisp-tool tools "repl-connect" (json-args "port" port))
             (note "  the agent's source attached to the image (repl-connect; port not printed)"))
           (show "the agent's tools" (lt:lisp-tools-names tools))
           (show "rules" (mapcar #'princ-to-string rules))
           (show "prompt" +shop-prompt+)
           (stage (format nil "~a: the run" label))
           (let* ((hp:*http-backend* (http-backend-async:make-async-backend))
                  (agent:*off-loop-specials* (union '(hp:*http-backend* http-backend-async:*event-backend-maker*)
                                                    agent:*off-loop-specials*))
                  (run (with-agent-loop
                         (handler-bind ((agent:agent-approval-required
                                          (lambda (c)
                                            (let ((inv (agent:agent-approval-required-invocation c)))
                                              (push (list (agent:agent-invocation-name inv)
                                                          (agent:tool-permission-resources
                                                           tools (agent:agent-invocation-name inv)
                                                           (agent:agent-invocation-arguments inv)))
                                                    asked))
                                            (agent:invoke-deny c "not allowed by the demo's rules; use repl-eval on pure code, source-read and edit-replace on cart.lisp"))))
                           (agent:run-ai-agent
                            (agent:make-ai-agent
                             :name "fixer" :backend backend :model model
                             :instructions (format nil "You fix Common Lisp code in a live image with the tools. Be brief.~%~%~a"
                                                   (lt:lisp-tools-instructions tools))
                             :tools (list tools) :permissions rules
                             :settings (agent:make-agent-settings :max-steps 20))
                            +shop-prompt+)))))
             (%print-run run :width 400)
             (print-lm-usage label)
             (show "answer" (or (ignore-errors (agent:agent-run-text run)) ""))
             (when asked (show "asks the demo denied" (reverse asked))))
           (stage (format nil "~a: checked by the demo after the run" label))
           (let ((value (tool setup "repl-eval" "code" +shop-check+)))
             (show "cart.lisp now" (read-file root "cart.lisp"))
             (setf ok (and (stringp value) (search "=> 11" value) t))
             (show "fixed (=> 11 in the image)" ok)))
      (lt:close-lisp-tools tools)
      (lt:close-lisp-tools setup))
    ok))

(defun demo-agent-real ()
  (banner "Demo 5: an agent finds and fixes a seeded bug in a system loaded into the image")
  (let ((real (and *lm-model*
                   (%shop-run (format nil "LM Studio ~a (real model, varies run to run)" *lm-model*)
                              "real" (lm-backend) *lm-model*))))
    (cond (real t)
          (t (if *lm-model*
                 (note "~%  the real model did not fix the bug this run; the same tools and prompt on the scripted mock:")
                 (note "  no LM Studio model reachable: the real-model run is skipped; the scripted mock:"))
             (%shop-run "scripted mock" "mock" (scripted-backend (%shop-mock-steps)) nil)))))
