(in-package #:harness-demos-policy)

;;; Three settings layers, as text (what a settings file holds). Outer first:
;;; an inner layer can only tighten. Rule ids name the layer.

(defparameter *user-text* "
(defpolicy user (:combining :last-match :default :ask)
  (ask    :id \"user-bash\"      :action \"Bash\" :feedback \"Shell commands need a confirmation.\")
  (permit :id \"user-git\"       :action \"Bash\" :resource \"git *\")
  (ask    :id \"user-git-push\"  :action \"Bash\" :resource \"git push *\" :feedback \"Pushing needs a confirmation.\")
  (permit :id \"user-make\"      :action \"Bash\" :resource \"make *\")
  (permit :id \"user-read\"      :action \"Read\")
  (permit :id \"user-edit\"      :action \"Edit\")
  (permit :id \"user-github\"    :action \"WebFetch\" :resource (:host \"github.com\")))")

(defparameter *project-text* "
(defpolicy project (:combining :forbid-overrides)
  (permit :id \"proj-src\"       :action \"Read\" :resource (:path \"src\"))
  (forbid :id \"proj-secrets\"   :action \"Read\" :resource \".env*\" :feedback \"Secrets stay out of the context.\")
  (forbid :id \"proj-rm\"        :action \"Bash\" :resource \"rm -rf *\")
  (permit :id \"proj-typo\"      :action \"Bash\" :resource 42)
  (forbid :id \"proj-plan-edit\" :action \"Edit\" :when (and (mode :plan) (not (approved :plan)))
          :feedback \"Plan mode: propose the change, do not edit.\"))")

(defparameter *session-text* "
(defpolicy session (:combining :last-match)
  (ask    :id \"sess-make\"          :action \"Bash\" :resource \"make *\"
          :feedback \"No sandbox: tests would run unconfined.\")
  (permit :id \"sess-make-confined\" :action \"Bash\" :resource \"make *\"
          :when (sandbox :confined) :obligation (:run :confined)))")

(defparameter *requests*
  ;; (action resource context)
  '(("Read"     "src/main.lisp"          ())
    ("Read"     ".env"                   ())
    ("Bash"     "git status"             ())
    ("Bash"     "git push origin main"   ())
    ("Bash"     "ls -la"                 ())
    ("Bash"     "rm -rf build"           ())
    ("Edit"     "src/main.lisp"          ())
    ("Edit"     "src/main.lisp"          ((mode :plan)))
    ("WebFetch" "api.github.com"         ())
    ("WebFetch" "evil.example"           ())
    ("Bash"     "make test"              ((sandbox :confined)))
    ("Bash"     "make test"              ())))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun req (spec)
  (destructuring-bind (action resource context) spec
    (pol:make-policy-request :action action :resources (list resource) :context context)))

(defun call-text (spec)
  (format nil "~a(~a)~@[ ~{~(~a~)~^ ~}~]" (first spec) (second spec)
          (mapcar (lambda (f) (format nil "~(~{~a~^:~}~)" f)) (third spec))))

(defun load-layers ()
  "Load the three layers; a malformed rule is reported and skipped."
  (handler-bind ((pol:policy-syntax-error
                   (lambda (c)
                     (format t "  policy-syntax-error: ~a~%  -> invoking SKIP-RULE~%" c)
                     (pol:skip-rule c))))
    (mapcar #'pol:load-policy (list *user-text* *project-text* *session-text*))))

(defun print-proof (proof &optional (depth 0))
  (format t "~&    ~a~s~:[~; (base fact)~]~%" (make-string (* 2 depth) :initial-element #\Space)
          (logic:proof-fact proof) (logic:proof-base-p proof))
  (dolist (p (logic:proof-premises proof)) (print-proof p (1+ depth)))
  (dolist (n (logic:proof-negated proof))
    (format t "    ~a  not ~s~%" (make-string (* 2 depth) :initial-element #\Space) n)))

(defun run-demos ()
  (let ((*print-right-margin* 110))
    (banner "1. A layered agent-permission policy, loaded from text")
    (format t "Loading user / project / session text (project has a rule with :resource 42):~%")
    (let* ((layers (load-layers))
           (policy (apply #'pol:make-layered-policy layers))
           (decisions '()))
      (dolist (p layers)
        (format t "  ~a: ~(~s~)~@[, default ~(~a~)~], rules ~{~a~^ ~}~%"
                (pol:policy-name p) (pol:policy-combining p) (pol:policy-default p)
                (mapcar #'pol:policy-rule-id (pol:policy-rules p))))
      (let* ((user (first layers))
             (git (req '("Bash" "git status" ())))
             (fo (pol:make-policy :name "user-fo" :combining :forbid-overrides
                                  :rules (pol:policy-rules user))))
        (format t "~%Combining modes, user rules on Bash(git status):~%")
        (dolist (p (list user fo))
          (let ((d (pol:evaluate-policy p git)))
            (format t "  ~18a -> ~(~a~) by ~a~%" (pol:policy-combining p) (pol:decision-effect d)
                    (pol:policy-rule-id (pol:decision-rule d))))))

      (banner "2. Decisions (layered: user > project > session; inner layers only tighten)")
      (format t "~32a ~8a ~8a ~19a ~a~%" "request" "effect" "layer" "deciding rule" "feedback / obligations")
      (dolist (spec *requests*)
        (let ((d (pol:evaluate-policy policy (req spec))))
          (push (cons spec d) decisions)
          (format t "~32a ~8a ~8a ~19a ~@[~a~]~@[ obligations ~s~]~%"
                  (call-text spec) (string-downcase (pol:decision-effect d))
                  (if (pol:decision-layer d) (pol:policy-name (pol:decision-layer d)) "-")
                  (cond ((pol:decision-rule d) (pol:policy-rule-id (pol:decision-rule d)))
                        ((pol:decision-default-p d) "(layer default)")
                        (t "-"))
                  (pol:decision-feedback d) (pol:decision-obligations d))))
      (setf decisions (nreverse decisions))

      (banner "3. Proof of a permit, why-not for a deny")
      (let ((d (cdr (nth 10 decisions))))
        (format t "Bash(make test) sandbox:confined~%~a" (pol:policy-explain d))
        (format t "  per-layer trace:~%")
        (dolist (tr (pol:decision-trace d))
          (format t "    ~8a ~(~a~)~@[ by ~a~]~%" (pol:policy-name (pol:trace-policy tr))
                  (pol:trace-effect tr) (and (pol:trace-rule tr) (pol:policy-rule-id (pol:trace-rule tr)))))
        (format t "  logic-protocol proof (deciding layer):~%")
        (print-proof (pol:decision-proof d))
        (format t "  session layer's proof (sess-make-confined, its condition used the context):~%")
        (print-proof (pol:trace-proof (third (pol:decision-trace d)))))
      (flet ((why-not (n rule-id)
               (let* ((e (nth n decisions))
                      (w (pol:decision-why-not (cdr e) rule-id)))
                 (format t "~&~a is ~(~a~); why not ~a? -> ~:[NIL: it decided its own layer (an inner layer tightened)~;~:*~s~@[, literal ~s~]~]~%"
                         (call-text (car e)) (pol:decision-effect (cdr e)) rule-id
                         (and w (pol:policy-why-not-reason w)) (and w (pol:policy-why-not-literal w))))))
        (format t "~%Read(.env):~%~a~%" (pol:policy-explain (cdr (nth 1 decisions))))
        (why-not 1 "proj-src")
        (why-not 6 "proj-plan-edit")
        (why-not 2 "user-bash")
        (why-not 7 "user-edit"))

      (banner "4. Conditions and restarts: the malformed rule")
      (format t "Section 1 loaded the layers inside (LOAD-LAYERS):~%~{  ~a~%~}"
              '("(handler-bind ((pol:policy-syntax-error"
                "                 (lambda (c)"
                "                   (format t \"  policy-syntax-error: ~a~%  -> invoking SKIP-RULE~%\" c)"
                "                   (pol:skip-rule c))))"
                "  (mapcar #'pol:load-policy (list *user-text* *project-text* *session-text*)))"))
      (format t "SKIP-RULE left proj-typo out; the other ~d project rules stand.~%"
              (length (pol:policy-rules (second layers))))
      (format t "Restarts visible to that handler, innermost first: ~{~(~a~)~^, ~}~%"
              (block nil
                (handler-bind ((pol:policy-syntax-error
                                 (lambda (c) (return (mapcar #'restart-name (compute-restarts c))))))
                  (pol:load-policy *project-text*))))

      (banner "5. A budget: 4 logic steps per request (CONTINUE grants 4 more each time)")
      (let* ((signals 0)
             (d (handler-bind ((logic:logic-budget-exceeded
                                 (lambda (c)
                                   (when (zerop signals)
                                     (format t "  signalled ~s:~%    ~a~%" (type-of c) c))
                                   (incf signals)
                                   (continue c))))
                  (pol:evaluate-policy policy (req (nth 10 *requests*)) :budget 4))))
        (format t "  handler invoked CONTINUE ~d times~%" signals)
        (format t "  finished: ~(~a~) by ~a, obligations ~s (same as the unbudgeted decision)~%"
                (pol:decision-effect d) (pol:policy-rule-id (pol:decision-rule d))
                (pol:decision-obligations d)))


      (banner "6. Under the hood")
      (format t "~{~a~%~}Backend: ~(~a~) (logic-backend-datalog)~%Compiled programs (rules per layer): ~{~d~^ / ~}.~%"
              '("Every layer compiles once to a logic-protocol program (stratified Datalog with"
                "glob, prefix, =, /=, < built-ins); each request is evaluated with :assume on it.")
              (logic:backend-name logic:*logic-backend*)
              (mapcar (lambda (p) (length (logic:program-rules (pol:policy-program p)))) layers))
      (finish-output)
      t)))
