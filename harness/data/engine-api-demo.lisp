(in-package #:harness-demos-data)

;;; Demo 6 — diff-protocol 0.3.0 engine extension API (#6, #7): an engine written
;;; outside diff-protocol, registered with REGISTER-DIFF-ENGINE, for a non-text value
;;; (an environment: an alist of ("NAME" . "value") strings), with a unified form
;;; through RENDER-UNIFIED-CONTENT, so a tree diff shows it as hunks, not as binary.

(defclass env-diff-engine (d:diff-engine) ()
  (:documentation "Demo engine: states are alists of (NAME . VALUE) strings."))

(defmethod d:engine-name ((e env-diff-engine)) :env)

(defun env-state-p (x)
  (and (listp x)
       (every (lambda (c) (and (consp c) (stringp (car c)) (stringp (cdr c)))) x)))

(defun check-env (e x)
  (unless (env-state-p x)
    (error 'd:unsupported-content :engine e :content x)))

(defun env-get (state name) (assoc name state :test #'string=))

(defun env-footprint (name) (list (format nil "env:/~A" name)))

(defmethod d:state-equal ((e env-diff-engine) a b)
  (and (= (length a) (length b))
       (every (lambda (c) (equal (cdr c) (cdr (env-get b (car c))))) a)))

(defmethod d:compute-diff ((e env-diff-engine) a b &key (level :changes) &allow-other-keys)
  (check-env e a)
  (check-env e b)
  (d:make-diff
   (append
    (loop for (name . value) in a
          for other = (env-get b name)
          if (null other)
            collect (d:make-change :remove :path (list :var name) :footprint (env-footprint name)
                                           :before value)
          else unless (string= value (cdr other))
                 collect (d:make-change :modify :path (list :var name) :footprint (env-footprint name)
                                                :before value :after (cdr other)))
    (loop for (name . value) in b
          unless (env-get a name)
            collect (d:make-change :add :path (list :var name) :footprint (env-footprint name)
                                        :after value)))
   :engine e :level level))

(defmethod d:apply-change ((e env-diff-engine) state change diff)
  (check-env e state)
  (let* ((name (second (d:change-path change)))
         (cell (env-get state name)))
    (flet ((conflict (reason)
             (error 'd:apply-conflict :diff diff :change change :reason reason
                                      :actual (cdr cell))))
      (ecase (d:change-kind change)
        (:add (when cell (conflict :exists))
         (append state (list (cons name (d:change-after change)))))
        (:remove (cond ((null cell) (conflict :missing))
                       ((string/= (cdr cell) (d:change-before change)) (conflict :before-mismatch)))
         (remove name state :key #'car :test #'string=))
        (:modify (cond ((null cell) (conflict :missing))
                       ((string/= (cdr cell) (d:change-before change)) (conflict :before-mismatch)))
         (mapcar (lambda (c) (if (string= (car c) name) (cons name (d:change-after change)) c))
                 state))))))

(defun env-text (state)
  (format nil "~:{~A=~A~%~}" (mapcar (lambda (c) (list (car c) (cdr c))) state)))

(defmethod d:render-unified-content ((e env-diff-engine) before after changes stream)
  ;; Whole states when the diff carries them; else one NAME=value line per change side.
  (let ((text (d:find-diff-engine :text)))
    (if (or before after (null changes))
        (d:render-unified-content text (env-text before) (env-text after) nil stream)
        (d:render-unified-content
         text
         (env-text (loop for c in changes when (d:change-before c)
                         collect (cons (second (d:change-path c)) (d:change-before c))))
         (env-text (loop for c in changes when (d:change-after c)
                         collect (cons (second (d:change-path c)) (d:change-after c))))
         nil stream))))

(defvar *env-engine* (d:register-diff-engine :env (make-instance 'env-diff-engine)))

(defun demo-engine-api ()
  (banner "Demo 6: diff-protocol 0.3.0 engine extension API (register-diff-engine, non-text values)")
  (let ((a '(("PATH" . "/usr/bin") ("LANG" . "C") ("DEBUG" . "0")))
        (b '(("PATH" . "/opt/bin:/usr/bin") ("LANG" . "C") ("TZ" . "UTC"))))

    (demo-step ("6a. register-diff-engine and find-diff-engine")
      (say "registered => ~S" *env-engine*)
      (check "find-diff-engine :env is the registered instance" (eq *env-engine* (d:find-diff-engine :env)))
      (check "registering :env again keeps the first instance"
             (eq *env-engine* (d:register-diff-engine :env (make-instance 'env-diff-engine)))))

    (demo-step ("6b. a diff of two environments: apply / invert / sexp round trip / :unified")
      (let ((diff (d:compute-diff *env-engine* a b)))
        (show-changes "env diff" diff)
        (show-text "render :summary" (d:render-diff diff :summary nil))
        (show-text "render :unified (render-unified-content, no base text needed)"
                   (d:render-diff diff :unified nil))
        (say "apply-diff a diff => ~S" (d:apply-diff a diff))
        (check "apply-diff a diff state-equal b" (d:state-equal *env-engine* b (d:apply-diff a diff)))
        (check "apply-diff b (invert-diff diff) state-equal a"
               (d:state-equal *env-engine* a (d:apply-diff b (d:invert-diff diff))))
        (check "parse-diff :sexp finds :env by name and applies"
               (d:state-equal *env-engine* b
                              (d:apply-diff a (d:parse-diff :sexp (d:render-diff diff :sexp nil)))))))

    (demo-step ("6c. apply-diff restarts on a stale state come from diff-protocol, not the engine")
      (let* ((diff (d:compute-diff *env-engine* a b))
             (stale '(("PATH" . "/bin") ("LANG" . "C") ("DEBUG" . "0")))
             (result (handler-bind ((d:apply-conflict
                                      (lambda (c)
                                        (say "apply-conflict ~(~A~) on ~{~A~} (found ~S); restarts ~{~(~A~)~^ ~} -> skip-change"
                                             (d:apply-conflict-reason c)
                                             (d:change-footprint (d:diff-error-change c))
                                             (d:apply-conflict-actual c)
                                             (remove nil (mapcar #'restart-name (compute-restarts c))))
                                        (d:skip-change c))))
                       (d:apply-diff stale diff))))
        (say "result => ~S" result)))

    (demo-step ("6d. three-way merge through the default merge-states: one clean key, one conflict")
      (let ((ours '(("PATH" . "/opt/bin:/usr/bin") ("LANG" . "C") ("DEBUG" . "1")))
            (theirs '(("PATH" . "/usr/local/bin:/usr/bin") ("LANG" . "en_US.UTF-8") ("DEBUG" . "0"))))
        (multiple-value-bind (diff conflicts) (d:three-way-diff *env-engine* a ours theirs)
          (show-changes "merged diff" diff)
          (show-conflicts conflicts)
          (say "apply-diff base merged => ~S" (d:apply-diff a diff)))))

    (demo-step ("6e. :env files inside a tree: :unified shows hunks (0.2 printed \"Binary files differ\")")
      (let* ((tree-e (make-instance 'd:tree-diff-engine
                                    :leaf-engines '((:text . :text) (:env . :env))))
             (ta (d:make-tree "README" (d:make-tree-file (lines "demo"))
                              "deploy" (d:make-tree "prod.env" (d:make-tree-file a :type :env))))
             (tb (d:make-tree "README" (d:make-tree-file (lines "demo, edited"))
                              "deploy" (d:make-tree "prod.env" (d:make-tree-file b :type :env))))
             (diff (d:compute-diff tree-e ta tb))
             (content (d:compute-diff tree-e ta tb :level :content)))
        (say "footprints: ~{~A~^ ~}" (d:diff-footprint diff))
        (show-text "render :unified (:changes level)" (d:render-diff diff :unified nil))
        (show-text "render :unified (:content level, whole states)" (d:render-diff content :unified nil))
        (check "apply-diff ta diff state-equal tb" (d:state-equal tree-e tb (d:apply-diff ta diff)))))))
