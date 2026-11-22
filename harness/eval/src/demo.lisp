(in-package #:harness-demos-eval)

;;; agent-eval 0.1.0 (E0): a Terminal-Bench-format dataset of three tasks
;;; (tasks/), run in the local environment by the nop, oracle and
;;; cl-stack-code agents; reports, comparisons, and REWARD-MISSING.

(defun demo-dir ()
  "harness/eval/ (this system's directory)."
  (asdf:system-source-directory "harness-demos-eval"))

(defvar *demo-dir* nil "DEMO-DIR, bound by RUN-DEMOS.")

(defvar *run-root* nil "A fresh temporary directory holding every run.")

(defun demo-path (relative) (merge-pathnames relative *demo-dir*))

(defun run-dir (label) (merge-pathnames (format nil "~a/" label) *run-root*))

(defun fmt-ratio (x) (if x (format nil "~,2f" x) "n/a"))

(defun fmt-int (x) (if x (format nil "~d" x) "-"))

(defun fmt-cost (x) (if x (format nil "$~,4f" x) "n/a"))

(defun fmt-s (ms) (if ms (format nil "~,1f s" (/ ms 1000)) "-"))

;;; ------------------------------------------------------------------
;;; Printing runs

(defun print-trials (run)
  (format t "~&   ~22a ~6a ~6a ~11a ~16a ~5a ~7a ~7a ~8a~%"
          "task" "reward" "passed" "source" "status" "turns" "in-tok" "out-tok" "wall")
  (dolist (tr (ae:run-trials run))
    (let ((ctx (ae:trial-context tr)) (g (ae:trial-grade tr)))
      (format t "~&   ~22a ~6a ~6a ~11a ~16a ~5a ~7a ~7a ~8a~%"
              (ae:trial-task-id tr) (ae:trial-reward tr) (if (ae:trial-passed-p tr) "yes" "no")
              (if g (string-downcase (ae:grade-source g)) "-") (string-downcase (ae:trial-status tr))
              (fmt-int (and ctx (ae:context-turns ctx)))
              (fmt-int (and ctx (ae:context-input-tokens ctx)))
              (fmt-int (and ctx (ae:context-output-tokens ctx)))
              (fmt-s (ae:trial-wall-ms tr)))))
  (finish-output))

(defun print-run-summary (run)
  (let ((totals (ae:run-totals run)))
    (note "   pass@1 ~a · accuracy ~a · eval-run mean ~a, ~d/~d passed"
          (fmt-ratio (ae:run-pass-at-k run 1)) (fmt-ratio (ae:run-accuracy run))
          (fmt-ratio (ev:eval-run-mean run)) (ev:eval-run-pass-count run) (ev:eval-run-n run))
    (note "   totals: ~d trials · tokens in ~a / out ~a (cache ~a) · cost ~a~:[ (incomplete)~;~] · wall ~a · agent ~a"
          (getf totals :trials) (fmt-int (getf totals :input-tokens))
          (fmt-int (getf totals :output-tokens)) (fmt-int (getf totals :cache-tokens))
          (fmt-cost (getf totals :cost-usd)) (getf totals :cost-complete-p)
          (fmt-s (getf totals :wall-ms)) (fmt-s (getf totals :agent-ms)))))

(defun run-markdown (run)
  "A Markdown summary of RUN (agent-eval 0.1.0 has Markdown only for
comparisons; its run reports are :SEXP and :JSON)."
  (let ((totals (ae:run-totals run)))
    (with-output-to-string (s)
      (format s "# agent-eval run ~a (~a)~%~%" (ae:run-label run) (ae:run-agent-name run))
      (format s "| task | reward | passed | status | turns | input tokens | output tokens | cost | wall |~%")
      (format s "|------|--------|--------|--------|-------|--------------|---------------|------|------|~%")
      (dolist (tr (ae:run-trials run))
        (let ((ctx (ae:trial-context tr)))
          (format s "| ~a | ~a | ~:[no~;yes~] | ~(~a~) | ~a | ~a | ~a | ~a | ~a |~%"
                  (ae:trial-task-id tr) (ae:trial-reward tr) (ae:trial-passed-p tr)
                  (ae:trial-status tr)
                  (fmt-int (and ctx (ae:context-turns ctx)))
                  (fmt-int (and ctx (ae:context-input-tokens ctx)))
                  (fmt-int (and ctx (ae:context-output-tokens ctx)))
                  (fmt-cost (and ctx (ae:context-cost-usd ctx)))
                  (fmt-s (ae:trial-wall-ms tr)))))
      (format s "~%pass@1 ~a; accuracy ~a; tokens in ~a / out ~a; cost ~a; wall ~a~%"
              (fmt-ratio (ae:run-pass-at-k run 1)) (fmt-ratio (ae:run-accuracy run))
              (fmt-int (getf totals :input-tokens)) (fmt-int (getf totals :output-tokens))
              (fmt-cost (getf totals :cost-usd)) (fmt-s (getf totals :wall-ms))))))

(defun save-reports (run label)
  "SAVE-RUN into the run root, then copy its result.json (paths scrubbed) and
a Markdown summary into reports/."
  (let ((dir (ae:save-run run (run-dir label))))
    (note "   save-run → ~a (~{~a~^, ~})" (scrub (uiop:native-namestring dir))
          (mapcar (lambda (p) (scrub (enough-namestring p dir)))
                  (append (uiop:directory-files dir)
                          (directory (merge-pathnames "trials/*/*/*.*" dir)))))
    (write-text (demo-path (format nil "reports/~a.json" label))
                (scrub (read-text (merge-pathnames "result.json" dir))))
    (write-text (demo-path (format nil "reports/~a.md" label)) (run-markdown run))
    (note "   reports/~a.json, reports/~a.md written" label label)
    dir))

(defun run-agent (dataset agent label)
  (note "~&-- ~a agent: run-agent-eval over ~d tasks" (ae:agent-name agent)
        (length (ev:eval-dataset-cases dataset)))
  (let ((run (ae:run-agent-eval dataset agent :id (format nil "~a-run" label) :label label
                                              :environment-factory #'seeded-environment
                                              :directory (run-dir (format nil "~a-trials" label)))))
    (print-trials run)
    (print-run-summary run)
    run))

;;; ------------------------------------------------------------------
;;; 1. The dataset

(defun demo-dataset ()
  (banner "1. A Terminal-Bench-format dataset (tasks/)")
  (let ((dataset (show (ae:load-tb-dataset (demo-path "tasks/") :name "harness-demos-eval"))))
    (note "   digest ~a" (ae:dataset-digest dataset))
    (dolist (task (ev:eval-dataset-cases dataset))
      (note "   ~a: workdir ~a, agent timeout ~d s, verifier ~d s, image ~:[none (built from the Dockerfile)~;~:*~a~], Dockerfile ~:[no~;yes~], solution ~:[no~;yes~], ~a / ~a, tags ~{~a~^, ~}"
            (ae:task-id task) (ae:task-workdir task) (round (ae:task-agent-timeout task))
            (round (ae:task-verifier-timeout task)) (ae:task-image task) (ae:task-dockerfile-p task)
            (ae:task-solution-p task) (ae:task-difficulty task) (ae:task-category task)
            (ae:task-tags task))
      (print-block (ae:task-instruction task) :prefix "      instruction | " :max-lines 3))
    (note "~&   Environment: agent-eval's LOCAL-ENVIRONMENT (path-mapped: /app is <trial>/root/app),~%   subclassed here as SEEDED-LOCAL-ENVIRONMENT to do the Dockerfile's COPY app/ /app/.")
    dataset))

;;; ------------------------------------------------------------------
;;; 2-3. nop and oracle, compared

(defun demo-nop-oracle (dataset)
  (banner "2. The nop and oracle agents")
  (let ((nop (run-agent dataset (ae:make-nop-agent) "nop")))
    (save-reports nop "nop")
    (let ((oracle (run-agent dataset (ae:make-oracle-agent) "oracle")))
      (save-reports oracle "oracle")
      (note "~&-- the oracle run's Markdown summary (reports/oracle.md):")
      (print-block (run-markdown oracle) :prefix "   ")
      (note "~&-- the :JSON report's top-level members (ev:eval-report :format :json):")
      (let ((h (json:decode (ev:eval-report oracle :format :json))))
        (note "   ~{~a~^, ~}" (let (ks) (maphash (lambda (k v) (declare (ignore v)) (push k ks)) h)
                                (sort ks #'string<))))
      (note "~&-- load-run round trip:")
      (let ((back (ae:load-run (run-dir "oracle"))))
        (show (equal (ev:eval-report back) (ev:eval-report oracle))))
      (banner "3. compare-runs: nop (baseline) vs oracle (candidate)")
      (let* ((cmp (ae:compare-runs nop oracle))
             (md (ae:comparison-report cmp)))
        (show (list :wins (ae:comparison-wins cmp) :ties (ae:comparison-ties cmp)
                    :losses (ae:comparison-losses cmp) :mean-delta (ae:comparison-mean-delta cmp)
                    :wall-delta (ae:comparison-wall-delta cmp)))
        (print-block md :prefix "   ")
        (write-text (demo-path "reports/nop-vs-oracle.md") md)
        (write-text (demo-path "reports/nop-vs-oracle.json")
                    (ae:comparison-report cmp :format :json))
        (note "   reports/nop-vs-oracle.md, reports/nop-vs-oracle.json written"))
      (values nop oracle))))

;;; ------------------------------------------------------------------
;;; 4. cl-stack-code, live

(defun cl-stack-code-agent (model)
  (ae:make-cl-stack-code-agent
   :command (list (uiop:native-namestring (demo-path "bin/cl-stack-code")))
   :model (format nil "openai:~a" model)
   :max-turns 20
   :model-params (sampling-model-params)
   ;; A local model: priced at 0 so the cost column is a sum, not n/a.
   :prices '(:input 0 :output 0)
   :version "0.10.0"
   :env (list (cons "OPENAI_BASE_URL" (base-url))
              (cons "OPENAI_API_KEY" (api-key))
              (cons "CL_SOURCE_REGISTRY" (source-registry-text)))))

(defun print-transcript-digest (run)
  (dolist (tr (ae:run-trials run))
    (let* ((events (ae:trial-transcript tr))
           (types (let ((counts '()))
                    (dolist (e events)
                      (let* ((ty (or (gethash "type" e) "?"))
                             (cell (assoc ty counts :test #'equal)))
                        (if cell (incf (cdr cell)) (push (cons ty 1) counts))))
                    (nreverse counts)))
           (ctx (ae:trial-context tr)))
      (note "   ~a: ~d stream-json events ~{~a~^ ~}; status-detail ~a; error ~a"
            (ae:trial-task-id tr) (length events)
            (mapcar (lambda (c) (format nil "~a×~d" (car c) (cdr c))) types)
            (ae:context-status-detail ctx) (or (ae:trial-error tr) "none"))
      (when (null events)
        (let ((err (merge-pathnames "agent/cl-stack-code.stderr"
                                    (uiop:ensure-directory-pathname (ae:trial-directory tr)))))
          (when (probe-file err)
            (print-block (let* ((lines (remove-if (lambda (l) (or (zerop (length l))
                                                                  (search "WARNING" l)
                                                                  (char= #\; (char l 0))))
                                                  (uiop:split-string (read-text err)
                                                                     :separator (string #\Newline)))))
                           (format nil "~{~a~^~%~}" (last lines 6)))
                         :prefix "      stderr | "))))
      (when (ae:context-result-text ctx)
        (print-block (ae:context-result-text ctx) :prefix "      answer | " :max-lines 4))
      (let ((g (ae:trial-grade tr)))
        (when g (print-block (ae:grade-output g) :prefix "      verifier | " :max-lines 4))))))

(defun demo-cl-stack-code (dataset oracle)
  "→ the run, or NIL when the endpoint is unreachable (after saying so loudly)."
  (banner "4. cl-stack-code -p, live, on the same three tasks")
  (let* ((url (base-url))
         (ids (handler-case (probe-endpoint)
                (live-endpoint-unreachable (c)
                  (note "~%!!! LIVE STEP FAILED: ~a" c)
                  (note "!!! No fallback: the cl-stack-code run did not happen. The demo exits non-zero.~%")
                  (return-from demo-cl-stack-code nil))))
         (model (pick-model ids)))
    (note "   GET /v1/models at ~a: ~{~a~^, ~}" (url-host url) ids)
    (note "   MODEL THAT RAN: ~a at ~a (~a); API key given: ~:[no~;yes~] (never printed)"
          model (url-host url) (if (%env "HARNESS_DEMOS_MODEL") "from HARNESS_DEMOS_MODEL" "first listed")
          (plusp (length (api-key))))
    (note "   SAMPLING PRESET SENT: ~(~a~) (HARNESS_DEMOS_MODE) ~a" (sampling-mode) (sampling-json))
    (let ((agent (cl-stack-code-agent model)))
      (note "   agent-config (saved with the run; :env, which holds the key, is not in it):")
      (print-block (let ((*print-right-margin* 100) (*print-case* :downcase))
                     (prin1-to-string (ae:agent-config agent)))
                   :prefix "     ")
      (note "   argv for the first task:")
      (print-block (format nil "~{~s~^ ~}"
                           (ae:cl-stack-code-argv agent "<instruction.md>" "<trial>/root/app"))
                   :prefix "     ")
      (let ((run (run-agent dataset agent "cl-stack-code")))
        (print-transcript-digest run)
        (save-reports run "cl-stack-code")
        (note "~&-- load-run of this run (its agent-config holds the chat_template_kwargs JSON object):")
        (handler-case (progn (ae:load-run (run-dir "cl-stack-code")) (note "   loaded"))
          (error (c)
            (note "   load-run FAILED: ~a" (first (uiop:split-string (princ-to-string c)
                                                                 :separator (string #\Newline))))
            (note "   (save-run printed the hash table as #.(…); egao1980/agent-eval#4)")))
        (banner "4b. compare-runs: oracle (baseline) vs cl-stack-code (candidate)")
        (let* ((cmp (ae:compare-runs oracle run))
               (md (ae:comparison-report cmp)))
          (print-block md :prefix "   ")
          (write-text (demo-path "reports/oracle-vs-cl-stack-code.md") md)
          (write-text (demo-path "reports/oracle-vs-cl-stack-code.json")
                      (ae:comparison-report cmp :format :json))
          (note "   reports/oracle-vs-cl-stack-code.md, .json written"))
        run))))

;;; ------------------------------------------------------------------
;;; 5. Conditions and restarts

(defun demo-conditions (oracle)
  (banner "5. Conditions and restarts: a verifier that writes no reward")
  (let* ((broken (ae:load-tb-dataset (demo-path "tasks-broken/") :name "broken"))
         (task (first (ev:eval-dataset-cases broken))))
    (print-block (read-text (demo-path "tasks-broken/broken-verifier/tests/test.sh"))
                 :prefix "   test.sh | ")
    (note "~&-- 5a. run-trial … :on-missing-reward :error, inside a handler-bind that picks SCORE-ZERO")
    (let ((trial
            (handler-bind ((ae:reward-missing
                             (lambda (c)
                               (note "   signalled ~s: ~a" (type-of c) c)
                               (note "   task-id ~a, reason ~s; is an eval-protocol:eval-error: ~a"
                                     (ae:reward-missing-task-id c) (ae:reward-missing-reason c)
                                     (typep c 'ev:eval-error))
                               (note "   restarts: ~{~(~a~)~^, ~}"
                                     (mapcar #'restart-name
                                             (remove-if-not #'restart-name (compute-restarts c))))
                               (note "   → (invoke-restart 'agent-eval:score-zero)")
                               (invoke-restart (find-restart 'ae:score-zero c)))))
              (ae:run-trial task (ae:make-oracle-agent)
                            :environment-factory #'seeded-environment
                            :on-missing-reward :error
                            :directory (run-dir "broken-trial")))))
      (let ((g (ae:trial-grade trial)))
        (note "   trial: status ~(~a~), reward ~a, passed ~a, grade source ~(~a~), test.sh exit ~a"
              (ae:trial-status trial) (ae:trial-reward trial) (ae:trial-passed-p trial)
              (ae:grade-source g) (ae:grade-exit-code g))
        (print-block (ae:grade-output g) :prefix "   verifier | ")))
    (note "~&-- 5b. the same restart chosen by the caller instead: USE-EXIT-CODE (test.sh exited 0)")
    (let ((trial (handler-bind ((ae:reward-missing
                                  (lambda (c) (invoke-restart (find-restart 'ae:use-exit-code c)))))
                   (ae:run-trial task (ae:make-oracle-agent)
                                 :environment-factory #'seeded-environment
                                 :on-missing-reward :error
                                 :directory (run-dir "broken-trial-exit")))))
      (note "   trial: reward ~a, passed ~a, grade source ~(~a~)"
            (ae:trial-reward trial) (ae:trial-passed-p trial) (ae:grade-source (ae:trial-grade trial))))
    (note "~&-- 5c. run-agent-eval … :on-missing-reward :error: the error reaches the caller as~%   eval-protocol:scorer-error (restarts skip-case / retry-case / score-as-failure); its cause~%   still has SCORE-ZERO live")
    (let ((run (handler-bind ((ev:scorer-error
                                (lambda (c)
                                  (let ((cause (ev:scorer-error-cause c)))
                                    (note "   signalled ~s, cause ~s" (type-of c) (type-of cause))
                                    (note "   restarts for the scorer-error: ~{~(~a~)~^, ~}"
                                          (mapcar #'restart-name
                                                  (remove-if-not #'restart-name (compute-restarts c))))
                                    (note "   → (invoke-restart (find-restart 'agent-eval:score-zero cause))")
                                    (invoke-restart (find-restart 'ae:score-zero cause))))))
                 (ae:run-agent-eval broken (ae:make-oracle-agent)
                                    :environment-factory #'seeded-environment
                                    :on-missing-reward :error :label "broken"
                                    :directory (run-dir "broken-run")))))
      (print-trials run)
      (note "~&-- 5d. compare-runs on different datasets: DATASET-MISMATCH, resolved with CONTINUE")
      (let ((cmp (handler-bind ((ae:dataset-mismatch
                                  (lambda (c)
                                    (note "   signalled: ~a" c)
                                    (note "   → (continue)")
                                    (continue c))))
                   (ae:compare-runs oracle run))))
        (note "   rows ~d; only in the baseline ~s; only in the candidate ~s"
              (length (ae:comparison-rows cmp)) (ae:comparison-only-baseline cmp)
              (ae:comparison-only-candidate cmp))))))

;;; ------------------------------------------------------------------

(defun run-demos ()
  (let* ((start (get-internal-real-time))
         (*run-root* (uiop:ensure-directory-pathname
                      (merge-pathnames (format nil "harness-demos-eval-~36r/" (random (expt 36 6) (make-random-state t)))
                                       (uiop:temporary-directory))))
         (*scrub-paths* '())
         (*demo-dir* (demo-dir))
         (live nil))
    (ensure-directories-exist *run-root*)
    (add-scrub (user-homedir-pathname) "~/")
    (add-scrub (uiop:temporary-directory) "<tmp>/")
    (add-scrub *demo-dir* "<demo>/")
    (add-scrub *run-root* "<runs>/")
    (note "agent-eval ~a · eval-protocol run records · local environment (the podman lane needs compute-backend-podman 0.3.0)"
          ae:+version+)
    (let ((dataset (demo-dataset)))
      (multiple-value-bind (nop oracle) (demo-nop-oracle dataset)
        (declare (ignore nop))
        (setf live (demo-cl-stack-code dataset oracle))
        (demo-conditions oracle)))
    (banner "Summary")
    (note "   wall ~,1f s" (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (uiop:delete-directory-tree *run-root* :validate t :if-does-not-exist :ignore)
    (unless live
      (note "!!! the live cl-stack-code step FAILED (see section 4)")
      (uiop:quit 2))
    t))
