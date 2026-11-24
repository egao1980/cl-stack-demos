;;;; Sections 9 to 12: sat-backend-cdcl 0.1.0 (Track L, slice L2b-3) next to
;;;; sat-backend-dpll 0.1.0 on the same instances. Every front-end call names
;;;; its backend with :BACKEND: RUN-DEMOS binds *SAT-BACKEND* to NIL, so a call
;;;; without one would signal SAT-NO-BACKEND. Every search starts from nothing
;;;; (no state survives a call), so the order of the runs does not matter.

(in-package #:harness-demos-sat)

;;; Instances ---------------------------------------------------------------------
;;; Generated from fixed seeds by the 31-bit LCG of sat-protocol's conformance
;;; suite, ported draw for draw, so every Lisp gets the same instances. Clauses
;;; are lists of non-zero integers over variables 1..N (DIMACS style).

(defun make-rng (seed) (list (mod seed 2147483648)))

(defun rng-int (rng n)
  "A pseudo-random integer in [0, N)."
  (setf (car rng) (mod (+ (* 1103515245 (car rng)) 12345) 2147483648))
  (mod (ash (car rng) -8) n))

(defun random-3sat (rng nvars nclauses)
  "NCLAUSES clauses of 3 distinct variables out of 1..NVARS, random signs."
  (loop repeat nclauses
        collect (let ((vars '()))
                  (loop while (< (length vars) 3)
                        do (pushnew (1+ (rng-int rng nvars)) vars))
                  (mapcar (lambda (v) (if (zerop (rng-int rng 2)) v (- v))) vars))))

(defun pigeonhole (pigeons holes)
  "PHP(PIGEONS, HOLES): variable (i-1)*HOLES + j is pigeon i in hole j; every
pigeon in a hole, no two in one hole. Unsatisfiable when PIGEONS > HOLES.
(values clauses nvars)."
  (flet ((v (i j) (+ (* (1- i) holes) j)))
    (values (append (loop for i from 1 to pigeons collect (loop for j from 1 to holes collect (v i j)))
                    (loop for j from 1 to holes
                          nconc (loop for i from 1 to pigeons
                                      nconc (loop for k from (1+ i) to pigeons
                                                  collect (list (- (v i j)) (- (v k j)))))))
            (* pigeons holes))))

(defun literal-true-p (literal value)
  "VALUE: variable -> boolean."
  (if (plusp literal) (funcall value literal) (not (funcall value (- literal)))))

(defun clauses-satisfied-p (clauses value)
  (every (lambda (c) (some (lambda (l) (literal-true-p l value)) c)) clauses))

(defun planted-3sat (rng nvars nclauses)
  "Random 3-clauses kept when a random planted assignment satisfies them:
satisfiable by construction."
  (let ((planted (make-array (1+ nvars) :initial-element nil)))
    (loop for v from 1 to nvars do (setf (aref planted v) (zerop (rng-int rng 2))))
    (loop with clauses = '()
          while (< (length clauses) nclauses)
          do (let ((c (first (random-3sat rng nvars 1))))
               (when (clauses-satisfied-p (list c) (lambda (v) (aref planted v)))
                 (push c clauses)))
          finally (return (nreverse clauses)))))

(defun shift-clauses (clauses offset)
  (mapcar (lambda (c) (mapcar (lambda (l) (if (plusp l) (+ l offset) (- l offset))) c)) clauses))

(defun hidden-core (rng &key (noise-vars 200) (noise-clauses 840) (pigeons 6) (holes 5))
  "Planted 3-SAT noise over 1..NOISE-VARS, then PHP(PIGEONS, HOLES) over the
next variables: unsatisfiable, and only the pigeonhole clauses are needed for
that. (values clauses nvars)."
  (let ((noise (planted-3sat rng noise-vars noise-clauses)))
    (multiple-value-bind (core n) (pigeonhole pigeons holes)
      (values (append noise (shift-clauses core noise-vars)) (+ noise-vars n)))))

(defun clauses->cnf (clauses nvars)
  "A CNF over variables named 1..NVARS."
  (let ((cnf (sat:make-cnf)))
    (loop for i from 1 to nvars do (sat:cnf-ensure-variable cnf i))
    (dolist (c clauses cnf)
      (sat:cnf-add-clause cnf c))))

(defun model-function (model)
  "MODEL (an alist over the names 1..N) as a function variable -> boolean."
  (lambda (v) (sat:model-value model v)))

;;; Solving within a time limit -----------------------------------------------------
;;; SOLVE has :BUDGET (decisions) but no timeout. A SAT-BUDGET-EXCEEDED handler
;;; makes one: STORE-VALUE resumes the search with a larger budget (the same
;;; search as with no budget, on both backends), USE-VALUE gives up and SOLVE
;;; returns the value.

(defparameter *budget-step* 4000 "Decisions between two looks at the clock.")

(defun solve-within (backend cnf seconds &key assumptions)
  "SOLVE CNF on BACKEND for at most SECONDS (wall clock).
(values seconds model sat statistics); MODEL is :TIMEOUT when the limit ran out."
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (model sat)
        (handler-bind ((sat:sat-budget-exceeded
                         (lambda (c)
                           (if (> (seconds-since start) seconds)
                               (use-value :timeout c)
                               (store-value (+ (sat:sat-budget-exceeded-budget c) *budget-step*) c)))))
          (sat:solve cnf :backend backend :assumptions assumptions :budget *budget-step*))
      (values (seconds-since start) model sat (copy-list (last-statistics backend))))))

(defun stat (statistics key)
  "KEY of STATISTICS, or \"-\" when the backend does not count it."
  (let ((v (getf statistics key :none)))
    (if (eq v :none) "-" v)))

;;; 9. DPLL against CDCL --------------------------------------------------------------

(defparameter *ratio* 4.26 "Clauses per variable of the random 3-SAT instances (the threshold).")
(defparameter *3sat-seed* 4260)
(defparameter *3sat-per-size* 4)
(defparameter *3sat-sizes* '(100 150 200))

(defparameter *gate-3sat-status* '(t t t nil)
  "MiniSat 2.2's status of the first four instances of seed 4260 at 150
variables (sat-backend-cdcl's performance gate batch; Glucose 4 agrees).")

(defparameter *compare-limit* 120
  "Seconds per instance and backend. Generous: the runs check answers and rough
speed, not a benchmark.")

(defparameter *hidden-core-seed* 7700)

(defun comparison-instances ()
  "List of (label group clauses nvars expected); EXPECTED :SAT, :UNSAT or NIL
(then only the two backends' agreement and the models are checked)."
  (append
   (loop for n in *3sat-sizes*
         nconc (let ((rng (make-rng *3sat-seed*)) (m (round (* *ratio* n))))
                 (loop for i below *3sat-per-size*
                       collect (list (format nil "3-SAT n=~d #~d" n i) (format nil "random 3-SAT, n=~d" n)
                                     (random-3sat rng n m) n
                                     (and (= n 150) (if (nth i *gate-3sat-status*) :sat :unsat))))))
   (loop for (p h) in '((8 7) (9 8))
         collect (multiple-value-bind (clauses n) (pigeonhole p h)
                   (list (format nil "PHP(~d,~d)" p h) "pigeonhole" clauses n :unsat)))
   (list (multiple-value-bind (clauses n) (hidden-core (make-rng *hidden-core-seed*))
           (list "hidden PHP(6,5) core" "hidden core" clauses n :unsat)))))

(defun answer-label (model sat)
  (cond ((eq model :timeout) "timeout") (sat "SAT") (t "UNSAT")))

(defun demo-comparison ()
  (out "The same CNFs on sat-backend-dpll (chronological backtracking, DLIS, pure~%~
        literals) and sat-backend-cdcl (two watched literals, first-UIP learning,~%~
        EVSIDS, phase saving, Luby restarts, LBD-based reduction), both with their~%~
        default parameters, through (sat:solve cnf :backend b), at most ~d s each.~%~
        Random 3-SAT at ratio ~,2f, ~d instances per size from seed ~d; the n=150 ones~%~
        are the first of sat-backend-cdcl's 3-SAT gate batch, whose MiniSat statuses are~%~
        recorded (~{~:[UNSAT~;SAT~]~^ ~}). The hidden core is planted 3-SAT noise (200~%~
        variables, 840 clauses) with PHP(6,5) on 30 fresh variables, seed ~d.~%~
        Every SAT model is checked against the clauses; the statuses must agree.~%~%"
       *compare-limit* *ratio* *3sat-per-size* *3sat-seed* *gate-3sat-status* *hidden-core-seed*)
  (out "~22a ~5@a ~7@a ~7a ~8a ~9@a ~10@a ~10@a ~8@a ~8@a~%"
       "instance" "vars" "clauses" "backend" "answer" "seconds" "decisions" "conflicts" "learnt" "restarts")
  (let ((groups '()))                   ; (group dpll-seconds cdcl-seconds dpll-timeouts), newest first
    (dolist (instance (comparison-instances))
      (destructuring-bind (label group clauses n expected) instance
        (unless (equal group (first (first groups)))
          (push (list group 0d0 0d0 0) groups))
        (let ((cnf (clauses->cnf clauses n)) (statuses '()) (row (first groups)))
          (loop for (key backend) in (list (list :dpll (make-instance 'dpll:dpll-backend))
                                           (list :cdcl (make-instance 'cdcl:cdcl-backend)))
                for first = t then nil
                do (multiple-value-bind (seconds model sat statistics)
                       (solve-within backend cnf *compare-limit*)
                     (incf (nth (if (eq key :dpll) 1 2) row) seconds)
                     (when (and (eq key :dpll) (eq model :timeout)) (incf (nth 3 row)))
                     (out "~22a ~5@a ~7@a ~(~7a~) ~8a ~9,3f ~10d ~10d ~8@a ~8@a~%"
                          (if first label "") (if first n "") (if first (length clauses) "")
                          key (answer-label model sat) seconds
                          (stat statistics :decisions) (stat statistics :conflicts)
                          (stat statistics :learnt-clauses) (stat statistics :restarts))
                     (unless (eq model :timeout)
                       (push sat statuses)
                       (when sat
                         (check (clauses-satisfied-p clauses (model-function model)) t
                                (format nil "~a ~(~a~): the model satisfies every clause" label key)))
                       (when expected
                         (check (if sat :sat :unsat) expected (format nil "~a ~(~a~) status" label key))))
                     (when (eq key :cdcl)
                       (check (eq model :timeout) nil (format nil "~a: CDCL within ~d s" label *compare-limit*))
                       (unless (eq model :timeout)
                         (check (- (getf statistics :conflicts) (getf statistics :learnt-clauses))
                                (if sat 0 1)
                                (format nil "~a: conflicts - learnt clauses" label))))))
          (when (= 2 (length statuses))
            (check (first statuses) (second statuses) (format nil "~a: DPLL and CDCL agree" label))))))
    (out "~%Seconds per group (a timeout counts its limit):~%")
    (out "  ~20a ~10@a ~10@a  ~a~%" "group" "DPLL" "CDCL" "DPLL / CDCL")
    (loop for (group dpll cdcl timeouts) in (reverse groups)
          do (out "  ~20a ~10,3f ~10,3f  ~:[~;> ~]~,1fx~%" group dpll cdcl (plusp timeouts)
                  (/ dpll (max cdcl 0.001d0))))
    (out "Random 3-SAT near the threshold is where learning pays off with size: DPLL~%~
          backtracks chronologically and meets the same conflicts again. Pigeonhole goes~%~
          the other way: DPLL's pure-literal rule refutes PHP(n+1,n) in (n-1)! decisions,~%~
          while resolution, so CDCL, needs exponentially many conflicts. The hidden core~%~
          is what DPLL cannot do: it refutes PHP(6,5) again under every noise assignment~%~
          it backtracks through. A clause learnt from a pigeonhole conflict mentions~%~
          pigeonhole variables only, so CDCL refutes the core once and backjumps over the~%~
          noise.~%")))

;;; 9b. The chains, minimised on both backends ------------------------------------------

(defparameter *chain-timeout* 30)

(defvar *cdcl* nil "The COUNTING-CDCL instance of this run.")

(defun chain-problems ()
  "List of (label objective optimum check-function); the optimum from the DP."
  (let ((violations (loop for (p . l) in *requests* collect (cons 1 (sat:sat-not (x p l))))))
    (list (list "chain (section 3)" (chain-objective) (chain-dp) *observed*)
          (list "revised (section 5)" (chain-objective *observed-2*) (chain-dp *observed-2*) *observed-2*)
          (list "lex (section 6)" (list :lex violations (chain-objective))
                (chain-dp *observed* (lambda (p l c)
                                       (if p
                                           (list (if (and (assoc p *requests*)
                                                          (not (eql l (cdr (assoc p *requests*)))))
                                                     1 0)
                                                 c)
                                           (list 0 c))))
                nil))))

(defun lex-costs-of (labels)
  (list (count-if-not (lambda (r) (eql (nth (car r) labels) (cdr r))) *requests*)
        (labelling-cost labels)))

(defun cost-no-better-p (cost optimum)
  "COST (a number or a lex vector) is OPTIMUM or worse."
  (if (listp optimum)
      (or (equal cost optimum) (< (first optimum) (first cost))
          (and (= (first optimum) (first cost)) (<= (second optimum) (second cost))))
      (<= optimum cost)))

(defun demo-chain-comparison ()
  (let ((formula (chain-formula)))
    (out "The weighted chains of sections 3, 5 and 6 (78 variables, 114 clauses; the~%~
          lexicographic one adds the six requests), minimised with :core-guided and~%~
          :linear-totalizer on each backend under :timeout ~d. The backends are~%~
          counting subclasses (:around methods on BACKEND-SOLVE and~%~
          BACKEND-FINAL-CONFLICT summing each search's statistics). A run the timeout~%~
          stops returns its incumbent, the anytime result, with optimal-p NIL.~%~%"
         *chain-timeout*)
    (out "~20a ~18a ~5a ~9a ~10a ~6@a ~6@a ~10@a ~10@a ~a~%"
         "problem" "strategy" "" "cost" "optimal-p" "solves" "final" "decisions" "conflicts" "seconds")
    (dolist (problem (chain-problems))
      (destructuring-bind (label objective optimum observed) problem
        (dolist (strategy '(:core-guided :linear-totalizer))
          (dolist (b (list *backend* *cdcl*))
            (let ((key (if (eq b *cdcl*) :cdcl :dpll)))
              (multiple-value-bind (seconds solves finals decisions model cost optimal-p)
                  (counted-on (b) (sat:minimize formula objective :backend b :strategy strategy
                                                                  :timeout *chain-timeout*))
                (out "~20a ~(~18s~) ~(~5a~) ~9a ~10a ~6d ~6d ~10d ~10d ~,3f~%"
                     label strategy key (format nil "~s" cost) optimal-p solves finals decisions
                     (total b :conflicts) seconds)
                (let* ((labels (model-labels model))
                       (actual (and (listp labels)
                                    (if observed (labelling-cost labels observed) (lex-costs-of labels))))
                       (name (format nil "~a ~(~s ~a~)" label strategy key)))
                  (if observed
                      (check (and actual (<= actual cost)) t (format nil "~a: a valid labelling" name))
                      (check (and actual (= (first actual) (first cost)) (<= (second actual) (second cost))) t
                             (format nil "~a: the lex costs of the labelling" name)))
                  (if optimal-p
                      (check cost optimum (format nil "~a = DP optimum" name))
                      (check (cost-no-better-p cost optimum) t (format nil "~a: incumbent no better than DP" name)))
                  (when (eq key :cdcl)
                    (check optimal-p t (format nil "~a proven within ~d s" name *chain-timeout*))))))))))
    (out "~%DP optima: ~d, ~d and ~s. A proven run (optimal-p T) found the DP optimum;~%~
          a stopped one returned a valid incumbent no better than it.~%~
          On DPLL, :core-guided gets every assumption back from BACKEND-FINAL-CONFLICT~%~
          (sat-protocol's default method) and shrinks it by deletion: hundreds of calls.~%~
          On CDCL the final conflict is already small, and each call is one search from~%~
          scratch (no incremental sessions in 0.1.0). The :linear-totalizer refutations~%~
          that take DPLL past the timeout (~a, sections 5 and 6)~%~
          take CDCL a few hundred conflicts (the conflicts column).~%"
         (chain-dp) (chain-dp *observed-2*) (third (third (chain-problems))) *phase-issue*)))

;;; 10. Final-conflict cores ---------------------------------------------------------

(defun guarded-instance (&key (seed 5400) (noise-vars 40) (noise-clauses 168) (pigeons 5) (holes 4))
  "Planted 3-SAT noise, then PHP(PIGEONS, HOLES) on fresh variables; clause i
also gets the literal -s_i of a selector variable s_i, so assuming s_i switches
the clause on. (values cnf selectors php-selectors), the selectors in clause
order (noise first)."
  (let* ((noise (planted-3sat (make-rng seed) noise-vars noise-clauses))
         (php (multiple-value-bind (clauses n) (pigeonhole pigeons holes)
                (declare (ignore n))
                (shift-clauses clauses noise-vars)))
         (n (+ noise-vars (* pigeons holes)))
         (clauses (append noise php))
         (selectors (loop for i from 1 to (length clauses) collect (+ n i))))
    (values (clauses->cnf (loop for c in clauses for s in selectors collect (cons (- s) c))
                          (+ n (length clauses)))
            selectors
            (last selectors (length php)))))

(defun core-row (label backend cnf assumptions)
  "BACKEND-FINAL-CONFLICT on BACKEND (counted): print and return the core."
  (multiple-value-bind (seconds solves finals decisions core unsat)
      (counted-on (backend) (sat:backend-final-conflict backend cnf assumptions))
    (out "  ~32a ~4d of ~4d  ~6d ~6d ~9d ~,3f~%" label (length core) (length assumptions)
         solves finals decisions seconds)
    (check unsat t (format nil "~a: unsat" label))
    (check (subsetp core assumptions) t (format nil "~a: within the assumptions" label))
    (check (nth-value 1 (sat:backend-solve (make-instance 'cdcl:cdcl-backend) cnf :assumptions core)) nil
           (format nil "~a: CNF plus the core is unsatisfiable" label))
    core))

(defun demo-final-conflict ()
  (let* ((formula (chain-formula))
         (cnf (sat:to-cnf formula))
         (pins (loop for (p . l) in *pins* collect (x p l)))
         (ints (mapcar (lambda (lit) (sat:cnf-literal cnf lit)) pins))
         (header (lambda ()
                   (out "  ~32a ~12a  ~6@a ~6@a ~9@a ~a~%" "backend" "core" "solves" "final" "decisions"
                        "seconds"))))
    (out "BACKEND-FINAL-CONFLICT on an assumption set that is unsatisfiable. sat-backend-dpll~%~
          keeps sat-protocol's default method: one BACKEND-SOLVE, and on UNSAT every~%~
          assumption. sat-backend-cdcl returns the final conflict of its own search~%~
          (MiniSat's analyzeFinal): the assumptions reached from the failed one through~%~
          reasons. One call, no deletion.~%~%~
          The six level pins of section 7 (~{~a~^ ~}):~%"
         (mapcar #'pin-label pins))
    (funcall header)
    (let ((dpll-core (core-row "sat-backend-dpll" *backend* cnf ints))
          (cdcl-core (core-row "sat-backend-cdcl" *cdcl* cnf ints)))
      (out "  DPLL: ~{~a~^ ~}~%  CDCL: ~{~a~^ ~}~%"
           (mapcar (lambda (i) (pin-label (sat:sat-var (sat:cnf-variable-name cnf i)))) dpll-core)
           (mapcar (lambda (i) (pin-label (sat:sat-var (sat:cnf-variable-name cnf i)))) cdcl-core))
      (check (length dpll-core) 6 "DPLL final conflict: every pin")
      (check (mapcar (lambda (i) (pin-label (sat:sat-var (sat:cnf-variable-name cnf i)))) cdcl-core)
             '("x3=0" "x5=2") "CDCL final conflict: the minimal core of section 7"))
    (out "~%Through the front end, on both backends: UNSAT-CORE :minimal nil maps the final~%~
          conflict back; the minimal UNSAT-CORE shrinks by deletion.~%")
    (dolist (b (list *backend* *cdcl*))
      (dolist (minimal '(nil t))
        (multiple-value-bind (seconds solves finals decisions core unsat)
            (counted-on (b) (sat:unsat-core formula :backend b :assumptions pins :minimal minimal))
          (declare (ignore unsat))
          (out "  ~(~a~) :minimal ~3a -> ~33a ~3d solves ~3d final ~5d decisions ~,3f s~%"
               (if (eq b *cdcl*) :cdcl :dpll) minimal (format nil "~{~a~^ ~}" (mapcar #'pin-label core))
               solves finals decisions seconds)
          (when (or minimal (eq b *cdcl*))
            (check (mapcar #'pin-label core) '("x3=0" "x5=2")
                   (format nil "unsat-core :minimal ~a on ~a" minimal (if (eq b *cdcl*) "cdcl" "dpll")))))))
    (multiple-value-bind (cnf selectors php-selectors) (guarded-instance)
      (out "~%A larger set: 168 planted 3-SAT clauses over 40 variables (satisfiable) and~%~
            PHP(5,4) (45 clauses, 20 variables), each clause guarded by a selector; the~%~
            assumptions switch on all ~d clauses, noise first. PHP(5,4) is minimally~%~
            unsatisfiable, so the only minimal core is its 45 selectors.~%"
           (length selectors))
      (funcall header)
      (core-row "sat-backend-dpll" *backend* cnf selectors)
      (let ((core (core-row "sat-backend-cdcl" *cdcl* cnf selectors)))
        (check (sort (copy-list core) #'<) php-selectors "CDCL core = the 45 pigeonhole selectors")
        (out "  The CDCL core is the ~d pigeonhole selectors~:[ and more~;, exactly~]: the noise~%~
              ~2@Tclauses share no variable with the pigeonhole, so no reason leads to them.~%"
             (length core) (equal (sort (copy-list core) #'<) php-selectors)))
      (multiple-value-bind (seconds solves finals decisions core)
          (counted-on (*cdcl*) (sat:backend-unsat-core *cdcl* cnf selectors))
        (out "  The minimal core by deletion on CDCL (BACKEND-UNSAT-CORE): ~d selectors after~%~
              ~2@T~d solves, ~d decisions, ~,3f s.~%" (length core) solves decisions seconds)
        (check (sort (copy-list core) #'<) php-selectors "deletion core = the 45 pigeonhole selectors")
        (check finals 0 "deletion makes no final-conflict call")))))

;;; 11. CDCL parameters ----------------------------------------------------------------

(defun luby (k)
  "The K-th term (K >= 1) of the Luby sequence 1 1 2 1 1 2 4 ... (MiniSat's luby)."
  (let ((x (1- k)) (size 1) (seq 0))
    (loop while (< size (1+ x)) do (incf seq) (setf size (1+ (* 2 size))))
    (loop until (= (1- size) x)
          do (setf size (ash (1- size) -1)) (decf seq) (setf x (mod x size)))
    (expt 2 seq)))

(defun luby-restarts (unit learnt)
  "R with S(R) <= LEARNT < S(R+1), S(r) = UNIT * (luby(1) + ... + luby(r))."
  (loop for r from 0
        for s = 0 then (+ s (* unit (luby r)))
        when (> (+ s (* unit (luby (1+ r)))) learnt) return r))

(defun scheduled-reductions (base increment learnt)
  "#{k >= 1 : k*BASE + INCREMENT*k*(k-1)/2 <= LEARNT}; 0 when BASE is NIL."
  (if base
      (loop for k from 1 while (<= (+ (* k base) (/ (* increment k (1- k)) 2)) learnt) count t)
      0))

(defparameter *parameter-runs*
  '(("defaults (:luby, phase saving)" ())
    (":restart-policy :glucose" (:restart-policy :glucose))
    (":restart-policy nil" (:restart-policy nil))
    (":luby-unit 32" (:luby-unit 32))
    (":phase-saving nil" (:phase-saving nil))
    (":reduce-base 500 :increment 100" (:reduce-base 500 :reduce-increment 100))
    (":reduce-base nil (never)" (:reduce-base nil))
    (":clause-minimization nil" (:clause-minimization nil))))

(defun parameter-instance ()
  "(values label clauses nvars): the first UNSAT 3-SAT instance at 200 variables."
  (let ((rng (make-rng *3sat-seed*)) (m (round (* *ratio* 200))))
    (loop for i below *3sat-per-size*
          for clauses = (random-3sat rng 200 m)
          when (null (nth-value 1 (sat:solve (clauses->cnf clauses 200)
                                             :backend (make-instance 'cdcl:cdcl-backend))))
            return (values (format nil "3-SAT n=200 #~d (UNSAT)" i) clauses 200))))

(defun demo-parameters ()
  (multiple-value-bind (label clauses n) (parameter-instance)
    (let ((cnf (clauses->cnf clauses n)))
      (out "One instance, ~a: ~d variables, ~d clauses, under each~%~
            parameter change from the defaults (luby-unit 100, restart-window 50, margin 0.8,~%~
            reduce-base 2000, reduce-increment 300, :recursive minimisation).~%~%"
           label n (length clauses))
      (out "~34a ~7a ~9@a ~9@a ~8@a ~8@a ~7@a ~7@a ~a~%" "parameters" "answer" "decisions" "conflicts" "learnt"
           "restarts" "reduce" "deleted" "seconds")
      (dolist (run *parameter-runs*)
        (destructuring-bind (name initargs) run
          (let ((backend (apply #'make-instance 'cdcl:cdcl-backend initargs)))
            (multiple-value-bind (seconds model sat s) (solve-within backend cnf *compare-limit*)
              (out "~34a ~7a ~9d ~9d ~8d ~8d ~7d ~7d ~,3f~%" name (answer-label model sat)
                   (getf s :decisions) (getf s :conflicts) (getf s :learnt-clauses) (getf s :restarts)
                   (getf s :reductions) (getf s :deleted-clauses) seconds)
              (check (answer-label model sat) "UNSAT" (format nil "~a: UNSAT" name))
              (let ((learnt (getf s :learnt-clauses)) (restarts (getf s :restarts)))
                ;; The schedules of src/search.lisp, recomputed from the learnt-clause count.
                (check restarts
                       (ecase (cdcl:cdcl-backend-restart-policy backend)
                         (:luby (luby-restarts (cdcl:cdcl-backend-luby-unit backend) learnt))
                         ((nil) 0)
                         (:glucose restarts))
                       (format nil "~a: restarts on schedule" name))
                (when (eq (cdcl:cdcl-backend-restart-policy backend) :glucose)
                  (check (<= restarts (floor learnt (cdcl:cdcl-backend-restart-window backend))) t
                         (format nil "~a: at most one restart per window" name)))
                (check (getf s :reductions)
                       (scheduled-reductions (cdcl:cdcl-backend-reduce-base backend)
                                             (cdcl:cdcl-backend-reduce-increment backend) learnt)
                       (format nil "~a: reductions on schedule" name))
                (check (- (getf s :conflicts) learnt) 1
                       (format nil "~a: one final level-0 conflict" name))))))))
    (out "~%The restart and reduction counts are checked against the schedules of~%~
          src/search.lisp, recomputed here from the learnt-clause count: :luby makes R~%~
          restarts with S(R) <= learnt < S(R+1); :glucose at most one per 50-clause window;~%~
          the k-th reduction is due at k*base + increment*k(k-1)/2 learnt clauses.~%")))

;;; 12. Determinism ------------------------------------------------------------------

(defparameter *statistics-keys*
  '(:decisions :propagations :conflicts :learnt-clauses :backjumps :restarts :reductions :deleted-clauses)
  "CDCL-BACKEND-STATISTICS's keys, in the order of src/backend.lisp.")

(defun print-statistics (plist)
  "PLIST's counters on two indented lines, in *STATISTICS-KEYS* order."
  (loop for keys in (list (subseq *statistics-keys* 0 4) (subseq *statistics-keys* 4))
        do (out "    ~{~{~(~a~) ~d~}~^, ~}~%"
                (mapcar (lambda (k) (list k (getf plist k 0))) keys))))

(defun all-equal-p (list)
  (every (lambda (e) (equal e (first list))) list))

(defun demo-determinism ()
  (out "No randomness, no hash order, no addresses: the same CNF, arguments and~%~
        parameters give the same answer, the same learnt clauses and the same~%~
        statistics, on one backend called again or on a fresh one. Each instance runs~%~
        three times on one backend and once on a new one; a :learnt-hook records~%~
        every learnt clause (literals, asserting literal first, and LBD).~%~%")
  (let ((instances (list (let ((rng (make-rng *3sat-seed*)))
                           (list "3-SAT n=200 #0" (random-3sat rng 200 (round (* *ratio* 200))) 200))
                         (multiple-value-bind (clauses n) (hidden-core (make-rng *hidden-core-seed*))
                           (list "hidden PHP(6,5) core" clauses n)))))
    (loop for (label clauses n) in instances
          do (let* ((cnf (clauses->cnf clauses n))
                    (learnt '())        ; one list per run, newest run first
                    (hook (lambda (literals lbd) (push (cons lbd literals) (first learnt))))
                    (shared (make-instance 'cdcl:cdcl-backend :learnt-hook hook))
                    (runs (loop for backend in (list shared shared shared
                                                     (make-instance 'cdcl:cdcl-backend :learnt-hook hook))
                                collect (progn
                                          (push '() learnt)
                                          (multiple-value-bind (model sat) (sat:solve cnf :backend backend)
                                            (list model sat
                                                  (copy-list (cdcl:cdcl-backend-statistics backend)))))))
                    (clauses-of-run (mapcar #'reverse (reverse learnt)))
                    (same-answers (all-equal-p (mapcar (lambda (r) (subseq r 0 2)) runs)))
                    (same-statistics (all-equal-p (mapcar #'third runs)))
                    (same-learnt (all-equal-p clauses-of-run)))
               (out "~a: ~a; statistics of the first run:~%" label (if (second (first runs)) "SAT" "UNSAT"))
               (print-statistics (third (first runs)))
               (out "  the 4 runs: same answer and model ~:[no~;yes~], same statistics ~:[no~;yes~], ~
                     same ~d learnt clauses ~:[no~;yes~]~%"
                    same-answers same-statistics (length (first clauses-of-run)) same-learnt)
               (loop for (lbd . literals) in (first clauses-of-run) for i from 1 to 2
                     do (out "    learnt clause ~d: LBD ~d, ~{~d~^ ~}~%" i lbd literals))
               (check same-answers t (format nil "~a: same answer and model" label))
               (check same-statistics t (format nil "~a: same statistics" label))
               (check same-learnt t (format nil "~a: same learnt clauses" label)))))
  (let* ((formula (chain-formula)) (objective (chain-objective))
         (runs (loop repeat 3
                     collect (multiple-value-bind (seconds solves finals decisions model cost optimal-p)
                                 (counted-on (*cdcl*) (sat:minimize formula objective :backend *cdcl*
                                                                                      :strategy :core-guided))
                               (declare (ignore seconds decisions))
                               (list model cost optimal-p solves finals (copy-list (totals *cdcl*)))))))
    (destructuring-bind (model cost optimal-p solves finals totals) (first runs)
      (declare (ignore model))
      (out "~%MINIMIZE :core-guided on the chain of section 3, 3 times (counting backend):~%~
            ~2@Tcost ~d, optimal-p ~a, ~d solves and ~d final-conflict calls; summed statistics:~%"
           cost optimal-p solves finals)
      (print-statistics totals)
      (out "  the 3 runs: same model, calls and statistics ~:[no~;yes~]~%" (all-equal-p runs))
      (check (all-equal-p runs) t "core-guided runs identical"))))

;;; Entry point ----------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 110)
        (*backend* (make-instance 'counting-dpll))
        (*cdcl* (make-instance 'counting-cdcl))
        ;; No default backend: every call below names its own.
        (sat:*sat-backend* nil))
    (format t "~&sat-protocol ~a on sat-backend-dpll ~a and sat-backend-cdcl ~a (~a ~a)~%"
            (asdf:component-version (asdf:find-system "sat-protocol"))
            (asdf:component-version (asdf:find-system "sat-backend-dpll"))
            (asdf:component-version (asdf:find-system "sat-backend-cdcl"))
            (lisp-implementation-type) (lisp-implementation-version))
    (section "1. Cardinality constraints and their encodings" (demo-cardinality))
    (section "1b. Pseudo-Boolean constraints: a knapsack" (demo-pseudo-boolean))
    (section "2. N-queens" (demo-queens))
    (section "3. Weighted sequence labelling: every MINIMIZE strategy" (demo-chain))
    (section "4. Anytime: budgets, timeouts, SAT-BUDGET-EXCEEDED and CONTINUE" (demo-anytime))
    (section "5. Hints: a warm start from a previous model" (demo-hints))
    (section "6. Lexicographic objectives" (demo-lex))
    (section "7. Final-conflict cores and minimal cores" (demo-cores))
    (section "8. Conditions and restarts" (demo-conditions))
    (section "9. DPLL against CDCL: random 3-SAT, pigeonhole, a hidden core" (demo-comparison))
    (section "9b. DPLL against CDCL: the chains under :core-guided and :linear-totalizer"
      (demo-chain-comparison))
    (section "10. Final-conflict cores from conflict analysis" (demo-final-conflict))
    (section "11. CDCL parameters: restarts, phase saving, reduction" (demo-parameters))
    (section "12. Determinism" (demo-determinism))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
