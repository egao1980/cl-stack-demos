;;;; A tour of sat-protocol 0.3.0's incremental sessions (Track L, slice L2b-2)
;;;; on sat-backend-cdcl 0.1.0. That backend has no native session yet, so every
;;;; session here runs on the protocol's default (stateless) methods: each call
;;;; re-solves the session CNF from scratch with the backend's ordinary GFs.
;;;;
;;;; One instance runs through the tour: a chain of 50 segments, each at one of
;;;; three levels, adjacent levels at most one apart, with an observed level per
;;;; segment. Every session answer is printed next to the fresh front-end call
;;;; on (SESSION-CNF session) and, where it has one, a dynamic-programming
;;;; oracle written here. RUN-DEMOS returns NIL when a check fails or a section
;;;; stops on an error.

(in-package #:harness-demos-sat-sessions)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args))

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (actual expected label &key (test #'equal))
  "Count one self-check; print it only when it fails. Returns ACTUAL."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (let ((*print-pretty* nil))
      (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected)))
  actual)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

(defun seconds-since (start)
  (/ (float (- (get-internal-real-time) start) 1d0) internal-time-units-per-second))

(defmacro timed (&body body)
  "(values seconds . BODY's values)."
  (let ((start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (let ((values (multiple-value-list (progn ,@body))))
         (values-list (cons (seconds-since ,start) values))))))

(defun yes (x) (if x "yes" "NO"))

;;; The chain ----------------------------------------------------------------------
;;; Segment i (0..49) takes one level 0..2: three session variables, exactly one
;;; true (SAT-EXACTLY-ONE :PAIRWISE, a formula the session encodes, no
;;; auxiliaries). Adjacent segments differ by at most one level: for each link,
;;; the two clauses (-x[i-1,0] -x[i,2]) and (-x[i-1,2] -x[i,0]).

(defconstant +segments+ 50)
(defconstant +levels+ 3)

(defparameter *observed*
  (let ((state 2026))
    (coerce (loop repeat +segments+
                  do (setf state (mod (+ (* 1103515245 state) 12345) (expt 2 31)))
                  collect (mod (ash state -16) +levels+))
            'vector))
  "The observed level of each segment, from a fixed linear congruential generator.")

(defvar *backend* nil "The sat-backend-cdcl instance of the run.")

(defstruct (chain (:constructor %make-chain (session)))
  session
  (vars (make-array +segments+ :initial-element nil))
  (length 0))

(defun x (chain i level)
  "The session variable of segment I at LEVEL."
  (aref (aref (chain-vars chain) i) level))

(defun add-segment (chain)
  "Add the next segment: its 3 variables, its exactly-one formula, its link."
  (let* ((s (chain-session chain))
         (i (chain-length chain))
         (indices (sat:session-add-variables s +levels+)))
    (setf (aref (chain-vars chain) i) (coerce indices 'vector))
    (sat:session-add-clauses s (sat:sat-exactly-one (mapcar #'sat:sat-var indices)
                                                    :encoding :pairwise))
    (when (plusp i)
      (sat:session-add-clauses s (list (list (- (x chain (1- i) 0)) (- (x chain i 2)))
                                       (list (- (x chain (1- i) 2)) (- (x chain i 0))))))
    (incf (chain-length chain))
    chain))

(defun pin (chain i level)
  "The assumption literal: segment I at LEVEL."
  (x chain i level))

(defun pin-not (chain i level)
  "The assumption literal: segment I not at LEVEL."
  (- (x chain i level)))

(defun levels (chain model)
  "The level string of MODEL (integer literals): one digit per segment, ? when
not exactly one level is true."
  (with-output-to-string (s)
    (dotimes (i (chain-length chain))
      (let ((true (loop for l below +levels+ when (member (x chain i l) model) collect l)))
        (write-char (if (= 1 (length true)) (digit-char (first true)) #\?) s)))))

(defun observed-string (&optional (n +segments+))
  (map 'string #'digit-char (subseq *observed* 0 n)))

;;; Objectives (index level) ------------------------------------------------------

(defun requests (chain)
  "Cost 1 for each segment not at its observed level."
  (loop for i below (chain-length chain)
        collect (cons 1 (- (x chain i (aref *observed* i))))))

(defun effort (chain)
  "Level 1 costs 1, level 2 costs 2."
  (loop for i below (chain-length chain)
        collect (cons 1 (x chain i 1))
        collect (cons 2 (x chain i 2))))

(defun lex-objective (chain)
  (list :lex (requests chain) (effort chain)))

;;; The fresh front-end calls on (SESSION-CNF session): name level ---------------
;;; A user variable i of the session CNF is named i.

(defun lit-formula (literal)
  (if (plusp literal) (sat:sat-var literal) (sat:sat-not (sat:sat-var (- literal)))))

(defun name-objective (objective)
  (flet ((plain (level) (mapcar (lambda (e) (cons (car e) (lit-formula (cdr e)))) level)))
    (if (and (consp objective) (eq (first objective) :lex))
        (cons :lex (mapcar #'plain (rest objective)))
        (plain objective))))

(defun model-literals (model)
  "A front-end model ((name . boolean) ...) as integer literals."
  (mapcar (lambda (cell) (if (cdr cell) (car cell) (- (car cell)))) model))

(defun fresh-solve (chain assumptions)
  "(values literals sat) of SOLVE on a fresh copy of the session CNF."
  (multiple-value-bind (model sat)
      (sat:solve (sat:session-cnf (chain-session chain))
                 :assumptions (mapcar #'lit-formula assumptions) :backend *backend*)
    (values (and sat (model-literals model)) sat)))

(defun fresh-minimize (chain objective assumptions &rest keys)
  "(values literals cost optimal-p) of MINIMIZE on a fresh copy of the session CNF."
  (multiple-value-bind (model cost optimal-p)
      (apply #'sat:minimize (sat:session-cnf (chain-session chain)) (name-objective objective)
             :assumptions (mapcar #'lit-formula assumptions) :backend *backend* keys)
    (values (and cost (model-literals model)) cost optimal-p)))

;;; Independent checks ---------------------------------------------------------------

(defun satisfies-p (model clauses assumptions)
  "Every clause has a literal of MODEL, and MODEL holds every assumption."
  (and (every (lambda (c) (some (lambda (l) (member l model)) c)) clauses)
       (every (lambda (a) (member a model)) assumptions)))

(defun model-cost (objective model)
  (flet ((plain (level) (loop for (w . l) in level when (member l model) sum w)))
    (if (eq (first objective) :lex)
        (mapcar #'plain (rest objective))
        (plain objective))))

(defun lex< (a b)
  (loop for x in a for y in b
        do (cond ((< x y) (return t))
                 ((> x y) (return nil)))
        finally (return nil)))

(defun allowed-p (chain assumptions i level)
  "Do ASSUMPTIONS allow segment I at LEVEL?"
  (loop for a in assumptions
        always (loop for l below +levels+
                     for v = (x chain i l)
                     always (cond ((= a v) (= l level))
                                  ((= a (- v)) (/= l level))
                                  (t t)))))

(defun oracle (chain assumptions)
  "The least (requests effort) over level sequences of the chain under
ASSUMPTIONS, by dynamic programming over segments; NIL when none exists."
  (let ((best (make-array +levels+ :initial-element nil)))
    (dotimes (i (chain-length chain))
      (let ((next (make-array +levels+ :initial-element nil)))
        (dotimes (m +levels+)
          (when (allowed-p chain assumptions i m)
            (let ((own (list (if (= m (aref *observed* i)) 0 1) m)))
              (if (zerop i)
                  (setf (aref next m) own)
                  (loop for l from (max 0 (1- m)) to (min (1- +levels+) (1+ m))
                        for prev = (aref best l)
                        when prev
                          do (let ((total (mapcar #'+ prev own)))
                               (when (or (null (aref next m)) (lex< total (aref next m)))
                                 (setf (aref next m) total))))))))
        (setf best next)))
    (let ((answers (remove nil (coerce best 'list))))
      (when answers
        (reduce (lambda (a b) (if (lex< b a) b a)) answers)))))

(defun feasible-p (chain assumptions)
  (and (oracle chain assumptions) t))

;;; 1. Building the chain ---------------------------------------------------------------

(defun make-chain (session)
  (%make-chain session))

(defun build-chain (session &optional (segments +segments+) (report nil))
  (let ((chain (make-chain session)))
    (dotimes (k segments chain)
      (add-segment chain)
      (when (member (chain-length chain) report)
        (let ((cnf (sat:session-cnf session)))
          (out "  ~2d segments: ~3d variables, ~3d clauses~%"
               (chain-length chain) (sat:session-variable-count session)
               (sat:cnf-clause-count cnf)))))))

(defvar *chain* nil "The 50-segment chain of sections 1 to 4, in an open session.")

(defun demo-build ()
  (out "WITH-SAT-SESSION on ~a; segments added one at a time, each with~%~
        SESSION-ADD-VARIABLES 3, SESSION-ADD-CLAUSES of (sat-exactly-one ... :encoding :pairwise)~%~
        and, from the second segment on, the two link clauses:~%~%" *backend*)
  (setf *chain* (build-chain (chain-session *chain*) +segments+ '(1 2 10 25 50)))
  (let* ((s (chain-session *chain*))
         (cnf (sat:session-cnf s)))
    (out "~%  session-backend-state ~s: a stateless session (no native session in this backend)~%"
         (sat:session-backend-state s))
    (out "  session-cnf: ~d variables (~d auxiliary), ~d clauses = 50 x 4 exactly-one + 49 x 2 links~%"
         (sat:cnf-variable-count cnf)
         (loop for i from 1 to (sat:cnf-variable-count cnf) count (sat:cnf-aux-variable-p cnf i))
         (sat:cnf-clause-count cnf))
    (check (sat:session-variable-count s) 150 "150 variables")
    (check (sat:cnf-clause-count cnf) (+ (* 50 4) (* 49 2)) "298 clauses")
    (check (sat:session-backend-state s) nil "stateless")
    (out "  the observed levels: ~a~%" (observed-string))))

;;; 2. SESSION-SOLVE under changing assumptions --------------------------------------

(defun describe-assumptions (chain assumptions)
  (if (null assumptions)
      "(none)"
      (format nil "~{~a~^ ~}"
              (loop for a in assumptions
                    collect (block found
                              (dotimes (i (chain-length chain))
                                (dotimes (l +levels+)
                                  (when (= (abs a) (x chain i l))
                                    (return-from found
                                      (format nil "s~d~a~d" i (if (plusp a) "=" "/=") l))))))))))

(defun assumption-sets (chain)
  (flet ((p (i l) (pin chain i l))
         (n (i l) (pin-not chain i l)))
    (list '()
          (list (p 0 0))
          (list (p 0 0) (p 49 2))
          (list (p 0 0) (p 49 2) (p 25 0))
          (list (p 0 2) (p 1 0))
          (list (p 10 0) (p 12 2))
          (list (p 10 0) (p 11 2))
          (list (n 24 0) (n 24 1) (p 23 1))
          (list (n 24 0) (n 24 1) (n 24 2)))))

(defun demo-solve ()
  (let* ((chain *chain*)
         (s (chain-session chain))
         (clauses (sat:cnf-clauses (sat:session-cnf s))))
    (out "Each SESSION-SOLVE next to SOLVE on (session-cnf s) with the same assumptions as~%~
          literal formulas, on the same backend. \"oracle\" is the DP over level sequences.~%~%")
    (out "  assumptions          session  fresh  oracle  same model  model checks  levels~%")
    (dolist (assumptions (assumption-sets chain))
      (multiple-value-bind (model sat) (sat:session-solve s :assumptions assumptions)
        (multiple-value-bind (fresh fresh-sat) (fresh-solve chain assumptions)
          (let ((oracle (feasible-p chain assumptions))
                (ok (or (not sat) (satisfies-p model clauses assumptions))))
            (out "  ~20a ~7a  ~5a  ~6a  ~10a  ~12a  ~a~%"
                 (describe-assumptions chain assumptions)
                 (if sat "SAT" "UNSAT") (if fresh-sat "SAT" "UNSAT") (if oracle "SAT" "UNSAT")
                 (yes (equal model fresh)) (if sat (yes ok) "-")
                 (if sat (levels chain model) ""))
            (check sat fresh-sat "session status = fresh status")
            (check (and sat t) oracle "session status = oracle")
            (check model fresh "stateless: the session's model is SOLVE's")
            (check ok t "the model satisfies the clauses and the assumptions")))))
    (out "~%  after the last (UNSAT) call: session-model ~s, session-unsat-core ~a~%"
         (multiple-value-list (sat:session-model s))
         (describe-assumptions chain (sat:session-unsat-core s)))))

;;; 3. SESSION-MINIMIZE with (:lex requests effort) -----------------------------------

(defparameter *strategies* '(:linear-bdd :linear-totalizer :stratified :core-guided))

(defun demo-minimize ()
  (let* ((chain *chain*)
         (s (chain-session chain))
         (objective (lex-objective chain)))
    (out "Objective (:lex requests effort): first the segments not at their observed level~%~
          (50 literals of weight 1), then the effort (level 1 costs 1, level 2 costs 2).~%~
          The oracle is the DP over level sequences with lexicographic sums.~%~%")
    (out "  strategy           assumptions     session cost  opt  fresh cost  opt  oracle    same model  time s  fresh s~%")
    (flet ((row (strategy assumptions)
             (multiple-value-bind (seconds model cost optimal-p)
                 (timed (sat:session-minimize s objective :assumptions assumptions :strategy strategy))
               (multiple-value-bind (fresh-seconds fresh fresh-cost fresh-optimal-p)
                   (timed (fresh-minimize chain objective assumptions :strategy strategy))
                 (let ((oracle (oracle chain assumptions)))
                   (out "  ~17s  ~14a  ~12a  ~3a  ~10a  ~3a  ~8a  ~10a  ~6,3f  ~7,3f~%"
                        strategy (describe-assumptions chain assumptions)
                        cost (if optimal-p "T" "-") fresh-cost (if fresh-optimal-p "T" "-")
                        oracle (yes (equal model fresh)) seconds fresh-seconds)
                   (check cost fresh-cost "session cost = fresh cost")
                   (check optimal-p fresh-optimal-p "session OPTIMAL-P = fresh")
                   (check cost oracle "cost = oracle")
                   (when cost
                     (check (model-cost objective model) cost "the model has its cost")
                     (check (satisfies-p model (sat:cnf-clauses (sat:session-cnf s)) assumptions) t
                            "the model satisfies the clauses"))
                   (values model cost))))))
      (let ((first-model nil) (first-cost nil))
        (dolist (strategy *strategies*)
          (multiple-value-bind (model cost) (row strategy '())
            (unless first-model (setf first-model model first-cost cost))))
        (out "~%  :linear-bdd's model:~%  levels   ~a  ~s~%  observed ~a~%~%"
             (levels chain first-model) first-cost (observed-string)))
      (row :linear-bdd (list (pin chain 0 2)))
      (row :core-guided (list (pin chain 0 2) (pin chain 49 0)))
      (row :stratified (list (pin chain 30 2) (pin chain 31 0))))))

;;; 4. SESSION-UNSAT-CORE --------------------------------------------------------------

(defun demo-core ()
  (let* ((chain *chain*)
         (s (chain-session chain))
         (assumptions (list (pin chain 20 1) (pin chain 5 0) (pin chain 30 1)
                            (pin-not chain 3 1) (pin chain 6 2) (pin chain 40 0))))
    (out "Six assumptions; s5=0 and s6=2 cannot hold together (adjacent levels two apart).~%~%")
    (out "  assumptions: ~a~%" (describe-assumptions chain assumptions))
    (multiple-value-bind (model sat) (sat:session-solve s :assumptions assumptions)
      (out "  (session-solve s :assumptions ...)          -> ~s ~s~%" model sat)
      (check sat nil "UNSAT"))
    (let ((stored (sat:session-unsat-core s)))
      (out "  (session-unsat-core s)                      -> ~a~%"
           (describe-assumptions chain stored))
      (out "     the backend's final conflict through the default BACKEND-SESSION-FINAL-CONFLICT~%")
      (check (fresh-solve chain stored) nil "the stored core is unsatisfiable"))
    (multiple-value-bind (core found) (sat:session-unsat-core s :minimal t)
      (let* ((formulas (mapcar #'lit-formula assumptions))
             (fresh (sat:unsat-core (sat:session-cnf s) :assumptions formulas :backend *backend*))
             (fresh-literals (loop for f in fresh collect (nth (position f formulas) assumptions))))
        (out "  (session-unsat-core s :minimal t)           -> ~a ~s~%"
             (describe-assumptions chain core) found)
        (out "  (unsat-core (session-cnf s) :assumptions ...) -> ~a~%"
             (describe-assumptions chain fresh-literals))
        (check core fresh-literals "minimal core = fresh UNSAT-CORE")
        (check core (list (pin chain 5 0) (pin chain 6 2)) "the core is s5=0 s6=2")
        (check (feasible-p chain core) nil "the core is infeasible (oracle)")
        (check (loop for i below (length core)
                     always (feasible-p chain (append (subseq core 0 i) (subseq core (1+ i)))))
               t "dropping any element makes it feasible (oracle)")
        (out "  the oracle: the core is infeasible, and dropping either pin makes it feasible~%")))
    (out "  session-model after the UNSAT call: ~s~%" (multiple-value-list (sat:session-model s)))
    (sat:session-solve s)
    (out "  after a SAT call, session-unsat-core: ~s~%" (multiple-value-list (sat:session-unsat-core s)))
    (check (multiple-value-list (sat:session-unsat-core s)) '(nil nil) "no core after SAT")))

;;; 6. Timing: the session against fresh re-solves ---------------------------------------

(defun step-assumption-sets (chain)
  "Step k's three assumption sets: s0 at its observed level and the newest
segment at each level in turn (some are UNSAT while the chain is short)."
  (let ((k (chain-length chain)))
    (loop for level below +levels+
          collect (remove-duplicates (list (pin chain 0 (aref *observed* 0))
                                           (pin chain (1- k) level))))))

(defvar *rounds* 5 "Timing rounds of section 6.")

(defun timing-round ()
  "One fresh session, 50 steps. Returns a plist of totals (seconds) and counts."
  (let ((add 0d0) (solve 0d0) (fresh-solve 0d0) (minimize 0d0) (fresh-minimize 0d0)
        (solves 0) (solves-agree 0) (sat-solves 0)
        (minimizes 0) (minimizes-agree 0) (models-equal 0))
    (sat:with-sat-session (s :backend *backend*)
      (let ((chain (make-chain s)))
        (dotimes (k +segments+)
          (incf add (timed (add-segment chain)))
          (dolist (assumptions (step-assumption-sets chain))
            (multiple-value-bind (seconds model sat) (timed (sat:session-solve s :assumptions assumptions))
              (incf solve seconds)
              (multiple-value-bind (seconds fresh fresh-sat) (timed (fresh-solve chain assumptions))
                (incf fresh-solve seconds)
                (incf solves)
                (when (and (eq sat fresh-sat) (equal model fresh)
                           (eq (and sat t) (feasible-p chain assumptions)))
                  (incf solves-agree))
                (when sat (incf sat-solves)))))
          (let ((objective (lex-objective chain)))
            (multiple-value-bind (seconds model cost optimal-p)
                (timed (sat:session-minimize s objective))
              (incf minimize seconds)
              (multiple-value-bind (seconds fresh fresh-cost fresh-optimal-p)
                  (timed (fresh-minimize chain objective '()))
                (incf fresh-minimize seconds)
                (incf minimizes)
                (when (and (equal cost fresh-cost) (eq optimal-p fresh-optimal-p)
                           (equal cost (oracle chain '())))
                  (incf minimizes-agree))
                (when (equal model fresh) (incf models-equal))))))))
    (list :add add :solve solve :fresh-solve fresh-solve :minimize minimize
          :fresh-minimize fresh-minimize :solves solves :solves-agree solves-agree
          :sat-solves sat-solves :minimizes minimizes :minimizes-agree minimizes-agree
          :models-equal models-equal)))

(defun median (values)
  (let ((sorted (sort (copy-list values) #'<)))
    (nth (floor (length sorted) 2) sorted)))

(defun native-session-method-p (backend)
  "Does BACKEND's class define a BACKEND-SESSION-OPEN method of its own? (FIND-METHOD,
for a session argument of class T or SAT-SESSION.)"
  (let ((class (class-of backend)))
    (and (loop for session-class in (list (find-class t) (find-class 'sat:sat-session))
                 thereis (find-method #'sat:backend-session-open '() (list class session-class) nil))
         t)))

(defun ms (seconds) (* 1000 seconds))

(defun demo-timing ()
  (out "50 incremental steps on a fresh session. Step k adds segment k, then runs~%~
        - three SESSION-SOLVEs: s0 at its observed level, the newest segment at level 0, 1, 2;~%~
        - one SESSION-MINIMIZE (:lex requests effort) over the k segments;~%~
        each followed by the same fresh SOLVE / MINIMIZE on (session-cnf s), the copy~%~
        included. ~@(~r~) rounds; milliseconds summed over the 50 steps (150 solves, 50 minimizes).~%~%"
       *rounds*)
  (let ((rounds (loop repeat *rounds* collect (timing-round))))
    (out "  round   add   session-solve  fresh solve  session-minimize  fresh minimize~%")
    (loop for r in rounds
          for i from 1
          do (out "  ~5d  ~5,1f  ~13,1f  ~11,1f  ~16,1f  ~14,1f~%"
                  i (ms (getf r :add)) (ms (getf r :solve)) (ms (getf r :fresh-solve))
                  (ms (getf r :minimize)) (ms (getf r :fresh-minimize))))
    (flet ((med (key) (median (mapcar (lambda (r) (getf r key)) rounds))))
      (let ((solve (med :solve)) (fresh-solve (med :fresh-solve))
            (minimize (med :minimize)) (fresh-minimize (med :fresh-minimize)))
        (out "  median ~5,1f  ~13,1f  ~11,1f  ~16,1f  ~14,1f~%"
             (ms (med :add)) (ms solve) (ms fresh-solve) (ms minimize) (ms fresh-minimize))
        (out "~%  per call, median round: session-solve ~,3f ms against fresh ~,3f ms (ratio ~,2f);~%  ~
              session-minimize ~,2f ms against fresh ~,2f ms (ratio ~,2f)~%"
             (/ (ms solve) 150) (/ (ms fresh-solve) 150) (/ solve fresh-solve)
             (/ (ms minimize) 50) (/ (ms fresh-minimize) 50) (/ minimize fresh-minimize))))
    (let ((r (first rounds)))
      (out "  agreement in every round: ~d/~d solves (status, model and the oracle), ~d of them SAT;~%  ~
            ~d/~d minimizes (cost and OPTIMAL-P against the fresh call and the oracle),~%  ~
            ~d/~d with the fresh call's model~%"
           (getf r :solves-agree) (getf r :solves) (getf r :sat-solves)
           (getf r :minimizes-agree) (getf r :minimizes) (getf r :models-equal) (getf r :minimizes))
      (dolist (r rounds)
        (check (getf r :solves-agree) 150 "solves agree")
        (check (getf r :minimizes-agree) 50 "minimizes agree")))
    (out "~%  sat-backend-cdcl ~a (the newest on GHCR when this ran) defines BACKEND-SESSION-OPEN: ~a~%"
         (asdf:component-version (asdf:find-system "sat-backend-cdcl"))
         (yes (native-session-method-p *backend*)))
    (out "  native sessions: pending. Each session call above re-solved the session CNF from~%  ~
          scratch (the protocol's default methods), so the session and the fresh calls do~%  ~
          the same search; the session saves the CNF copy and the name-level translation.~%  ~
          A stateful backend would keep its learnt clauses from one step to the next.~%")))

;;; 5. Budgets: SAT-BUDGET-REACHED and its restarts ------------------------------------

(defparameter *budget* 100 "The decision budget of section 6.")

(defun demo-budget ()
  (let* ((chain *chain*)
         (s (chain-session chain))
         (objective (lex-objective chain))
         (optimum (oracle chain '())))
    (out "(session-minimize s (:lex requests effort) :budget ~d :strategy :linear-bdd); the~%~
          optimum is ~s. With an incumbent and the budget spent, SESSION-MINIMIZE signals~%~
          SAT-BUDGET-REACHED (a condition, not an error) with USE-BEST, CONTINUE and STORE-VALUE.~%~%"
         *budget* optimum)
    ;; Unhandled: the incumbent.
    (multiple-value-bind (model cost optimal-p) (sat:session-minimize s objective :budget *budget*)
      (declare (ignore model))
      (multiple-value-bind (fresh fresh-cost fresh-optimal-p)
          (fresh-minimize chain objective '() :budget *budget*)
        (declare (ignore fresh))
        (out "  no handler:     cost ~s optimal-p ~s   (fresh MINIMIZE :budget ~d: ~s ~s)~%"
             cost optimal-p *budget* fresh-cost fresh-optimal-p)
        (check (list cost optimal-p) (list fresh-cost fresh-optimal-p) "anytime answer = fresh MINIMIZE's")
        (check optimal-p nil "the budget stops the search")))
    ;; USE-BEST.
    (let ((seen '()))
      (multiple-value-bind (model cost optimal-p)
          (handler-bind ((sat:sat-budget-reached
                           (lambda (c)
                             (push (list (sat:sat-budget-reached-budget c) (sat:sat-budget-reached-cost c)) seen)
                             (sat:use-best c))))
            (sat:session-minimize s objective :budget *budget*))
        (out "  USE-BEST:       signalled ~{at budget ~d with incumbent ~s~}; returns cost ~s optimal-p ~s~%"
             (first seen) cost optimal-p)
        (check (length seen) 1 "one signal")
        (check optimal-p nil "USE-BEST returns the incumbent")
        (check (model-cost objective model) cost "the incumbent has its cost")
        (check (second (first seen)) cost "the signal carries the incumbent's cost")))
    ;; CONTINUE doubles the budget.
    (let ((seen '()))
      (multiple-value-bind (model cost optimal-p)
          (handler-bind ((sat:sat-budget-reached
                           (lambda (c)
                             (push (list (sat:sat-budget-reached-budget c) (sat:sat-budget-reached-cost c)) seen)
                             (continue c))))
            (sat:session-minimize s objective :budget *budget*))
        (setf seen (reverse seen))
        (out "  CONTINUE:       ~d signals:~%" (length seen))
        (loop for (budget incumbent) in seen
              do (out "                    budget ~5d  incumbent ~s~%" budget incumbent))
        (out "                  then cost ~s optimal-p ~s~%" cost optimal-p)
        (check (loop for (a b) on (mapcar #'first seen) while b always (= b (* 2 a))) t
               "each CONTINUE doubles the budget")
        (check (loop for (a b) on (mapcar #'second seen) while b always (not (lex< a b))) t
               "the incumbent never gets worse")
        (check (list cost optimal-p) (list optimum t) "CONTINUE reaches the optimum")
        (check (model-cost objective model) optimum "the model has the optimum cost")))
    ;; STORE-VALUE.
    (multiple-value-bind (model cost optimal-p)
        (handler-bind ((sat:sat-budget-reached (lambda (c) (store-value 100000 c))))
          (sat:session-minimize s objective :budget *budget*))
      (declare (ignore model))
      (out "  STORE-VALUE 100000: cost ~s optimal-p ~s~%" cost optimal-p)
      (check (list cost optimal-p) (list optimum t) "STORE-VALUE reaches the optimum"))
    ;; No model yet: SAT-BUDGET-EXCEEDED, an error.
    (handler-case (sat:session-minimize s objective :budget 0)
      (sat:sat-budget-exceeded (c)
        (out "~%  :budget 0, no model yet: ~s (an error), budget ~s~%"
             (type-of c) (sat:sat-budget-exceeded-budget c))
        (check (type-of c) 'sat:sat-budget-exceeded "SAT-BUDGET-EXCEEDED with no incumbent")))
    (multiple-value-bind (value status)
        (handler-bind ((sat:sat-budget-exceeded (lambda (c) (use-value :gave-up c))))
          (sat:session-minimize s objective :budget 0))
      (out "  with USE-VALUE :gave-up: ~s ~s; session-model then: ~s~%"
           value status (multiple-value-list (sat:session-model s)))
      (check (list value status) '(:gave-up nil) "USE-VALUE returns (values VALUE NIL)"))
    (multiple-value-bind (model sat) (sat:session-solve s :assumptions (list (pin chain 0 0)))
      (out "  the session is still usable: session-solve -> ~a~%" (if sat (levels chain model) "UNSAT"))
      (check sat t "usable after a USE-VALUE exit"))))

;;; 7. Closing ---------------------------------------------------------------------

(defun demo-closed ()
  (let ((escaped nil))
    (sat:with-sat-session (s :backend *backend*)
      (setf escaped s)
      (sat:session-add-variables s 2)
      (sat:session-add-clauses s '((1 2))))
    (out "After WITH-SAT-SESSION exits: session-open-p ~s, close-session again -> ~s~%"
         (sat:session-open-p escaped) (sat:close-session escaped))
    (handler-case (sat:session-solve escaped)
      (sat:sat-session-closed (c)
        (out "  (session-solve s) on it: ~s~%    \"~a\"~%" (type-of c) c)
        (check (type-of c) 'sat:sat-session-closed "SAT-SESSION-CLOSED")))
    (check (sat:session-open-p escaped) nil "closed")))

;;; Run ------------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 120)
        (*backend* (make-instance 'cdcl:cdcl-backend))
        ;; No default backend: every call below names its own.
        (sat:*sat-backend* nil))
    (format t "~&sat-protocol ~a on sat-backend-cdcl ~a (~a ~a)~%"
            (asdf:component-version (asdf:find-system "sat-protocol"))
            (asdf:component-version (asdf:find-system "sat-backend-cdcl"))
            (lisp-implementation-type) (lisp-implementation-version))
    (sat:with-sat-session (s :backend *backend*)
      (setf *chain* (make-chain s))
      (section "1. A 50-segment chain, added to a session one segment at a time" (demo-build))
      (section "2. SESSION-SOLVE under changing assumptions, against fresh SOLVE" (demo-solve))
      (section "3. SESSION-MINIMIZE with (:lex requests effort), against fresh MINIMIZE" (demo-minimize))
      (section "4. SESSION-UNSAT-CORE on a contradictory assumption set" (demo-core))
      (section "5. Budgets: SAT-BUDGET-REACHED with USE-BEST, CONTINUE and STORE-VALUE" (demo-budget)))
    (section "6. Timing: 50 incremental steps, the session against fresh re-solves" (demo-timing))
    (section "7. A closed session: SAT-SESSION-CLOSED" (demo-closed))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
