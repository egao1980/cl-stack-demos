;;;; A tour of the weak constraints of logic-protocol 0.2.0 on logic-backend-datalog
;;;; 0.1.0, and of the bridge from a Datalog model to a weighted MaxSAT problem on
;;;; sat-protocol 0.2.0 / sat-backend-dpll 0.1.0.
;;;;
;;;; Every number printed is computed by this run. The self-checks compare results
;;;; with values worked out by hand in the comments below, and the SAT optimum with a
;;;; brute-force search that uses only logic-protocol (WEAK-INSTANCES and WEAK-COST
;;;; under :ASSUME) as the definition of cost. The summary reports the mismatches.

(in-package #:harness-demos-logic)

;;; Bookkeeping -----------------------------------------------------------------

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
    (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected))
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

;;; MINIMIZE drives the SAT backend through two generic functions; :AROUND methods
;;; on a subclass count the calls each strategy makes.

(defclass counting-dpll (dpll:dpll-backend)
  ((solves :initform 0 :accessor solves)
   (final-conflicts :initform 0 :accessor final-conflicts))
  (:documentation "sat-backend-dpll, counting BACKEND-SOLVE and BACKEND-FINAL-CONFLICT calls."))

(defmethod sat:backend-solve :around ((backend counting-dpll) cnf &key &allow-other-keys)
  (declare (ignore cnf))
  (incf (solves backend))
  (call-next-method))

(defmethod sat:backend-final-conflict :around ((backend counting-dpll) cnf assumptions
                                               &key &allow-other-keys)
  (declare (ignore cnf assumptions))
  (incf (final-conflicts backend))
  (call-next-method))

(defvar *backend* nil "The COUNTING-DPLL instance of this run.")

(defmacro counted (&body body)
  "(values seconds solves final-conflicts . BODY's values)."
  (let ((all (gensym "ALL")))
    `(progn
       (setf (solves *backend*) 0 (final-conflicts *backend*) 0)
       (let ((,all (multiple-value-list (timed ,@body))))
         (values-list (list* (first ,all) (solves *backend*) (final-conflicts *backend*)
                             (rest ,all)))))))

(defun signalled (thunk)
  "The error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (c) c)))

(defun one-line (object)
  "OBJECT printed readably on one line."
  (let ((*print-pretty* nil)) (prin1-to-string object)))

;;; 1. Weak rules in the rule DSL ----------------------------------------------------
;;; (rule (weak COST TERM...) :- LIT...) is clingo's  :~ LIT, ... . [W@L, TERM, ...].
;;; COST is a weight W, or (W L) with a level L; L is 0 when only W is written.

(defparameter *parse-examples*
  ;; form, then the expected (weight level terms body)
  '(((rule (weak 3 ?x) :- (late ?x))
     (3 0 (?x) ((late ?x))))
    ((rule (weak (5 1) ?a ?b) :- (clash ?a ?b))
     (5 1 (?a ?b) ((clash ?a ?b))))
    ((rule (weak (3 0) ?x) :- (late ?x))
     (3 0 (?x) ((late ?x))))
    ((rule (weak ?c ?x) :- (cost ?x ?c))
     (?c 0 (?x) ((cost ?x ?c))))
    ((rule (weak (-1 2)) :- (bonus ?x) (not (late ?x)))
     (-1 2 () ((bonus ?x) (not (late ?x)))))))

(defparameter *malformed-weak-rules*
  '((rule (weak) :- (late ?x))
    (rule (weak "3" ?x) :- (late ?x))
    (rule (weak 1.5 ?x) :- (late ?x))
    (rule (weak (3 ?l) ?x) :- (late ?x))
    (rule (weak (3 1 2) ?x) :- (late ?x))
    (rule (weak 3 ?x))
    (rule (flagged ?x) :- (late ?x) (weak 1 ?x))))

(defparameter *toy-facts*
  '((late job1) (late job2) (clash job1 job3) (cost job1 2) (cost job2 high)))

(defun demo-dsl ()
  (out "Each form is read by PARSE-RULE. A weak rule has kind :WEAK, no head, and its~%~
        cost in RULE-WEIGHT / RULE-LEVEL / RULE-TERMS (also at the front of RULE-PROPERTIES).~%~%")
  (dolist (example *parse-examples*)
    (destructuring-bind (form (weight level terms body)) example
      (let ((r (logic:parse-rule form :properties '(:source "demo"))))
        (out "  ~a~%    kind ~s  weight ~s  level ~s  terms ~s~%    properties ~s~%"
             (one-line form) (logic:rule-kind r) (logic:rule-weight r) (logic:rule-level r)
             (logic:rule-terms r) (logic:rule-properties r))
        (check (list (logic:rule-kind r) (logic:rule-head r) (logic:rule-weight r)
                     (logic:rule-level r) (logic:rule-terms r) (logic:rule-body r))
               (list :weak nil weight level terms body)
               (one-line form))
        (check (logic:rule-properties r)
               (list :weight weight :level level :terms terms :source "demo")
               (format nil "properties of ~a" (one-line form))))))
  (out "~%(weak 3 ?x) and (weak (3 0) ?x) are the same cost: weight 3 at level 0.~%")
  (out "~%Malformed weak rules signal LOGIC-SYNTAX-ERROR (FORM = the whole rule):~%")
  (dolist (form *malformed-weak-rules*)
    (let ((c (signalled (lambda () (logic:parse-rule form)))))
      (out "  ~38a -> ~a~%" (one-line form)
           (if (typep c 'logic:logic-syntax-error) (logic:logic-error-reason c) c))
      (check (and (typep c 'logic:logic-syntax-error) (equal (logic:logic-error-form c) form))
             t (format nil "syntax error for ~a" (one-line form)))))
  ;; Safety: a variable weight must be bound by a positive body literal.
  (let ((unsafe (logic:parse-rule '(rule (weak ?w ?x) :- (late ?x)))))
    (out "~%Safety: ~a parses, but RULE-UNSAFE-VARIABLES = ~s:~%~
          the weight ?w is a head variable that no positive body literal binds.~%"
         (one-line (logic:rule-form unsafe)) (logic:rule-unsafe-variables unsafe))
    (check (logic:rule-unsafe-variables unsafe) '(?w) "unsafe weight variable"))
  (let ((p (logic:make-program :facts *toy-facts*)))
    ;; The Datalog backend refuses a weak rule in the program itself.
    (out "~%A weak rule in the program: ADD-RULE on the Datalog backend (features ~s)~%"
         (logic:backend-features (logic:program-backend p)))
    (let ((restarts '()))
      (handler-bind ((logic:logic-unsupported-rule
                       (lambda (c)
                         (setf restarts (mapcar #'restart-name (compute-restarts c)))
                         ;; Not the report itself: it prints the backend object's address.
                         (out "  ~s: backend ~s lacks ~s, needed by ~a~%    restarts ~s -> DROP-RULE~%"
                              (type-of c) (logic:backend-name (logic:logic-error-backend c))
                              (logic:logic-error-feature c)
                              (one-line (logic:rule-form (logic:logic-error-rule c)))
                              (remove-if-not (lambda (n) (member n '(logic:drop-rule logic:use-backend)))
                                             restarts))
                         (check (logic:logic-error-feature c) :weak-constraints "missing feature")
                         (logic:drop-rule c))))
        (logic:add-rule p '(rule (weak 3 ?x) :- (late ?x))))
      (check (and (member 'logic:drop-rule restarts) (member 'logic:use-backend restarts) t) t
             "DROP-RULE and USE-BACKEND offered")
      (check (logic:program-rules p) '() "the weak rule was left out"))
    ;; WEAK-INSTANCES grounds weak rules against the model on any backend.
    (out "~%WEAK-INSTANCES grounds them against the model instead, on any backend.~%~
          Base facts: ~a~%" (one-line *toy-facts*))
    (let ((rules '((rule (weak 3 ?x) :- (late ?x))
                   (rule (weak (5 1) ?a ?b) :- (clash ?a ?b))
                   (rule (weak ?w ?x) :- (late ?x))
                   (rule (late-or-clash ?x) :- (late ?x)))))
      (out "Rules given:~{~%  ~a~}~%" (mapcar #'one-line rules))
      (multiple-value-bind (instances groundings)
          (handler-bind ((logic:logic-error
                           (lambda (c)
                             (out "  ~s: ~a~%    -> DROP-RULE~%" (type-of c) c)
                             (logic:drop-rule c))))
            (logic:weak-instances p rules))
        (out "Instances (WEIGHT LEVEL . TERMS), sorted, each with its ground bodies:~%")
        (loop for i in instances for g in groundings
              do (out "  ~24a <- ~{~a~^ | ~}~%" (one-line i) (mapcar #'one-line g)))
        ;; By hand: (late job1), (late job2) give (3 0 job1), (3 0 job2);
        ;; (clash job1 job3) gives (5 1 job1 job3). Sorted by weight first.
        (check instances '((3 0 job1) (3 0 job2) (5 1 job1 job3)) "toy instances")
        (check groundings '((((late job1))) (((late job2))) (((clash job1 job3))))
               "toy groundings")
        (out "WEAK-COST, highest level first: ~s~%" (logic:weak-cost instances))
        (check (logic:weak-cost instances) '((1 . 5) (0 . 6)) "toy cost")))
    ;; Instances are a set: the same (W L . TERMS) from two rules is counted once.
    (out "~%Instances are a set, as in clingo. (weak 3 ?x) :- (cost ?x ?c) gives (3 0 job1)~%~
          and (3 0 job2) again, so they merge with the late ones:~%")
    (multiple-value-bind (instances groundings)
        (logic:weak-instances p '((rule (weak 3 ?x) :- (late ?x))
                                  (rule (weak 3 ?x) :- (cost ?x ?c))))
      (loop for i in instances for g in groundings
            do (out "  ~24a <- ~{~a~^ | ~}~%" (one-line i) (mapcar #'one-line g)))
      (out "  WEAK-COST ~s: each instance costs 3 once~%" (logic:weak-cost instances))
      (check (logic:weak-cost instances) '((0 . 6)) "merged instances cost once")
      (check (mapcar #'length groundings) '(2 2) "two groundings per merged instance"))
    (out "A tag term keeps them apart, the usual way to write it:~%")
    (let ((instances (logic:weak-instances p '((rule (weak 3 late ?x) :- (late ?x))
                                               (rule (weak 3 costed ?x) :- (cost ?x ?c))))))
      (out "  ~{~a~^ ~}~%  WEAK-COST ~s~%" (mapcar #'one-line instances) (logic:weak-cost instances))
      (check (logic:weak-cost instances) '((0 . 12)) "tagged instances"))
    ;; A variable weight bound to a non-integer: LOGIC-SYNTAX-ERROR with CONTINUE.
    (out "~%A variable weight takes its value from the body. (cost job2 high) binds it to a~%~
          symbol: LOGIC-SYNTAX-ERROR on that instance, inside a CONTINUE restart~%~
          (skip it, as clingo drops it):~%")
    (let ((instances (handler-bind ((logic:logic-syntax-error
                                      (lambda (c)
                                        (out "  ~a~%    -> CONTINUE~%" c)
                                        (check (logic:logic-error-form c) '(high 0 job2)
                                               "the bad instance")
                                        (continue c))))
                       (logic:weak-instances p '((rule (weak ?c ?x) :- (cost ?x ?c)))))))
      (out "  instances ~s~%" instances)
      (check instances '((2 0 job1)) "variable weight instances"))))

;;; 2. A scheduling problem: Datalog derives the facts -----------------------------------
;;; Six tasks go into four time slots (slot 4 is overtime). Datalog derives which slots
;;; each task can use, the order the tasks must keep, which pairs share a person, and
;;; when a task would be late or in its lead's preferred slot. The choice itself,
;;; (assign TASK SLOT), is left to the solver.

(defparameter *tasks* '("design" "build" "test" "docs" "review" "release"))
(defparameter *slots* '(1 2 3 4))
(defparameter *capacity* 3 "At most this many tasks per slot.")

(defparameter *facts*
  (append
   (mapcar (lambda (task) (list 'task task)) *tasks*)
   (mapcar (lambda (slot) (list 'slot slot)) *slots*)
   '((needs "design" ana) (needs "design" ben) (needs "build" ben) (needs "test" cy)
     (needs "docs" dee) (needs "review" ana) (needs "review" cy)
     (needs "release" ben) (needs "release" dee)
     (lead "design" ana) (lead "build" ben) (lead "test" cy) (lead "docs" dee)
     (lead "review" ana) (lead "release" dee)
     (away ana 3) (away cy 1) (away cy 3)
     (precedes "design" "build") (precedes "build" "release") (precedes "test" "release")
     (due "test" 2) (due "docs" 2) (due "review" 2)
     (likes dee 1) (likes ben 2)
     (slot-cost 4 3))))

(defparameter *rules*
  '((rule (blocked ?t ?s) :- (needs ?t ?p) (away ?p ?s))
    (rule (option ?t ?s) :- (task ?t) (slot ?s) (not (blocked ?t ?s)))
    (rule (before ?a ?b) :- (precedes ?a ?b))
    (rule (before ?a ?c) :- (precedes ?a ?b) (before ?b ?c))
    (rule (shares ?a ?b ?p) :- (needs ?a ?p) (needs ?b ?p) (< ?a ?b))
    (rule (late-in ?t ?s) :- (option ?t ?s) (due ?t ?d) (> ?s ?d))
    (rule (preferred ?t ?s) :- (option ?t ?s) (lead ?t ?p) (likes ?p ?s))))

;;; The ASP-shaped part, kept out of the program: weak constraints over the choice
;;; atoms, and one integrity constraint. A later ASP backend takes them unchanged.
(defparameter *weak-rules*
  '((rule (weak (5 1) clash ?a ?b) :- (assign ?a ?s) (assign ?b ?s) (shares ?a ?b ?p))
    (rule (weak 3 late ?t) :- (assign ?t ?s) (late-in ?t ?s))
    (rule (weak ?w overtime ?t) :- (assign ?t ?s) (slot-cost ?s ?w))
    (rule (weak -1 preferred ?t) :- (assign ?t ?s) (preferred ?t ?s))))

(defparameter *constraints*
  '((rule :- (assign ?a ?sa) (assign ?b ?sb) (before ?a ?b) (>= ?sa ?sb))))

(defparameter *earliest*
  '((assign "design" 1) (assign "docs" 1) (assign "build" 2) (assign "test" 2)
    (assign "review" 2) (assign "release" 3))
  "Every task in its earliest slot that keeps the order.")

(defvar *program* nil "The scheduling program of this run.")

(defun choice-atom-p (literal)
  (and (consp literal) (eq (car literal) 'assign)))

(defun options ()
  "((TASK SLOT...) ...) in *TASKS* order, from the derived OPTION facts."
  (loop for task in *tasks*
        collect (cons task (loop for s in *slots*
                                 when (logic:holds-p *program* (list 'option task s))
                                   collect s))))

(defun candidates ()
  "Every choice atom the solver may set: (assign TASK SLOT) for each option."
  (loop for (task . slots) in (options)
        append (loop for s in slots collect (list 'assign task s))))

(defun answers->bodies (rule answers)
  (mapcar (lambda (a) (logic:substitute-bindings (logic:rule-body rule) a)) answers))

(defun constraint-groundings (rule assume)
  "The ground bodies of the integrity constraint RULE under ASSUME, via QUERY."
  (answers->bodies rule (logic:query *program* (cons 'and (logic:rule-body rule)) :assume assume)))

(defun violated-p (assignment)
  "Does ASSIGNMENT break an integrity constraint or the capacity?"
  (or (some (lambda (form)
              (logic:holds-p *program* (cons 'and (logic:rule-body (logic:parse-rule form)))
                             :assume assignment))
            *constraints*)
      (some (lambda (s) (> (count s assignment :key #'third) *capacity*)) *slots*)))

(defun cost-vector (cost levels)
  "WEAK-COST's ((LEVEL . SUM) ...) as a list per LEVEL, a missing level as 0."
  (mapcar (lambda (l) (or (cdr (assoc l cost)) 0)) levels))

(defun schedule-cost (assignment)
  "The WEAK-COST of ASSIGNMENT: the weak rules ground with ASSIGNMENT assumed."
  (logic:weak-cost (logic:weak-instances *program* *weak-rules* :assume assignment)))

(defun print-schedule (assignment)
  (dolist (s *slots*)
    (out "    slot ~d: ~:[-~;~:*~{~a~^, ~}~]~%" s
         (loop for (nil task slot) in assignment when (= slot s) collect task))))

(defun demo-datalog ()
  (out "Base facts (~d): tasks, slots, who each task needs and who leads it, who is away~%~
        when, the order to keep, due slots, preferred slots, and the overtime cost.~%"
       (length *facts*))
  (out "Rules:~{~%  ~a~}~%" (mapcar #'one-line *rules*))
  (setf *program* (logic:make-program :rules *rules* :facts *facts*))
  (out "~%Derived OPTION facts (a task may use a slot when nobody it needs is away):~%")
  (out "  ~8a ~{~7a~}~a~%" "task" (mapcar (lambda (s) (format nil "slot ~d" s)) *slots*) "needs")
  (loop for (task . slots) in (options)
        do (out "  ~8a ~{~7a~}~{~(~a~)~^ ~}~%" task
                (mapcar (lambda (s) (if (member s slots) "yes" "-")) *slots*)
                (mapcar (lambda (a) (cdr (first a)))
                        (logic:query *program* (list 'needs task '?p)))))
  ;; By hand: cy is away at 1 and 3, ana at 3.
  (check (options) '(("design" 1 2 4) ("build" 1 2 3 4) ("test" 2 4) ("docs" 1 2 3 4)
                     ("review" 2 4) ("release" 1 2 3 4))
         "options")
  (let ((before (logic:program-facts *program* :relation 'before))
        (shares (logic:program-facts *program* :relation 'shares))
        (late (logic:program-facts *program* :relation 'late-in))
        (preferred (logic:program-facts *program* :relation 'preferred)))
    (out "~%BEFORE, the transitive closure of PRECEDES (a recursive rule):~%  ~{~a~^ ~}~%"
         (mapcar #'one-line before))
    (check before '((before "build" "release") (before "design" "build")
                    (before "design" "release") (before "test" "release"))
           "before closure")
    (out "SHARES (pairs that need the same person, each pair once by string order):~%  ~{~a~^ ~}~%"
         (mapcar #'one-line shares))
    (check (length shares) 6 "sharing pairs")
    (out "LATE-IN (slots past the due slot): ~{~a~^ ~}~%" (mapcar #'one-line late))
    (check late '((late-in "docs" 3) (late-in "docs" 4) (late-in "review" 4) (late-in "test" 4))
           "late-in")
    (out "PREFERRED (the lead likes that slot): ~{~a~^ ~}~%" (mapcar #'one-line preferred))
    (check preferred '((preferred "build" 2) (preferred "docs" 1) (preferred "release" 1))
           "preferred"))
  (out "~%The choice (assign TASK SLOT) stays open. The weak and hard rules are written~%~
        ASP-style over it and kept out of the program:~%")
  (out "~{  ~a~%~}" (mapcar #'one-line (append *weak-rules* *constraints*)))
  (out "plus: each task takes exactly one of its options, and a slot holds at most ~d tasks.~%"
       *capacity*)
  ;; Ground the weak rules over every candidate choice at once: :ASSUME them all.
  (let ((candidates (candidates))
        (facts-before (logic:program-facts *program*)))
    (out "~%WEAK-INSTANCES with all ~d candidate choice atoms assumed (one call, the program~%~
          is unchanged afterwards):~%" (length candidates))
    (multiple-value-bind (instances groundings)
        (logic:weak-instances *program* *weak-rules* :assume candidates)
      (loop for i in instances for g in groundings
            do (out "  ~34a <- ~{~a~^~%~37T|  ~}~%" (one-line i) (mapcar #'one-line g)))
      ;; By hand: 6 sharing pairs that each have a common option slot, 3 late
      ;; tasks, 6 tasks with an overtime option, 3 preferred task slots.
      (check (length instances) 18 "instances over all candidates")
      (check (length (second (assoc '(3 0 late "docs") (mapcar #'list instances groundings)
                                    :test #'equal)))
             2 "docs is late in two slots: one instance, two groundings"))
    (check (logic:program-facts *program*) facts-before "the program is unchanged"))
  ;; WEAK-COST of one hand-made schedule.
  (out "~%A hand-made schedule, every task as early as the order allows:~%")
  (print-schedule *earliest*)
  (check (violated-p *earliest*) nil "the earliest schedule is feasible")
  (let ((instances (logic:weak-instances *program* *weak-rules* :assume *earliest*)))
    (out "  its instances: ~{~a~^ ~}~%  WEAK-COST: ~s~%" (mapcar #'one-line instances)
         (logic:weak-cost instances))
    ;; By hand: test and review share cy in slot 2 (5 at level 1); docs at 1 and
    ;; build at 2 are preferred (-1 each); nobody is late or in overtime.
    (check (logic:weak-cost instances) '((1 . 5) (0 . -2)) "earliest schedule cost")))

;;; 3. The bridge: ground weak instances to weighted MaxSAT -------------------------------
;;; SAT variables are the ground choice atoms themselves, (assign TASK SLOT), plus one
;;; soft variable per weak instance, named (:weak . INSTANCE).
;;;   hard:  exactly one option per task (SAT-EXACTLY-ONE), at most *CAPACITY* tasks per
;;;          slot (SAT-AT-MOST-K), and one clause per ground body of an integrity
;;;          constraint (not all of its choice atoms);
;;;   soft:  v(I) <-> OR over I's groundings of AND of their choice atoms. The other
;;;          literals of a grounding are fixed facts of the model;
;;;   cost:  one objective per level, highest first, as (:lex O_hi ... O_lo). A
;;;          weight w > 0 is (w . v); w < 0 is (-w . (not v)) plus the constant w,
;;;          since sat-protocol weights are non-negative.
;;; This is sound when no program rule reads the choice relation (so the fixed facts
;;; do not depend on the choice) and the choice atoms occur only positively in the
;;; weak and constraint bodies. The demo checks both.

(defun soft-var (instance) (sat:sat-var (cons :weak instance)))

(defun bridge-preconditions-p ()
  (flet ((mentions-choice-p (literal)
           (or (choice-atom-p literal)
               (and (consp literal) (string= (symbol-name (car literal)) "NOT")
                    (choice-atom-p (second literal))))))
    (and (notany (lambda (rule) (some #'mentions-choice-p (logic:rule-body rule)))
                 (logic:program-rules *program*))
         (notany #'choice-atom-p (logic:program-base-facts *program*))
         (every (lambda (form)
                  (notany (lambda (l) (and (consp l) (string= (symbol-name (car l)) "NOT")
                                           (choice-atom-p (second l))))
                          (logic:rule-body (logic:parse-rule form))))
                (append *weak-rules* *constraints*)))))

(defun hard-formula (candidates)
  "One-hot choices, capacity, and the integrity-constraint clauses."
  (let ((parts '()))
    (loop for (task . slots) in (options)
          do (push (sat:sat-exactly-one (mapcar (lambda (s) (sat:sat-var (list 'assign task s))) slots))
                   parts))
    (dolist (s *slots*)
      (let ((xs (loop for c in candidates when (= (third c) s) collect (sat:sat-var c))))
        (when (> (length xs) *capacity*)
          (push (sat:sat-at-most-k xs *capacity*) parts))))
    (dolist (form *constraints*)
      (dolist (body (constraint-groundings (logic:parse-rule form) candidates))
        (push (sat:sat-or* (mapcar (lambda (a) (sat:sat-not (sat:sat-var a)))
                                   (remove-if-not #'choice-atom-p body)))
              parts)))
    (sat:sat-and* (nreverse parts))))

(defun soft-definitions (instances groundings)
  (sat:sat-and*
   (loop for i in instances for gs in groundings
         collect (sat:sat-iff (soft-var i)
                              (sat:sat-or* (mapcar (lambda (g)
                                                     (sat:sat-and* (mapcar #'sat:sat-var
                                                                           (remove-if-not #'choice-atom-p g))))
                                                   gs))))))

(defun level-objective (instances level)
  "(values OBJECTIVE OFFSET) for the instances at LEVEL."
  (values (loop for (w l) in instances for i in instances
                when (and (= l level) (plusp w)) collect (cons w (soft-var i))
                when (and (= l level) (minusp w)) collect (cons (- w) (sat:sat-not (soft-var i))))
          (loop for (w l) in instances when (and (= l level) (minusp w)) sum w)))

(defun decode (model candidates)
  (remove-if-not (lambda (c) (sat:model-value model c)) candidates))

(defun brute-force (candidates)
  "Every one-hot assignment over the options: (values feasible-count lex-best
lex-optima flat-best flat-optima seconds), costs as vectors over LEVELS (1 0)."
  (declare (ignore candidates))
  (let ((feasible 0) (lex-best nil) (lex-optima '()) (flat-best nil) (flat-optima '())
        (start (get-internal-real-time)))
    (labels ((lex< (a b) (loop for x in a for y in b
                               when (< x y) return t when (> x y) return nil))
             (walk (rest acc)
               (if (null rest)
                   (let ((a (reverse acc)))
                     (unless (violated-p a)
                       (incf feasible)
                       (let* ((v (cost-vector (schedule-cost a) '(1 0)))
                              (flat (reduce #'+ v)))
                         (cond ((or (null lex-best) (lex< v lex-best))
                                (setf lex-best v lex-optima (list a)))
                               ((equal v lex-best) (push a lex-optima)))
                         (cond ((or (null flat-best) (< flat flat-best))
                                (setf flat-best flat flat-optima (list a)))
                               ((= flat flat-best) (push a flat-optima))))))
                   (destructuring-bind (task . slots) (first rest)
                     (dolist (s slots)
                       (walk (rest rest) (cons (list 'assign task s) acc)))))))
      (walk (options) '()))
    (values feasible lex-best (reverse lex-optima) flat-best (reverse flat-optima)
            (seconds-since start))))

(defvar *lex-optimum* nil "The assignment MINIMIZE found for the :LEX objective.")

(defun demo-bridge ()
  (let ((candidates (candidates)))
    (out "Preconditions: ASSIGN is read by no program rule and is no base fact, and it~%~
          occurs only positively in the weak and constraint bodies: ~:[no~;yes~]~%"
         (bridge-preconditions-p))
    (check (bridge-preconditions-p) t "bridge preconditions")
    (multiple-value-bind (instances groundings)
        (logic:weak-instances *program* *weak-rules* :assume candidates)
      (let ((fixed (loop for g in groundings append (loop for b in g append (remove-if #'choice-atom-p b)))))
        (out "The ~d non-choice literals of the groundings hold in the model with no choice~%~
              assumed: ~:[no~;yes~]~%"
             (length fixed) (logic:holds-p *program* (cons 'and (remove-duplicates fixed :test #'equal))))
        (check (logic:holds-p *program* (cons 'and (remove-duplicates fixed :test #'equal))) t
               "fixed literals hold"))
      (let* ((hard (hard-formula candidates))
             (formula (sat:sat-and hard (soft-definitions instances groundings)))
             (levels (sort (remove-duplicates (mapcar #'second instances)) #'>))
             (nogoods (loop for form in *constraints*
                            sum (length (constraint-groundings (logic:parse-rule form) candidates))))
             (cnf (sat:to-cnf formula)))
        (out "~%Compiled: ~d choice variables, ~d soft variables; ~d one-hot groups, ~d capacity~%~
              constraints, ~d order clauses (ground bodies of the integrity constraint).~%~
              CNF: ~d variables (~d auxiliary), ~d clauses.~%"
             (length candidates) (length instances) (length *tasks*)
             (count-if (lambda (s) (> (count s candidates :key #'third) *capacity*)) *slots*)
             nogoods (sat:cnf-variable-count cnf)
             (loop for i from 1 to (sat:cnf-variable-count cnf) count (sat:cnf-aux-variable-p cnf i))
             (sat:cnf-clause-count cnf))
        ;; By hand, pairs with the first slot >= the second: design/build 7,
        ;; build/release 10, design/release 7, test/release 6.
        (check nogoods 30 "order clauses")
        (multiple-value-bind (objectives offsets)
            (loop for l in levels
                  for (o off) = (multiple-value-list (level-objective instances l))
                  collect o into os collect off into offs
                  finally (return (values os offs)))
          (out "Objective (:lex~{ level-~d~}): ~{~d~^ and ~} weighted literals; constant offsets ~s~%~
                (the -1 rewards become (1 . (not v)) minus 1).~%"
               levels (mapcar #'length objectives) offsets)
          ;; Brute force over every one-hot assignment, with logic-protocol's own
          ;; WEAK-COST as the definition of cost.
          (multiple-value-bind (feasible lex-best lex-optima flat-best flat-optima seconds)
              (brute-force candidates)
            (let ((models (length (sat:collect-models
                                   (sat:solve-all hard :project (mapcar #'sat:sat-var candidates))))))
              (out "~%Brute force over all ~d one-hot assignments (WEAK-COST under :ASSUME, ~,3fs):~%~
                    ~d feasible; the hard formula has ~d models on the choice variables.~%~
                    lex optimum ~s (~d assignment~:p); one weighted sum: optimum ~d (~d assignment~:p).~%"
                   (reduce #'* (mapcar (lambda (o) (length (cdr o))) (options))) seconds
                   feasible models lex-best (length lex-optima) flat-best (length flat-optima))
              (check models feasible "hard formula models = feasible assignments")
              ;; By hand: test and review both need cy and can use only slots 2 and 4,
              ;; and test must come before release, so test takes slot 2. Without a
              ;; clash review goes to 4: late and in overtime (3 + 3), with docs at 1
              ;; and build at 2 preferred (-2): (0 4). The earliest schedule sums to 3.
              (check lex-best '(0 4) "brute-force lex optimum")
              (check flat-best 3 "brute-force flat optimum"))
            (out "~%MINIMIZE (:lex~{ level-~d~}) with :STRATEGY :CORE-GUIDED:~%" levels)
            (multiple-value-bind (seconds solves conflicts model cost optimal-p)
                (counted (sat:minimize formula (cons :lex objectives) :strategy :core-guided))
              (let* ((assignment (decode model candidates))
                     (per-level (mapcar #'+ cost offsets))
                     (logic-cost (cost-vector (schedule-cost assignment) levels)))
                (setf *lex-optimum* assignment)
                (print-schedule assignment)
                (out "  SAT cost ~s + offsets ~s = per level ~s; optimal-p ~s (~,3fs)~%  ~
                      backend calls: ~d solve, ~d final-conflict (sat-backend-dpll keeps the default~%  ~
                      BACKEND-FINAL-CONFLICT, which returns every assumption, so each core is shrunk~%  ~
                      by deletion through further calls)~%  ~
                      WEAK-COST of that schedule (logic-protocol): ~s~%  ~
                      (no level-1 instance holds, so WEAK-COST has no level-1 entry: it counts as 0)~%"
                     cost offsets (mapcar #'cons levels per-level) optimal-p seconds solves conflicts
                     (schedule-cost assignment))
                (check optimal-p t "core-guided optimal-p")
                (check (plusp conflicts) t "core-guided asked for cores")
                (check per-level lex-best "core-guided cost = brute force")
                (check logic-cost per-level "SAT cost = WEAK-COST")
                (check (violated-p assignment) nil "optimum is feasible")
                (check (and (member assignment lex-optima :test #'equal) t) t
                       "optimum is one of the brute-force optima")))
            (out "~%Every strategy on the same :lex objective (:timeout 60):~%")
            (out "  ~18a ~10a ~10a ~7a ~15a ~a~%" "strategy" "per level" "optimal-p" "solves" "final-conflict" "seconds")
            (dolist (strategy '(:linear-bdd :linear-totalizer :stratified :core-guided))
              (multiple-value-bind (seconds solves conflicts model cost optimal-p)
                  (counted (sat:minimize formula (cons :lex objectives) :strategy strategy :timeout 60))
                (declare (ignore model))
                (let ((per-level (mapcar #'+ cost offsets)))
                  (out "  ~(~18s~) ~10a ~10a ~7a ~15a ~,3f~%" strategy (one-line per-level) optimal-p
                       solves conflicts seconds)
                  (check per-level lex-best (format nil "~(~s~) cost" strategy))
                  (check optimal-p t (format nil "~(~s~) optimal-p" strategy)))))
            ;; Levels matter: one weighted sum trades a clash for less level-0 cost.
            (out "~%Without levels: one weighted sum of every instance, :CORE-GUIDED:~%")
            (multiple-value-bind (model cost optimal-p)
                (sat:minimize formula (reduce #'append objectives) :strategy :core-guided)
              (let* ((assignment (decode model candidates))
                     (total (+ cost (reduce #'+ offsets))))
                (print-schedule assignment)
                (out "  cost ~d (optimal-p ~s), per level ~s: it accepts a level-1 clash~%  ~
                      to save level-0 cost, which the :lex objective never does.~%"
                     total optimal-p (schedule-cost assignment))
                (check total flat-best "flat cost = brute force")
                (check (and (member assignment flat-optima :test #'equal) t) t
                       "flat optimum is one of the brute-force optima")
                (check (cost-vector (schedule-cost assignment) levels) '(5 -2) "flat optimum per level")))))))))

;;; 4. Explanations: why is this schedule penalised? --------------------------------------

(defun print-proof (proof &optional (indent 2))
  (let ((pad (make-string indent :initial-element #\Space))
        (rule (logic:proof-rule proof)))
    (if (logic:proof-base-p proof)
        (out "~a~a  base fact~%" pad (one-line (logic:proof-fact proof)))
        (progn
          (out "~a~a~%~a  by ~a~%~a  with ~{~a~^, ~}~%" pad (one-line (logic:proof-fact proof))
               pad (one-line (logic:rule-form rule))
               pad (mapcar (lambda (b) (format nil "~s = ~s" (car b) (cdr b)))
                           (logic:proof-bindings proof)))
          (when (logic:proof-negated proof)
            (out "~a  not in the model: ~{~a~^, ~}~%" pad (mapcar #'one-line (logic:proof-negated proof))))
          (when (logic:proof-builtins proof)
            (out "~a  built-ins: ~{~a~^, ~}~%" pad (mapcar #'one-line (logic:proof-builtins proof))))
          (dolist (p (logic:proof-premises proof))
            (print-proof p (+ indent 4)))))))

(defun demo-explain ()
  (let* ((optimum *lex-optimum*)
         (instances (logic:weak-instances *program* *weak-rules* :assume optimum)))
    (multiple-value-bind (instances groundings)
        (logic:weak-instances *program* *weak-rules* :assume optimum)
      (out "The :lex optimum's penalties and rewards, with the ground bodies that fired:~%")
      (loop for i in instances for g in groundings
            do (out "  ~28a <- ~{~a~^ | ~}~%" (one-line i) (mapcar #'one-line g))))
    (let* ((late (find 'late instances :key #'third))
           (task (fourth late))
           (grounding (first (second (multiple-value-list
                                      (logic:weak-instances *program* (list (second *weak-rules*))
                                                            :assume optimum)))))
           (fact (find-if-not #'choice-atom-p (first grounding))))
      (out "~%Why is ~s late? The fixed fact in that grounding is ~a. EXPLAIN:~%" task (one-line fact))
      (let ((proof (logic:explain *program* fact)))
        (print-proof proof)
        (out "  height ~d; base facts ~{~a~^ ~}~%" (logic:proof-height proof)
             (mapcar #'one-line (logic:proof-base-facts proof)))
        (check (logic:proof-height proof) 2 "late-in proof height"))
      (let* ((due (cdr (first (first (logic:query *program* (list 'due task '?d))))))
             (slot 1)
             (fact (list 'option task slot)))
        (out "~%Why not on time? ~s is due at ~d. Slot ~d: EXPLAIN ~a is a why-not:~%"
             task due slot (one-line fact))
        (let ((why (logic:explain *program* fact)))
          (check (typep why 'logic:why-not) t "option at slot 1 does not hold")
          (dolist (r (logic:why-not-reasons why))
            (out "  rule ~a~%  fails at literal ~d: ~a~%"
                 (one-line (logic:rule-form (logic:reason-rule r))) (logic:reason-index r)
                 (one-line (logic:reason-literal r))))
          (let ((blocked (list 'blocked task slot)))
            (out "  and ~a holds:~%" (one-line blocked))
            (print-proof (logic:explain *program* blocked) 4)))
        (let* ((other (find-if (lambda (a) (= (third a) 2))
                               (remove-if-not (lambda (a) (member (second a) '("test" "review") :test #'equal))
                                              optimum)))
               (what-if (substitute (list 'assign task 2) (list 'assign task 4) optimum :test #'equal)))
          (out "~%Slot 2 has room (at most ~d tasks), but ~s is there. What if ~s moves to slot 2 too?~%"
               *capacity* (second other) task)
          (multiple-value-bind (instances groundings)
              (logic:weak-instances *program* *weak-rules* :assume what-if)
            (let* ((clash (find 'clash instances :key #'third))
                   (shares (find-if (lambda (l) (eq (car l) 'shares))
                                    (first (nth (position clash instances :test #'equal) groundings)))))
              (out "  WEAK-COST ~s (sum ~d, against ~d): the new penalty is ~a~%  ~
                    and its fixed fact ~a holds because~%"
                   (logic:weak-cost instances) (reduce #'+ (mapcar #'cdr (logic:weak-cost instances)))
                   (reduce #'+ (mapcar #'cdr (schedule-cost optimum)))
                   (one-line clash) (one-line shares))
              (print-proof (logic:explain *program* shares) 4)
              (check (logic:weak-cost instances) '((1 . 5) (0 . -2)) "what-if cost")
              (out "  A level-1 cost outweighs any level-0 saving, so the :lex optimum keeps the~%  ~
                    two apart and pays 3 for lateness plus 3 for overtime.~%"))))))))

;;; Entry point ----------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*package* (find-package '#:harness-demos-logic))
        (*print-pretty* nil)
        (*print-case* :downcase)
        (*program* nil) (*lex-optimum* nil))
    (let* ((logic:*logic-backend* (datalog:make-datalog-backend))
           (*backend* (make-instance 'counting-dpll))
           (sat:*sat-backend* *backend*))
      (format t "~&logic-protocol ~a on logic-backend-datalog ~a, sat-protocol ~a on sat-backend-dpll ~a~%(~a ~a)~%"
              (asdf:component-version (asdf:find-system "logic-protocol"))
              (asdf:component-version (asdf:find-system "logic-backend-datalog"))
              (asdf:component-version (asdf:find-system "sat-protocol"))
              (asdf:component-version (asdf:find-system "sat-backend-dpll"))
              (lisp-implementation-type) (lisp-implementation-version))
      (section "1. Weak rules in the rule DSL: weights, levels, errors and restarts" (demo-dsl))
      (section "2. A scheduling problem: Datalog derives the facts, WEAK-INSTANCES grounds the costs"
        (demo-datalog))
      (section "3. The bridge: Datalog to weighted MaxSAT, solved :CORE-GUIDED with a :LEX objective"
        (demo-bridge))
      (section "4. Explanations: why is this schedule penalised?" (demo-explain))
      (banner "Summary")
      (format t "~d results checked against expected values: ~d mismatched.~%~
                 ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
              *checks* *mismatches* (reverse *failed-sections*))
      (finish-output)
      (and (zerop *mismatches*) (null *failed-sections*)))))
