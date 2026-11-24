;;;; Sections 1 to 8: a tour of sat-protocol 0.2.0 (Track L, slice L2b-1) on
;;;; sat-backend-dpll 0.1.0. Sections 9 to 12 (cdcl.lisp) compare it with
;;;; sat-backend-cdcl 0.1.0, and RUN-DEMOS is there.
;;;; Every number printed is computed by this run; the self-checks compare the
;;;; solver's answers with independent counts (binomials, brute force, a
;;;; dynamic-programming oracle) and the summary reports the mismatches.

(in-package #:harness-demos-sat)

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

(defun binomial (n k)
  (if (or (< k 0) (> k n))
      0
      (loop with r = 1 for i from 1 to k do (setf r (/ (* r (- n (- k i))) i)) finally (return r))))

;;; A backend that counts ----------------------------------------------------------
;;; MINIMIZE drives the backend through two generic functions. Specialising them
;;; with :AROUND methods shows how much work each strategy asks for. The mixin
;;; works on either backend; each reports the counters of its last search.

(defgeneric last-statistics (backend)
  (:documentation "The statistics plist of BACKEND's last search.")
  (:method ((backend dpll:dpll-backend)) (dpll:dpll-backend-statistics backend))
  (:method ((backend cdcl:cdcl-backend)) (cdcl:cdcl-backend-statistics backend)))

(defclass counting-mixin ()
  ((solves :initform 0 :accessor solves)
   (final-conflicts :initform 0 :accessor final-conflicts)
   (totals :initform '() :accessor totals
           :documentation "Each statistic summed over the searches since RESET-COUNTS."))
  (:documentation "Counts BACKEND-SOLVE and BACKEND-FINAL-CONFLICT calls and sums the
statistics of the searches they ran."))

(defclass counting-dpll (counting-mixin dpll:dpll-backend) ()
  (:documentation "sat-backend-dpll, counted."))

(defclass counting-cdcl (counting-mixin cdcl:cdcl-backend) ()
  (:documentation "sat-backend-cdcl, counted."))

(defvar *in-search* nil
  "True inside a counted call: sat-backend-dpll's BACKEND-FINAL-CONFLICT is one
BACKEND-SOLVE (sat-protocol's default method), whose search is summed once.")

(defun add-statistics (backend)
  (loop for (key value) on (last-statistics backend) by #'cddr
        do (incf (getf (totals backend) key 0) value)))

(defmacro with-search ((backend) &body body)
  "Run BODY, then add BACKEND's last statistics to its totals, unless an
enclosing counted call does that."
  `(if *in-search*
       (progn ,@body)
       (let ((*in-search* t))
         (unwind-protect (progn ,@body)
           (add-statistics ,backend)))))

(defmethod sat:backend-solve :around ((backend counting-mixin) cnf &key &allow-other-keys)
  (declare (ignore cnf))
  (incf (solves backend))
  (with-search (backend) (call-next-method)))

(defmethod sat:backend-final-conflict :around ((backend counting-mixin) cnf assumptions
                                               &key &allow-other-keys)
  (declare (ignore cnf assumptions))
  (incf (final-conflicts backend))
  (with-search (backend) (call-next-method)))

(defun total (backend key)
  (getf (totals backend) key 0))

(defun decisions (backend)
  (total backend :decisions))

(defvar *backend* nil "The COUNTING-DPLL instance of sections 1 to 8.")

(defun reset-counts (&optional (backend *backend*))
  (setf (solves backend) 0 (final-conflicts backend) 0 (totals backend) '()))

(defmacro counted-on ((backend) &body body)
  "As COUNTED, on BACKEND."
  (let ((b (gensym "BACKEND")))
    `(let ((,b ,backend))
       (reset-counts ,b)
       (let ((all (multiple-value-list (timed ,@body))))
         (values-list (list* (first all) (solves ,b) (final-conflicts ,b)
                             (decisions ,b) (rest all)))))))

(defmacro counted (&body body)
  "(values seconds solves final-conflicts decisions . BODY's values), counted
on *BACKEND*."
  `(counted-on (*backend*) ,@body))

;;; Helpers on CNFs and models ------------------------------------------------------

(defun vars (prefix n)
  (loop for i below n collect (sat:sat-var (list prefix i))))

(defun aux-count (cnf)
  (loop for i from 1 to (sat:cnf-variable-count cnf) count (sat:cnf-aux-variable-p cnf i)))

(defun count-models (formula)
  "Distinct models projected on the user variables (SOLVE-ALL, blocking clauses)."
  (length (sat:collect-models (sat:solve-all formula :backend *backend*))))

(defun true-names (model)
  (loop for (name . value) in model when value collect name))

;;; 1. Cardinality and pseudo-Boolean constraints -----------------------------------

(defparameter *encodings* '(:pairwise :sequential :totalizer :bdd))

(defun demo-cardinality ()
  (out "SAT-EXACTLY-ONE and SAT-AT-MOST-K build one :AT-MOST node (sum of the true literals~%~
        <= K); TO-CNF turns it into clauses with the node's :ENCODING. Every encoding has the~%~
        same models on the user variables; they differ in size. Models are counted by~%~
        SOLVE-ALL and checked against the binomial sums.~%~%")
  (out "~16a ~3a ~11a ~8a ~6a ~7a ~a~%" "constraint" "n" "encoding" "clauses" "aux" "models" "expected")
  (dolist (n '(8 16))
    (let ((xs (vars :x n)))
      (dolist (spec `(("exactly-one" ,(lambda (enc) (sat:sat-exactly-one xs :encoding enc)) ,n)
                      ("at-most-3" ,(lambda (enc) (sat:sat-at-most-k xs 3 :encoding enc))
                                   ,(loop for i to 3 sum (binomial n i)))))
        (destructuring-bind (label build expected) spec
          (dolist (enc *encodings*)
            (let* ((formula (funcall build enc))
                   (cnf (sat:to-cnf formula))
                   (models (count-models formula)))
              (out "~16a ~3d ~(~11a~) ~8d ~6d ~7d ~d~%" label n enc (sat:cnf-clause-count cnf)
                   (aux-count cnf) models expected)
              (check models expected (format nil "~a n=~d ~a models" label n enc))
              (when (eq enc :pairwise)
                (check (aux-count cnf) 0 "pairwise adds no auxiliary")))))))
    (out "~%"))
  (let* ((x4 (vars :x 4)) (x8 (vars :x 8))
         (picks (list (list "at-most-one, n=4" (sat:sat-at-most-one x4) :pairwise)
                      (list "at-most-one, n=8" (sat:sat-at-most-one x8) :sequential)
                      (list "at-most-3, n=8" (sat:sat-at-most-k x8 3) :totalizer)
                      (list "weights 3,2,2,1 <= 4"
                            (sat:sat-pb-at-most (mapcar #'cons '(3 2 2 1) x4) 4) :bdd))))
    (out ":ENCODING :AUTO (the default) resolves on the folded node:~%")
    (dolist (p picks)
      (destructuring-bind (label formula expected) p
        (out "  ~22a -> ~(~s~)~%" label (sat:formula-encoding formula))
        (check (sat:formula-encoding formula) expected (format nil ":auto for ~a" label))))
    (out "Constants fold before any encoding:~%")
    (dolist (p (list (list "(sat-at-most-k x 8), n=8" (sat:sat-at-most-k x8 8) :true)
                     (list "(sat-at-least-k x 9), n=8" (sat:sat-at-least-k x8 9) :false)
                     (list "(sat-at-least-k x 1), n=4" (sat:sat-at-least-k x4 1) :or)
                     (list "(sat-at-most-k x 0), n=4" (sat:sat-at-most-k x4 0) :and)))
      (destructuring-bind (label formula op) p
        (out "  ~26a -> ~s~%" label (sat:formula->sexp formula))
        (check (sat:formula-operator formula) op label)))))

(defparameter *items*
  ;; (weight . value)
  '((5 . 10) (4 . 7) (6 . 12) (3 . 4) (7 . 13) (2 . 3) (8 . 15) (4 . 6) (5 . 9) (3 . 5)))

(defparameter *capacity* 20)

(defun knapsack-brute ()
  "Best value and number of feasible subsets, by enumerating all 2^n subsets."
  (let ((n (length *items*)) (best 0) (feasible 0))
    (dotimes (mask (expt 2 n))
      (let ((w 0) (v 0))
        (loop for (iw . iv) in *items* for i from 0
              when (logbitp i mask) do (incf w iw) (incf v iv))
        (when (<= w *capacity*)
          (incf feasible)
          (setf best (max best v)))))
    (values best feasible)))

(defun demo-pseudo-boolean ()
  (out "SAT-PB-AT-MOST is the weighted case: sum of w_i over the true literals <= K. A 0/1~%~
        knapsack: ~d items, weights ~{~d~^ ~}, values ~{~d~^ ~}, capacity ~d.~%~
        Maximising the value packed is minimising the value left out, the objective~%~
        ((value . (sat-not pick_i)) ...).~%~%"
        (length *items*) (mapcar #'car *items*) (mapcar #'cdr *items*) *capacity*)
  (multiple-value-bind (brute-best brute-feasible) (knapsack-brute)
    (let* ((picks (vars :pick (length *items*)))
           (objective (mapcar (lambda (item p) (cons (cdr item) (sat:sat-not p))) *items* picks))
           (total (reduce #'+ *items* :key #'cdr)))
      (out "~11a ~8a ~6a ~14a ~10a ~a~%" "encoding" "clauses" "aux" "feasible sets" "best value" "packed items")
      (dolist (enc *encodings*)
        (let* ((formula (sat:sat-pb-at-most (mapcar (lambda (item p) (cons (car item) p)) *items* picks)
                                            *capacity* :encoding enc))
               (cnf (sat:to-cnf formula))
               (feasible (count-models formula)))
          (multiple-value-bind (model cost optimal-p) (sat:minimize formula objective :backend *backend*)
            (let ((packed (loop for name in (true-names model) collect (second name))))
              (out "~(~11a~) ~8d ~6d ~14d ~10d ~{~d~^ ~}~%" enc (sat:cnf-clause-count cnf) (aux-count cnf)
                   feasible (- total cost) packed)
              (check feasible brute-feasible (format nil "~a feasible subsets" enc))
              (check (- total cost) brute-best (format nil "~a best value" enc))
              (check optimal-p t (format nil "~a optimal-p" enc))
              (check (<= (loop for i in packed sum (car (nth i *items*))) *capacity*) t
                     (format nil "~a packing fits" enc))))))
      (out "Brute force over all ~d subsets: ~d feasible, best value ~d.~%"
           (expt 2 (length *items*)) brute-feasible brute-best))))

;;; 2. N-queens ---------------------------------------------------------------------

(defun queen (r c) (sat:sat-var (list :q r c)))

(defun queens-formula (n &key (encoding :auto))
  "Exactly one queen per row, at most one per column and per diagonal."
  (let ((parts '()))
    (dotimes (r n)
      (push (sat:sat-exactly-one (loop for c below n collect (queen r c)) :encoding encoding) parts))
    (dotimes (c n)
      (push (sat:sat-at-most-one (loop for r below n collect (queen r c)) :encoding encoding) parts))
    ;; Diagonals r - c = d and anti-diagonals r + c = s; a single cell folds to true.
    (loop for d from (- 1 n) to (1- n)
          do (push (sat:sat-at-most-one (loop for r below n for c = (- r d)
                                              when (< -1 c n) collect (queen r c))
                                        :encoding encoding)
                   parts))
    (loop for s from 0 to (* 2 (1- n))
          do (push (sat:sat-at-most-one (loop for r below n for c = (- s r)
                                              when (< -1 c n) collect (queen r c))
                                        :encoding encoding)
                   parts))
    (sat:sat-and* (nreverse parts))))

(defun queens-columns (model n)
  "The column of the queen in each row (NIL for a row without exactly one)."
  (loop for r below n
        collect (let ((cs (loop for c below n when (sat:model-value model (list :q r c)) collect c)))
                  (and (= 1 (length cs)) (first cs)))))

(defun queens-valid-p (columns n)
  (and (= n (length columns))
       (every #'integerp columns)
       (loop for (r1 . more) on (loop for r below n collect r)
             always (loop for r2 in more
                          for c1 = (nth r1 columns) for c2 = (nth r2 columns)
                          always (and (/= c1 c2) (/= (abs (- c1 c2)) (- r2 r1)))))))

(defparameter *queens-counts* '((4 . 2) (5 . 10) (6 . 4) (7 . 40) (8 . 92))
  "Known numbers of N-queens solutions.")

(defun demo-queens ()
  (let ((n 8))
    (out "~d-queens: SAT-EXACTLY-ONE per row, SAT-AT-MOST-ONE per column and per diagonal.~%~%" n)
    (out "~11a ~8a ~6a ~12a ~a~%" "encoding" "clauses" "aux" "solve (s)" "first board valid")
    (let ((first-model nil))
      (dolist (enc (cons :auto *encodings*))
        (let* ((formula (queens-formula n :encoding enc))
               (cnf (sat:to-cnf formula)))
          (multiple-value-bind (seconds model sat) (timed (sat:solve formula :backend *backend*))
            (let ((valid (and sat (queens-valid-p (queens-columns model n) n))))
              (out "~(~11a~) ~8d ~6d ~12,3f ~:[no~;yes~]~%" enc (sat:cnf-clause-count cnf) (aux-count cnf)
                   seconds valid)
              (check valid t (format nil "~a board valid" enc))
              (unless first-model (setf first-model model))))))
      (out "~%The :auto board (row r has its queen in column c):~%")
      (let ((columns (queens-columns first-model n)))
        (dotimes (r n)
          (out "    ~{~a~^ ~}~%" (loop for c below n collect (if (eql c (nth r columns)) "Q" "."))))))
    (out "~%Every solution, by SOLVE-ALL, against the known counts:~%")
    (loop for (size . known) in *queens-counts*
          do (multiple-value-bind (seconds models)
                 (timed (sat:collect-models (sat:solve-all (queens-formula size) :backend *backend*)))
               (let ((valid (count-if (lambda (m) (queens-valid-p (queens-columns m size) size)) models)))
                 (out "  n=~d: ~3d solutions (known ~3d), all valid: ~:[no~;yes~]  ~,2fs~%"
                      size (length models) known (= valid (length models)) seconds)
                 (check (length models) known (format nil "~d-queens count" size))
                 (check valid (length models) (format nil "~d-queens all valid" size)))))))

;;; 3. Weighted sequence labelling on a chain --------------------------------------
;;; Label each of 12 positions with one of 3 levels. Position p has an observed
;;; level; labelling it l costs |l - observed|. Adjacent positions may differ by
;;; at most one level, and two specific adjacent pairs are forbidden as well.
;;; A step up costs 2, a step down 1. Minimise the total.

(defparameter *levels* 3)
(defparameter *observed* '(0 2 2 1 0 0 2 2 1 1 0 2))
(defparameter *extra-forbidden* '((3 0 1) (8 1 0))
  "(p a b): level a at position p followed by level b at p+1 is forbidden.")

(defun positions () (length *observed*))

(defun step-cost (a b) (cond ((= a b) 0) ((< a b) 2) (t 1)))

(defun forbidden-p (p a b)
  (or (> (abs (- a b)) 1)
      (member (list p a b) *extra-forbidden* :test #'equal)))

(defun unary-cost (p l &optional (observed *observed*))
  (abs (- l (nth p observed))))

(defun x-name (p l) (list :x p l))
(defun y-name (p a b) (list :y p a b))
(defun x (p l) (sat:sat-var (x-name p l)))
(defun y (p a b) (sat:sat-var (y-name p a b)))

(defun chain-formula ()
  "Clause-shaped: TO-CNF adds no auxiliary (one-hot by :pairwise, nogoods and
the step indicators as clauses)."
  (let ((parts '()) (n (positions)) (k *levels*))
    (dotimes (p n)
      (push (sat:sat-exactly-one (loop for l below k collect (x p l))) parts))
    (dotimes (p (1- n))
      (dotimes (a k)
        (dotimes (b k)
          (cond ((forbidden-p p a b)
                 (push (sat:sat-or (sat:sat-not (x p a)) (sat:sat-not (x (1+ p) b))) parts))
                ((/= a b)
                 ;; x(p,a) and x(p+1,b) -> y(p,a,b): the step is paid for.
                 (push (sat:sat-or (sat:sat-not (x p a)) (sat:sat-not (x (1+ p) b)) (y p a b))
                       parts))))))
    (sat:sat-and* (nreverse parts))))

(defun chain-objective (&optional (observed *observed*))
  (append (loop for p below (positions)
                nconc (loop for l below *levels*
                            for c = (unary-cost p l observed)
                            when (plusp c) collect (cons c (x p l))))
          (loop for p below (1- (positions))
                nconc (loop for a below *levels*
                            nconc (loop for b below *levels*
                                        when (and (/= a b) (not (forbidden-p p a b)))
                                          collect (cons (step-cost a b) (y p a b)))))))

(defun labelling-cost (labels &optional (observed *observed*))
  "Cost of a labelling, or NIL when it uses a forbidden pair."
  (and (loop for (a b) on labels for p from 0 while b never (forbidden-p p a b))
       (+ (loop for l in labels for p from 0 sum (unary-cost p l observed))
          (loop for (a b) on labels while b sum (step-cost a b)))))

(defun chain-dp (&optional (observed *observed*) (unary nil))
  "The oracle: least cost and one optimal labelling by dynamic programming
(Viterbi) over the positions. UNARY, when given, is a function (p l) -> a cost
added at that position (default: none). Costs may be numbers or lists (compared
lexicographically, added element-wise)."
  (let* ((n (positions)) (k *levels*)
         (add (lambda (a b) (if (listp a) (mapcar #'+ a b) (+ a b))))
         (less (lambda (a b) (if (listp a) (loop for u in a for v in b
                                                   do (cond ((< u v) (return t)) ((> u v) (return nil))))
                                  (< a b))))
         (local (lambda (p l) (let ((c (unary-cost p l observed)))
                                (if unary (funcall unary p l c) c))))
         (step (lambda (a b) (let ((c (step-cost a b)))
                               (if unary (funcall unary nil nil c) c))))
         (best (make-array k)) (back (make-array (list n k) :initial-element nil)))
    (dotimes (l k) (setf (aref best l) (funcall local 0 l)))
    (loop for p from 1 below n
          do (let ((next (make-array k :initial-element nil)))
               (dotimes (b k)
                 (dotimes (a k)
                   (when (and (aref best a) (not (forbidden-p (1- p) a b)))
                     (let ((c (funcall add (funcall add (aref best a) (funcall step a b))
                                       (funcall local p b))))
                       (when (or (null (aref next b)) (funcall less c (aref next b)))
                         (setf (aref next b) c (aref back p b) a))))))
               (setf best next)))
    (let ((end nil))
      (dotimes (l k)
        (when (and (aref best l) (or (null end) (funcall less (aref best l) (aref best end))))
          (setf end l)))
      (when end
        (values (aref best end)
                (let ((labels (list end)))
                  (loop for p from (1- n) downto 1
                        do (push (aref back p (first labels)) labels))
                  labels))))))

(defun model-labels (model)
  "The level of each position in MODEL, or :INVALID unless exactly one is true."
  (let ((labels (loop for p below (positions)
                      collect (let ((ls (loop for l below *levels*
                                              when (sat:model-value model (x-name p l)) collect l)))
                                (and (= 1 (length ls)) (first ls))))))
    (if (every #'integerp labels) labels :invalid)))

(defun print-labelling (labels &optional (observed *observed*))
  (out "    position ~{~3d~}~%" (loop for p below (positions) collect p))
  (out "    observed ~{~3d~}~%" observed)
  (out "    labelled ~{~3d~}~%" labels)
  (loop for level from (1- *levels*) downto 0
        do (out "    level ~d  ~{  ~a~}~%" level
                (loop for l in labels for o in observed
                      collect (cond ((and (= l level) (= o level)) "#")
                                    ((= l level) "o")
                                    ((= o level) ".")
                                    (t " ")))))
  (out "    (# labelled = observed, o labelled, . observed only)~%"))

(defparameter *strategies* '(:linear-bdd :linear-totalizer :stratified :core-guided))

(defparameter *default-path-issue* "egao1980/sat-protocol#6"
  "Filed from this demo: the default MINIMIZE call takes the slower 0.1 descent.")

(defparameter *phase-issue* "egao1980/sat-protocol#7"
  "Filed from this demo: MINIMIZE's own default phases make some DPLL refutations explode.")

(defun check-chain-result (label model cost optimum &key (observed *observed*))
  "MODEL decodes to a valid labelling whose cost is COST; COST equals OPTIMUM."
  (let ((labels (model-labels model)))
    (check (and (listp labels) (labelling-cost labels observed)) cost (format nil "~a labelling cost" label))
    (check cost optimum (format nil "~a cost = DP optimum" label))
    labels))

(defun demo-chain ()
  (let* ((formula (chain-formula))
         (objective (chain-objective))
         (cnf (sat:to-cnf formula)))
    (out "~d positions, ~d levels, observed ~{~d~^ ~}.~%~
          Hard: one level per position; adjacent levels differ by at most 1; also forbidden:~%~
          ~{~{level ~d -> ~d from position ~d to ~d~}~^, ~}.~%~
          Costs: |label - observed| per position; a step up 2, a step down 1.~%~%"
         (positions) *levels* *observed*
         (mapcar (lambda (f) (destructuring-bind (p a b) f (list a b p (1+ p)))) *extra-forbidden*))
    (out "Encoding: x(p,l) one-hot per position (SAT-EXACTLY-ONE, :auto = :pairwise), a~%~
          clause (-x(p,a) -x(p+1,b)) per forbidden pair, and a step indicator y(p,a,b) with~%~
          (-x(p,a) -x(p+1,b) y(p,a,b)) that the objective pays for. CNF: ~d variables~%~
          (~d auxiliary), ~d clauses; objective: ~d weighted literals, weights ~{~d~^, ~}.~%~%"
         (sat:cnf-variable-count cnf) (aux-count cnf) (sat:cnf-clause-count cnf) (length objective)
         (sort (remove-duplicates (mapcar #'car objective)) #'<))
    (check (aux-count cnf) 0 "chain formula is clause-shaped")
    (multiple-value-bind (optimum dp-labels) (chain-dp)
      (out "~26a ~5a ~10a ~7a ~15a ~10a ~a~%" "run" "cost" "optimal-p" "solves" "final-conflict" "decisions" "seconds")
      (let ((results '()))
        (dolist (run `((":linear-bdd" (:strategy :linear-bdd))
                       (":linear-bdd :timeout 600" (:strategy :linear-bdd :timeout 600))
                       ,@(mapcar (lambda (s) (list (format nil "~(~s~)" s) (list :strategy s)))
                                 (rest *strategies*))))
          (destructuring-bind (label keys) run
            (multiple-value-bind (seconds solves conflicts decisions model cost optimal-p)
                (counted (apply #'sat:minimize formula objective :backend *backend* keys))
              (out "~26a ~5d ~10a ~7d ~15d ~10d ~,3f~%" label cost optimal-p solves conflicts decisions seconds)
              (check optimal-p t (format nil "~a optimal-p" label))
              (push (check-chain-result label model cost optimum) results))))
        (out "~%Dynamic-programming oracle: optimum ~d, labelling ~{~d~^ ~}.~%" optimum dp-labels)
        (out "Every run's model decodes to a valid labelling of cost ~d. The labelling from~%~
              :linear-totalizer:~%" optimum)
        (print-labelling (third results))
        (out "All four strategies return an optimum; they differ in the work. :core-guided~%~
              assumes every cost literal false and relaxes the conflicts it meets~%~
              (BACKEND-FINAL-CONFLICT; sat-backend-dpll returns every assumption, so each core~%~
              is shrunk by deletion: hence the many calls); the linear strategies descend from~%~
              the first model with a tightening bound (BDD or totalizer); :stratified descends~%~
              over the heaviest weights first, then over all of them.~%~%~
              The first two rows are the same strategy. With nothing but the strategy given,~%~
              MINIMIZE keeps its 0.1 descent, whose BDD bound propagates only in BDD order;~%~
              any 0.2 argument (here :timeout) selects the 0.2 descent with the propagating~%~
              bound (~a).~%"
             *default-path-issue*)))))

;;; 4. Anytime: budgets and timeouts -----------------------------------------------

(defun demo-anytime ()
  (let* ((formula (chain-formula))
         (objective (chain-objective))
         (optimum (chain-dp))
         (last-cost nil))
    (out "With :BUDGET (decisions, over every backend call) or :TIMEOUT (seconds) the search~%~
          stops early and returns the best model so far: (values model cost optimal-p), with~%~
          OPTIMAL-P NIL until the bound is proven. :linear-bdd, budgets doubling:~%~%")
    (out "~8a ~14a ~10a ~a~%" "budget" "cost" "optimal-p" "labelling")
    (loop for budget = 0 then (if (zerop budget) 8 (* 2 budget))
          do (multiple-value-bind (model cost optimal-p)
                 (handler-case (sat:minimize formula objective :backend *backend* :budget budget)
                   (sat:sat-budget-exceeded () (values :none nil nil)))
               (cond ((eq model :none)
                      (out "~8d ~14a ~10a -~%" budget "(no model yet)" "-"))
                     (t
                      (let ((labels (model-labels model)))
                        (out "~8d ~14d ~10a ~{~d~}~%" budget cost optimal-p labels)
                        (check (and (listp labels) (<= (labelling-cost labels) cost)) t
                               (format nil "budget ~d: valid labelling, no dearer than its cost" budget))
                        (when last-cost
                          (check (<= cost last-cost) t (format nil "budget ~d: cost never rises" budget)))
                        (setf last-cost cost)
                        (when optimal-p
                          (check cost optimum "budgeted optimum = DP")
                          (return))))))
             (when (> budget 1000000) (out "  (stopped the sweep)~%") (return)))
    (out "~%:TIMEOUT, :linear-totalizer (times vary from run to run; the costs are checked~%~
          only for validity):~%")
    (dolist (timeout '(0.02 0.2 1 30))
      (multiple-value-bind (seconds model cost optimal-p)
          (timed (handler-case (sat:minimize formula objective :backend *backend* :strategy :linear-totalizer :timeout timeout)
                   (sat:sat-budget-exceeded () (values :none nil nil))))
        (if (eq model :none)
            (out "  timeout ~5a: no model yet (SAT-BUDGET-EXCEEDED)~%" timeout)
            (let ((labels (model-labels model)))
              (out "  timeout ~5a: cost ~3d, optimal-p ~3a  (~,3fs)~%" timeout cost optimal-p seconds)
              (check (and (listp labels) (<= optimum (labelling-cost labels) cost)) t
                     (format nil "timeout ~a: valid incumbent" timeout))
              (when optimal-p (check cost optimum (format nil "timeout ~a optimum" timeout)))))))
    (out "~%No model yet when the limit runs out: MINIMIZE signals SAT-BUDGET-EXCEEDED (an~%~
          error when unhandled):~%")
    (handler-case (sat:minimize formula objective :backend *backend* :budget 3)
      (sat:sat-budget-exceeded (c)
        (out "  (minimize ... :budget 3) -> ~s~%    ~a~%    budget slot ~s, timeout slot ~s~%"
             (type-of c) c (sat:sat-budget-exceeded-budget c) (sat:sat-budget-exceeded-timeout c))
        (check (sat:sat-budget-exceeded-budget c) 3 "budget slot")))
    (handler-case (sat:minimize formula objective :backend *backend* :timeout 0)
      (sat:sat-budget-exceeded (c)
        (out "  (minimize ... :timeout 0) -> ~a~%" c)
        (check (sat:sat-budget-exceeded-timeout c) 0 "timeout slot")))
    (out "~%CONTINUE doubles the limit that ran out and resumes the search where it stopped:~%")
    (let ((seen '()))
      (multiple-value-bind (model cost optimal-p)
          (handler-bind ((sat:sat-budget-exceeded
                           (lambda (c)
                             (push (sat:sat-budget-exceeded-budget c) seen)
                             (continue c))))
            (sat:minimize formula objective :backend *backend* :budget 1))
        (out "  :budget 1, a handler invoking CONTINUE: signalled at budgets ~{~d~^, ~}; then a~%~
              ~2@Tmodel of cost ~d, optimal-p ~a~:[ (the last budget ran out with that incumbent:~%~
              ~2@Tno signal)~;~].~%"
             (reverse seen) cost optimal-p optimal-p)
        (check (listp (model-labels model)) t "CONTINUE: a valid model")
        (check (reverse seen) (loop for b = 1 then (* 2 b) repeat (length seen) collect b)
               "CONTINUE doubles the budget")))
    (let ((value (handler-bind ((sat:sat-budget-exceeded
                                  (lambda (c) (use-value :gave-up c))))
                   (multiple-value-list (sat:minimize formula objective :backend *backend* :budget 0)))))
      (out "  USE-VALUE :gave-up instead: MINIMIZE returns ~s~%" value)
      (check value '(:gave-up nil) "USE-VALUE return"))))

;;; 5. Hints: a warm start ----------------------------------------------------------

(defparameter *hint-timeout* 30)

(defparameter *observed-2*
  (let ((o (copy-list *observed*))) (setf (nth 9 o) 2 (nth 10 o) 2 (nth 11 o) 2) o)
  "The same chain after a revision: positions 9 to 11 are now observed at level 2.")

(defun demo-hints ()
  (let* ((formula (chain-formula))
         (old-model (sat:minimize formula (chain-objective) :backend *backend* :strategy :linear-totalizer))
         (objective (chain-objective *observed-2*))
         (optimum (chain-dp *observed-2*))
         (old-labels (model-labels old-model)))
    (out "The observations change at positions 9 to 11:~%  ~{~d~^ ~}  ->  ~{~d~^ ~}~%~
          The previous optimum, ~{~d~^ ~} (from :linear-totalizer),~%~
          costs ~d under the new costs; the new optimum (DP) is ~d. Re-solve, once from~%~
          scratch and once with :HINT the previous model (a complete model; any partial~%~
          assignment works too). The hint is tried first, as assumptions; a model that~%~
          agrees with it becomes the first incumbent, and the descent starts from there.~%~%"
         *observed* *observed-2* old-labels (labelling-cost old-labels *observed-2*) optimum)
    (multiple-value-bind (model cost optimal-p) (sat:minimize formula objective :backend *backend* :hint old-model :budget 0)
      (out "  :hint + :budget 0: the hint itself comes back, cost ~d, optimal-p ~a~%"
           cost optimal-p)
      (check (model-labels model) old-labels ":budget 0 returns the hint")
      (check cost (labelling-cost old-labels *observed-2*) "the hint's cost")
      (check optimal-p nil ":budget 0 is not a proof"))
    (out "~%Each run under :timeout ~d (seconds):~%~%" *hint-timeout*)
    (out "~18a ~20a ~5a ~10a ~7a ~10a ~a~%" "strategy" "start" "cost" "optimal-p" "solves" "decisions" "seconds")
    (dolist (run `((:linear-totalizer "cold" ())
                   (:linear-totalizer "hint" (:hint ,old-model))
                   (:linear-totalizer "cold, :phase :false" (:phase :false))
                   (:stratified "cold" ())
                   (:stratified "hint" (:hint ,old-model))))
      (destructuring-bind (strategy start keys) run
        (multiple-value-bind (seconds solves conflicts decisions model cost optimal-p)
            (counted (apply #'sat:minimize formula objective :backend *backend* :strategy strategy :timeout *hint-timeout* keys))
          (declare (ignore conflicts))
          (out "~(~18s~) ~20a ~5d ~10a ~7d ~10d ~,3f~%" strategy start cost optimal-p solves decisions seconds)
          (let ((label (format nil "~a ~a" strategy start)))
            (if optimal-p
                (check-chain-result label model cost optimum :observed *observed-2*)
                (let ((labels (model-labels model)))
                  (check (and (listp labels) (<= optimum (labelling-cost labels *observed-2*) cost)) t
                         (format nil "~a: valid unproven incumbent" label))))
            (when (getf keys :hint)
              (check optimal-p t (format nil "~a proven" label)))))))
    (out "The hint skips the descent from a poor first model: the hinted runs prove the new~%~
          optimum in a few calls. The cold :linear-totalizer run meets~%~
          ~a: with :phase NIL, MINIMIZE guides the backend with its own~%~
          phases (the incumbent's values), and on sat-backend-dpll (DLIS, no clause~%~
          learning) one refutation then takes over a million decisions; with :phase :false~%~
          (third row) it does not here. The timeout turns the slow run into an anytime answer.~%"
         *phase-issue*)
    (let ((partial (loop for l in old-labels for p from 0 collect (x p l)))
          (clash (list (cons (x-name 0 0) t) (cons (x-name 0 1) t))))
      (dolist (h (list (list "the old levels only (12 literals)" partial)
                       (list "an inconsistent hint (two levels at 0)" clash)))
        (multiple-value-bind (model cost optimal-p) (sat:minimize formula objective :backend *backend* :hint (second h))
          (out "  hint = ~40a -> cost ~d, optimal-p ~a~%" (first h) cost optimal-p)
          (check-chain-result (first h) model cost optimum :observed *observed-2*))))
    (out "An inconsistent hint is ignored: the first call fails and the search goes on~%~
          without it.~%")))

;;; 6. Lexicographic objectives ------------------------------------------------------

(defparameter *requests* '((1 . 0) (2 . 2) (5 . 2) (6 . 0) (9 . 0) (10 . 2))
  "(position . level) wishes, some of which the hard rules cannot all grant.")

(defparameter *lex-timeout* 20)

(defun demo-lex ()
  (let* ((formula (chain-formula))
         (violations (loop for (p . l) in *requests* collect (cons 1 (sat:sat-not (x p l)))))
         (cost (chain-objective))
         (objective (list :lex violations cost))
         (requested (lambda (p l) (eql l (cdr (assoc p *requests*))))))
    (out "Requests (position . level): ~{~a~^ ~}.~%~
          Adjacent requests 1/2, 5/6 and 9/10 jump by two levels, so no labelling grants all~%~
          six. Objective (:lex violations cost): first minimise the requests not granted,~%~
          then the chain cost among those.~%~%"
         (mapcar (lambda (r) (format nil "(~d . ~d)" (car r) (cdr r))) *requests*))
    (multiple-value-bind (dp dp-labels)
        (chain-dp *observed* (lambda (p l c)
                               ;; vector costs: (violations cost)
                               (if p
                                   (list (if (and (assoc p *requests*) (not (funcall requested p l))) 1 0) c)
                                   (list 0 c))))
      (out "Each strategy under :timeout ~d (seconds):~%~%" *lex-timeout*)
      (out "~32a ~11a ~10a ~7a ~10a ~a~%" "run" "cost (lex)" "optimal-p" "solves" "decisions" "seconds")
      (let ((labels nil) (unproven '()))
        (dolist (run (append (mapcar (lambda (s) (list (format nil "~(~s~)" s) s '())) *strategies*)
                             (list (list ":linear-totalizer :phase :false" :linear-totalizer '(:phase :false))
                                   (list ":linear-totalizer :order #'first" :linear-totalizer
                                         (list :order #'first)))))
          (destructuring-bind (label strategy keys) run
            (multiple-value-bind (seconds solves conflicts decisions model costs optimal-p)
                (counted (apply #'sat:minimize formula objective :backend *backend* :strategy strategy :timeout *lex-timeout* keys))
              (declare (ignore conflicts))
              (unless optimal-p (push label unproven))
              (out "~32a ~11a ~10a ~7d ~10d ~,3f~%" label (format nil "~s" costs) optimal-p
                   solves decisions seconds)
              (let ((ls (model-labels model)))
                (when (eq strategy :core-guided) (setf labels ls))
                (check (and (listp ls)
                            (let ((missed (count-if-not (lambda (r) (eql (nth (car r) ls) (cdr r))) *requests*)))
                              (and (= missed (first costs)) (<= (labelling-cost ls) (second costs)))))
                       t (format nil "~a lex labelling matches its cost" label))
                (if optimal-p
                    (check costs dp (format nil "~a lex = DP" label))
                    (check (or (equal costs dp) (< (first dp) (first costs))
                               (and (= (first dp) (first costs)) (< (second dp) (second costs))))
                           t (format nil "~a unproven incumbent no better than DP" label)))))))
        (out "~%DP oracle over cost vectors: ~s, labelling ~{~d~^ ~}.~%" dp dp-labels)
        (out "The :core-guided labelling:~%")
        (print-labelling labels)
        (out "A run the timeout stopped returns its incumbent with OPTIMAL-P NIL; here:~%~
              ~:[  none~;~:*~{  ~a~%~}~]~
              The second level of :linear-totalizer meets ~a, as in section 5.~%~
              Here :phase :false is not enough: the backend still decides the totalizer's~%~
              auxiliary variables freely. An :ORDER hook (MINIMIZE then decides the user~%~
              variables first and an auxiliary only when no user variable is left) is.~%"
             (reverse unproven) *phase-issue*))
      (multiple-value-bind (model plain) (sat:minimize formula cost :backend *backend* :strategy :linear-totalizer)
        (let ((labels (model-labels model)))
          (out "~%Cost alone (no requests, :linear-totalizer): cost ~d, granting ~d of the 6 requests.~%"
               plain (count-if (lambda (r) (eql (nth (car r) labels) (cdr r))) *requests*))))
      (let* ((big (1+ (reduce #'+ cost :key #'car)))
             (weighted (append (mapcar (lambda (e) (cons (* big (car e)) (cdr e))) violations) cost)))
        (multiple-value-bind (model total) (sat:minimize formula weighted :backend *backend* :strategy :core-guided)
          (declare (ignore model))
          (out "The same order as one weighted sum (violations x ~d + cost, :core-guided):~%~
                ~d = ~d x ~d + ~d. :lex needs no such big-M weight: a proven level becomes a~%~
                hard bound on the next.~%"
               big total big (first dp) (second dp))
          (check total (+ (* big (first dp)) (second dp)) "big-M sum = lex"))))))

;;; 7. Final-conflict cores ---------------------------------------------------------

(defparameter *pins* '((0 . 1) (3 . 0) (5 . 2) (7 . 2) (9 . 1) (11 . 2))
  "Levels fixed by assumptions; together they are unsatisfiable.")

(defun pin-label (lit)
  (let ((name (sat:literal-name lit)))
    (format nil "x~d=~d" (second name) (third name))))

(defun demo-cores ()
  (let* ((formula (chain-formula))
         (pins (loop for (p . l) in *pins* collect (x p l)))
         (cnf (sat:to-cnf formula))
         (ints (mapcar (lambda (lit) (sat:cnf-literal cnf lit)) pins)))
    (out "Assume the levels ~{~a~^ ~}.~%" (mapcar #'pin-label pins))
    (multiple-value-bind (model sat) (sat:solve formula :backend *backend* :assumptions pins)
      (out "  (solve ... :assumptions pins) -> ~s ~s: unsatisfiable~%" model sat)
      (check sat nil "pins are unsatisfiable"))
    (multiple-value-bind (core unsat) (sat:backend-final-conflict *backend* cnf ints)
      (out "~%BACKEND-FINAL-CONFLICT (index level): ~s ~s~%  = ~{~a~^ ~}~%"
           core unsat (mapcar (lambda (i) (format nil "~a" (pin-label (sat:sat-var (sat:cnf-variable-name cnf i)))))
                              core))
      (check unsat t "final conflict: unsat")
      (check (subsetp core ints) t "final conflict within the assumptions")
      (check (nth-value 1 (sat:backend-solve *backend* cnf :assumptions core)) nil
             "final conflict is unsatisfiable"))
    (out "  sat-backend-dpll keeps the default method: one BACKEND-SOLVE, and on UNSAT every~%~
          ~2@Tassumption (sat-backend-cdcl returns the assumptions its final conflict used:~%~
          ~2@Tsection 10).~%")
    (multiple-value-bind (core unsat) (sat:unsat-core formula :backend *backend* :assumptions pins :minimal nil)
      (out "~%(unsat-core ... :minimal nil), the final conflict mapped back: ~{~a~^ ~} (~a)~%"
           (mapcar #'pin-label core) unsat)
      (check (subsetp core pins) t "minimal nil: a subset"))
    (multiple-value-bind (core unsat) (sat:unsat-core formula :backend *backend* :assumptions pins)
      (out "(unsat-core ... ), minimal (deletion over the assumptions): ~{~a~^ ~} (~a)~%"
           (mapcar #'pin-label core) unsat)
      (check (mapcar #'pin-label core) '("x3=0" "x5=2") "the minimal core")
      (check (nth-value 1 (sat:solve formula :backend *backend* :assumptions core)) nil "the core is unsatisfiable")
      (dolist (drop core)
        (check (nth-value 1 (sat:solve formula :backend *backend* :assumptions (remove drop core))) t
               (format nil "core without ~a is satisfiable" (pin-label drop))))
      (out "  x3=0 forces x4=0 (0 -> 1 is forbidden after position 3, 0 -> 2 jumps two),~%~
            ~2@Tand x5=2 cannot follow x4=0.~%"))
    (let* ((*extra-forbidden* '())
           (relaxed (chain-formula))
           (named (append (loop for (p . l) in *pins* collect (cons (pin-label (x p l)) (x p l)))
                          (list (cons "rule: no 0->1 at 3->4" (sat:sat-or (sat:sat-not (x 3 0)) (sat:sat-not (x 4 1))))
                                (cons "rule: no 1->0 at 8->9" (sat:sat-or (sat:sat-not (x 8 1)) (sat:sat-not (x 9 0))))))))
      (multiple-value-bind (core unsat) (sat:unsat-core relaxed :backend *backend* :named named)
        (out "~%With the two extra rules as named soft formulas too, the core names a rule:~%  ~s (~a)~%"
             core unsat)
        (check core '("x3=0" "x5=2" "rule: no 0->1 at 3->4") "named core")))))

;;; 8. Conditions and restarts -------------------------------------------------------

(defun signalled (thunk)
  "The error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (c) c)))

(defun demo-conditions ()
  (let* ((xs (vars :x 3))
         (bad (sat:sat-and (first xs) (second xs)))
         (seen nil)
         (formula (handler-bind ((sat:sat-invalid-formula
                                   (lambda (c)
                                     (setf seen c)
                                     (let ((*print-pretty* nil))
                                       (out "  ~s:~%    ~a~%    -> USE-VALUE (sat-var (:x 9))~%" (type-of c) c))
                                     (use-value (sat:sat-var (list :x 9)) c))))
                    (sat:sat-at-most-k (append xs (list bad)) 1))))
    (out "  (sat-at-most-k (x0 x1 x2 (and x0 x1)) 1) with the replacement:~%    ~s~%"
         (sat:formula->sexp formula))
    (check (and seen (sat:formula->sexp (sat:sat-invalid-formula-datum seen))) (sat:formula->sexp bad)
           "the datum is the non-literal element")
    (check (sat:formula-variables formula) (list '(:x 0) '(:x 1) '(:x 2) '(:x 9)) "use-value replaced it")
    (let ((bare (sat:sat-at-most-k (list "a" "b" 42) 1)))
      (out "  Bare names are positive literals (any non-NIL object): ~s~%" (sat:formula->sexp bare))
      (check (sat:formula-variables bare) '("a" "b" 42) "bare names"))
    (dolist (case (list (list ":encoding :quantum" (lambda () (sat:sat-at-most-k xs 1 :encoding :quantum)))
                        (list ":strategy :fastest"
                              (lambda () (sat:minimize (sat:sat-or* xs) (list (cons 1 (first xs)))
                                                       :backend *backend* :strategy :fastest)))
                        (list "objective (:lex), no level" (lambda () (sat:minimize (sat:sat-or* xs) (list :lex) :backend *backend*)))
                        (list ":budget -1" (lambda () (sat:minimize (sat:sat-or* xs) (list (cons 1 (first xs)))
                                                                    :backend *backend* :budget -1)))))
      (let ((c (signalled (second case))))
        (out "  ~28a -> ~s~%" (first case) (type-of c))
        (check (typep c 'type-error) t (first case))))
    (let ((values (multiple-value-list
                   (sat:minimize (sat:sat-and* (mapcar #'sat:sat-not xs)) (list (cons 1 (first xs)))
                                 :backend *backend* :assumptions (list (first xs))))))
      (out "  unsatisfiable under the assumptions -> ~s (two values, as in 0.1)~%" values)
      (check values '(nil nil) "unsat minimize"))))
