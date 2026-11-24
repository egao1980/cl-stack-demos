# Logic demo: weak constraints and the Datalog to weighted MaxSAT bridge

`harness-demos-logic` is a tour of the **weak constraints of logic-protocol 0.2.0** (the `:weak` rule kind, `weak-instances` and `weak-cost`) on the **logic-backend-datalog 0.1.0** backend, and of the bridge that turns them into a weighted MaxSAT problem for **sat-protocol 0.2.0** on **sat-backend-dpll 0.1.0**. All four packages come from GHCR.

The Datalog backend has no `:weak-constraints` feature, so it refuses a weak rule in the program. `weak-instances` grounds weak rules anyway, on any backend, because it only queries the model. A program can therefore be written in ASP style now: Datalog derives the facts, the weak rules and integrity constraints stay outside the program, and their ground instances become a SAT objective. The same rules can move to an ASP backend later unchanged.

Every number comes from the run. The self-checks compare results with values worked out by hand (noted in comments in [`src/demo.lisp`](src/demo.lisp)). The SAT optimum is also checked against a brute-force search whose only definition of cost is logic-protocol's own `weak-cost` of `weak-instances` under `:assume`. The summary line reports the mismatches. [`demo-output.txt`](demo-output.txt) is the output of the last run.

## Run

```bash
cd harness/logic
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-logic")' \
  --eval '(harness-demos-logic:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`. There is no model, network or git step, and a run takes a few seconds.

## What each section shows

1. **Weak rules in the rule DSL.** `(rule (weak COST TERM...) :- LIT...)` is clingo's `:~ LIT, ... . [W@L, TERM, ...]`. COST is a weight `W`, or `(W L)` with a level `L`, which is 0 when only `W` is written.
   - `parse-rule` on `(rule (weak 3 ?x) :- (late ?x))`, `(rule (weak (5 1) ?a ?b) :- (clash ?a ?b))`, `(weak (3 0) ?x)` (the same cost as `(weak 3 ?x)`), a variable weight `(weak ?c ?x)`, and a negative weight at level 2 with no terms. Each prints its `rule-kind`, `rule-weight`, `rule-level`, `rule-terms` and `rule-properties`; the properties start with `:weight :level :terms`, followed by the caller's own plist.
   - Seven malformed forms, each with the `logic-syntax-error` reason: no cost, a string or float weight, a variable level, a three-element cost, an empty body, and `(weak …)` in a body.
   - `(rule (weak ?w ?x) :- (late ?x))` parses, but `rule-unsafe-variables` is `(?w)`: the weight is a head variable that no body literal binds.
   - `add-rule` of a weak rule on the Datalog backend signals `logic-unsupported-rule` (feature `:weak-constraints`) with `drop-rule` and `use-backend`. `drop-rule` leaves the rule out.
   - `weak-instances` on a five-fact program. The unsafe rule (`logic-unsafe-rule`) and a normal rule (`logic-syntax-error`) are each dropped through `drop-rule`, and the rest are grounded: `(3 0 job1) (3 0 job2) (5 1 job1 job3)`, each with its ground bodies, and `weak-cost` `((1 . 5) (0 . 6))`, highest level first.
   - Instances are a set, as in clingo. A second rule that yields `(3 0 job1)` merges with the first: the instance now has two groundings and still costs 3 once. A tag term (`(weak 3 late ?x)`) keeps such instances apart.
   - A variable weight bound to a symbol (`(cost job2 high)`) signals `logic-syntax-error` on that instance inside a `continue` restart. `continue` skips it, as clingo drops such an instance.
2. **A scheduling problem: Datalog derives the facts.** Six tasks go into four time slots, and slot 4 is overtime. The base facts say who each task needs and who leads it, who is away when, which tasks must precede which, the due slots, the leads' preferred slots, and the overtime cost. Seven Datalog rules derive:
   - `option` (stratified negation over `blocked`): which slots a task may use, printed as a grid;
   - `before`: the transitive closure of `precedes`, through a recursive rule;
   - `shares`: pairs of tasks that need the same person, each pair once in string order (the `<` built-in);
   - `late-in` and `preferred`: the slots where a task would be late, or in its lead's preferred slot.

   The choice `(assign TASK SLOT)` stays open. Four weak rules are written over it in ASP style: a clash between two tasks that share a person in one slot (`(5 1)`, level 1), a late task (3), overtime with a variable weight read from `slot-cost`, and a reward of -1 for a preferred slot. There is also one integrity constraint, `(rule :- (assign ?a ?sa) (assign ?b ?sb) (before ?a ?b) (>= ?sa ?sb))`. `weak-instances` with all 19 candidate choice atoms in `:assume` gives the 18 ground instances and their bodies in one call. `docs` is late in two slots, so it has one instance with two groundings. The program is unchanged afterwards. A hand-made schedule (each task as early as the order allows) costs `((1 . 5) (0 . -2))`: test and review clash on the person they share, and two tasks get their preferred slots.
3. **The bridge: Datalog to weighted MaxSAT (the headline).** The SAT variables are the ground choice atoms themselves, plus one soft variable per weak instance.
   - Hard part: `sat-exactly-one` over each task's options, `sat-at-most-k` for the slot capacity (Datalog cannot count), and one clause per ground body of the integrity constraint, found by `query` under the same `:assume` (30 order clauses).
   - Soft part: `v ↔ ⋁ groundings`, each grounding contributing the conjunction of its choice atoms. Its other literals are facts of the model, and the demo checks that they hold with no choice assumed.
   - Objective: one level per weak level, `(:lex level-1 level-0)`. A negative weight `w` becomes `(-w . (not v))` plus the constant `w`, because sat-protocol weights are non-negative.

   The compile is sound when no program rule reads the choice relation and the choice atoms occur only positively in the weak and constraint bodies. The demo checks both conditions. Choice-dependent derived relations would need completion, which this compile does not do.

   The brute force enumerates all 768 one-hot assignments with `weak-cost` under `:assume`. It finds 29 feasible ones, the same number as the models of the hard formula from `solve-all`. Their lex optimum is `(0 4)`. `minimize` with `:strategy :core-guided` reaches it with `optimal-p` T, and the cost it reports (plus the offsets) equals the `weak-cost` of the schedule it returns. Because the DPLL backend keeps the default `backend-final-conflict` (every assumption), each core is shrunk by deletion: 38 final-conflict calls. The other three strategies reach the same optimum. Without levels, a single weighted sum has optimum 3 instead of 4. That optimum is the earliest schedule: it accepts the level-1 clash to save level-0 cost, which the `:lex` objective never does.
4. **Explanations: why is this schedule penalised?** The optimum's instances and the ground bodies that fired. `explain` on `(late-in "review" 4)`, the fact behind the lateness penalty, gives a proof tree of height 2: the rule, its bindings, the `(> 4 2)` built-in, and an `option` premise whose negated literal is `(blocked "review" 4)`. `explain` on `(option "review" 1)` gives a why-not that fails at `(not (blocked "review" 1))`, and that `blocked` fact's proof says the task needs a person who is away. A what-if then moves review into slot 2: `weak-cost` gives `((1 . 5) (0 . -2))`, the new clash penalty, and the proof of the `shares` fact behind it.

## Issues found

None. The demo found no bug in logic-protocol 0.2.0, logic-backend-datalog 0.1.0 (whose `logic-protocol >= 0.1.0` dependency loads 0.2.0 unchanged) or sat-protocol 0.2.0.
