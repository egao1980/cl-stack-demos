# SAT demo: cardinality, optimisation strategies, anytime search, cores

`harness-demos-sat` is a tour of **sat-protocol 0.2.0** (Track L, slice L2b-1) on the **sat-backend-dpll 0.1.0** backend, both from GHCR. It covers cardinality and pseudo-Boolean constraints in every encoding, a weighted sequence-labelling chain minimised with each `minimize` strategy, budgets and timeouts with anytime results, hints, lexicographic objectives, and final-conflict and minimal cores.

Every number comes from the run. Each answer is checked independently: model counts against binomial sums, the knapsack against brute force over all subsets, N-queens against the known solution counts, and every chain optimum (plain, revised and lexicographic) against a dynamic-programming oracle in the demo. The summary line reports the mismatches. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run.

## Run

```bash
cd harness/sat
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-sat")' \
  --eval '(harness-demos-sat:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`, and has no model, network or git step. A run takes about two and a half minutes on an M-series Mac. Most of that is the default `:linear-bdd` row of section 3 (sat-protocol#6) and the timed-out runs of sections 5 and 6 (sat-protocol#7).

## What each section shows

1. **Cardinality constraints and their encodings.** `sat-exactly-one` and `sat-at-most-k` (k = 3) over n = 8 and 16, with each `:encoding` (`:pairwise`, `:sequential`, `:totalizer`, `:bdd`). Each row gives the clause and auxiliary-variable counts, plus the number of models from `solve-all` against its binomial sum. At n = 16, at-most-3 is 1,820 clauses pairwise against 100 sequential. The section also shows what `:auto` resolves to and how constants fold before any encoding (`k >= n` gives true, `k > n` for at-least gives false, at-least-1 is a clause).
   **1b. Pseudo-Boolean constraints.** A 10-item 0/1 knapsack with `sat-pb-at-most`, in every encoding. It counts the feasible subsets and maximises the value packed, written as minimising the value left out, `((value . (sat-not pick)) ...)`. Both results are checked against brute force over all 1,024 subsets.
2. **N-queens.** 8-queens with `sat-exactly-one` rows and `sat-at-most-one` columns and diagonals, in each encoding, with one board printed. Then every solution for n = 4 to 8, through `solve-all`, against the known counts (2, 10, 4, 40, 92).
3. **Weighted sequence labelling: every `minimize` strategy (the headline).** Twelve positions each get one of three levels. Labelling position p with level l costs |l - observed(p)|. Adjacent levels may differ by at most one, and two specific adjacent pairs are forbidden. A step up costs 2 and a step down 1. The CNF is clause-shaped (78 variables, 114 clauses, no auxiliaries), and the objective has 66 weighted literals. The table gives cost, `optimal-p`, backend `solve` and `final-conflict` calls, DPLL decisions and wall time for `:linear-bdd`, `:linear-totalizer`, `:stratified` and `:core-guided`. A counting subclass of `dpll-backend` with `:around` methods collects the call and decision counts. Every strategy reaches the DP optimum (9). `:core-guided` makes 536 final-conflict calls, because sat-backend-dpll keeps the default `backend-final-conflict`, which returns every assumption, so each core is shrunk by deletion.
4. **Anytime: budgets and timeouts.** `:budget` doubling from 0 under `:linear-bdd`: no model yet, then incumbents of cost 66, 39, 20, 11, 9 with `optimal-p` NIL, and finally the proof at 1,024 decisions. The cost never rises, which is checked. Then `:timeout` from 0.02 s to 30 s under `:linear-totalizer`. With no model yet when the limit runs out, `minimize` signals `sat-budget-exceeded`, with its budget or timeout slot set. A `handler-bind` that invokes `continue` doubles the limit (1, 2, 4, 8) until a model is found, and `use-value` makes `minimize` return a value instead.
5. **Hints: a warm start.** The observations change at positions 9 to 11. The previous optimum now costs 10 and the new optimum is 8. With `:hint` and `:budget 0`, the hint itself comes back with its new cost and `optimal-p` NIL. With the hint, `:linear-totalizer` and `:stratified` prove the new optimum in 4 and 5 calls. A partial hint (just the old levels) and an inconsistent hint (two levels at one position, which is ignored) still give the optimum. The cold `:linear-totalizer` run is stopped by the 30 s timeout (sat-protocol#7, see below). With `:phase :false` it proves the optimum in about 1,000 decisions.
6. **Lexicographic objectives.** Six level requests, three of which conflict pairwise with the hard rules. `(:lex violations cost)` minimises the requests not granted, then the chain cost. Each strategy runs under `:timeout 20` against a DP oracle over cost vectors. The DP optimum is (3 11), against cost 9 when only the cost is minimised (that grants only 1 request). The result is also checked against one big-M weighted sum, which `:lex` doesn't need. `:linear-totalizer` is stopped by the timeout here, and so is `:linear-totalizer :phase :false`. `:linear-totalizer :order #'first` is not.
7. **Final-conflict cores and minimal cores.** Six level pins as assumptions, unsatisfiable together:
   - `backend-final-conflict` at the CNF level, and `unsat-core :minimal nil`, return all six (DPLL keeps the default method).
   - `unsat-core` (minimal, by deletion) returns `x3=0 x5=2`. This is checked: the core is unsatisfiable, and dropping either pin makes it satisfiable.
   - With the two forbidden-pair rules given as `:named` soft formulas, the core names the rule that explains the conflict.
8. **Conditions and restarts.** A non-literal operand of `sat-at-most-k` signals `sat-invalid-formula`, and `use-value` replaces it. Bare names count as positive literals. A bad `:encoding`, `:strategy`, empty `(:lex)` or negative `:budget` signals a `type-error`. An unsatisfiable `minimize` returns exactly two values.

## Issues found

- **egao1980/sat-protocol#6.** With nothing but the strategy given, `(minimize f o)` runs the frozen 0.1 descent (`%linear-bdd`), whose BDD bound clause `(-N l N(i+1,k))` propagates only in BDD order. Any 0.2 key (`:timeout`, `:budget`, `:phase`, ...) selects the 0.2 descent with the propagating bound. On the section 3 chain that is 108,552 decisions and 76 s against 694 decisions and 0.44 s for the same `:linear-bdd`. Section 3 prints both rows.
- **egao1980/sat-protocol#7.** With `:phase` NIL, MINIMIZE passes its own (solution-guided) phases, and DPLL's DLIS also chooses freely among the bound's auxiliary variables. On two chain instances some `:linear-totalizer` refutations then take over a million decisions. The cold re-solve of section 5 took 1,727,932 decisions and 112 s unbounded, against 1,058 with `:phase :false` and 379 with `:order #'first`. The `:lex` objective of section 6 took 2,090,053 decisions and 270 s unbounded, against 612 with `:order #'first`; there `:phase :false` alone is not enough. Sections 5 and 6 run these under a timeout and print the comparison rows.

No step was dropped or worked around: the slow rows are printed as measured, and the extra rows only show the cause.
