# SAT sessions demo: incremental solving, minimising and cores

`harness-demos-sat-sessions` is a tour of **sat-protocol 0.3.0**'s incremental sessions (Track L, slice L2b-2) on **sat-backend-cdcl 0.1.0**, both from GHCR. A session is one growing set of clauses over one backend. It is solved and minimised many times, each time under that call's assumptions. sat-backend-cdcl 0.1.0 has no native session yet (it defines no `backend-session-open`), so every session here runs on the protocol's default stateless methods: each call re-solves the session CNF from scratch. Native sessions in sat-backend-cdcl: **pending**. No release with them is on GHCR yet.

The instance is a chain of 50 segments. Each segment takes one of three levels, and adjacent levels may differ by at most one. Every segment has an observed level from a fixed generator. Each session answer is printed next to the fresh front-end call on `(session-cnf s)` with the same assumptions (as literal formulas) on the same backend. Where an independent answer exists, it is also checked against a dynamic-programming oracle over level sequences, written in the demo. There are 105 checks. Timings are printed and not checked. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9.

## Run

```bash
cd harness/sat-sessions
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-sat-sessions")' \
  --eval '(harness-demos-sat-sessions:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`, and takes about five seconds. `run-demos` binds `sat:*sat-backend*` to NIL, and every call names its backend. It returns `NIL` if a check fails or a section stops on an error.

## What each section shows

1. **The chain, added incrementally.** Inside `with-sat-session`, each segment gets `session-add-variables 3`, then `session-add-clauses` with the formula `(sat-exactly-one … :encoding :pairwise)`, which the session encodes, then the two link clauses as integer lists. The counts grow from 3 variables and 4 clauses to 150 variables and 298 clauses, with no auxiliaries. `session-backend-state` is NIL, so the session is stateless.
2. **`session-solve` under changing assumptions.** Nine assumption sets run against the same session:
   - none;
   - `s0=0`, then `s0=0 s49=2`, then `s0=0 s49=2 s25=0`;
   - `s0=2 s1=0` (UNSAT);
   - `s10=0 s12=2` and `s10=0 s11=2` (UNSAT);
   - `s24/=0 s24/=1 s23=1`, built from negative literals;
   - all three levels of `s24` excluded (UNSAT).

   Each row shows the session status, the fresh `solve` status and the oracle's, and they agree on every row. On a stateless session the model is exactly `solve`'s, and it satisfies every clause and assumption. After an UNSAT call, `session-model` is `(NIL NIL)` and `session-unsat-core` holds the core.
3. **`session-minimize` with `(:lex requests effort)`.** The first level counts the segments that are not at their observed level. The second counts effort: level 1 costs 1 and level 2 costs 2. Every strategy (`:linear-bdd`, `:linear-totalizer`, `:stratified`, `:core-guided`) gives `(7 40)` with `optimal-p` T, the same cost, `optimal-p` and model as the fresh `minimize`, and the oracle's optimum. The levels are printed under the observed ones. Under assumptions the optima are `s0=2` → `(9 42)` and `s0=2 s49=0` → `(11 40)`. `s30=2 s31=0` is unsatisfiable, so the result is `NIL`.
4. **`session-unsat-core`.** The six assumptions `s20=1 s5=0 s30=1 s3/=1 s6=2 s40=0` are UNSAT together. The stored core is the backend's final conflict, reached through the default `backend-session-final-conflict`: `s5=0 s6=2`. `:minimal t` gives the same core, and so does the fresh `unsat-core` on the session CNF. The oracle confirms that the core is infeasible and that dropping either pin makes it feasible. After a SAT call there is no core.
5. **Budgets.** `session-minimize … :budget 100` has an incumbent when the budget runs out, so it signals `sat-budget-reached`. That is a condition, not an error, and it comes with three restarts:
   - Unhandled, the call returns the incumbent `(32 1)` with `optimal-p` NIL. That is the anytime answer of the fresh `minimize :budget 100`.
   - `use-best` returns the same incumbent after one signal.
   - `continue` doubles the budget: 100, 200, 400, 800, 1600, 3200. The incumbents never get worse: (32 1), (29 6), (27 11), (23 15), (16 24), (7 45). The search then proves `(7 40)`.
   - `store-value 100000` resumes with that budget and proves the optimum.

   With `:budget 0` there is no model yet, so the call signals `sat-budget-exceeded`, which is an error. `use-value :gave-up` makes the call return `(:gave-up nil)`, and the session stays usable.
6. **Timing: 50 incremental steps.** Each round opens a fresh session. Step k adds segment k, then runs three `session-solve`s (s0 at its observed level, the newest segment at each level) and one `session-minimize (:lex …)` over the k segments. Each call is followed by the same fresh call on `(session-cnf s)`, with the CNF copy included in its time. Over five rounds the medians are:
   - solve: 2.0 ms for 150 session solves against 4.2 ms fresh (0.013 against 0.028 ms per call, ratio 0.47);
   - minimize: 129.2 ms for 50 session minimizes against 128.7 ms fresh (2.58 against 2.57 ms per call, ratio 1.00).

   All 150 solves and 50 minimizes agree with the fresh calls and the oracle, models included. On the default methods the session and the fresh calls run the same search. The session saves only the CNF copy and the name-level translation, which is why solves are faster and minimizes are even. sat-backend-cdcl 0.1.0 defines no `backend-session-open` (checked with `find-method`), so native sessions are pending. A stateful backend would keep its learnt clauses from one step to the next. Timings vary by machine.
7. **A closed session.** After `with-sat-session` exits, `session-open-p` is NIL, a second `close-session` returns NIL, and `session-solve` signals `sat-session-closed`.

The 0.2.0 features (cardinality encodings, strategies against DPLL, anytime `minimize`, hints, DPLL against CDCL) are in [`../sat/`](../sat/).

## Issues

None. sat-protocol 0.3.0 and sat-backend-cdcl 0.1.0 matched every check here.
