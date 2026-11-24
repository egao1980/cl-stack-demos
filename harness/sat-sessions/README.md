# SAT sessions demo: incremental solving, minimising and cores

`harness-demos-sat-sessions` is a tour of **sat-protocol 0.3.0**'s incremental sessions (Track L, slice L2b-2) on **sat-backend-cdcl 0.2.0**, both from GHCR. A session is one growing set of clauses over one backend, solved and minimised many times under per-call assumptions. sat-backend-cdcl 0.2.0 makes sessions native: one persistent CDCL solver per session keeps its clauses, learnt clauses, activities and saved phases from one call to the next. Version 0.1.0 of this demo ran on sat-backend-cdcl 0.1.0, whose sessions use the protocol's stateless default methods (every call re-solves the session CNF). Its output is kept in [`demo-output-cdcl-0.1.0.txt`](demo-output-cdcl-0.1.0.txt), and section 7 prints its timing next to the native one.

The instance is a chain of 50 segments. Each segment takes one of three levels, adjacent levels may differ by at most one, and every segment has an observed level from a fixed generator. Each session answer is printed next to the fresh front-end call on `(session-cnf s)` with the same assumptions (as literal formulas) on the same backend. Where an independent answer exists, it is also compared with a dynamic-programming oracle over level sequences written in the demo. A native session keeps its phases, so its model may differ from the fresh one. The checks compare status, cost and `optimal-p`, and check every model against every clause and assumption. There are 135 checks. Timings are printed and not checked. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9.

## Run

```bash
cd harness/sat-sessions
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-sat-sessions")' \
  --eval '(harness-demos-sat-sessions:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`, and takes about 15 seconds, most of it in section 7's native rounds. `run-demos` binds `sat:*sat-backend*` to NIL, and every call names its backend. It returns `NIL` if a check fails or a section stops on an error.

## What each section shows

1. **The chain, added incrementally.** Inside `with-sat-session`, each segment gets `session-add-variables 3`, then `session-add-clauses` of the formula `(sat-exactly-one … :encoding :pairwise)` (the session encodes it), then the two link clauses as integer lists. The session ends with 150 variables and 298 clauses, and no auxiliaries. `session-backend-state` is the backend's own solver state, so the session is stateful.
2. **`session-solve` under changing assumptions.** Nine assumption sets run against the same session:
   - none;
   - `s0=0`, then `s0=0 s49=2`, then `s0=0 s49=2 s25=0`;
   - `s0=2 s1=0` (UNSAT);
   - `s10=0 s12=2`, and `s10=0 s11=2` (UNSAT);
   - `s24/=0 s24/=1 s23=1`, from negative literals;
   - all three levels of `s24` excluded (UNSAT).

   The session's status, the fresh `solve`'s and the oracle's agree on every row. The models satisfy every clause and assumption. Two of them differ from the fresh model, because the native solver keeps its saved phases.
3. **`session-minimize` with `(:lex requests effort)`.** The first level counts the segments not at their observed level. The second is the effort: level 1 costs 1, level 2 costs 2. Every strategy gives `(7 40)` with `optimal-p` T, which matches the fresh `minimize` and the oracle. Under assumptions, `s0=2` gives `(9 42)`, `s0=2 s49=0` gives `(11 40)`, and `s30=2 s31=0` is unsatisfiable (`NIL`).
4. **`session-unsat-core`.** The six assumptions `s20=1 s5=0 s30=1 s3/=1 s6=2 s40=0` are UNSAT together. The stored core is the session solver's own final conflict, `s5=0 s6=2`. `:minimal t` and the fresh `unsat-core` on the session CNF give the same core. The oracle confirms that the core is infeasible and that dropping either pin makes it feasible.
5. **Budgets.** Each call runs on a fresh session with the chain.
   - **Budget table.** Budgets from 50 to 6400 run next to the fresh `minimize` with the same `:budget`. A budgeted answer need not be the fresh one, because the native search learns as it goes. The native session proves `(7 40)` within 3200 decisions; the fresh `minimize` needs 6400.
   - **`sat-budget-reached`.** At `:budget 100` it is signalled with the incumbent `(32 1)`. It is a condition, not an error.
     - `use-best` returns the incumbent.
     - `continue` doubles the budget: 100, 200, 400, 800, 1600. The incumbents never get worse: (32 1), (29 6), (26 12), (7 45), (7 45). Then it proves `(7 40)`.
     - `store-value 100000` proves it in one resume.
   - **No model yet.** `:budget 0` signals `sat-budget-exceeded`, an error. `use-value` returns `(:gave-up nil)`, and the session stays usable.
6. **Learnt clauses survive.** PHP(7,6) has 133 clauses over 42 variables, each guarded by `-S`. The first `session-solve :assumptions (S)` is UNSAT after **740 conflicts** (932 decisions, 740 learnt clauses). The same call again, and once more, is UNSAT with **0 conflicts** and 0 decisions. What call 1 learnt makes `-S` a level-0 fact. The core is `(S)` each time. Without the assumption the session is SAT with `-S`. A fresh `solve` on the session CNF starts from nothing and refutes it again with 740 conflicts.
7. **Timing: 50 incremental steps.** Step k adds segment k, then runs three `session-solve`s and one `session-minimize (:lex …)`. Each is followed by the same fresh call on `(session-cnf s)`, with the CNF copy included. A second workload leaves the minimizes out. All values are medians over five rounds, in milliseconds summed over the 50 steps:

   | sessions | solve: session | fresh | ratio | minimize: session | fresh | ratio | session variables |
   |---|---:|---:|---:|---:|---:|---:|---:|
   | cdcl 0.1.0, protocol defaults (recorded) | 2.0 | 4.2 | 0.47 | 129.2 | 128.7 | 1.00 | 150 |
   | cdcl 0.2.0, native | 325.5 | 137.6 | 2.37 | 1395.2 | 596.1 | 2.34 | 77,579 |
   | cdcl 0.2.0, native, solves only | 0.9 | 3.8 | 0.23 | – | – | – | 150 |

   All 150 solves and 50 minimizes agree with the fresh calls and the oracle in every round.
   - **Solves only.** The native session is 4 times faster than fresh re-solves (0.006 against 0.026 ms per solve). It is also about twice as fast as the recorded stateless session.
   - **With the minimizes.** Each native `session-minimize` leaves its encoding's auxiliaries in the session; that is sat-protocol's session path. After the 50 steps the session has 77,579 variables. One more `session-solve` takes 53,135 decisions (7.6 ms), against 98 decisions (0.014 ms) without the minimizes. The retired auxiliaries keep their activities in the decision heap and are decided first (egao1980/sat-backend-cdcl#4). The fresh calls on `session-cnf` pay for the extra variables too, at 0.92 ms per solve (egao1980/sat-protocol#10).

   The recorded row comes from `demo-output-cdcl-0.1.0.txt`. sat-backend-cdcl 0.2.0 makes every CDCL session native, so the stateless configuration can't run in the same image. Timings vary by machine.
8. **A closed session.** After `with-sat-session` exits, `session-open-p` is NIL, a second `close-session` returns NIL, and `session-solve` signals `sat-session-closed`.

The 0.2.0 features (cardinality encodings, strategies against DPLL, anytime `minimize`, hints, DPLL against CDCL) are in [`../sat/`](../sat/).

## Issues

- [egao1980/sat-backend-cdcl#4](https://github.com/egao1980/sat-backend-cdcl/issues/4), filed by this demo. After `session-minimize` calls, a native session decides the retired auxiliaries of their encodings first. On this chain, `session-solve` is 2.37 times slower than a fresh re-solve; on a 100-variable path cover minimised 20 times, it is 32 times slower than before. Every answer is still right.
- [egao1980/sat-protocol#10](https://github.com/egao1980/sat-protocol/issues/10), item 1 (growth), with this demo's numbers in a comment. The session path allocates fresh auxiliaries per `session-minimize` and never reclaims them, so `session-cnf` grows to 77,579 variables over the 50 steps.
