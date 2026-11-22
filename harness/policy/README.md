# Policy demo: layered agent permissions on Datalog

`harness-demos-policy` uses **policy-protocol 0.1.0** (Track L, slice L1) from GHCR to decide a coding agent's tool calls. It loads three settings layers from text, decides twelve tool calls, explains them, and shows the conditions and restarts. All of it runs on **logic-protocol 0.1.0** with the **logic-backend-datalog 0.1.0** backend. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run.

## Run

```bash
cd harness/policy
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-policy")' \
  --eval '(harness-demos-policy:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`. There is no model, network or git step, and it runs in about a second once the packages are installed.

## What each section shows

1. **A layered policy, loaded from text.** Each layer is a `(defpolicy …)` string read by `load-policy`. The text is read as data: `*read-eval*` is off and symbols go into a temporary package. The layers, outer first:
   - **user** uses `:last-match` with `:default :ask`. A broad `ask` on `Bash` comes first, then `permit Bash(git *)`, `ask Bash(git push *)`, `permit Bash(make *)`, `Read`, `Edit`, and `WebFetch` on the host `github.com` (subdomains included).
   - **project** uses `:forbid-overrides`. It has `permit Read(src)` as a `(:path "src")` pattern, `forbid Read(.env*)`, `forbid Bash(rm -rf *)`, and `forbid Edit` when `(and (mode :plan) (not (approved :plan)))`. A malformed rule (`:resource 42`) is skipped.
   - **session** uses `:last-match`. It has `ask Bash(make *)`, then `permit Bash(make *)` when `(sandbox :confined)`, with the obligation `(:run :confined)`.

   The two combining modes give different answers on the same rules: `Bash(git status)` is a permit by `user-git` under `:last-match` and an ask by `user-bash` under `:forbid-overrides`.
2. **Decisions.** The table has twelve requests. Each row shows the effect, the deciding layer, the deciding rule (or the layer default), and the feedback or obligations. An inner layer can only tighten the result: the project forbids `.env`, `rm -rf` and edits in plan mode, the session asks for an unconfined `make test`, and `WebFetch(evil.example)` falls to the user default (ask).
3. **Proof and why-not.** `policy-explain` prints the permit of `make test` in a sandbox, followed by the trace of each layer. It also prints the logic-protocol `proof` trees for the deciding layer and for the session rule, whose proof includes the context fact `sandbox confined`. Then `decision-why-not` is called on four rules:
   - `:resource` on the forbidden `Read(.env)`;
   - `:condition`, with the false literal, for plan-mode editing;
   - `:overridden`, an earlier match that a later one replaced;
   - `NIL` for a rule that decided its own layer before an inner layer tightened the result.
4. **Conditions and restarts.** This section shows the `handler-bind` that loaded the layers. It invoked `skip-rule` on the `policy-syntax-error`, so the other four project rules were kept. It also lists the restarts that the handler sees (`skip-rule` and `use-value` from policy-protocol).
5. **A budget.** `evaluate-policy … :budget 4` signals `logic-budget-exceeded`. The handler invokes `continue`, and each `continue` grants another 4 steps. The evaluation finishes with the same decision and obligations as the run without a budget.
6. **Under the hood.** Each layer compiles once to a stratified Datalog program on logic-backend-datalog. The section prints the size of each program. Requests are evaluated with `:assume`, so the programs never change.

## Rough edges found

These are filed on [egao1980/policy-protocol](https://github.com/egao1980/policy-protocol/issues):

- Proofs are written in the compiler's internal relations (`%applies 3 :permit 0`), which use rule indices instead of rule ids, and there is no printer for them. ([#3](https://github.com/egao1980/policy-protocol/issues/3))
- A why-not literal from a policy loaded from text holds uninterned symbols (`(#:MODE :PLAN)`) instead of the canonical `("mode" "plan")`. ([#4](https://github.com/egao1980/policy-protocol/issues/4))
