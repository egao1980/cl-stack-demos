# Eval demo: Terminal-Bench tasks on agent-eval

`harness-demos-eval` uses **agent-eval 0.1.0** (Track E, slice E0) from GHCR. It runs a small Terminal-Bench-format dataset with three agents: `nop`, `oracle` and the **cl-stack-code** adapter (`-p --output-format stream-json`) on a live model. It prints rewards, pass@1, tokens, cost and time, saves run records and reports, compares the runs, and shows the conditions and restarts. Everything is in [`src/demo.lisp`](src/demo.lisp) and [`src/support.lisp`](src/support.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run. Home and temp paths in it are replaced with `~`, `<tmp>`, `<runs>` and `<demo>`. The reports are in [`reports/`](reports/).

Everything runs in agent-eval's **local environment**. The container lane (`agent-eval/podman`) needs compute-backend-podman 0.3.0, which is not published yet.

## Run

```bash
cd harness/eval
HARNESS_DEMOS_BASE_URL=http://<host>:8888/v1 HARNESS_DEMOS_API_KEY=… \
HARNESS_DEMOS_MODEL=unsloth/Qwen3.8-Flash-Next-GGUF PATH=/opt/homebrew/bin:$PATH \
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-eval")' \
  --eval '(harness-demos-eval:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL, `cl-repo`, and an OpenAI-compatible `/v1` server for section 4. The `HARNESS_DEMOS_*` variables work as in the other harness sets (see [`../README.md`](../README.md)):

- `HARNESS_DEMOS_BASE_URL` is the server. The default is LM Studio's `http://127.0.0.1:1234/v1`.
- `HARNESS_DEMOS_API_KEY` is the bearer key. When it is unset, `LM_API_TOKEN` is used.
- `HARNESS_DEMOS_MODEL` is the model id. When it is unset, the first id from `/v1/models` is used.
- `HARNESS_DEMOS_MODE` is `thinking` (the default) or `instruct`. It picks the sampling preset, which is sent as `--model-param` flags.

The adapter passes the key to cl-stack-code as `OPENAI_API_KEY` in its `:env`, along with `OPENAI_BASE_URL`. `:env` is not part of `agent-config`, so the key is never saved with a run, and the demo never prints it.

If the server does not answer `GET /v1/models`, section 4 prints `LIVE STEP FAILED` and the demo exits with code 2. There is no fallback to a mock. The other sections need no network and take about two seconds.

cl-stack-code is a dependency of the demo system, so `cl-repo` installs it (0.10.0). [`bin/cl-stack-code`](bin/cl-stack-code) is the command the adapter runs. It starts a fresh SBCL, loads `cl-stack-code` and calls `cl-stack-code:run-cli`, the entry point of the binary. `cl-repo run` configures the source registry of the Lisp it starts but exports no `CL_SOURCE_REGISTRY`. The demo therefore passes its own image's registry to the command as `CL_SOURCE_REGISTRY`.

## The dataset

Each of the three tasks under [`tasks/`](tasks/) is a Terminal-Bench 2 / Harbor task directory. It has `task.toml`, `instruction.md`, `environment/Dockerfile`, `tests/test.sh` and `solution/solve.sh`:

| Task | What the agent must do | Verifier |
|------|------------------------|----------|
| [`fix-greeting`](tasks/fix-greeting/) | Fix the one-line bug in `greet.sh` (a misspelt variable) | runs `greet.sh Lisp` and `greet.sh` |
| [`write-release-notes`](tasks/write-release-notes/) | Write `notes/release.txt` with three given lines | byte-for-byte compare |
| [`fix-lisp-mean`](tasks/fix-lisp-mean/) | Fix `mean` in `stats.lisp` (it divides by n+1) | `sbcl --script tests/test-stats.lisp` |

The tests and the solutions need no network. `allow_internet = true` is only there because an agent needs its model endpoint. The verifiers use `$AGENT_EVAL_LOGS_DIR` and `$AGENT_EVAL_TESTS_DIR`, falling back to `/logs` and `/tests`, so the same scripts run unchanged in a container.

The starting files live in `environment/app/`, and the Dockerfile has `COPY app/ /app/`. The tasks set no `docker_image`, because agent-eval/podman uses a `docker_image` in preference to building the Dockerfile, and only the build does the `COPY`. agent-eval's local environment builds no image, so the demo subclasses it as `seeded-local-environment`. An `:after` method on `start-environment` uploads `environment/app/` to the work dir. That subclass is the `:environment-factory` of every run.

[`tasks-broken/broken-verifier`](tasks-broken/broken-verifier/) is a fourth task in its own dataset. Its `test.sh` exits 0 and writes no reward.

## What each section shows

1. **The dataset.** `load-tb-dataset` loads `tasks/`. The section prints the dataset digest and each task's work dir, timeouts, image, solution and tags.
2. **nop and oracle.** `run-agent-eval` runs each agent. The section prints one row per trial (reward, reward source, status, turns, tokens, wall time), then pass@1, accuracy, the eval-protocol mean and the run totals. `save-run` writes `run.sexp`, `result.json` and the per-trial records. The demo copies `result.json` (paths scrubbed) to `reports/<agent>.json` and writes a Markdown summary to `reports/<agent>.md`. A `load-run` round trip gives an `equal` report.
3. **compare-runs, nop vs oracle.** The section shows wins, ties and losses, the mean delta, and the `comparison-report` Markdown. The Markdown and JSON are saved to `reports/nop-vs-oracle.*`.
4. **cl-stack-code, live.** The section probes the endpoint and builds `make-cl-stack-code-agent` with the model, `--max-turns 20`, the sampling preset and zero prices (a local model, so the cost column is a sum rather than n/a). It prints `agent-config` and the argv, then runs the agent on the same three tasks. For each trial it prints the stream-json event counts, the final answer and the verifier output. Then it compares the run with oracle (`reports/oracle-vs-cl-stack-code.*`).
5. **Conditions and restarts.** The broken verifier signals `reward-missing`:
   - a. `run-trial … :on-missing-reward :error` inside a `handler-bind` that prints the condition and its restarts, then invokes `score-zero`. The result is reward 0, source `missing`.
   - b. The same handler invokes `use-exit-code` instead. `test.sh` exited 0, so the reward is 1, source `exit-code`.
   - c. `run-agent-eval … :on-missing-reward :error`. The error reaches the caller as `eval-protocol:scorer-error`, with its restarts `skip-case`, `retry-case` and `score-as-failure`. The handler finds `score-zero` on the condition's cause, which is still live.
   - d. `compare-runs` on runs over different datasets signals `dataset-mismatch`. `continue` compares the shared tasks, and there are none here.

## Last run

The run of 2026-11-22 used `unsloth/Qwen3.8-Flash-Next-GGUF` (UD-IQ4_XS) on Unsloth Studio with the thinking preset. The section 4 model step took 453 s; the rest of the demo took about 2 s.

| Agent | pass@1 | Tokens in / out (cache) | Cost | Wall |
|-------|--------|-------------------------|------|------|
| nop | 0.00 (0/3) | — | n/a | 0.5 s |
| oracle | 1.00 (3/3) | — | n/a | 0.6 s |
| cl-stack-code | 1.00 (3/3) | 55,137 / 1,015 (47,960) | $0.0000 (priced at 0) | 453.2 s |

Per task, cl-stack-code took:

| Task | Turns | Wall |
|------|-------|------|
| fix-greeting | 4 | 74 s |
| fix-lisp-mean | 4 | 292 s |
| write-release-notes | 3 | 88 s |

On `fix-lisp-mean`, cl-stack-code's own end state was `error_during_execution`, but it had already fixed `mean` and the verifier passed. agent-eval keeps only the subtype, not the error text ([#10](https://github.com/egao1980/agent-eval/issues/10)). An earlier run, not committed, also passed 3/3, with 9 turns on `fix-lisp-mean`. Against oracle the comparison is 3 ties with a mean delta of +0.00. The token and cost deltas are n/a because oracle has neither.

## Real Terminal-Bench 2 tasks

The loader reads the real dataset as it is:

```bash
git clone https://github.com/harbor-framework/terminal-bench-2.git ~/terminal-bench-2
```

```lisp
(let ((dataset (agent-eval:load-tb-dataset #p"~/terminal-bench-2/" :include '("hello-world"))))
  (agent-eval:run-agent-eval dataset (agent-eval:make-oracle-agent)))
```

Real tasks need their images, because the Dockerfile installs the tools and `COPY`s the starting files, and the tests expect `/app`, `/logs` and `/tests`. Run them on the podman lane once compute-backend-podman 0.3.0 is published:

```lisp
(asdf:load-system "agent-eval/podman")
(agent-eval:run-agent-eval dataset agent
  :environment-factory (lambda (task dir) (agent-eval/podman:make-container-environment task :directory dir)))
```

In a container, the cl-stack-code `:command` must be on the image's `PATH` or mounted into it. The local environment shown here is only for trusted fixtures like these. Nothing in it is confined, it builds no image, and absolute paths in a real task's scripts (`/app`, `/logs`) reach the host.

## Rough edges found

These are filed on [egao1980/agent-eval](https://github.com/egao1980/agent-eval/issues):

- A JSON-object model param (`chat_template_kwargs`) is kept as a hash table in `agent-config`. `save-run` then prints it as `#.(…)`, and `load-run` cannot read `run.sexp`. Section 4 shows the failure. ([#4](https://github.com/egao1980/agent-eval/issues/4))
- The local environment passes env values to its wrapper on the command line. This includes the adapter's `OPENAI_API_KEY`, which any local user can then see in `ps`. ([#5](https://github.com/egao1980/agent-eval/issues/5))
- The local environment's work dir starts empty, with no way to seed a task's starting files. The demo's `seeded-local-environment` adds that. ([#6](https://github.com/egao1980/agent-eval/issues/6))
- There is no Markdown report for a single run, so the demo writes its own `reports/<agent>.md`. ([#7](https://github.com/egao1980/agent-eval/issues/7))
- There is no documented way to run cl-stack-code from a cl-repo install. The demo uses the `bin/cl-stack-code` launcher and passes `CL_SOURCE_REGISTRY` itself. ([#8](https://github.com/egao1980/agent-eval/issues/8))
- The cl-stack-code adapter drops the `error` text of the result event and keeps only its subtype. ([#10](https://github.com/egao1980/agent-eval/issues/10))
