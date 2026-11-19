# Harness demos

Runnable demos of the coding-agent harness work (plan: cl-workspace `docs/HARNESS-PLAN.md`), run against the **published GHCR packages** — evidence that each feature works, and a bug finder: anything a demo trips over is filed on the owning repo, not worked around.

Three cl-repo projects, one per area. Each `demo-output.txt` is the captured output of the last run (home and temp paths replaced with `~` / `<tmp>`).

| Project | System | Packages demonstrated |
|---------|--------|-----------------------|
| [`data/`](data/) | `harness-demos-data` | diff-protocol 0.2.0, revision-protocol 0.2.0, sql-orm 0.2.0, sql-migrate 0.2.0 |
| [`fs/`](fs/) | `harness-demos-fs` | cl-stack-pathlib 0.5.0, process-protocol 0.3.0, process-backend-uiop 0.2.0, revision-backend-git 0.1.0, revision-backend-jj 0.1.0 |
| [`agent/`](agent/) | `harness-demos-agent` | ai-agent-protocol 0.10.0, agent-tools 0.1.0, tui-protocol 0.2.0 (widgets), tui-backend-tty 0.1.0, llm-protocol-openai 0.4.0 |

## Run

From a project directory (dependencies come from GHCR through `cl-repo`; `--no-userinit` keeps a personal `~/.sbclrc` out of the image):

```bash
cd harness/data   # or fs, agent
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-data")' \
  --eval '(harness-demos-data:run-demos)' > demo-output.txt 2>&1
```

Needs: SBCL via Roswell, `cl-repo`, `git` (≥ 2.43) and `jj` 0.47.1 for `fs/`. The prompt-cache step in `agent/` talks to a local LM Studio (`http://127.0.0.1:1234/v1`, token from `LM_API_TOKEN` in the workspace `.env`; it is never printed) and reports that it was skipped when no server is reachable.

## What each set shows

- **data** — unified diffs, three-way merge with conflict markers (default and `:diff3`), tree diffs with rename and mode changes, parsing real `git diff` output and applying / inverting it; a revision DAG with tags and as-of, a ref conflict resolved through the `retry` restart, plans and drift on a target, merge projections resolved with `take-ours`, composite restore; complete schema diffs and a SQLite table rebuild that keeps rows; migrations with offline SQL and a failing step that is both `step-failed` and `sqlite:sqlite-error`, resumed with `retry-step`.
- **fs** — ranged reads, compare-and-swap writes with restarts, `.gitignore` walk / grep, content-addressed snapshots and restore, a sandboxed area seeing an external shell edit and committing past a conflict; binary process output, env merge vs replace, octet streams, timeouts; the git backend (commits, refs, diffs, a working copy whose drift is captured without touching the worktree or index, a read-only revision filesystem) and the jj backend (change ids across rewrites, as-of via the operation log, conflicted revisions).
- **agent** — "always allow" approvals saved to a rule store and reject-all-pending on a mock-model agent run; the seven file tools (diff previews, actionable failures, root escapes refused, permission resources); widgets on the headless terminal (text input with wide characters and paste, a permission dialog, a follow-tail viewport with spinner and status line); the real-terminal backend on a pty with `stty -g` identical before and after; `prompt_cache_key` sent to LM Studio and the reported usage.

## Issues found by these demos

egao1980/cl-stack-pathlib#28 (glob patterns), egao1980/sql-migrate#5 (doubled report), egao1980/revision-protocol#6 (`:id` on composite / projection / capture commits), egao1980/json-protocol#19 (jzon tag version), egao1980/asdf-native#5 (recompiles after native libraries).
