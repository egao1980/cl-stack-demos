# Harness demos

Runnable demos of the coding-agent harness work (plan: cl-workspace `docs/HARNESS-PLAN.md`), run against the **published GHCR packages** — evidence that each feature works, and a bug finder: anything a demo trips over is filed on the owning repo, not worked around.

Three cl-repo projects, one per area. Each `demo-output.txt` is the captured output of the last run (home and temp paths replaced with `~` / `<tmp>`).

| Project | System | Packages demonstrated |
|---------|--------|-----------------------|
| [`data/`](data/) | `harness-demos-data` | diff-protocol 0.3.0 (engine extension API), diff-backend-lisp-source 0.1.0, revision-protocol 0.3.0, sql-orm 0.3.0, sql-migrate 0.2.1 |
| [`fs/`](fs/) | `harness-demos-fs` | cl-stack-pathlib 0.6.1, process-protocol 0.4.0, process-backend-uiop 0.3.1, revision-backend-git 0.2.0, revision-backend-jj 0.2.0 |
| [`agent/`](agent/) | `harness-demos-agent` | ai-agent-protocol 0.10.0, agent-tools 0.2.0 (file and shell tools), tui-protocol 0.2.0 (widgets), tui-markdown 0.1.0, tui-backend-tty 0.1.1, llm-protocol-openai 0.4.0 |

## Run

From a project directory (dependencies come from GHCR through `cl-repo`; `--no-userinit` keeps a personal `~/.sbclrc` out of the image):

```bash
cd harness/data   # or fs, agent
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-data")' \
  --eval '(harness-demos-data:run-demos)' > demo-output.txt 2>&1
```

Needs: SBCL via Roswell, `cl-repo`, `git` (≥ 2.43; ≥ 2.56 once revision-backend-git 0.4.0 resolves) and `jj` 0.47.1 for `fs/`. The prompt-cache step in `agent/` talks to a local LM Studio (`http://127.0.0.1:1234/v1`, token from `LM_API_TOKEN` in the workspace `.env`; it is never printed) and reports that it was skipped when no server is reachable.

## What each set shows

- **data** — unified diffs, three-way merge with conflict markers (default and `:diff3`), tree diffs with rename and mode changes, parsing real `git diff` output and applying / inverting it; a revision DAG with tags and as-of, a ref conflict resolved through the `retry` restart, plans and drift on a target, merge projections resolved with `take-ours`, composite restore (now with a caller `:id`); complete schema diffs and a SQLite table rebuild that keeps rows; migrations with offline SQL and a failing step that is both `step-failed` and `sqlite:sqlite-error`, resumed with `retry-step`.
  - **Demo 5, diff-backend-lisp-source** — a real file, process-protocol's `src/protocol.lisp` at 0.3.0 and 0.4.0 (committed under [`data/fixtures/`](data/fixtures/)): its top-level forms as units (header comments travel with their form), the 0.3.0 → 0.4.0 diff at `:summary` / `:changes` / `:content` (form adds, nested leaf edits such as `defun kill/2`), `unified-patch` with a path, byte-for-byte apply / invert / `:sexp` round trip; a comment edit, a rename (`alive-p` → `live-p`) and a move; a three-way merge of the real 0.4.0 change with edits to other forms (clean, symmetric) and against an edit of the same `defun kill` (one conflict, the two sides rendered with the `:text` engine); `lisp-source-unreadable` falling back through `use-text-diff`.
  - **Demo 6, diff-protocol 0.3.0 engine API** — an `:env` engine written in the demo (states are alists of strings, not text) registered with `register-diff-engine`, with `apply-change` and `render-unified-content` only: `:unified` of its own diffs, `parse-diff :sexp` finding it by name, diff-protocol's `skip-change` restart on a stale state, the default `merge-states` (one clean key, one conflict), and `.env` files in a tree diff rendered as hunks instead of `Binary files … differ`.
- **fs** — ranged reads, compare-and-swap writes with restarts, `.gitignore` walk / grep, content-addressed snapshots and restore, a sandboxed area seeing an external shell edit and committing past a conflict; binary process output, env merge vs replace, octet streams, timeouts; the git backend (commits, refs, diffs, a working copy whose drift is captured without touching the worktree or index, a read-only revision filesystem) and the jj backend (change ids across rewrites, as-of via the operation log, conflicted revisions).
  - **Demo 5, cl-stack-pathlib 0.6.1** — fnmatch globs (`?`, `[0-9]`, `[!0-9]`, escaped `\[x\]`, `**`, `rglob`) giving the same results on the local and the memory filesystem, `match-p`; probes on a file, a directory, links to each, a dangling link and a link cycle (`stat(2)` vs `lstat(2)` views, `symlink-loop` as a `path-error`). The `rglob` checks in Demo 1 now pass (cl-stack-pathlib#28 fixed in 0.6.0).
  - **Demo 6, process-protocol 0.4.0** — `supports-p`, `process-pid`; a shell whose background child (the grandchild) survives a plain `kill` but is stopped by `:process-group t` + `kill :tree t` (both in the shell's process group, checked with `ps -o pgid=`); `run :process-group t :timeout 1` whose grandchild never writes its marker; `kill :tree t` on a handle without a group signalling `process-group-error` with a `continue` restart.
- **agent** — "always allow" approvals saved to a rule store and reject-all-pending on a mock-model agent run; the seven file tools (diff previews, actionable failures, root escapes refused, permission resources); widgets on the headless terminal (text input with wide characters and paste, a permission dialog, a follow-tail viewport with spinner and status line); the real-terminal backend on a pty with `stty -g` identical before and after; `prompt_cache_key` sent to LM Studio and the reported usage.
  - **Demo 6, tui-markdown 0.1.0** — a model answer (headings, emphasis, strong, strike, inline code, a link with OSC 8 hyperlink, nested and ordered lists, a code block, an aligned table with wide characters, a quote) parsed to its AST and rendered at 60 and 34 cells in a viewport on the headless driver (the screen text is in the output, with per-run styles); a `markdown-stream` fed in chunks whose committed lines never change; a unified diff with line numbers, `⋮` between hunks and a word-level highlight, and a split layout from a diff-protocol value.
  - **Demo 7, agent-tools 0.2.0 shell tools** — foreground `bash` on the pty executor (ANSI and progress-bar cleaning, stderr merged, `[exit code 3]`, `workdir`, the `+shell-environment+`, stdin at EOF), the head + tail output cap, an unhandled timeout (killed after 0.8 s) and the `shell-timeout` restarts (`continue`, then `run-in-background`); a background job driven with `job_output`, `job_input` (a `read` answered), `job_list`, `job_kill`; the scanner's resources for ten command lines (`cd` tracking, writes, `*` for what it cannot model); a mock-model run where `ls` is allowed, `ls && rm …` is refused by `("bash" "program:rm" :deny)` with its feedback, a background job completes, and an undecided command asks. An allow on `program:ls` alone leaves `bash:ls src` undecided (it asks), so the run allows both `bash:ls*` and `program:ls`; that is the documented rule ("every resource must be decided").

## Windows

All three projects run on windows-latest in CI. The POSIX tools come from Git for Windows on PATH (`sh`, `env`, `printf`, `cat`, `sleep`); temp directories are cleared of read-only attributes (git objects) before deletion. Skipped there, each with a printed reason: fs Demo 5 symlink probes (no `fs-create-symlink` on Windows), fs Demo 6 process-group / tree-kill steps (POSIX groups and signals), agent Demo 4b (the pty child; Unix-only by design). Agent Demo 7 runs the shell tools with Git for Windows' `sh` instead of `cmd.exe` (executor `:process`).

## Issues found by these demos

Open: egao1980/diff-backend-lisp-source#3 (`:lisp` tree files still render as `Binary files … differ`; no `render-unified-content` method yet — Demo 5f shows the gap), egao1980/cl-stack-pathlib#33 (`file-size` of a directory: local returns `st_size`, memory signals `path-not-found` — fs Demo 5 prints the difference), egao1980/ai-agent-protocol#26 (the permission-denied text lists every resource of the call, the allowed `bash:ls` / `program:ls` included — agent Demo 7g). The agent output still shows native-dependent files (cl-stack-brotli, cl-stack-zstd) compiling mid-run: egao1980/asdf-native#5 is fixed in asdf-native 0.3.2, but the local cl-repository client preloads 0.3.1 (cl-repo prints that warning at the top of the output), so the fix is not active in this image.

Fixed since the first run, confirmed by these outputs: egao1980/cl-stack-pathlib#28 (glob patterns, 0.6.0), egao1980/sql-migrate#5 (doubled report, 0.2.1), egao1980/revision-protocol#6 (`:id` on composite / projection / capture commits, 0.3.0), egao1980/json-protocol#19 (jzon tag version; the agent project now pins json-backend-jzon 0.2.1).

No demo step was dropped.
