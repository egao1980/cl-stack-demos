# Zed demo: cl-stack-code fixes a bug in cl-stack-sample

cl-stack-code runs in [Zed](https://zed.dev) as an external agent over the Agent Client Protocol (`cl-stack-code --acp`). Zed's project is a fresh clone of [cl-stack-sample](https://github.com/egao1980/cl-stack-sample) at the branch **`demo/zed-seeded-bug`**. On that branch `%parse-duration` looks the unit letter up without `char-downcase`, so `"1H"` is an "unknown unit" and one test (`parse-duration-reads-units`) fails. The prompt is:

> The tests fail. Find and fix the bug.

The session uses:

- the sample's `AGENTS.md`;
- lisp-mcp's tools through the session's own Lisp image (`lispImage: spawn` in `.cl-stack-code/settings.json`): `source-read`, `edit-replace`, `repl-eval`, `debug-*`;
- the sample's skill `.claude/skills/duration-units` (the `skill` tool) and its rule `.claude/rules/lisp.md` (`paths: ["**/*.lisp"]`, loaded on the first `.lisp` read);
- Zed's buffers (`fs/read_text_file`), Zed's terminal (`terminal/*`) for `cl-repo test`, and Zed's permission prompt (`session/request_permission`) for every call the project settings don't allow.

## Status

| | |
|-|-|
| Headless rehearsal | done on cl-stack-code 0.12.3, `Checks failed: 0`: [`rehearsal/`](rehearsal/) (below) |
| Recording in Zed | **not recorded yet**: the Mac's screen was locked. The steps are in "Record in Zed" below |

## Headless rehearsal

[`rehearse.sh`](rehearse.sh) does what Zed does, without the screen: it clones the sample at `demo/zed-seeded-bug`, runs `cl-repo sync`, and starts [`cl-stack-code-logged.sh`](cl-stack-code-logged.sh) `--acp --model openai` under harness/acp's scripted ACP client (`harness-demos-zed-sample:rehearse`, which loads `../acp/src/client.lisp`). The client advertises `fs` and `terminal`, sends the prompt, answers every permission request with "Allow once", runs terminal commands for real, prints each update with the seconds since the prompt, and checks the result with an independent `cl-repo test`.

```bash
cd harness/zed-sample
HARNESS_DEMOS_BASE_URL=http://<host>:8888/v1 HARNESS_DEMOS_API_KEY=… \
HARNESS_DEMOS_MODEL=unsloth/Qwen3.8-Flash-Next-GGUF \\
ZED_SAMPLE_LAUNCHER=/abs/path/to/lisp-workspace/scripts/cl-stack-code.sh ./rehearse.sh
```

The model must be loaded on the server: an Unsloth Studio server with no model loaded answers 400 "No model loaded". Load it with enough context for the agent's tools and prompt (the run below used 48k): `POST /api/inference/load` with `{"model_path": "unsloth/Qwen3.8-Flash-Next-GGUF", "gguf_variant": "UD-IQ4_XS", "max_seq_length": 49152}`. When the run fails, the unfiltered SBCL output stays in `rehearsal/output.raw` (or `$ZED_SAMPLE_RAW_LOG`), so a load or compile error is never cut off.

It needs `cl-stack-code` (the lisp-workspace launcher, or `ZED_SAMPLE_LAUNCHER=/path/to/cl-stack-code.sh`), `cl-repo`, Roswell and git. Outputs: [`rehearsal/output.txt`](rehearsal/output.txt) (the client's log), [`rehearsal/wire-log.txt`](rehearsal/wire-log.txt) (every JSON-RPC line, `->` client to agent, `<-` agent to client) and [`rehearsal/fix.diff`](rehearsal/fix.diff). The project path is `<project>`, the home directory `~`, the user name `<user>` and e-mail addresses `<email>`. The key is only in the environment.

### Last run

Run 6, 2026-11-23, cl-stack-code **0.12.3** (lisp-workspace launcher, pre-built executable), `unsloth/Qwen3.8-Flash-Next-GGUF` (UD-IQ4_XS, 48k context) on Unsloth Studio, prompt "The tests fail. Find and fix the bug.". **`Checks failed: 0`**, 560 s wall, 1596 wire messages.

- `initialize` answered in 0.78 s. The first update (0.0 s) is the **Context card**: `Context: AGENTS.md · skills: duration-units · rules: 1`.
- 61.9 s: `bash ls -R` and `git log` in client terminals; 84.6 s: `cl-repo test` shows the failing `parse-duration-reads-units`.
- 135.4 s: the **`skill` tool** loads `duration-units`; 135.5 s, on the first `read src/duration.lisp`: **`Rule loaded: .claude/rules/lisp.md`** (the `**/*.lisp` rule, lazily).
- `source-read` (line 24), then three **`edit-replace`** calls (lisp-mcp structural edits), each behind a **permission prompt**: the first (287 s) is refused by the tool for a bad locator, the second (321.6 s) writes the fix (+3 −1), the third (386.3 s) tidies it into a `let*` (+2 −3). The diff is [`rehearsal/fix.diff`](rehearsal/fix.diff): `(assoc (char-downcase (char text pos)) *units*)`.
- **`cl-repo test` in a client terminal** (`terminal/create`), after the fix, for the main and `/agent` systems: all pass. 7 `session/request_permission` in all, each answered "Allow once".
- **Live streaming:** 1263 thought chunks at 221 distinct seconds over the whole turn (every minute from 0 to 8), and the final answer as 208 message chunks over 31 s (510.9 to 540.3 s).
- `stopReason end_turn` after 542.3 s; the independent `cl-repo test` passes.
- The model server did not drop the connection in this run (no retry needed).

Earlier runs: run 4 never reached the model, because `src/rehearsal.lisp` had an extra close paren (a reader error the old output filter hid; fixed, and the filter now keeps compiler errors). Run 5 reached a server with no model loaded: its 400 answer surfaced as `NO-APPLICABLE-METHOD stream-element-type` on http-protocol's buffered body stream ([http-protocol#48](https://github.com/egao1980/http-protocol/issues/48), fixed in http-protocol 0.4.2; the same gap in http-backend-async: [#74](https://github.com/egao1980/http-backend-async/issues/74)).

## Record in Zed

The Zed side follows lisp-workspace [`editors/zed/`](https://github.com/egao1980/lisp-workspace/tree/main/editors/zed) (its README and [`agent_servers.json`](https://github.com/egao1980/lisp-workspace/blob/main/editors/zed/agent_servers.json)); this demo only puts the wire-logging wrapper in front of the launcher. Zed 1.29.3 on macOS.

1. **Build the executable** (Zed gets the `initialize` reply in about a second): `lisp-workspace/scripts/cl-stack-code.sh --build`. `--which` prints it: `~/.cache/cl-stack-code/cl-stack-code-0.12.3-<lisp>`.
2. **Clone and sync** the sample at the demo branch, then load the model on the server (see above):

   ```bash
   git clone -b demo/zed-seeded-bug https://github.com/egao1980/cl-stack-sample <dir>/cl-stack-sample
   (cd <dir>/cl-stack-sample && cl-repo sync)   # the first image spawn would sync on camera otherwise
   ```

3. **Add the agent** to `~/.config/zed/settings.json`: `editors/zed/agent_servers.json`, with `command` set to this directory's `cl-stack-code-logged.sh` and three extra env entries for it. `PATH` must hold `cl-repo` and `ros`:

   ```json
   "agent_servers": {
     "cl-stack-code": {
       "type": "custom",
       "command": "/abs/path/to/cl-stack-demos/harness/zed-sample/cl-stack-code-logged.sh",
       "args": ["--acp", "--model", "openai"],
       "env": {
         "OPENAI_BASE_URL": "http://<host>:8888/v1",
         "OPENAI_MODEL": "unsloth/Qwen3.8-Flash-Next-GGUF",
         "OPENAI_TIMEOUT": "600",
         "OPENAI_STREAM_IDLE_TIMEOUT": "300",
         "PATH": "/Users/you/.local/bin:/Users/you/.roswell/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin",
         "CL_STACK_CODE_LAUNCHER": "/abs/path/to/lisp-workspace/scripts/cl-stack-code.sh",
         "ZED_SAMPLE_WIRE_LOG": "/abs/path/to/zed-wire-log.txt",
         "ZED_SAMPLE_REDACT": "<dir>/cl-stack-sample"
       }
     }
   }
   ```

   Without the wire log, `command` is the launcher itself and the last three entries go.
4. **Start Zed with the key in its environment only** (never in `settings.json`, never on screen). Quit Zed first; a running Zed keeps its old environment:

   ```bash
   export OPENAI_API_KEY=…
   /Applications/Zed.app/Contents/MacOS/zed --foreground <dir>/cl-stack-sample &
   ```

5. **Record.** The screen must be unlocked; System Events needs the Accessibility permission and `screencapture` the Screen Recording permission for the terminal that runs it.
   - Move Zed's window to a fixed region (here 60,40, 1800 × 1150) and start `screencapture -v -V <max seconds> -R60,40,1800,1150 -x zed.mov`.
   - In Zed: Agent panel (cmd-?) → **+** → **cl-stack-code** (or the command palette: `agent: new external agent thread`). The Context card appears first.
   - Type `the tests fail, find and fix the bug`, Enter. Approve each permission prompt (Allow once), slowly enough to read the diff of the `edit-replace`.
   - Stop when the turn ends: the reply to `session/prompt` in the wire log (`<- {"id":N,…"stopReason":"end_turn"}`).
   - `ffmpeg -i zed.mov -vf "scale=1600:-2,fps=15" -c:v libx264 -crf 30 -preset slow -pix_fmt yuv420p -movflags +faststart -an zed-demo.mp4`; keyframes with `ffmpeg -ss <t> -i zed-demo.mp4 -frames:v 1 keyframes/<n>.png` at the Context card, the `skill` / Rule loaded calls, the `edit-replace` permission prompt, the terminal's `cl-repo test` and the end of the turn.
   - Commit `zed-demo.mp4`, `keyframes/` and the wire log here; check the log for the user name and e-mail addresses first (the wrapper redacts the project path, `$HOME`, `$USER` and addresses).

## Files

- `cl-stack-code-logged.sh`: `tee`s cl-stack-code's stdin and stdout into the wire log, redacted.
- `rehearse.sh`, `harness-demos-zed-sample.asd`, `src/rehearsal.lisp`: the headless rehearsal.
- `rehearsal/`: its last output.
