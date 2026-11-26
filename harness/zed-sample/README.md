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
| Headless rehearsal | done on cl-stack-code 0.12.3 with a live model, `Checks failed: 0`: [`rehearsal/`](rehearsal/) (below); re-checked on **0.14.0** with a scripted server on 2026-11-26, `Checks failed: 0` (below) |
| Recording in Zed | done 2026-11-23 on cl-stack-code **0.13.2** with Claude Opus 5.5: [`recording/`](recording/) (below) |

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

### 0.14.0 check with a scripted server

2026-11-26, after cl-stack-sample was refreshed to the latest stack ([cl-stack-sample#9](https://github.com/egao1980/cl-stack-sample/pull/9)), and `demo/zed-seeded-bug` was rebased onto it, still with one failing test. `rehearse.sh` ran unchanged, with `ZED_SAMPLE_LAUNCHER` set to the lisp-workspace launcher at 0.14.0. `HARNESS_DEMOS_*` pointed at a scripted OpenAI-compatible server on 127.0.0.1 with a dummy key, not a model. The script made seven moves: `read`; `bash sleep 1; cl-repo test; sleep 2` (a permission prompt, then Zed's terminal); `skill duration-units`; `source-read`; `edit-replace` (the fix); `cl-repo test` for both suites; and an answer streamed over 8 s. **`Checks failed: 0`.** `initialize` → cl-stack-code 0.14.0 in 0.53 s, `end_turn` after 29.6 s. This checks the ACP plumbing on 0.14.0, not a model. The committed `rehearsal/` files are still run 6.

Two stack bugs surfaced on the way, both filed:
- [http-backend-async#89](https://github.com/egao1980/http-backend-async/issues/89): a streamed body that ends at connection close (no `Content-Length`, no chunking) is cut at the first pause between writes. The scripted server therefore uses chunked encoding.
- [cl-repository#201](https://github.com/egao1980/cl-repository/issues/201): `cl-repo test` keeps a fasl compiled in the same second as the edit that follows it. That is why the scripted `cl-repo test` sleeps 2 s before the edit.

Earlier runs: run 4 never reached the model, because `src/rehearsal.lisp` had an extra close paren (a reader error the old output filter hid; fixed, and the filter now keeps compiler errors). Run 5 reached a server with no model loaded: its 400 answer surfaced as `NO-APPLICABLE-METHOD stream-element-type` on http-protocol's buffered body stream ([http-protocol#48](https://github.com/egao1980/http-protocol/issues/48), fixed in http-protocol 0.4.2; the same gap in http-backend-async: [#74](https://github.com/egao1980/http-backend-async/issues/74)).

## Recording

[`recording/zed-demo.mp4`](recording/zed-demo.mp4) (80 s, 1600×900). Zed 1.29.3 on macOS, a 1920×1080 window on the sample at `demo/zed-seeded-bug`. cl-stack-code **0.13.2** (the lisp-workspace launcher) with `--model anthropic:claude-opus-5-5`, cl-repo client 0.27.31. The prompt: "the tests fail, find and fix the bug".

| s | |
|-|-|
| 8 | `session/new`: the thread opens with the Context card `Context: AGENTS.md · skills: duration-units · rules: 1` |
| 17 | prompt sent |
| … | `cl-repo test` twice in Zed's terminal (one failure: `parse-duration-reads-units/3`, `"1H"`); `skill` duration-units; `glob`; `source-read` of `%parse-duration` (lisp-mcp, the session's Lisp image); **Rule loaded**: `.claude/rules/lisp.md`; `edit-replace` twice as a dry run (the locator), then for real with `expected_hash`; `cl-repo test` and `cl-repo test --system cl-stack-sample/agent`: all pass |
| 73 | the turn ends (`end_turn`) with the cause, the fix and the test result |

The fix is [`recording/fix.diff`](recording/fix.diff): `char-downcase` before the lookup. Also [`recording/zed-wire-log.txt`](recording/zed-wire-log.txt) (every JSON-RPC line, redacted as the rehearsal's), [`recording/events.tsv`](recording/events.tsv) and [`recording/keyframes/`](recording/keyframes/). No permission prompt this time: the sample's settings allow every call the agent made.

What the recording needed fixed first:

- cl-stack-code [#43](https://github.com/egao1980/cl-stack-code/issues/43) (0.13.1): Zed hands its crash reporter's Mach exception port to the agent; SBCL's `brk` traps then repeat forever and the agent never answers `session/new`. The binary clears the port at start.
- lisp-workspace [#172](https://github.com/egao1980/lisp-workspace/pull/172): the launcher ran `ros config show` (an SBCL, same trap) before reaching the executable; it reads Roswell's config file now.
- cl-stack-ssl [#23](https://github.com/egao1980/cl-stack-ssl/issues/23) (3.5.9.4, cl-stack-code 0.13.2): a dumped image kept the build process's `SSL_CTX` and faulted on the first HTTPS request (the Anthropic provider).
- cl-repository [#152](https://github.com/egao1980/cl-repository/issues/152) (0.27.31): `cl-repo test` in Zed's terminal hung on the same trap; the client's launcher clears the port before Roswell starts.
- Open: http-backend-async [#75](https://github.com/egao1980/http-backend-async/issues/75) (a POST on a long-idle pooled connection fails with "incomplete HTTP response": restart the agent before a take), Zed [#60389](https://github.com/zed-industries/zed/issues/60389) upstream.

## Record in Zed

The Zed side follows lisp-workspace [`editors/zed/`](https://github.com/egao1980/lisp-workspace/tree/main/editors/zed) (its README and [`agent_servers.json`](https://github.com/egao1980/lisp-workspace/blob/main/editors/zed/agent_servers.json)); this demo only puts the wire-logging wrapper in front of the launcher. Zed 1.29.3 on macOS.

1. **Build the executable** (Zed gets the `initialize` reply in about a second): `lisp-workspace/scripts/cl-stack-code.sh --build`. `--which` prints it: `~/.cache/cl-stack-code/cl-stack-code-0.14.0-<lisp>` (the launcher's pin since lisp-workspace 064427e; the recording above used 0.13.2).
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

   Without the wire log, `command` is the launcher itself and the last three entries go. With Anthropic (the recording above), `args` is `["--acp", "--model", "anthropic:claude-opus-5-5"]` and the env has `ANTHROPIC_API_KEY` instead of the `OPENAI_*` entries. Needs cl-stack-code 0.13.2 (TLS in the built binary) and cl-repo 0.27.31 (the client's launcher, for `cl-repo test` in Zed's terminal: re-run cl-repository's `install.sh`).
4. **Start Zed with the key in its environment only** (never in `settings.json`, never on screen). Quit Zed first; a running Zed keeps its old environment:

   ```bash
   export OPENAI_API_KEY=…
   /Applications/Zed.app/Contents/MacOS/zed --foreground <dir>/cl-stack-sample &
   ```

5. **Record.** The screen must be unlocked; System Events needs the Accessibility permission and `screencapture` the Screen Recording permission for the terminal that runs it.
   - Move Zed's window to a fixed region (here 0,30, 1920 × 1080) and record it with ffmpeg: `ffmpeg -f avfoundation -capture_cursor 1 -framerate 15 -i "0:none" -vf "crop=3840:2160:0:60,scale=1920:1080" -c:v libx264 -preset ultrafast -pix_fmt yuv420p zed.mov` (crop in pixels: twice the points on a Retina screen), stopped with SIGINT. Not `screencapture -v`: without a terminal it ignores the stop key, and every signal discards the movie.
   - Restart the agent first (a long-idle pooled connection fails the first request, #75 above).
   - In Zed: Agent panel (cmd-?) → **+** → **cl-stack-code** (Zed 1.29.3 has no palette entry for an external agent). The Context card appears first. Click into the message box before typing.
   - Type `the tests fail, find and fix the bug`, Enter. Approve each permission prompt (Allow once), slowly enough to read the diff of the `edit-replace`.
   - Stop when the turn ends: the reply to `session/prompt` in the wire log (its `"id"`, a UUID from Zed, with `"result"` or `"error"`).
   - `ffmpeg -i zed.mov -vf "scale=1600:-2,fps=15" -c:v libx264 -crf 30 -preset slow -pix_fmt yuv420p -movflags +faststart -an zed-demo.mp4`; keyframes with `ffmpeg -ss <t> -i zed-demo.mp4 -frames:v 1 keyframes/<n>.png` at the Context card, the `skill` / Rule loaded calls, the `edit-replace` permission prompt, the terminal's `cl-repo test` and the end of the turn.
   - Commit `zed-demo.mp4`, `keyframes/` and the wire log here; check the log for the user name and e-mail addresses first (the wrapper redacts the project path, `$HOME`, `$USER` and addresses).

## Files

- `cl-stack-code-logged.sh`: `tee`s cl-stack-code's stdin and stdout into the wire log, redacted.
- `rehearse.sh`, `harness-demos-zed-sample.asd`, `src/rehearsal.lisp`: the headless rehearsal.
- `rehearsal/`: its last output.
