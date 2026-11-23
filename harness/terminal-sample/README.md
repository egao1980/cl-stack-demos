# Terminal demo: cl-stack-code fixes a bug in cl-stack-sample

The same task as [`../zed-sample`](../zed-sample/), with cl-stack-code's own interactive mode in a terminal (iTerm2) instead of Zed. The project is a fresh clone of [cl-stack-sample](https://github.com/egao1980/cl-stack-sample) at **`demo/zed-seeded-bug`**, where `%parse-duration` looks the unit letter up without `char-downcase`, so `"1H"` is an "unknown unit" and `parse-duration-reads-units` fails. The prompt is "the tests fail, find and fix the bug".

## Recording

[`recording/terminal-demo.mp4`](recording/terminal-demo.mp4) (63 s, 1600×900), 2026-11-23. cl-stack-code **0.13.2** (lisp-workspace launcher, pre-built executable) with `--model anthropic:claude-opus-5-5`, cl-repo client 0.27.31, iTerm2 at 1920×1080.

| s | |
|-|-|
| 5 | the interactive mode is up: status line `anthropic:claude-opus-5-5 · default · λ image · /Users/Shared/cl-demo/cl-stack-sample/` (the λ marks the session's live Lisp image) |
| 18 | prompt sent; "Working… (Esc to interrupt)" |
| 20 | `cl-repo test`: tests failed for cl-stack-sample/tests |
| 30 | `skill` duration-units |
| 35 | `source-read` of `%parse-duration` (lisp-mcp, the session's image) |
| 45 | `edit-replace` (structural, with the form's hash) |
| 55 | `cl-repo test` and `cl-repo test --system cl-stack-sample/agent`: all pass |
| 58 | the answer: the bug, the one-line fix, the tests |

The fix is [`recording/fix.diff`](recording/fix.diff); keyframes at 3, 14, 30, 45 and 60 s in [`recording/keyframes/`](recording/keyframes/).

## Run it

1. Clone the sample at the demo branch and sync it: `git clone -b demo/zed-seeded-bug https://github.com/egao1980/cl-stack-sample` and `cl-repo sync` in it.
2. Optional, for long turns: `.cl-stack-code/settings.local.json` with `{"modelParams": {"max_tokens": 32000}}` (gitignored).
3. [`run-agent.sh`](run-agent.sh) starts the interactive mode in the clone with the Anthropic provider. It reads `ANTHROPIC_API_KEY` from Zed's `settings.json` (the `cl-stack-code` agent's `env`), so the key never appears on screen; export it yourself instead if you don't use Zed. It expects the clone at `/Users/Shared/cl-demo/cl-stack-sample` and the lisp-workspace launcher at `~/Projects/cl-workspace/.lisp-workspace/scripts/cl-stack-code.sh`.
4. Recording: an iTerm2 window made with AppleScript (`create window with default profile command ".../run-agent.sh"`, bounds 0,30 to 1920,1110), the prompt sent to that window's session (`write text … newline no`, then `character id 13`), and ffmpeg's AVFoundation capture cropped to the window (see `../zed-sample` for the command). Set the font size before the agent starts: changing it twice in quick succession left the TUI drawing for the old size until the next resize.
