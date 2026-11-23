# MCP demo: cl-stack-code finds the breaking commit through a git MCP server

cl-stack-code's interactive mode in iTerm2 on [cl-stack-sample](https://github.com/egao1980/cl-stack-sample) at the branch `demo/zed-seeded-bug` (one failing test: `"1H"` is not parsed as one hour). The project has a [`.mcp.json`](mcp.json) with one server, the Python [`mcp-server-git`](https://pypi.org/project/mcp-server-git/) run by `uvx`. Prompt: "the tests fail on this branch: use the git MCP server to find the commit that broke them, then fix the bug".

## Recording

[`recording/mcp-demo.mp4`](recording/mcp-demo.mp4) (66 s, 1600×900), 2026-11-23. cl-stack-code **0.13.3** (lisp-workspace launcher) with `--model anthropic:claude-opus-5-5`, mcp-server-git from PyPI via uvx 0.13.6.

| s | |
|-|-|
| 6 | the project server asks for trust: "MCP server git from …/.mcp.json wants to run: uvx mcp-server-git --repository ." — 1. Yes, trust it in this project (saved as `enabledMcpjsonServers` in `~/.config/cl-stack-code/settings.json`) |
| 13 | `/mcp`: `git (project, stdio): connected · 14 tools`, as `mcp__git__*` |
| 20 | prompt sent; `cl-repo test` fails |
| 25 | `mcp__git__git_log` asks (MCP tools ask by default): Allow once |
| 31 | `mcp__git__git_show` of 373d16c asks: Allow once. "The commit dropped char-downcase, so 1H no longer parses." |
| 35 | `skill` duration-units, `source-read`, `edit-replace` (dry run, then with the form's hash) |
| 41 | `bash sleep 1; cl-repo test …` asks (`sleep` is not in the project's allow rules): Allow once |
| 55 | both suites pass; the answer names the commit, the cause and the one-line fix |

Fix: [`recording/fix.diff`](recording/fix.diff). Event times: [`recording/events.tsv`](recording/events.tsv). Keyframes: [`recording/keyframes/`](recording/keyframes/).

## Run it

1. Clone the sample at the demo branch: `git clone -b demo/zed-seeded-bug https://github.com/egao1980/cl-stack-sample`, then `cl-repo sync` in it.
2. Copy [`mcp.json`](mcp.json) to `.mcp.json` in the clone. Needs [uv](https://docs.astral.sh/uv/) (`uvx`).
3. Optional, for long turns: `.cl-stack-code/settings.local.json` with `{"modelParams": {"max_tokens": 32000}}`.
4. [`run-agent.sh`](run-agent.sh) starts the interactive mode with the Anthropic provider; it reads `ANTHROPIC_API_KEY` from Zed's `settings.json` (export it yourself otherwise). Paths: the clone at `/Users/Shared/cl-demo/mcp/cl-stack-sample`, the launcher at `~/Projects/cl-workspace/.lisp-workspace/scripts/cl-stack-code.sh`.
5. [`record.sh`](record.sh) is the take: it resets the clone and the trust answer, starts ffmpeg (AVFoundation, cropped to the window), opens the iTerm2 window, and drives it through that window's session only (`write text`; no global keystrokes), answering each dialog with 1.

Headless, the same server with `--mcp-config` (trusted, flag scope) and `--allowed-tools 'mcp__git__*'`: `init` lists it `connected` with 14 tools; the run answers in 3 turns.
