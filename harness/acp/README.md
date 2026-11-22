# ACP demo: a Lisp agent driving Zed

`harness-demos-acp` is a scripted coding agent written in Lisp. It speaks the
[Agent Client Protocol](https://agentclientprotocol.com) on stdio through
**acp-protocol 0.1.0** and **acp-backend-stdio 0.1.0**, both from GHCR. Its one
turn fixes the off-by-one in [`sample/fib.lisp`](sample/) and uses every
client-side ACP feature a coding agent needs:

1. it streams `agent_message_chunk` text and a four-step `plan`, which it updates as it goes;
2. it sends a `read` tool call, then `fs/read_text_file` (the editor's buffer, not the disk);
3. it sends an `edit` tool call whose content is a `diff`, so the editor shows the change;
4. it asks `session/request_permission` (Apply the fix / Leave fib.lisp as it is);
5. if you approve, `fs/write_text_file` writes the fix. If you reject, the tool call fails and the file stays as it was;
6. it runs the tests in an `execute` tool call: `terminal/create sbcl --script test.lisp`, `wait_for_exit`, `output`, `release`;
7. it ends with `end_turn`.

## Recording (Zed 1.29.3, macOS)

[`zed-demo.mp4`](zed-demo.mp4) (100 s, about 0.5 MB) shows a full turn in Zed.
The "cl-stack demo" thread opens on `fib-project`, the prompt is sent, the plan
and tool calls stream in, the diff is reviewed, "Apply the fix" is approved,
the buffer changes, and the terminal shows `11 of 11 tests passed`. The run uses
`ACP_DEMO_PACE=1.0`, so each step waits one second and can be followed.
Zed was driven through AppleScript / System Events and recorded with
`screencapture -v -V`. ffmpeg made the mp4.

| | |
|-|-|
| [`keyframes/00-streaming-plan-in-progress.png`](keyframes/00-streaming-plan-in-progress.png) | message streaming, plan in progress |
| [`keyframes/01-diff-review-permission.png`](keyframes/01-diff-review-permission.png) | the edit tool call's diff and the permission request |
| [`keyframes/02-edit-applied-tests-passed.png`](keyframes/02-edit-applied-tests-passed.png) | fix applied in the buffer, terminal: 11 of 11 tests passed |
| [`keyframes/03-plan-all-done.png`](keyframes/03-plan-all-done.png) | plan "All Done", Edits: 1 file +1 −1 |

[`zed-wire-log.txt`](zed-wire-log.txt) is every JSON-RPC line of that Zed
session, 104 messages (`->` from Zed, `<-` from the agent). The project path
is replaced with `<project>`.

## Run headless

The scripted Lisp ACP client in `src/client.lisp` spawns the agent with
`spawn-acp-agent`. It runs one turn that approves the edit (Demo 1) and one
that rejects it (Demo 2), and checks each step:

```bash
cd harness/acp
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-acp")' \
  --eval '(harness-demos-acp:run-demos)' > demo-output.txt 2>&1
```

[`demo-output.txt`](demo-output.txt) is the last run (`Checks failed: 0`), and
[`wire-log.txt`](wire-log.txt) holds its wire messages. The client starts
[`acp-agent.sh`](acp-agent.sh), the slow launcher: it runs `cl-repo run` and
compiles the agent at every start.

## Run in Zed

An editor waits for the `initialize` reply. `acp-agent.sh` takes minutes to
send it, and Zed shows "Loading…" until then. So build the agent once as an
SBCL executable:

```bash
cd harness/acp
./build-core.sh                       # writes ./acp-demo-agent (gitignored, ~50 MB)
./build-core.sh /some/where/acp-agent # or another path
```

`build-core.sh` loads the system once under `cl-repo run`, then calls
`save-lisp-and-die` with the toplevel `harness-demos-acp:agent-toplevel`. That
executable prints no banner and sends load output to stderr, so stdout carries
only ACP messages. It answers `initialize` **68 ms** after it starts. Rebuild it
after you change `src/`.

Then add it to `~/.config/zed/settings.json`:

```json
"agent_servers": {
  "cl-stack demo": {
    "type": "custom",
    "command": "/absolute/path/to/harness/acp/acp-demo-agent",
    "args": [],
    "env": {
      "ACP_DEMO_PACE": "1.0",
      "ACP_DEMO_WIRE_LOG": "/tmp/zed-wire-log.txt",
      "ACP_DEMO_REDACT": "/absolute/path/to/your/fib-project"
    }
  }
}
```

1. Copy `sample/` to a scratch directory, because the turn edits `fib.lisp`. Open that directory in Zed.
2. In the Agent panel, start a new **cl-stack demo** thread.
3. Send `Fix the off-by-one bug in fib.lisp and run the tests.`
4. When the permission request appears, choose **Apply the fix**.

Environment variables:

- `ACP_DEMO_PACE` is the delay between steps in seconds (default 0).
- `ACP_DEMO_WIRE_LOG` is a file that receives every wire line.
- `ACP_DEMO_REDACT` is a directory that the wire log shows as `<project>`.
- `ACP_DEMO_EXTRA_TREE` (for `acp-agent.sh` and `build-core.sh`) puts an unpublished checkout first in the source registry.

## Files

- `src/agent.lisp`: the demo agent (`demo-agent`, `serve-demo-agent`, `agent-toplevel`).
- `src/client.lisp`, `src/run.lisp`: the headless client and `run-demos`.
- `src/wire-log.lisp`: the Gray streams that log stdin and stdout.
- `agent-main.lisp`, `acp-agent.sh`: the `cl-repo run` launcher, which needs no build.
- `build-core.sh`: the fast executable for editors.
- `sample/`: the buggy project, which stays buggy because runs work on a copy.
