# LLM request timeouts demo

`harness-demos-llm-timeouts` shows the model-request timeouts from GHCR in action:

- **llm-protocol-openai 0.7.0** and **llm-protocol-anthropic 0.5.0**: the `:request-timeout`, `:stream-idle-timeout` and `:connect-timeout` initargs and their env vars;
- **http-backend-async 0.2.25**: `:read` is an idle timeout, and `:total` caps a request only when it is set.

Background: llm-protocol-openai#13, cl-stack-code#19, http-backend-async#66. Before these releases, every model request carried a fixed `:timeout 180`, which the backend enforced as a wall-clock cap, so a long answer from a slow local model died mid-stream with `sync await timed out`.

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the last run.

## Run

```bash
cd harness/llm-timeouts
HARNESS_DEMOS_BASE_URL=http://<host>:8888/v1 HARNESS_DEMOS_API_KEY=… \
HARNESS_DEMOS_MODEL=unsloth/Qwen3.8-Flash-Next-GGUF \
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-llm-timeouts")' \
  --eval '(harness-demos-llm-timeouts:run-demos)' > demo-output.txt 2>&1
```

Without `HARNESS_DEMOS_BASE_URL` and `HARNESS_DEMOS_MODEL`, Demo 6 is skipped. `HARNESS_DEMOS_MAX_TOKENS` (default 2800) sets the length of the live answer.

## What each demo shows

| Initarg | Env | Default | Bounds |
|---|---|---|---|
| `:request-timeout` | `OPENAI_TIMEOUT` / `ANTHROPIC_TIMEOUT` | 600 | a non-streaming call, in all |
| `:stream-idle-timeout` | `OPENAI_STREAM_IDLE_TIMEOUT` / `ANTHROPIC_STREAM_IDLE_TIMEOUT` | 300 | silence on a stream. There is no total cap |
| `:connect-timeout` | `OPENAI_CONNECT_TIMEOUT` / `ANTHROPIC_CONNECT_TIMEOUT` | 30 | TCP + TLS |

In every case `nil`, `0` or env `none` means no limit.

1. **Settings.** The table above in practice: the defaults, then initargs and env vars, where an initarg wins and a bad env value is an `llm-error`. It also shows the http-protocol plists sent on the wire: `(:connect 30 :read 300)` for a stream, `(:connect 30 :total 600)` otherwise.
2. **A slow, steady stream outlives the cap.** 24 tokens 0.5 s apart (12 s) arrive under `:request-timeout 3 :stream-idle-timeout 2`, and the stream finishes with `:stop`. The `on-part` times show the sync HTTP facade still delivering the stream at the end, not token by token (http-backend-async#70, fixed in 0.2.27: see [llm-live-stream](../llm-live-stream/)).
3. **A silent stream fails.** After two tokens the origin goes quiet, and the request fails in about 2 s with `http-timeout-error` "read timed out: no data for 2.0 s".
4. **Non-streaming.** The answer takes 4 s. `:request-timeout 2` times out; `10` and `nil` get the whole answer.
5. **No blind retry.** Even on a client that retries 3 times, a timed-out stream is sent once: it is a POST, and `http-retry` retries only idempotent methods.
6. **Live.** A long tutorial is streamed from a slow local model with the default timeouts. The demo prints the elapsed time every 60 s, the time to the first part, the total time against the old 180 s cap, and output tokens per second.
