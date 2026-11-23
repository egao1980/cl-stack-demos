# Live LLM streaming demo

`harness-demos-llm-live-stream` shows model tokens reaching the caller as they arrive, on **http-backend-async 0.2.27** (http-backend-async#70, #72).

Before 0.2.26, the blocking HTTP `send` with `:want-stream t` read the whole body before it returned. 0.2.26 fixed that only when `send` made its own event loop; 0.2.27 also covers a caller that binds `event-protocol:*event-loop*`, as cl-stack-code and this demo do. llm-protocol-openai and llm-protocol-anthropic stream through that call, so every `on-part` callback fired in one burst at the end of the model call. A UI such as cl-stack-code in Zed showed nothing until a turn ended. The [llm-timeouts](../llm-timeouts/) Demo 2 output shows that burst: every part is logged at 12.4 s.

Now `send` returns at the headers, with the live body stream. A pump thread runs the request's event loop (a private one when the caller bound its own) until the body ends. The `:read` idle timeout from 0.2.25 still bounds the silence between octets. No llm-protocol change was needed: both libraries already parse SSE from the body stream, one event at a time (sse-protocol).

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the last run.

## Run

```bash
cd harness/llm-live-stream
HARNESS_DEMOS_BASE_URL=http://<host>:8888/v1 HARNESS_DEMOS_API_KEY=… \
HARNESS_DEMOS_MODEL=unsloth/Qwen3.8-Flash-Next-GGUF \
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-llm-live-stream")' \
  --eval '(harness-demos-llm-live-stream:run-demos)' > demo-output.txt 2>&1
```

Without `HARNESS_DEMOS_BASE_URL` and `HARNESS_DEMOS_MODEL`, Demo 4 is skipped. `HARNESS_DEMOS_MAX_TOKENS` (default 160) sets the length of the live answer.

## What each demo shows

1. **OpenAI-compatible stream.** A loopback origin sends 8 chat-completion chunks 0.5 s apart and logs when it sent each one. The table pairs each part's send time with its `on-part` time: the lag stays a few milliseconds.
2. **Anthropic Messages stream.** The same with `message_start` … `content_block_delta` … `message_stop` events.
3. **Stopping early.** The caller leaves `stream-generate` after 3 of 40 parts. Closing the body ends the request: the origin sees the hang-up, and no pump thread is left running.
4. **Live.** A local model counts to forty. Each text chunk is printed with its arrival time, and the summary checks that the chunks spread over the call.
