# cl-stack-code resilience demo (H3f)

`harness-demos-code-resilience` runs **cl-stack-code 0.14.0** headless (`-p --output-format stream-json`, in process through `cl-stack-code:run-cli`) against a scripted OpenAI-compatible server on 127.0.0.1 inside the demo process. The built-in `openai` provider is pointed at it with `OPENAI_BASE_URL` and a dummy `OPENAI_API_KEY`, so llm-protocol-openai 0.8.0's HTTP path, llm-protocol 0.7.1's typed errors, ai-agent-protocol 0.16.0's retry loop and cl-stack-code's classification and exit codes all run end to end. No real model, no real key. Settings come from a user `settings.json` per scene, with delays of 50 ms, so the run takes about 15 s.

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the last run, from its first banner on (the load log is cut; the loopback port shows as `<port>`). Jittered retry delays are checked against their range instead of being printed.

## Run

```bash
cd harness/code-resilience
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-code-resilience")' \
  --eval '(harness-demos-code-resilience:run-demos)' > demo-output.txt 2>&1
sed -n '/^========/,$p' demo-output.txt > demo-output.tmp && mv demo-output.tmp demo-output.txt
```

## Scenes

1. **529, then 429 with `Retry-After: 1`, then an answer.** Two `system` / `retry` events: `overloaded`, attempt 1 of 4, status 529, a jittered delay in (80, 100] ms (50 ms × 2 for overloaded); then `rate_limit`, attempt 2, status 429, `retry_after_ms` 1000 and `delay_ms` 1000 exactly (Retry-After wins over the backoff). The server sees the third request at least 1 s after the second. The result is `success` with `"retries": 2`; the assistant event carries `context_tokens` / `context_pct` against `contextWindow` 4096.
2. **Retries exhausted.** The server answers 503 every time. With `maxAttempts` 3: two retry events (delays in (80, 100] and (160, 200] ms), then the result `error_overloaded`, `error_class` `model-overloaded`, "openai is overloaded (HTTP 503) after 3 attempts: …", exit code 3. **2b:** the same in text mode with `--max-retries 0`: one request, the message on stderr, exit 3.
3. **A silent server.** `requestTimeoutMs` 1000 with `turnTimeoutMs` 4000 as a backstop: the turn ends `error_turn_timeout` (`turn-timeout`, exit 124) at the turn timeout, not the request timeout. **3b:** `--turn-timeout 1.5` alone. See the findings below.
4. **`--continue --retry`.** A turn fails on a 503 (exit 3). `-p --continue --retry` (no prompt) re-sends it: one request whose last user message is the failed turn's prompt, result `success`, exit 0. A second `--retry`, when the last turn did not fail, is "the last turn did not fail; nothing to retry", exit 2, with no request.
5. **401 echoing part of the key.** `error_auth` / `model-auth-error`, exit 3, no retry, and the 12-character key prefix the server echoed is `[redacted]` in the message.

## Findings (cl-stack-code 0.14.0)

- **`requestTimeoutMs` does not bound the request `-p` makes.** `apply-request-timeout` sets llm-protocol-openai's `openai-request-timeout`, which bounds only non-streaming calls; the agent loop always streams, and a stream is bounded by `openai-stream-idle-timeout` (300 s by default). Scene 3 ends at `turnTimeoutMs`, not at 1 s.
- **The run returns only when the server hangs up.** The turn timeout fires and the result says so, but `run-cli` returns only when the in-flight HTTP request ends (6 s here, when the scripted server closes the connection). A server that holds the connection open keeps the process running past `turnTimeoutMs`.
- **The turn-timeout message prints a ratio.** `--turn-timeout 1.5` gives "the turn timed out after 3/2 s (--turn-timeout)".
