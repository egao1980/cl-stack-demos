# cl-stack-code MCP requests demo (H3e-2b)

`harness-demos-code-mcp-requests` runs **cl-stack-code 0.16.0** ([egao1980/cl-stack-code#54](https://github.com/egao1980/cl-stack-code/pull/54), brief "H3e-2b — locked" in cl-stack `docs/capabilities/cl-stack-code.md`) from GHCR. It shows what an MCP server may ask of its client (sampling, elicitation) and deferred tool loading behind `tool_search`. The `-p` scenes run `-p --output-format stream-json` in process through `cl-stack-code:run-cli`. The elicitation answer goes through the in-process SDK (`make-client`, `client-pending-approvals`, `client-respond-approval`).

The model is a scripted OpenAI-compatible server on 127.0.0.1 inside the demo process. The built-in `openai` provider is pointed at it with `OPENAI_BASE_URL` and a dummy `OPENAI_API_KEY`, so llm-protocol-openai's HTTP path and ai-agent-protocol 0.17.0's tool loop run end to end. No real model, no real key. The server records every request, so the output shows what the model saw: the tools offered, the tool results, and the sample the MCP server asked for. A request whose system prompt is the stub's sampling prompt is answered as a sample and does not use the script.

The MCP server is a stub stdio server. It is a child SBCL that loads this image's mcp-protocol 0.5.0 and rpc-backend-stdio 0.4.1 fasls and serves `dispatch-mcp-method` on an rpc-backend-stdio transport in client mode. That way a tool's handler can send the client `sampling/createMessage` or `elicitation/create` while it runs and wait for the answer, as a 2025-11-25 server does. This follows the shape of cl-stack-code's `fake2` test server. `STUB_MCP_TOOLS` picks its tools:

- `requests`: `summarize` (asks for a sample: `maxTokens` 10000, model hint `gpt-x`, `includeContext` `allServers`) and `deploy` (asks for a form: `env` enum staging|production, required; `replicas` integer 1..5).
- `catalog`: eight plain tools.

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the last run, from its first banner on. The load log is cut, the scratch directory shows as `<tmp>` and the loopback port as `<port>`. The scenes take about 3 s, and the whole run takes about 8 s once the packages are compiled.

## Run

```bash
cd harness/code-mcp-requests
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-code-mcp-requests")' \
  --eval '(harness-demos-code-mcp-requests:run-demos)' > demo-output.txt 2>&1
sed -n '/^========/,$p' demo-output.txt > demo-output.tmp && mv demo-output.tmp demo-output.txt
```

## Scenes

1. **Sampling.** In the user settings, `mcpServers.docs` is the stub with `"sampling": {"maxTokens": 256}` (the per-server opt-in). The init event's `mcp_servers` entry has `sampling true`.
   - **1a: `-p` with no rule.** The sample is denied: the stub gets `-1 Sampling needs approval and cl-stack-code is running headless (-p) …`. There is a `system` / `mcp_sampling` event (outcome `denied`, reason `headless`, `max_tokens` 256, usage null), and the result's `permission_denials` has `{"name": "mcp_sampling", "id": "mcp-sampling-1", "input": {"server": "docs"}, "reason": "headless"}`. No sample reached the model.
   - **1b: `--allowed-tools 'mcp_sampling(docs)'`.** The model server sees a second request between the two tool-loop steps. It has the stub's system prompt and message only, no tools, `max_tokens` 256 (the server asked for 10000; the config caps it), and the session's model `demo-model`, not the hint `gpt-x`. The event is outcome `allowed` with `usage {"input_tokens": 31, "output_tokens": 14}`, the mock's numbers for the sample. The tool result is `summary: <the mock's text> (model demo-model, stop endTurn)`.
2. **Elicitation.** `"elicitation"` is left at its default, true.
   - **2a: `-p`.** It declines: `deploy: decline`, and the `mcp_elicitation` event has outcome `declined`, reason `headless`, `fields []`.
   - **2b: the SDK.** A turn with `mcp__docs__deploy` allowed. `client-pending-approvals` lists an `approval_request` of kind `mcp_elicitation`, id `mcp-elicitation-1`, with the server, the message, the requested schema, `tool_use_id` and the summary "MCP server docs asks: …". `:accept` with `{"env": "qa"}` signals `mcp-elicitation-content-error` ("env: enum") and the request stays pending. `:accept` with `{"env": "staging", "replicas": 2}` answers it. The output then shows `approval_resolved` (decision `accept`), the `mcp_elicitation` event (outcome `accepted`, `fields ["env", "replicas"]`, no values), and the tool result `deploy: accept env=staging replicas=2`, which also comes back to the model.
3. **Deferred tool loading.** `mcpServers.catalog` is the eight-tool stub with `"mcpToolSearch": "always"`.
   - The init event has `mcp_tool_search {"mode": "always", "threshold": 40, "active": true, "deferred": 8}`. Its tools list only `tool_search` beside the built-ins, and the server entry has `deferred true, loaded []`.
   - Request 1 offers the 17 built-ins and `tool_search`. Its description lists the eight deferred names, and its parameters are `query` and `max_results`.
   - The model calls `tool_search {"query": "invoice"}`. The result reads "Loaded 3 tools; they can be called from the next step" and lists the three invoice tools with descriptions. Request 2 offers them before `tool_search`, and `invoice_send` is called and runs.
   - The model then calls `mcp__catalog__weather_today` by its exact name, though it is deferred and was never searched for. The call takes the normal permission path (allowed by `--allowed-tools` here), runs, and request 4 offers the tool.

## Findings (cl-stack-code 0.16.0)

- **A compile-time `WARNING`** (filed as [egao1980/cl-stack-code#67](https://github.com/egao1980/cl-stack-code/issues/67)). Compiling 0.16.0 reports `undefined variable: CL-STACK-CODE:+TOOL-SEARCH-TOOL-NAME+`, a full warning, not a style warning. `src/mcp.lisp` uses it in the deferred-source wrapper, but `src/mcp-context.lisp` defines it with `defparameter`, and that file compiles later. At run time the variable is bound, so scene 3 works. But `compile-file` reports failure for `mcp.lisp`, which breaks any build that treats warnings as errors.
- **The SDK asks `GET /v1/models` once; `-p` does not.** The mock answers it with a one-model list and logs it apart ("other requests"), so it does not use a scripted step. A mock that pops a step for every HTTP request loses its first step to this probe.
- **A headless elicitation decline is not a `permission_denials` entry.** Only sampling denials are, as the brief says. The `mcp_elicitation` event (reason `headless`) is the only trace in `-p` output.
- Everything else matched the brief: the default ask and the headless denial, the `maxTokens` cap, the host's model, the per-request usage on the event, schema validation that keeps the question pending, content values kept out of events, and both ways a deferred tool loads.
