# MCP host demo: sampling and elicitation answered by the host

`harness-demos-mcp-host` uses **mcp-protocol 0.5.0** ("host answers", [egao1980/mcp-protocol#17](https://github.com/egao1980/mcp-protocol/issues/17), [#18](https://github.com/egao1980/mcp-protocol/pull/18)) from GHCR. A client with sampling and elicitation handlers talks to a stub MCP server over the in-memory pipe pair of **rpc-backend-stdio** (`make-stdio-rpc-pair`). The client is wired with `mcp-client-transport-handlers`. The server is an `rpc-serve` loop around `dispatch-mcp-method`, in a thread of its own, and it records each request and its answer. The sampling "model" and the elicitation "user" are Lisp functions. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run.

## Run

```bash
cd harness/mcp-host
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-mcp-host")' \
  --eval '(harness-demos-mcp-host:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`. There is no model, network or git step. Every `tools/call` round and `tools/list` has a 10 s timeout, and the server thread is joined with a 5 s bound. It runs in a few seconds once the packages are installed. `run-demos` prints 19 checks and returns `T` when all of them pass.

## What each section shows

1. **Capabilities from the client's handlers.** `mcp-client-default-capabilities` returns `{}` for a bare client. For a client with all three, it returns `sampling`, `elicitation: {form: {}}` and `roots`, in that order. Then the demo prints what the stub server got:
   - the `_meta` `io.modelcontextprotocol/clientCapabilities` of a 2026-07-28 `server/discover` and `tools/list`;
   - the `capabilities` of a 2025-11-25 (`:era :legacy`) `initialize`, which has sampling only because that client has no elicitation handler. Its later requests carry no `_meta`.

   Explicit `:capabilities` (`{"experimental": …}`) replace the defaults whole. Starting from `(mcp-client-default-capabilities client)` extends them instead.
2. **Sampling over MRTR.** Tool `summarize_diff` calls `request-sampling` (id `s1`, `maxTokens` 64), and the server answers round 0 with `input_required`. `call-tool` runs the client's sampling handler, a stub model that prints the params it got, then calls again with `inputResponses.s1`. The tool's handler re-runs from the top, finds the answer and returns `summary: …`. For each `tools/call` the server got, the output lists the round, the `inputResponses` ids, the `requestState` and the answer. It also shows what each handler run found in `*mcp-input-responses*`.
3. **Elicitation over MRTR.** Tool `confirm_push` asks for a form: a `requestedSchema` with `branch` and `force`, where `branch` is required. The stub user accepts with `{"branch": "main", "force": false}`, which reaches the tool on the retry. Then `summarize_and_push` asks for sampling (`requestState` `step-1`) and then for elicitation (`step-2`). That takes three rounds, and the third carries both answers and the latest `requestState`. Each handler is asked once.
4. **Refusal and limits.**
   - A sampling handler signals `mcp-error` with code -1 and "User rejected sampling request" (the MCP spec's example). `call-tool` ends after one round with that same condition object, and the demo prints it as the tool error a host would return.
   - `ask_forever` asks a new question every round. The server gets 11 `tools/call`: round 0 and 10 answered rounds, the last carrying all ten answers. Then `call-tool` signals `mcp-error` -32603, "tools/call ask_forever still needs input after 10 rounds" (`+max-input-rounds+`).
   - A client without a sampling handler advertises only `elicitation` and gets -32601.
5. **Under the hood.** This section covers:
   - how the pair is built;
   - why a 2026-07-28 server never calls the client: the MRTR answers come back in the next `tools/call`;
   - `mcp-input-response` outside a request, which returns `NIL`, `NIL`;
   - the loaded versions (mcp-protocol 0.5.0, rpc-backend-stdio 0.4.1, …).

## Notes

- json-backend-jzon writes the members of a hash table sorted by key, on the wire and in the printed JSON. So the server gets `{"elicitation": …, "sampling": …}`, and the third round's `inputResponses` read `e1 s1`. JSON member order has no meaning. The demo compares key sets on what the server got, and shows the rule's order on the Lisp object.
- bordeaux-threads 2 starts a thread with standard IO syntax (`*print-readably*` T). There, `~s` prints a decoded base string as `#A((6) BASE-CHAR . "step-1")`, so the demo's server thread binds `*print-readably*` to NIL.

Nothing was filed: every check passed against the published packages.
