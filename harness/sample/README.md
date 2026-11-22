# Sample demo: cl-stack-sample with the cl-stack harness

`harness-demos-sample` follows [cl-stack-sample](https://github.com/egao1980/cl-stack-sample)'s README, section 4 ("Work on it with the cl-stack harness"), the way a newcomer would. Every step runs a real command:

1. Clone `egao1980/cl-stack-sample` (`main`) into a temporary directory. This includes its `.cl-stack-code/settings.json`.
2. Download the launcher `cl-stack-code.sh` from `egao1980/lisp-workspace` `main` (the README's "direct download").
3. Build the executable into a **fresh cache**: `XDG_CACHE_HOME` points into the temporary directory, so nothing from an earlier build is reused. This step runs `--which`, `--version` and `--acp`; the last one prints the "needs 0.11.0" message.
4. Run the README's headless session against a live model:

   ```bash
   cl-stack-code --model openai --permission-mode acceptEdits --output-format stream-json \
     -p "Add a w (week) unit to parse-duration, with tests. Run them."
   ```

   The demo prints one line per tool call and the result.
5. Print `git status` and `git diff` of the clone.
6. Run `cl-repo test` separately and give a verdict. `OK` needs a passing test run and a diff that adds `#\w` and a `…w"` test case.

[`demo-output.txt`](demo-output.txt) holds the last run. Temp paths, the home directory and the key are scrubbed out of it.

## Run

```bash
cd harness/sample
HARNESS_DEMOS_BASE_URL=http://<host>:8888/v1 HARNESS_DEMOS_API_KEY=… \
HARNESS_DEMOS_MODEL=unsloth/Qwen3.8-Flash-Next-GGUF PATH=/opt/homebrew/bin:$PATH \
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-sample")' \
  --eval '(harness-demos-sample:run-demos)' > demo-output.txt 2>&1
```

You need SBCL through Roswell, `cl-repo`, `git` and `curl`, plus an OpenAI-compatible `/v1` server that has a tool-calling model.

| Variable | Meaning |
|---|---|
| `HARNESS_DEMOS_BASE_URL`, `HARNESS_DEMOS_API_KEY`, `HARNESS_DEMOS_MODEL` | The model, as in the other harness sets. The demo passes them to cl-stack-code as `OPENAI_BASE_URL`, `OPENAI_API_KEY` and `OPENAI_MODEL`. |
| `HARNESS_SAMPLE_FRESH_CACHE=0` | Reuse `~/.cache/cl-stack-code` instead of building from scratch. |
| `HARNESS_SAMPLE_REPO`, `HARNESS_SAMPLE_REF` | Another clone URL or branch of the sample. |
| `HARNESS_SAMPLE_LAUNCHER_URL` | Another launcher URL. |
| `HARNESS_SAMPLE_KEEP=1` | Keep the temporary directory. |

The exit code is 0 when the verdict is `OK` and 1 otherwise. A slow model makes this a long demo. On the shared Unsloth server, the session took about 20 minutes and 20 or more model calls.
