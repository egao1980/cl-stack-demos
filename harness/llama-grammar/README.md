# llama-grammar demo: grammar-constrained generation that does not abort

`harness-demos-llama-grammar` runs **llama-cpp 0.2.1** (egao1980/llama-cpp#21) on a 1 MB GGUF. It covers grammar-constrained generation, the python-to-cl "llama-cpp-python / outlines" job. Before this release:
- every grammar-constrained `complete` killed the Lisp image with SIGABRT (llama-cpp#17);
- exiting with an engine in a global ended in a Metal `GGML_ASSERT` instead of the exit code (#18);
- the finalizers could never run (#20).

The code is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the stdout of the last run (macOS arm64 Metal, SBCL 2.4.11; llama.cpp's own log goes to stderr), with home and temp paths replaced by `~` and `<tmp>`. That run used the PR #21 checkout (head `b40ba0e`), with `libllamastack` built by `scripts/build-llama.sh` against llama.cpp master `c212d25` (`LLAMA_CPP_NATIVE`), before 0.2.1 was on GHCR. Re-run it with `cl-repo run` once 0.2.1 is published.

## Run

```bash
curl -L -o /tmp/stories260K.gguf \
  https://huggingface.co/ggml-org/models/resolve/main/tinyllamas/stories260K.gguf
# optional, section 7 (92 MB): a vocabulary whose token 0 is end-of-generation
curl -L -o /tmp/smollm2-135m.gguf \
  https://huggingface.co/bartowski/SmolLM2-135M-Instruct-GGUF/resolve/main/SmolLM2-135M-Instruct-Q4_0.gguf
cd harness/llama-grammar
HARNESS_LLAMA_MODEL=/tmp/stories260K.gguf HARNESS_LLAMA_CHAT_MODEL=/tmp/smollm2-135m.gguf \
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-llama-grammar")' \
  --eval '(uiop:quit (if (harness-demos-llama-grammar:run-demos) 0 1))' 2>/dev/null > demo-output.txt
```

It takes a few seconds and makes 25 checks (20 without the chat model).

## Sections

1. **Engine.** `llama-version`, ABI 4, and a plain completion.
2. **Grammar-constrained `complete` (#17).** With `root ::= ("yes" | "no")`, three prompts give `"yes"` or `"no"`. With `root ::= "The answer is " ("yes" | "no") "."` the result is a whole sentence.
   - What broke: 0.2.0 accepted every token twice (`llama_sampler_sample` already accepts), so the grammar threw on the first piece. The C shim could not catch the exception, so the image aborted.
   - Why it ends now: llama.cpp only allows end-of-generation once the grammar is complete, so generation stops there.
3. **Reuse and streaming.** One `parse-grammar` serves three completions. `:on-token` pieces (`"y" "e" "s"`) join to the text. Temperature 0.8 still gives a sentence of the grammar.
4. **`:max-tokens` cut.** An open-ended `"{" [a-z]+ "}"` gives a prefix of a sentence at 2, 4 and 8 tokens.
5. **Dead ends and exceptions are conditions (#17).** `root ::= "\x00"` matches no token. 0.2.0 picked one anyway and llama.cpp's accept threw (the image aborted). 0.2.1 applies the grammar in the shim, drops what it rules out and sees that nothing is left: `llama-error (RUNTIME) … grammar dead end …`, and the image goes on. An undefined rule is `(INVALID) grammar parse failed`. `llama_stack_guard_selftest` throws a `std::runtime_error`, a `std::bad_alloc` and a non-std exception inside the ABI guard; each comes back as `:runtime` with its text.
6. **Exit with a live engine (#18, #20).** A child SBCL keeps an engine in a global and then calls `(sb-ext:exit :code 3)` or signals an unhandled error. It exits with 3 or 1, with no `GGML_ASSERT`. `free-live-handles` frees one engine and one grammar, then nothing.
7. **A vocabulary whose token 0 is end-of-generation (#17, round 2).** On SmolLM2-135M a dead end used to pick token 0, `<|endoftext|>`, and llama.cpp called `GGML_ABORT`, which no exception guard can catch. `root ::= ("yes" | "no") <|im_end|>` got there after one word, because llama.cpp masks end-of-generation tokens until the grammar is complete. Now both that grammar and `"\x00"` are a `grammar dead end` llama-error, and `root ::= ("yes" | "no")` still ends at EOG with `"yes"`.
