# cl-stack-code plan mode demo (H5c-0)

`harness-demos-plan` runs **cl-stack-code 0.15.0** headless (`-p --output-format stream-json`, in process through `cl-stack-code:run-cli`) against a scripted OpenAI-compatible server on 127.0.0.1 inside the demo process. The built-in `openai` provider is pointed at it with `OPENAI_BASE_URL` and a dummy `OPENAI_API_KEY`, so llm-protocol-openai's HTTP path, ai-agent-protocol 0.16.1's tool loop (a tool output that ends the run), the `plan-mode-tools` policy, R4d-4b's Seatbelt sandbox and the plan documents all run end to end. No real model, no real key. The server records every request, so the output shows what the model saw: the tools offered and the tool results. The run takes about 7 s.

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the last run, from its first banner on (the load log is cut; the scratch directory shows as `<tmp>`, the project key derived from it as `<tmp-key>`, the loopback port as `<port>`).

## Run

```bash
cd harness/plan
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-plan")' \
  --eval '(harness-demos-plan:run-demos)' > demo-output.txt 2>&1
sed -n '/^========/,$p' demo-output.txt > demo-output.tmp && mv demo-output.tmp demo-output.txt
```

## Scenes

1. **Plan mode proposes a plan document.** `--permission-mode plan "let greet.sh take a name"` in a git project. The `plan` `opened` event names `plan.md` under `<data-dir>/plan-docs/` and the exploration area, `sandbox` `confined` (Seatbelt, network `none`). The scripted model runs `cat greet.sh; sh greet.sh; echo probe > probe.out; ls` (it runs, in the area), tries `write greet.sh` (refused by the policy with "You are in plan mode: project files cannot be edited now …"), writes `.plan-scratch/greet-try.sh` and runs it (allowed: `Hello, Ada!`), then calls `propose_plan`. Plan turns are offered `propose_plan` and `ask_user`. The `proposed` event has revision 1, the title, two steps and the files; the server saw 4 requests, so no model step followed `propose_plan`. The result's `plan` is `{status proposed, revision 1}`; stderr has the one-time migration notice and the `--approve-plan` / `--reject-plan` / revise hint. The plan directory holds `meta.sexp plan.md revisions/1.md`, outside the project; the project's `git status` is clean (no `probe.out`). **1b:** a second plan run on the same data directory that answers without a plan: result `plan` null, "no plan proposed yet", no migration notice.
2. **Approval and execution.** The user appends a Notes section to `plan.md`; `--resume ID --approve-plan` records it as revision 2 (`approved`, revision 2, mode `acceptEdits`) and runs one turn in the project whose user text is the plan (`<plan id=… revision="2">`, the user's Notes included). Execution turns are offered `update_plan`, not `propose_plan`. Four `step` events tick the checklist (`◐` / `☑` in the tool results); `executed` lists `README.md` and `greet.sh` as `modified`, `planned true`, with the diff; the project has both changes. **2b:** approving the executed plan again: "session ID has no plan to approve", exit 2, no request.
3. **Revise, then reject.** A new plan (r1); `--resume ID --permission-mode plan "greet in French"` reopens it and proposes revision 2 with the prompt as feedback (`revisions/1.md revisions/2.md`, `plan.md` now says Bonjour); `--reject-plan` emits `rejected` with no model request; approving afterwards is exit 2. The project stays clean.
4. **Preview mode, the fifth mode.** `--permission-mode sandbox` is refused with "use default, acceptEdits, bypassPermissions, plan, preview". `--permission-mode preview` is H5a under its new name: `preview` `opened` (confined) and `projected` events with `preview_id`, no `plan` events, the result's `preview` (status open, the diff), no `plan` key in the result, no migration notice, the project clean; `--resume ID --apply` applies it (`preview` `applied`, `M greet.sh`).

## Findings (cl-stack-code 0.15.0)

- **A headless approval cannot run the plan's tests.** `--approve-plan` defaults to acceptEdits, which allows edits but not `bash`, so the execution's `sh greet.sh; sh greet.sh Ada` is refused ("cannot ask for approval") although the plan's Test plan names exactly that command. As designed (the brief: headless cannot ask), but a plan that says how to test it gets executed untested unless the user adds `--allowed-tools 'bash(sh *)'` or picks `bypassPermissions`.
- **The exploration's scratch files and shell writes stay out of the project**, as the brief says: `probe.out` written by an exploratory `bash` and `.plan-scratch/greet-try.sh` never reach the project.
