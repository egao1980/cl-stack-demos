# browser demo: Playwright jobs on headless Chrome

`harness-demos-browser` drives a real headless Chrome with **browser-protocol 0.2.0** (egao1980/browser-protocol#4). That release fixes browser-protocol#1 (stack JSON and base64), #2 (launch, CDP replies by id, `navigate` waits for the page) and #3 (`*browser-backend*` for the `NIL`-backend methods). It covers what the python-to-cl Selenium / Playwright page needs: launch a browser, go to a page and wait for it, evaluate JavaScript, take a screenshot, print a PDF, and close.

The code is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run (macOS arm64, SBCL 2.4.11, Chrome 158), with home and temp paths replaced by `~` and `<tmp>`. That run used the PR #4 checkout (head `191ada6`) on the ASDF path, with its locked dependencies, before 0.2.0 was on GHCR. Re-run it with `cl-repo run` once 0.2.0 is published.

## Run

```bash
cd harness/browser
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-browser")' \
  --eval '(uiop:quit (if (harness-demos-browser:run-demos) 0 1))' > demo-output.txt 2>&1
```

You need Chrome, Chromium or Edge. `find-browser-executable` checks `$CHROME_PATH`, the usual macOS and Windows install paths, and `google-chrome` / `chromium` on `PATH`. The demo needs no network: every page is a `data:` URL, and the unresolvable host is meant to fail. It takes about 10 seconds. It makes 47 checks, and `run-demos` returns `NIL` if one fails or a section stops on an error.

## Sections

1. **Chrome's JSON and base64 go through the stack (#1).** Decoding the verbatim `Page.navigate` reply gives `isDownload` → `NIL` (present). 0.1.0's private parser failed here with "bad JSON false". `null` decodes to `:null`. The private `%parse-json` / `%base64-*` are gone, and malformed JSON is a `browser-error` whose cause is the json-protocol condition.
2. **Launch.** `launch-browser`: the executable, the process, the temporary profile, the browser websocket, and the flattened page session.
3. **Replies by id (#2).** The `Target.targetCreated` events that Chrome sends before the `Target.setDiscoverTargets` reply sit in the event buffer. `eval-js` 1+1, 2+2, 3+3 give 2, 4, 6. 0.1.0 gave `NIL NIL NIL`.
4. **`navigate` waits (#2).**
   - With the default `:wait-until :load`, `readyState` is `"complete"`.
   - On a page whose image never arrives, `:domcontentloaded` returns at once (`"interactive"`), and `:load` with `:timeout 2` signals `browser-timeout` after 2 s, with the URL.
   - A `#fragment` navigation needs no load event.
   - An unresolvable host is a `browser-error` carrying Chrome's `net::ERR_NAME_NOT_RESOLVED`.
5. **Events.** An `on-cdp-event` handler sees `console.log` from the page while `eval-js` waits for its reply. `wait-for-cdp-event` takes the buffered event first, and `:test` waits for a later one.
6. **Screenshot, PDF, DOM.** The screenshot is a PNG (size from IHDR) and the PDF starts with `%PDF-`, both base64-decoded by encoding-protocol.
7. **`page.route`: commands from an event handler (#2, round 2).** `Fetch.enable` on every URL and a `Fetch.requestPaused` handler that sends `Fetch.continueRequest` (Playwright's `page.route("**/*", r => r.continue_())`), then `navigate` to a page with six images. The handlers nest. PR #4's first build lost replies and the load event here ("1 of 6 requests continued", then `browser-timeout`); now every reply reaches its command, all six requests continue and `navigate` returns on the load event.
8. **JavaScript values (#1, round 2).** `eval-js` maps the RemoteObject: `1 === 2` → `NIL` (PR #4's first build returned the RemoteObject, a hash-table, which is true), `undefined` → `NIL`, `null` → `:null`, a BigInt → an integer, `0/0` → `:nan`, `[1, false, null, 'x']` → `#(1 NIL :NULL "x")`. A thrown `Error` is a `browser-error` with its text, and `click` on a missing selector signals instead of returning `T`.
9. **Two threads on one browser (#2, round 2).** 25 `eval-js` calls in each of two threads on one websocket; each thread gets its own 25 answers.
10. **`connect-cdp`.** A second client on the running browser. Its `close-browser` detaches and the launched one keeps working.
11. **`close-browser`.** The process is gone and the temporary profile is removed.
12. **`with-browser` and `*browser-backend*` (#3).** `(eval-js nil …)` inside `with-browser` uses the bound backend, and the browser is closed on the way out.
