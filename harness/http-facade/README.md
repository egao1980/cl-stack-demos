# HTTP facade and promises demo

`harness-demos-http-facade` shows three gaps that the [Python to Common Lisp](https://egao1980.github.io/python-to-cl/) site found, each fixed in the GHCR packages:

- **cl-stack-http 0.2.0** (cl-stack-http#26, #27, #28, #29)
- **event-protocol 0.3.0** (event-protocol#26, #27)
- **http-backend-dexador 0.2.0** underneath

Each request goes to a loopback HTTP/1.1 keep-alive origin ([`src/origin.lisp`](src/origin.lisp)). The origin logs the connection number, the path and the `User-Agent` of every request, so the output shows which backend sent each one and over which connection. [`demo-output.txt`](demo-output.txt) is the last run, from the version line on. The cl-repo sync and compile output before it is cut, and the home directory is shown as `~`.

## Run

```bash
cd harness/http-facade
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-http-facade")' \
  --eval '(harness-demos-http-facade:run-demos)' > demo-output.txt 2>&1
```

The demo uses no network beyond 127.0.0.1.

## What each demo shows

1. **The requests-shaped surface (#26).** Response readers, conditions, `make-http-timeout`, `make-http-retry`, `make-secret`, `reveal` and `*http-backend*` are `:EXTERNAL` in `cl-stack-http`. They are the same symbols as `http-protocol`'s, so using both packages causes no conflict. In 0.1.12 they were only inherited, so the cl-stack QUICKSTART section 3 form did not read (`SIMPLE-READER-PACKAGE-ERROR`). The demo reads that form verbatim and runs it, with the URL pointed at the origin. It also shows `http-status-error-status` on a 404 and `http-timeout-error` from `(make-http-timeout :read 0.5)` after 0.5 s.
2. **The two `:auto` orders (#27).** Blocking calls resolve to dexador and `get-async` to http-backend-async over libuv. The User-Agent column tells them apart: dexador sends its own, the async backend sends none (filed as http-protocol#54). `(with-backend (:async) (get …))` works with no `*event-backend-maker*` or event loop set by the caller; in 0.1.12 it signalled "No event loop". Blocking `:auto` calls from 4 threads all succeed. That is why blocking calls stay on dexador: event-backend-libuv's handle tables are not thread-safe yet (event-backend-libuv#31, cl-stack-http#32). A default session keeps dexador for `session-get` and sends `session-get-async` through the async backend.
3. **asyncio.run / gather / wait_for (event-protocol#26).** `run-promise` makes a loop, binds it, awaits and closes it. Three `get-async` calls, each answered 1 s late, are gathered in 1.0 s. `run-promise` returns then, although the clients' pools still hold idle keep-alive connections; event-protocol 0.2.3's `run` waited 61.9 s for those (http-backend-async#79). `gather` keeps argument order. `await` takes a promise. `:timeout` signals `await-timeout`. A rejection, here a connection refused, is re-signalled as `http-connection-error`.

## Notes

- The `.asd` depends on plain `"event-protocol"` and `"http-backend-dexador"`, not on `(:version … "0.3.0")` / `"0.2.0"`. With the local client (cl-oci 0.27.34), the event-protocol pin fails in the client image ("installed … but below the required 0.3.0", the shape of cl-repository#180). The http-backend-dexador pin pulls `uiop:3.1.1` and gets 403 (cl-repository#171). The version line at the top of the output shows what the image loaded.
