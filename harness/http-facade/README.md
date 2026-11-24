# HTTP facade and promises demo

`harness-demos-http-facade` shows three gaps that the [Python to Common Lisp](https://egao1980.github.io/python-to-cl/) site found, and a fourth found while prosecuting http-backend-dexador#23. Each is fixed in the GHCR packages:

- **cl-stack-http 0.2.0** (cl-stack-http#26, #27, #28, #29)
- **event-protocol 0.3.0** (event-protocol#26, #27)
- **http-protocol 0.4.5**, **http-backend-async 0.2.29** and **http-backend-dexador 0.2.2**: `https://` proxies (http-protocol#53, demo 4)

Demos 1 to 3 send each request to a loopback HTTP/1.1 keep-alive origin ([`src/origin.lisp`](src/origin.lisp)); demo 4 goes through a loopback TLS proxy. The origin logs the connection number, the path and the `User-Agent` of every request, so the output shows which backend sent each one and over which connection. [`demo-output.txt`](demo-output.txt) is the last run, from the version line on. The cl-repo sync and compile output before it is cut, and the home directory is shown as `~`.

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

4. **`https://` proxies are reached over TLS (http-protocol#53).** `HTTPS_PROXY=https://user:secret@127.0.0.1:P` (made-up credentials) is read by a trust-env session. The proxy is a loopback TLS listener ([`src/tls-proxy.lisp`](src/tls-proxy.lisp)). It logs the first octets off the socket and the head it reads inside TLS, answers CONNECT, and through the tunnel it is the origin too. The certificates in [`certs/`](certs/) come from the http-backend-async tests: a test CA whose key was not kept, a proxy certificate for 127.0.0.1 and an origin.test one. They are for this demo only.
   - **http-backend-async 0.2.29.** The first octets the proxy reads are a TLS ClientHello (`16 03 01 …`), for an `http://` origin and an `https://` one.
     - `http://` origin: the absolute-form `GET http://origin.test/demo` and `Proxy-Authorization` arrive inside TLS.
     - `https://` origin: the `CONNECT origin.test:443` and its `Proxy-Authorization` arrive inside TLS. The tunnel then starts with the origin's own ClientHello (TLS inside TLS). The origin reads `GET /demo` without `Proxy-Authorization`.
     - The credentials are never in the octets off the socket.
     - Before the fix, http-protocol 0.4.4 classed the proxy `:http`, and 0.2.28 sent all of this in plain TCP (the evidence run on http-backend-async#81).
   - **http-backend-dexador 0.2.2.** It signals `proxy-tls-unsupported` (an `unsupported-operation`, operation `:https-proxy`) before any connection, so the proxy sees nothing. The report shows the proxy URL without its userinfo. The demo also shows the `use-value` restart the backend offers. dexador dials every proxy in plain TCP, and it would redial on its own after a failed request even if it were handed a TLS stream (http-backend-dexador#38, #39).
   - The proxy hop uses the session's TLS settings. With `:verify t` and the test CA trusted, the proxy's certificate is checked against 127.0.0.1 and the origin's against origin.test. Without that CA, the request ends in `http-tls-error` after the ClientHello, and nothing is sent inside TLS.

## Notes

- The `.asd` depends on plain `"event-protocol"` and `"http-backend-dexador"`, not on `(:version … "0.3.0")` / `"0.2.0"`. With the local client (cl-oci 0.27.34), the event-protocol pin fails in the client image ("installed … but below the required 0.3.0", the shape of cl-repository#180). The http-backend-dexador pin pulls `uiop:3.1.1` and gets 403 (cl-repository#171). The version line at the top of the output shows what the image loaded.
