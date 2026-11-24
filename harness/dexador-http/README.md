# Dexador backend: proxies, timeouts, request bodies and redirects demo

`harness-demos-dexador-http` shows **http-backend-dexador 0.2.1** on **dexador 0.9.16.1**, which is published from the egao1980/dexador fork through cl-stack-systems. These are the fixes for gaps that the [Python to Common Lisp](https://egao1980.github.io/python-to-cl/) site found: http-backend-dexador#23 (proxies) and #24 (timeouts) in 0.2.0, and #32 (request bodies) and #33 (redirect history) in 0.2.1. Since cl-stack-http 0.2.0, `:auto` sends blocking calls through this backend off Windows, so #32 broke every form post on the default path.

Every origin and proxy is a loopback listener ([`src/fixtures.lisp`](src/fixtures.lisp)). The HTTPS origins use the self-signed localhost certificate in [`certs/`](certs/), which is for this demo only; it is the one from the backend's tests. [`demo-output.txt`](demo-output.txt) is the last run, from the version line on. The cl-repo sync and compile output before it is cut.

## Run

```bash
cd harness/dexador-http
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-dexador-http")' \
  --eval '(harness-demos-dexador-http:run-demos)' > demo-output.txt 2>&1
```

## What each demo shows

1. **Proxies (#23).** The protocol's `http-proxy-config` is resolved per URL and reaches dexador as a URL. 0.1.5 passed the object itself, which fell through an `ETYPECASE`, so every trust-env session failed.
   - Plain HTTP reaches an HTTP proxy in absolute-form (`GET http://origin.test/path?q=1 HTTP/1.1`, RFC 9112 §3.2.2) with `Proxy-Authorization`. dexador 0.9.16 sent the origin-form, which squid answers with 400.
   - HTTPS goes through `CONNECT origin.test:443`. When the proxy refuses the tunnel, the caller gets a TLS error rather than a proxy error (filed as http-backend-dexador#30).
   - A request's `:proxy` wins over the client's.
   - `no_proxy` hosts go direct.
   - The default config with no proxy variables set goes direct.
   - `https://` proxies (TLS to the proxy), SOCKS4 and SOCKS5 with credentials are refused with `unsupported-operation` before any byte is sent, and the messages leave the password out. dexador would have sent the request and `Proxy-Authorization` to an `https://` proxy over plain TCP; this was the round-3 prosecution case.
   - Userinfo in a URL no longer ends up in `Host` (dexador 0.9.16.1). It is not used as Basic auth either, unlike requests (filed as http-protocol#55).
2. **Timeouts (#24).**
   - `:timeout 2.0` is a deadline on HTTP and HTTPS alike.
   - `(:connect 5 :read 1)` ends a silent HTTPS origin after 1.0 s, also behind an `http://` → 301 → `https://` redirect (the round-3 case).
   - `:read 1.5` is exact.
   - A body that keeps arriving (8 octets 0.5 s apart) is not cut by `:read 1`.
   - A streamed HTTPS body (`:want-stream t`) keeps the read timeout after `send` returns.
   - In 0.1.5 the HTTPS rows with `:read` returned the response after 6 s: TLS reads went through cl+ssl's socket BIO, which on SBCL never sees the fd-stream timeout.
3. **Pools keyed by `:verify`.** A `:verify nil` client's idle connection to a self-signed origin is not handed to a `:verify t` client. The second client opens its own connection and fails certificate verification. The failure arrives as cl+ssl's `SSL-ERROR-SSL`, not as `http-tls-error` (filed as http-backend-dexador#31).

4. **Request bodies (#32).** Each POST goes to a loopback origin that records the head and body it read. Dexador encodes a body again when its Content-Type is exactly `application/x-www-form-urlencoded` or starts with `multipart/`, and 0.2.0 handed it http-protocol's encoded octets.
   - `:form-data` (alist or string) and an alist `:data` now arrive as `key1=value1`. 0.2.0 failed before sending anything: `The value of QURI.ENCODE::PARAMS-ALIST is #(107 101 …), which is not of type LIST`.
   - `:files` posts arrive as http-protocol's multipart body. 0.2.0 refused them as stream bodies.
   - A pre-encoded multipart `:content` arrives unchanged (0.2.0: a `TYPE-ERROR` in dexador's `multipart-content-length`). So does a gzip-coded form (0.2.0: refused as a stream body).
   - Those last three go with a leading space in the Content-Type value. That is optional whitespace before a field value (RFC 9112 §5), which the origin strips, and it keeps dexador from encoding the body again.
   - JSON worked before and still does.
   - The #32 report itself, `(http:post url :form-data '(("key1" . "value1")))` through http-protocol's facade, returns 200.
5. **Redirects (#33).** The backend now follows redirects itself, so `response-history` lists them, oldest first (`302 …/r/2, 301 …/r/1`). 0.2.0 left it empty.
   - A POST answered by a 303 is followed by a GET with no body. Dexador's own redirect sent the POST body, its Content-Type and its Content-Length again.
   - A 307 resends the POST and its body.
   - More than `:max-redirects` signals `http-redirect-error`, as on http-backend-async. 0.2.0 returned the last 302.
   - A redirect to another origin drops `Authorization`, and `Host` keeps the port. Dexador sent `Host: 127.0.0.1`.

## Notes

- The `.asd` does not depend on `"dexador"` directly. Doing that (or pinning `(:version "dexador" "0.9.16.1")`) makes the local client pull `uiop:3.1.1`, which gets 403 (cl-repository#171). The version line shows the dexador that loaded.
