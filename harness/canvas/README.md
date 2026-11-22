# Canvas demo: live htmx views from the image

`harness-demos-canvas` is the first canvas demo (H4h). It uses **canvas-protocol 0.1.1**, **canvas-backend-svg 0.1.0**, **rpc-backend-htmx 0.1.0** (with `rpc-backend-htmx/canvas`), **engels 0.2.1** and **html-protocol 0.1.2**, all from GHCR. A canvas is a plain CL function in the running image: keyword arguments arrive as request strings and it returns a canvas-protocol tree. No page exists on disk. The image renders every request from its current state.

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, with home and temp paths replaced by `~`, `<demo>` and `<tmp>`. [`dashboard.html`](dashboard.html) is the frozen dashboard, and [`dashboard.png`](dashboard.png) is a headless Chrome screenshot of it.

## Run

```bash
cd harness/canvas
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-canvas")' \
  --eval '(harness-demos-canvas:run-demos)' > demo-output.txt 2>&1
# optional: the screenshot
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless --hide-scrollbars \
  --window-size=1024,1240 --screenshot="$PWD/dashboard.png" "file://$PWD/dashboard.html"
```

Once the packages are installed, a run takes a few seconds. The server listens on `127.0.0.1:18467` only while the run lasts (`run-demos :port`), and `with-server` stops it at the end.

## The canvases

| Function | What it returns |
|----------|-----------------|
| `test-table &key sort` | A sortable `canvas:table` of this session's assertion counts. The header buttons `hx-get` the same canvas with `sort=` and swap `#tests`. |
| `test-chart &key min` | `<div id="chart">` with a bar chart of the systems that have at least `min` assertions. `coerce-form` turns a bad value into an inline `<small role="alert">` and uses the default instead. |
| `threshold-form &key min` | A `canvas:form` that `hx-post`s to `test-chart` with `hx-target="#chart"` and `outerHTML`, so a submit re-renders only the chart. |
| `timing-chart` | A line chart (log y axis) of the unkernel guest and host SBCL timings from [`../unkernel`](../unkernel). |
| `ticker stream &key n` | A `:call-stream` canvas that sends `n` `fragment-event`s named `row` over SSE. |
| `dashboard` | All of the above in `<main>`, with a `live-region` subscribed to `ticker`. |

The handler table (`make-htmx-handlers`) is the allowlist. The theme is engels plus `canvas:chart-css` (the `--chart-1..8` Okabe–Ito series colours), registered as a `theme-stylesheet` method for `:engels`. canvas-protocol 0.1.1 ships only `:placeholder` (see the rough edges below).

## What each section shows

1. **In-process client.** `htmx-client` calls the Clack app with no socket. It shows the table fragment sorted by `-tests`, the form's `hx-post` attributes, a POST of `min=400` (3 bars), and `min=lots` (the inline coercion error, then the default). It also shows the SSE ticker's three `row` events, and that `COMMON-LISP-USER::SECRET` is a 404 because it is not in the allowlist.
2. **Real HTTP.** Hunchentoot (`http-server-backend-hunchentoot`) serves the app on 127.0.0.1, and `http-backend-async` on libuv drives it. The app is wrapped in a small gate that follows the brief's H4h-2 shape:
   - a request without the token gets a 403;
   - the one-time `?token=` URL returns a 303 with an `HttpOnly; SameSite=Strict` cookie, and the client's cookie jar carries the cookie from then on;
   - the same URL from another client gets a 403, because the URL token is spent.

   Every response carries the brief's `Content-Security-Policy`. The section shows the full page (htmx-config meta, assets from `'self'`), the live region's `sse-connect` markup, `csp-violations` → `NIL`, a form POST that returns only the chart fragment, the SSE stream on the wire, and the vendored htmx 2.0.11 served from the asset route.
3. **`freeze`.** The dashboard becomes one static HTML file with the stylesheet inlined. htmx is dropped rather than disabled, and the SVG charts stay inline. Two calls give byte-identical output. The scan finds no `http:`, `https:`, `//`, `url(`, `@import`, `<script`, `<link`, `hx-` or `sse-` in it. The file is committed as `dashboard.html`.
4. **Live redefinition.** While the server runs, the demo `eval`s a new `defun test-chart` (a scatter chart with a vermillion series colour). The next HTTP request returns `data-chart="scatter"` with `color="#D55E00"`, with no restart and no reload. A symbol entry in the handler table is called through the symbol, so the image's current definition answers.

## Rough edges found

- A canvas with no parameters, `(defun dashboard () …)`, answers 500 "invalid number of arguments: 2", because handlers are called with `:allow-other-keys t`. The demo declares `(&key)`. ([rpc-backend-htmx#3](https://github.com/egao1980/rpc-backend-htmx/issues/3))
- rpc-backend-htmx has no CSP header or token gate. The demo wraps the app in about 20 lines; the brief puts this in H4h-2 (cl-stack-code). ([rpc-backend-htmx#4](https://github.com/egao1980/rpc-backend-htmx/issues/4))
- Long bar-chart category labels overlap ("canvas-backend-svg" and "compute-backend-unkernel" in the screenshot). ([canvas-backend-svg#2](https://github.com/egao1980/canvas-backend-svg/issues/2))
- canvas-protocol 0.1.1 has no `:engels` theme yet, and engels does not include the chart series rules ([engels#4](https://github.com/egao1980/engels/issues/4)), so the demo combines `engels:css` and `canvas:chart-css` itself. engels styles `main` as the page container, but `page` and `freeze` emit the body bare. ([canvas-protocol#5](https://github.com/egao1980/canvas-protocol/issues/5))
