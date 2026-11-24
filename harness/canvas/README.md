# Canvas demo: live htmx views from the image

`harness-demos-canvas` is the canvas demo (H4h). It uses **canvas-protocol 0.2.0**, **canvas-backend-svg 0.1.0**, **rpc-backend-htmx 0.1.0** (with `rpc-backend-htmx/canvas`), **engels 0.2.1**, **html-protocol 0.1.2** and **mime-protocol 0.2.0**, all from GHCR. A canvas is a plain CL function in the running image: keyword arguments arrive as request strings and it returns a canvas-protocol tree. No page exists on disk. The image renders every request from its current state.

Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, with home and temp paths replaced by `~`, `<demo>` and `<tmp>`. [`dashboard.html`](dashboard.html) is the frozen dashboard, and [`dashboard.png`](dashboard.png) is a headless Chrome screenshot of it. [`diagram.html`](diagram.html) is the frozen engine-SVG page from section 6, with its screenshot [`diagram.png`](diagram.png).

## Run

```bash
cd harness/canvas
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-canvas")' \
  --eval '(harness-demos-canvas:run-demos)' > demo-output.txt 2>&1
# optional: the screenshot
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless --hide-scrollbars \
  --window-size=1024,1240 --screenshot="$PWD/dashboard.png" "file://$PWD/dashboard.html"
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless --hide-scrollbars \
  --window-size=1024,700 --screenshot="$PWD/diagram.png" "file://$PWD/diagram.html"
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
| `grid-editor` | A 4×4 grid in a `<section>` that binds the arrow keys, Backspace and a–z (`hx-post … :keys … :sync :queue-all`). |
| `grid-key &key key` | One key press: the new grid for `#grid`, plus the status bar and a key-log line out of band (`hx-swap-oob`). An unbound chord is a 422. |
| `diagram` | The engine-generated SVG after `svg-fragment` and `strip-violations`, with the list of what was stripped. Frozen to `diagram.html`. |
| `upload-form` / `upload-images &key images` | A form with `file-input :accept "image/png,.svg" :multiple t`, and its handler, which runs `coerce-form` with a `:file` spec. |

The handler table (`make-htmx-handlers`) is the allowlist. The theme is engels plus `canvas:chart-css` (the `--chart-1..8` Okabe–Ito series colours), registered as a `theme-stylesheet` method for `:engels`. canvas-protocol 0.2.0 still ships only `:placeholder` (see the rough edges below).

## What each section shows

1. **In-process client.** `htmx-client` calls the Clack app with no socket. It shows the table fragment sorted by `-tests`, the form's `hx-post` attributes, a POST of `min=400` (3 bars), and `min=lots` (the inline coercion error, then the default). It also shows the SSE ticker's three `row` events, and that `COMMON-LISP-USER::SECRET` is a 404 because it is not in the allowlist.
2. **Real HTTP.** Hunchentoot (`http-server-backend-hunchentoot`) serves the app on 127.0.0.1, and `http-backend-async` on libuv drives it. The app is wrapped in a small gate that follows the brief's H4h-2 shape:
   - a request without the token gets a 403;
   - the one-time `?token=` URL returns a 303 with an `HttpOnly; SameSite=Strict` cookie, and the client's cookie jar carries the cookie from then on;
   - the same URL from another client gets a 403, because the URL token is spent.

   Every response carries the brief's `Content-Security-Policy`. The section shows the full page (htmx-config meta, assets from `'self'`), the live region's `sse-connect` markup, `csp-violations` → `NIL`, a form POST that returns only the chart fragment, the SSE stream on the wire, and the vendored htmx 2.0.11 served from the asset route.
3. **`freeze`.** The dashboard becomes one static HTML file with the stylesheet inlined. htmx is dropped rather than disabled, and the SVG charts stay inline. Two calls give byte-identical output. The scan finds no `http:`, `https:`, `//`, `url(`, `@import`, `<script`, `<link`, `hx-` or `sse-` in it. The file is committed as `dashboard.html`.
4. **Live redefinition.** While the server runs, the demo `eval`s a new `defun test-chart` (a scatter chart with a vermillion series colour). The next HTTP request returns `data-chart="scatter"` with `color="#D55E00"`, with no restart and no reload. A symbol entry in the handler table is called through the symbol, so the image's current definition answers.
5. **A keyboard-driven grid editor** (0.2.0). The cells and the selection live in the image. `key-chord` spells each binding the way `canvas-keys.js` reports the key press (`"ctrl+Z"` → `"Ctrl+z"`), and it refuses chords that can never match (`"Shift+/"`: the browser reports `?`). The editor `<section>` gets `hx-post` with `:keys` (→ `data-canvas-keys` and `hx-trigger="canvas:key"`) and `:sync :queue-all` (→ `hx-sync="this:queue all"`), so key presses queue in order. The section is never swapped, so its queue survives every response. `(page body :keys t)` adds the same-origin `canvas-keys.js` after htmx and the SSE extension, and the asset route serves it. The in-process client then sends three key presses (`ArrowRight`, `ArrowDown`, `k`) the way htmx does after `canvas:key` fires: a POST with `key=<chord>`, `HX-Trigger: editor` and `HX-Target: grid`. Each response is the new grid for `#grid`, plus `<p id="status" hx-swap-oob="true">` and `<ol hx-swap-oob="beforeend:#log">`, so one response updates three panels. A chord sent by hand that the editor does not bind (`F5`) gets a 422 from `coerce-param :choice`. Last, an htmx event filter (`keydown[key=='ArrowUp']`) is refused with `canvas-csp-error`, and `csp-violations` reports `:eval-trigger` for it. The reason: htmx runs a filter through the Function constructor, which needs `'unsafe-eval'`. The CSP has no `'unsafe-eval'`, and with the page's `allowEval: false` htmx 2.0.11 would let the filter match every event.
6. **Engine-generated SVG** (0.2.0). `engine-svg` stands in for a third-party renderer. Its output has `<defs>` with a gradient, a marker and a `<symbol>`, `<use href="#…">` (one as `xlink:href`), groups with ids and transforms, a `<style>` block and `<metadata>`. It also has six things a page must not run: an `onclick`, a `<script>`, a `<foreignObject>`, an external `<use href>`, `fill="url(https://…)"` and an `hx-get`. `svg-fragment` signals `unsafe-svg` (a `canvas-csp-error`) with those six violations in document order. The `strip-violations` restart returns the cleaned tree: the ids, the camelCase names (`viewBox`, `preserveAspectRatio`, `linearGradient`, `gradientUnits`, `refX`, `markerWidth`) and `url(#stage-fill)` are kept, `xlink:href` becomes `href`, and `<style>`, `<metadata>`, comments and the namespace declarations are dropped. The cleaned tree has no CSP violations, `neutralize` leaves it unchanged, and `svg-fragment` of its own output gives the same text. The page built around it scans clean, and `freeze` writes `diagram.html`, which is byte-deterministic and has no `<script`, `on*`, `foreignObject`, `http:` or `https:`. The stripped badge `<rect>` keeps its geometry and loses only its fill, so it renders as a small black box in `diagram.png`.
7. **Uploads** (0.2.0). The form holds `file-input :accept "image/png,.svg" :multiple t`, so `form` adds `enctype="multipart/form-data"`, which htmx sends as `FormData`. Three files under one name are written as a browser body and parsed by mime-protocol: a 1×1 PNG named `../../chart.png`, a 6 KB PNG, and a GIF. `coerce-param :file :max-size 2048 :accept "image/png,.svg"` gives:
   - for the PNG, an `uploaded-file` whose filename is `chart.png` (only the last path segment is kept);
   - for the large PNG, `canvas-upload-too-large` (`size` 6070, detail 2048), and `use-default` returns the default;
   - for the GIF, a `canvas-coercion-error` with `expected :accept`, and `use-value` returns the value given. Its `error-fragment` is the `<small role="alert">` that renders next to the input.

   The handler `upload-images` (`coerce-form`, `:multiple t`) re-renders the form with the first error inline, or lists the files. The same body POSTed through the app gets a 400, because rpc-backend-htmx 0.1.0 reads multipart parts as UTF-8 text (see below).

The run ends with a self-check line for sections 5–7 (18 checks, all `T`). Section 4 keeps its own check line.

## Rough edges found

- A canvas with no parameters, `(defun dashboard () …)`, answers 500 "invalid number of arguments: 2", because handlers are called with `:allow-other-keys t`. The demo declares `(&key)`. ([rpc-backend-htmx#3](https://github.com/egao1980/rpc-backend-htmx/issues/3))
- rpc-backend-htmx has no CSP header or token gate. The demo wraps the app in about 20 lines; the brief puts this in H4h-2 (cl-stack-code). ([rpc-backend-htmx#4](https://github.com/egao1980/rpc-backend-htmx/issues/4))
- Long bar-chart category labels overlap ("canvas-backend-svg" and "compute-backend-unkernel" in the screenshot). ([canvas-backend-svg#2](https://github.com/egao1980/canvas-backend-svg/issues/2))
- canvas-protocol 0.2.0 still has no `:engels` theme, and engels does not include the chart series rules ([engels#4](https://github.com/egao1980/engels/issues/4)), so the demo combines `engels:css` and `canvas:chart-css` itself. engels styles `main` as the page container, but `page` and `freeze` emit the body bare. ([canvas-protocol#5](https://github.com/egao1980/canvas-protocol/issues/5))
- rpc-backend-htmx's full page (`htmx-full-page`) calls `canvas:page` without `:keys`, so no page the transport serves loads `canvas-keys.js` (section 5 prints `NIL`). The demo shows the bound page by calling `page :keys t` under the transport's resolver. ([rpc-backend-htmx#6](https://github.com/egao1980/rpc-backend-htmx/issues/6))
- Uploads do not work end to end yet. rpc-backend-htmx 0.1.0's codec turns every multipart part into UTF-8 text, so a binary file is a 400 ("Invalid UTF-8 in multipart body"), and `htmx-client` cannot send a file part. Section 7 passes the parts that mime-protocol parsed straight to `coerce-param`. ([rpc-backend-htmx#5](https://github.com/egao1980/rpc-backend-htmx/issues/5))
