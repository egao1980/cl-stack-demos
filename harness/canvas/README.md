# Canvas demo: live htmx views from the image

`harness-demos-canvas` is the canvas demo (H4h). It uses **canvas-protocol 0.2.0**, **canvas-backend-svg 0.1.0**, **rpc-backend-htmx 0.2.0** (with `rpc-backend-htmx/canvas`), **engels 0.2.1**, **html-protocol 0.1.2** and **mime-protocol 0.3.0**, all from GHCR. A canvas is a plain CL function in the running image: keyword arguments arrive as request strings and it returns a canvas-protocol tree. No page exists on disk. The image renders every request from its current state.

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
| `upload-form` / `upload-images &key images` | A form with `file-input :accept "image/png,.svg" :multiple t`, and its handler. The handler runs `coerce-param :file :multiple t :max-size 4096` on what the codec decoded and lists each file with its first bytes in hex. A bad or missing file is a 422. |

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
5. **A keyboard-driven grid editor** (0.2.0). The cells and the selection live in the image. `key-chord` spells each binding the way `canvas-keys.js` reports the key press (`"ctrl+Z"` → `"Ctrl+z"`), and it refuses chords that can never match (`"Shift+/"`: the browser reports `?`). The editor `<section>` gets `hx-post` with `:keys` (→ `data-canvas-keys` and `hx-trigger="canvas:key"`) and `:sync :queue-all` (→ `hx-sync="this:queue all"`), so key presses queue in order. The section is never swapped, so its queue survives every response. The server's transport is made with `make-canvas-htmx-transport :page-keys '(grid-editor)` (rpc-backend-htmx 0.2.0, [#6](https://github.com/egao1980/rpc-backend-htmx/issues/6)). A plain GET of `grid-editor` returns the whole page, and its `<head>` loads `htmx.min.js`, `htmx-ext-sse.min.js` and then `canvas-keys.js`, all from the asset prefix `/canvas-assets/`, which serves them. The same GET through a transport made without `:page-keys` returns that page without the one `canvas-keys.js` script. `dashboard`, which is not in the list, is byte-identical with and without the option. The in-process client then sends three key presses (`ArrowRight`, `ArrowDown`, `k`) the way htmx does after `canvas:key` fires: a POST with `key=<chord>`, `HX-Trigger: editor` and `HX-Target: grid`. Each response is the new grid for `#grid`, plus `<p id="status" hx-swap-oob="true">` and `<ol hx-swap-oob="beforeend:#log">`, so one response updates three panels. A chord sent by hand that the editor does not bind (`F5`) gets a 422 from `coerce-param :choice`. Last, an htmx event filter (`keydown[key=='ArrowUp']`) is refused with `canvas-csp-error`, and `csp-violations` reports `:eval-trigger` for it. The reason: htmx runs a filter through the Function constructor, which needs `'unsafe-eval'`. The CSP has no `'unsafe-eval'`, and with the page's `allowEval: false` htmx 2.0.11 would let the filter match every event.
6. **Engine-generated SVG** (0.2.0). `engine-svg` stands in for a third-party renderer. Its output has `<defs>` with a gradient, a marker and a `<symbol>`, `<use href="#…">` (one as `xlink:href`), groups with ids and transforms, a `<style>` block and `<metadata>`. It also has six things a page must not run: an `onclick`, a `<script>`, a `<foreignObject>`, an external `<use href>`, `fill="url(https://…)"` and an `hx-get`. `svg-fragment` signals `unsafe-svg` (a `canvas-csp-error`) with those six violations in document order. The `strip-violations` restart returns the cleaned tree: the ids, the camelCase names (`viewBox`, `preserveAspectRatio`, `linearGradient`, `gradientUnits`, `refX`, `markerWidth`) and `url(#stage-fill)` are kept, `xlink:href` becomes `href`, and `<style>`, `<metadata>`, comments and the namespace declarations are dropped. The cleaned tree has no CSP violations, `neutralize` leaves it unchanged, and `svg-fragment` of its own output gives the same text. The page built around it scans clean, and `freeze` writes `diagram.html`, which is byte-deterministic and has no `<script`, `on*`, `foreignObject`, `http:` or `https:`. The stripped badge `<rect>` keeps its geometry and loses only its fill, so it renders as a small black box in `diagram.png`.
7. **Uploads end to end** (0.2.0). The form holds `file-input :accept "image/png,.svg" :multiple t`, so `form` adds `enctype="multipart/form-data"`, which htmx sends as `FormData`. With rpc-backend-htmx 0.2.0 ([#5](https://github.com/egao1980/rpc-backend-htmx/issues/5)), `htmx-client` posts files the way a browser does: `htmx-request … :http-method :post :encoding :multipart`, with each file a value `(:filename F :content C :content-type T)`, sent as one raw binary part (no base64). The section prints that request body first. Its CR, LF and binary octets are shown escaped. Then four posts reach the handler `upload-images` through the token gate:
   - the 1×1 PNG named `../../chart.png` plus the engine's SVG (`pipeline.svg`, 2068 octets) under one name: the handler gets a list of two `mime-entity` parts in part order. The response lists `chart.png: image/png, 70 bytes, 89 50 4E 47 0D 0A 1A 0A …` and `pipeline.svg: image/svg+xml, 2068 bytes, 3C 3F 78 6D 6C …`. Each part's `mime-content` equals the octets sent, and `coerce-param` keeps only the last path segment of the filename;
   - a 6070-octet PNG alone: the handler gets one entity, not a list. `:max-size 4096` signals `canvas-upload-too-large`, and the transport answers 422 with `<p role="alert">images must be at most 4096 bytes</p>` (`HX-Retarget #canvas-error`);
   - an empty file input (filename `""`, no content, as a browser sends it): the codec drops the part, the handler gets `NIL`, and `:required t` gives 422 "images is required";
   - a GIF: 422, "images must be a file of type: image/png, .svg".

   Last, the decoded PNG and GIF entities go through `coerce-param` in the image under `handler-bind`. For the PNG, `canvas-upload-too-large` reports `size` 6070 and detail 4096, and `use-default` returns the default. For the GIF, a `canvas-coercion-error` with `expected :accept` takes `use-value`, and its `error-fragment` is the `<small role="alert">` that renders next to the input.

The run ends with a self-check line for sections 5–7 (25 checks, all `T`). Section 4 keeps its own check line.

## Rough edges found

- A canvas with no parameters, `(defun dashboard () …)`, answers 500 "invalid number of arguments: 2", because handlers are called with `:allow-other-keys t`. The demo declares `(&key)`. ([rpc-backend-htmx#3](https://github.com/egao1980/rpc-backend-htmx/issues/3))
- rpc-backend-htmx has no CSP header or token gate. The demo wraps the app in about 20 lines; the brief puts this in H4h-2 (cl-stack-code). ([rpc-backend-htmx#4](https://github.com/egao1980/rpc-backend-htmx/issues/4))
- Long bar-chart category labels overlap ("canvas-backend-svg" and "compute-backend-unkernel" in the screenshot). ([canvas-backend-svg#2](https://github.com/egao1980/canvas-backend-svg/issues/2))
- canvas-protocol 0.2.0 still has no `:engels` theme, and engels does not include the chart series rules ([engels#4](https://github.com/egao1980/engels/issues/4)), so the demo combines `engels:css` and `canvas:chart-css` itself. engels styles `main` as the page container, but `page` and `freeze` emit the body bare. ([canvas-protocol#5](https://github.com/egao1980/canvas-protocol/issues/5))
- Fixed in rpc-backend-htmx 0.2.0: no page the transport served could load `canvas-keys.js` ([#6](https://github.com/egao1980/rpc-backend-htmx/issues/6)), and a binary upload POSTed through the app was a 400 "Invalid UTF-8 in multipart body" ([#5](https://github.com/egao1980/rpc-backend-htmx/issues/5)). Sections 5 and 7 now show both through the app.
