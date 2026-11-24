# serdes demo: backend errors translated in place, backend restarts reachable

`harness-demos-serdes` is a short tour of **serdes-protocol 0.3.0** from GHCR. The release changes one thing ([serdes-protocol#11](https://github.com/egao1980/serdes-protocol/issues/11)). `encode` and `decode` now translate a backend's error where it is signalled, before anything unwinds. The original condition is kept as `serdes-error-cause`, and the backend's restarts stay reachable from a handler outside the call. In 0.2 the error was caught with `handler-case`, so the stack was gone by the time the caller's handler ran, and the original condition survived only as text in the message.

The demo uses a real backend: [score-backend-musicxml](../musicxml/) 0.1.1 registers the formats `:musicxml` (text) and `:mxl` (octets). It runs on score-protocol 0.3.0. The value is the first phrase of the "Ode to Joy" melody (Beethoven, Symphony No. 9, 1824), in C major and 4/4, built in code. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9. Home paths in it are replaced with `~`.

## Run

```bash
cd harness/serdes
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-serdes")' \
  --eval '(harness-demos-serdes:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`, and nothing else. It makes 28 checks. `run-demos` returns `NIL` if any check fails or a section stops on an error.

## What each section shows

1. **Formats and round trips.** The section shows the two formats' media types, whether each is binary, and the format found for a media type. The score goes through `encode` / `decode` as MusicXML text (the same text `write-musicxml` gives), through `encode-to-octets` / `decode-octets` and `:stream`, and through `:mxl` octets (a ZIP archive). Every round trip is `score-equal` to the original, ids and all. A warning is not an error, so serdes leaves it alone. A note with a confidence gives `lossy-conversion` ("provenance and confidence dropped"), and a handler around `encode` sees that warning itself.
2. **Decode errors, translated in place.** A trill (`<ornaments>`) is inserted on the fifth note. Read directly, it signals `musicxml-unsupported` inside `skip-element`. Through `decode` the caller gets `serdes-decode-error`. A `handler-bind` outside `decode` finds three things: the cause is the reader's `musicxml-unsupported` (element `"ornaments"`), the most recent restart is the reader's `skip-element`, and `(find-restart 'skip-element e)` finds it through the new condition. A handler that tests the cause and calls `(skip-element e)` reads a file with trills on the 5th and 13th notes. It is called once per trill, in document order, with each one's path. The result is `score-equal` to the melody and to the reader's own `:unsupported :skip` read. Three rules stay as before. A handler for `musicxml-unsupported` bound around `decode` is not called. A `handler-case` clause runs after the unwind: the cause is kept, but no restart is left. `backend-decode` on `find-backend` translates nothing.
3. **Encode errors.** `write-musicxml` checks its argument with `check-type`, which signals a `type-error` inside a `store-value` restart. `(encode "Ode to Joy" :format :musicxml)` gives `serdes-encode-error` with that `type-error` as its cause. A handler answers `(store-value *melody* e)`, and `encode` returns the melody's MusicXML. `:mxl` works the same way.
4. **What is passed through.** `serdes-unknown-format` is serdes's own error: it reaches the caller as it is, with no cause. Text that is not well-formed XML, and an `.mxl` archive cut short, arrive as `serdes-decode-error`. In both cases the cause is the reader's `score-format-error`, which wraps the XML or ZIP error.

## Rough edges found

- [egao1980/serdes-protocol#14](https://github.com/egao1980/serdes-protocol/issues/14): `sexp-protocol` can't be used with serdes-protocol 0.3.0. The `serdes-protocol:0.3.0` image has no `sexp-protocol.asd`, and the only `sexp-protocol` image (0.2.2) is the serdes-protocol 0.2.2 tree. So a project that pins serdes-protocol 0.3.0 and loads `sexp-protocol` doesn't resolve. That is why this demo uses the MusicXML formats rather than `:sexp`.
- [egao1980/cl-repository#199](https://github.com/egao1980/cl-repository/issues/199): in that case `cl-repo sync` stops with "serdes-protocol 0.3.0 is installed ... but below the required 0.3.0".
- Already filed and confirmed here: [egao1980/score-backend-musicxml#7](https://github.com/egao1980/score-backend-musicxml/issues/7). The `serdes.lisp` header of score-backend-musicxml still says `skip-element` can't be reached through `serdes-protocol:decode`, and section 2 shows it can.

Everything else behaved as the header of `src/protocol.lisp` says.
