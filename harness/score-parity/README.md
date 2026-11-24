# score-parity demo: the cross-format parity corpus and matrix

`harness-demos-score-parity` is a tour of **score-parity 0.1.0** from GHCR. That is slice SC5 of the score family: a corpus of 34 MusicXML files of 30 public-domain tunes, and the checks every score-protocol format backend must pass on it. The checks are:

- MusicXML and `.mxl` round trips;
- the hymnal voice split;
- score → MIDI → score, as written and unfolded;
- two external oracles, MuseScore's re-export and the MusicXML 4.0 XSD.

The backends under test are [score-backend-musicxml](../musicxml/) 0.1.0 and score-backend-midi 0.1.0 (part 2 of the [midi demo](../midi/)). They run on score-protocol 0.2.0 and music-theory 0.5.1.

The corpus ships with score-parity. Its provenance is in the package's `corpus/PROVENANCE.md`: every tune and text is public domain, and every file was re-encoded from tune data in `generate-corpus.py`. `corpus/facts.sexp` says what each file holds. It is computed by a second implementation of the facet rules and cross-checked with music21.

The demo code is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, with no MuseScore and without `MUSICXML_XSD`, so both oracle columns say `skip`. The home directory in it is written as `~`.

## Run

```bash
cd harness/score-parity
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-score-parity")' \
  --eval '(harness-demos-score-parity:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`. A run takes about six seconds, most of it start-up. The demo checks every printed result that has a known answer: 48 checks, or 49 with the schema. `run-demos` returns `NIL` if any check fails or a section stops on an error, and CI then exits 1.

## What each section shows

1. **The matrix.** `parity-matrix` runs 213 checks: six checks on each of the 34 pieces, and the hymnal split on the nine hymns. The section prints one line per piece and one column per check, then the `all` row from `matrix-cells`. MusicXML, `.mxl`, MIDI and unfolded MIDI each pass 34/34, and the hymnal split passes 9/9. MuseScore and XSD are `skip`, and the reasons are printed. Then `print-matrix` shows the same results per notation feature (lyrics, verses, chord symbols, pickups, repeats, endings, jumps, ties, tuplets, grace notes, key and meter changes, shared staves, several parts), ending with its own `all` row.
2. **One check up close.** This is `:midi` on the lead sheet of *Amazing Grace*. Each facet is printed and compared with `corpus/facts.sexp`:
   - the features;
   - the bar lengths (a 1/4 pickup);
   - the voice line (17 rows of onset, end, MIDI key and spelled pitch);
   - the lyric line (28 rows, two verses with syllabic types);
   - the eight chord symbols with their onsets.

   Then the score goes through `export-midi` (482 octets, one muffled `lossy-conversion`: ids dropped) and `import-midi`, with a table of the six facets: in `+midi-facets+` or not, and whether score and MIDI agree. Only `:lyrics` differs, because a MIDI file holds one verse. That is why the check compares `:verse-1`. `run-check` gives `:pass`.
3. **A deliberate failure, and an error kept as data.**
   - **Failure.** The writer is wrapped for one run so that the first MusicXML text it returns has the pickup's `<step>D</step>` changed to `E`, in memory. `run-check :musicxml` gives `:fail`, "read, write, read is not SCORE-EQUAL", with differences in `:notes` (`(0 1/4 62)` became `(0 1/4 64)`) and `:spelled` (`"D4"` became `"E4"`). With the writer put back, the check passes.
   - **Error.** The MIDI exporter is wrapped so that its file for *Jingle Bells* is cut in half. A matrix over three pieces and both MIDI checks still returns all six results. The two *Jingle Bells* cells are `:error`, with the importer's report as the reason ("midi format error: data ends inside a track chunk at 364"). `print-matrix` lists them under its table.
   - **Misuse.** An unknown check name is still the caller's `type-error`.
4. **The oracles.**
   - **MuseScore.** The section shows `+musescore-names+` and `find-musescore`. A wrong `MUSESCORE_BIN` gives `NIL`, because `PATH` does not override it. With no MuseScore installed the check skips with its reason.
   - **XSD.** `musicxml-schema` is `NIL` for an empty or missing `MUSICXML_XSD`. Unset, the XSD check skips with "MUSICXML_XSD unset, or xmllint missing".

## With the schema

The XSD check is optional and the schema is not in this repository. To run it, fetch the `schema/` directory of [w3c/musicxml](https://github.com/w3c/musicxml/tree/v4.0/schema) at tag v4.0 (`musicxml.xsd`, with `xml.xsd`, `xlink.xsd` and `catalog.xml` beside it). Then point `MUSICXML_XSD` at that `musicxml.xsd` and have `xmllint` on `PATH`. The xsd column of section 1 then fills in, and section 4 shows a rejected file. A local run with the schema (not committed) printed:

```
  all                  34/34     34/34 9/9     34/34 34/34        skip       34/34

  (musicxml-schema) => ("<xsd-dir>/musicxml.xsd" "<xsd-dir>/catalog.xml")
  (count :pass xsd :key #'parity-result-status)
    => 34   ; the xsd column of section 1: every written file valid

A file the schema rejects gives :FAIL with xmllint's message as the reason.
XMLLINT-VALIDATE on the written Amazing Grace with the pitch step H:

  valid => nil
  message, first line: -:47: element step: Schemas validity error : Element 'step': [facet 'enumeration'] The value 'H' is not an element of the set {'A', 'B', 'C', 'D', 'E', 'F', 'G'}.
```

## Rough edges found

Filed on [egao1980/score-parity](https://github.com/egao1980/score-parity/issues):

- Open: the MuseScore skip reason names `mscore`, `musescore` and `MuseScore4`, but `find-musescore` also accepts `mscore4` (section 4 prints both). ([#5](https://github.com/egao1980/score-parity/issues/5))

Everything else in SC5 behaved as its file headers say: every check passes on the whole corpus, and every facet of the piece in section 2 matches the facts.
