# musicxml demo: MusicXML 4.0 scores read, split, written and archived

`harness-demos-musicxml` is a tour of **score-backend-musicxml 0.1.1** from GHCR. That is slice SC2 of the score family: it reads MusicXML 4.0 partwise files into [score-protocol](../score/) scores and writes them back, as text (`.musicxml`) or as a compressed archive (`.mxl`). Element ids travel as MusicXML `id` attributes. Chord symbols are music-theory 0.5.1 values. The backend also registers the serdes-protocol formats `:musicxml` and `:mxl`.

The input is two public-domain hymns, re-encoded by hand for this demo, and for section 8 two bars of a public-domain keyboard minuet. All three files validate against the MusicXML 4.0 XSD.

- [`scores/new-britain.musicxml`](scores/new-britain.musicxml) holds NEW BRITAIN, the tune of "Amazing Grace" (American folk melody, 1829; words by John Newton, 1779). It is the first phrase pair, sung twice: 3/4, G major, a one-beat pickup, two verses, ten `<harmony>` chord symbols, a tie, two slurs, a dynamic, a tempo mark and a repeat with first and second endings. Fourteen elements carry `id` attributes.
- [`scores/old-hundredth-satb.musicxml`](scores/old-hundredth-satb.musicxml) holds the first line of OLD HUNDREDTH ("All people that on earth do dwell": melody by Louis Bourgeois, 1551; words by William Kethe, 1561) in hymnal layout on two staves. The alto, tenor and bass were written for the demo.

The demo code is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, with `MUSICXML_XSD` set (section 6). The home directory in it is written as `~` and the run's temp directory as `<tmp>`.

The `.asd` asks for score-backend-musicxml 0.1.1, score-protocol 0.1.0 and music-theory 0.5.1. cl-repo treats every version in `:depends-on` as a minimum, so it loads the newest of each: score-protocol 0.3.0, music-theory 0.5.2 and serdes-protocol 0.3.0 in the last run, as the first line of the output shows.

## Run

```bash
cd harness/musicxml
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-musicxml")' \
  --eval '(harness-demos-musicxml:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`. There is no network step once the packages are installed. A run takes about five seconds, most of it start-up. The demo checks every printed result that has a known answer: 170 checks with the schema check, 166 without it. `run-demos` returns `NIL` if any check fails or a section stops on an error, and CI then exits 1.

The schema check is optional. Fetch the `schema/` directory of [w3c/musicxml](https://github.com/w3c/musicxml/tree/v4.0/schema) at tag v4.0 (`musicxml.xsd`, with `xml.xsd`, `xlink.xsd`, `container.xsd` and `catalog.xml` beside it). Then point `MUSICXML_XSD` at that `musicxml.xsd` and have `xmllint` on `PATH`. The XSD is not in this repository. Without it, section 6 says it was skipped and why.

## What each section shows

1. **Reading.** The section prints the whole hymn file, then `read-musicxml` on its pathname. The summary shows:
   - the metadata (`<work-title>` is the title, `<movement-title>` the subtitle);
   - the part, its staff and its voice;
   - the measure map: the pickup as bar 0, the tempo mark, the repeat start, the endings with their passes, the repeat count and the final barline;
   - every event with its position, tie, dynamic and fermata, and its lyrics in two verses, with `:begin` / `:middle` / `:end` syllables and extenders;
   - both verses rebuilt as text;
   - the ten chord symbols at `(bar . offset)`, among them N.C. and G7/B at `(2 . 1/2)`;
   - the two slurs.

   Then a table of all 14 `id` attributes in the file, with what each one became. Twelve became element ids: a measure's slice id `pickup.melody` gives the measure `pickup`, `<tied id="held">` gives the tie, and `<barline id="first-time">` gives the first ending. `held.note` is a valid NCName but not a valid score id (`.` is kept for slice ids), so that note gets a minted id. `make-score` mints the other 67 ids in document order, and the id counter is 67. A second read gives the same ids, and a string, UTF-8 octets and a character stream read like the pathname.
2. **Hymnal voices.** Read as written, the SATB excerpt has three voices: 1 and 2 on the treble staff, and 5 on the bass staff, holding shared-stem chords. With `:split-voices :auto` it reads as four voice lines, S, A, T and B, with stems up, down, up and down. The unison D4 in bar 1 (one printed note sung by soprano and alto) gives the alto a copy. The copy is the one `low-confidence-guess` ("unison", 1/2), with confidence 1/2 and provenance `score-backend-musicxml`. The soprano keeps the chord events' ids, the alto takes the lower chord notes' ids, and the new bass voice is minted. `split-voices` on the unsplit score gives the same score, `check-score` finds nothing, and a split score has nothing left to split.
3. **Chord symbols.** Twenty `<harmony>` elements are read to music-theory chord symbols. They cover sixteen kinds, a slash bass, an added degree, both orders of `subtract 3` / `add 4`, and `none`. Each one is printed, written back with the kind and degrees music-theory 0.5.1 picks, and read again to an equal value. Two cases are worth a note:
   - `Neapolitan` on Db reads as Db and is written as `major`.
   - `major` with `subtract 3, add 4` on A over E reads as Asus4/E and is written as `suspended-fourth`. In the other order, `add 4, subtract 3`, it is A(add4,omit3)/E.

   `use-symbols="yes"` and `text="Δ7"` on `<kind>` are display attributes: all three spellings of a C major seventh read alike, and the writer emits neither. music-theory's `:jazz` dialect prints the symbols (CΔ7). A symbol parsed from text loses its text and dialect in MusicXML, and `chord-symbol-to-harmony` says so as a second value. `<kind>other</kind>` is `musicxml-unsupported`, and `skip-element` drops it.
4. **Content outside the model.** The hymn with one `<ornaments><trill-mark/>` added. By default the read stops with `musicxml-unsupported`, which gives the element `"ornaments"`, the path `/score-partwise[1]/part[1]/measure[3]/note[1]/notations[1]/ornaments[1]`, the format and the reason. `skip-element`, the innermost restart, is invoked from `handler-bind`. The result is `score-equal` to the hymn (ids and all), writes the same bytes, and has no trace of the ornament. `:unsupported :warn` drops it with one `lossy-conversion` warning (`<ornaments>`). `:skip` drops it with no condition at all. `:ignore` is a `type-error`. Through serdes-protocol the error arrives as `serdes-decode-error`, with the path in its message. Since serdes-protocol 0.3.0 the reader's condition is its cause, and `skip-element` taken from a handler around `serdes-protocol:decode` gives the hymn again.
5. **Writing.**
   - `write-musicxml` writes the XML declaration, the DOCTYPE, a `<player>` per voice and an `id` on every element's carrier: 80 attributes for 79 elements, since the part's id is on `<score-part>` and also on `<part>`.
   - Read, write, read gives a `score-equal` score. The output is byte-stable when written twice, and a fixed point under read and write.
   - Writing to a pathname works too.
   - Writing the split SATB score warns `lossy-conversion` for "provenance and confidence", and the guess's confidence is gone after a re-read.
   - `.mxl`: `write-mxl` gives the same bytes every time. The archive holds `mimetype` first, then `META-INF/container.xml`, then `score.musicxml`, all stored. The section prints `container.xml`. `read-mxl` reads the archive from octets and from a file. MusicXML text given to `read-mxl` is a `score-format-error` ("not a zip archive").
   - The `:musicxml` and `:mxl` serdes formats: media types, `find-format-for-media-type`, `format-binary-p`, and `encode` / `decode` equal to the functions above.
6. **Schema check (optional).** With `MUSICXML_XSD` and `xmllint`, four files are validated with `--nonet` and the schema's catalog: the written hymn, the written SATB split, the hymn after an `.mxl` round trip, and `container.xml` against `container.xsd`. All four validate. Without them, the section prints why it was skipped.
7. **XXE safety.** An external entity naming a file in the run's temp directory is refused at its declaration (line 3, column 24): a `score-format-error` whose cause is `xml-protocol:xml-entity-error`. The secret is nowhere in the report. A declared but unused entity is refused just the same, and so is a parameter entity naming `http://127.0.0.1:9/`. The refusal is about the declaration, not a file or a connection, so nothing was fetched. Internal entities are not expanded either: "billion laughs" stops at its first reference. The external DTD subset in every MusicXML DOCTYPE is refused with a `continue` restart, which `read-musicxml` takes.
8. **The 0.1.1 fixes.** Each case shows the input, what 0.1.0 read (from the 0.1.1 contract and its issue), and what 0.1.1 reads.
    - **Spanner stops close their own voice first ([#3](https://github.com/egao1980/score-backend-musicxml/issues/3)).** [`scores/minuet-in-g.musicxml`](scores/minuet-in-g.musicxml) holds the first two bars of the Minuet in G major, BWV Anh. 114 (Christian Petzold, 1725; the left hand is simplified). It has a slur in each hand, and both are number 1, as when a file numbers slurs per staff. The right hand's slur (voice 1) opens first. The left hand's (voice 5) opens after the `<backup>`. The right hand's stop comes first in bar 2. 0.1.1 reads the slurs G4 → D5 in the right hand and G3 → B3. in the left, each within one voice. A read, write, read round trip is `score-equal`. 0.1.0 paired each stop with the slur of its number opened last, so it read G3 → D5 and G4 → B3., across the hands. The same cross-wiring put a soprano crescendo on the bass in the OLD HUNDREDTH corpus file. The file validates against the MusicXML 4.0 XSD.
    - **Unison bars are split ([#4](https://github.com/egao1980/score-backend-musicxml/issues/4)).** `without-alto-in-bar-2` takes the alto's own notes (voice 2) out of bar 2 of the SATB excerpt. The treble staff of that bar then holds only the soprano's G4 A4 B4, a unison bar as a hymnal prints it. With `:split-voices :auto`, 0.1.1 gives the alto G4 A4 B4 in bar 2 too. Each copy is a `low-confidence-guess` (`"unison"`, 1/2): three in bar 2, and the one in bar 1. `check-score` finds nothing, and `split-voices` gives the same score. 0.1.0 split only bars with a chord in them, so the alto had no notes in bar 2 (and a tie into such a bar was lost). The unchanged excerpt still reads as in section 2.

## Rough edges found

Filed on [egao1980/score-protocol](https://github.com/egao1980/score-protocol/issues):

- Fixed in score-protocol 0.3.0, not yet used by the reader: `lossy-conversion`'s report said "lossy conversion to musicxml" whatever the direction. When `read-musicxml :unsupported :warn` drops an element, the warning read as if something were being written. score-protocol 0.3.0 gives the condition a `:direction`, but the 0.1.1 reader doesn't pass `:from` yet ([score-backend-musicxml#8](https://github.com/egao1980/score-backend-musicxml/issues/8)), so section 4 still shows "lossy conversion to musicxml: <ornaments> dropped". ([#9](https://github.com/egao1980/score-protocol/issues/9))

Section 8 reads two cases that score-backend-musicxml 0.1.1 fixes, [#3](https://github.com/egao1980/score-backend-musicxml/issues/3) and [#4](https://github.com/egao1980/score-backend-musicxml/issues/4). Still open: [#5](https://github.com/egao1980/score-backend-musicxml/issues/5) (a tie between a chord's lower note and a unison ends in the other voice) and [#7](https://github.com/egao1980/score-backend-musicxml/issues/7) (the `serdes.lisp` header still says `skip-element` is out of reach through serdes, and section 4 shows it is now reachable).

Everything else in SC2 behaved as its file headers say.
