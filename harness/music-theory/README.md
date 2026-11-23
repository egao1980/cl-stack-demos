# music-theory demo: spelled pitches, the line of fifths, intervals

`harness-demos-music-theory` takes a short tour of **music-theory 0.1.0** (slice MT0) from GHCR. The library treats tonal music theory as values and exact arithmetic: spelled pitches, the line of fifths and intervals. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9. Home and temp paths in it are replaced with `~` and `<tmp>`.

## Run

```bash
cd harness/music-theory
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-music-theory")' \
  --eval '(harness-demos-music-theory:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`, and nothing else: no network once the package is installed, and no model. It runs in well under a second. The demo checks each printed result that has a known answer (118 checks). `run-demos` returns `NIL` if any check fails or a section stops on an error, and CI then exits 1.

## What each section shows

1. **Spelled pitches.** `parse-pitch` and `pitch-name` in ASCII (`F#4`, `Bbb3`, `C-1`) and Unicode (`F♯4`, `B𝄫3`). The parser takes `x` for a double sharp, either letter case and surrounding blanks. A `-` is never a flat: in `B-1` it belongs to octave -1. The section also shows a round trip over all 35 spellings in octaves -1 to 9 (385 names), readable `#S(...)` printing, and `pitch-class-name`.
2. **MIDI and the scientific octave.** The octave belongs to the letter: B#3 and C4 are both MIDI 60, but they are not `equalp`. `pitch-enharmonic-p` says they sound the same, and `interval-between` C4 and B#3 is `-d2`, not a unison. The section also shows the staff position (`pitch-diatonic-index`) and the pitch class.
3. **The line of fifths.** A table of positions -15 to 19 (Fbb to B##), and the twelve pitch classes with their two or three spellings, twelve positions apart. Each interval is a fixed distance on the line (P5 +1, M2 +2, A4 +6, d5 -6). Over all 1225 pairs of spellings, the distance and the simple interval name determine each other.
4. **Intervals.** `interval-between` with its name, quality, number, direction, compound flag and simple form (E♭4 to A4 is A4, C4 to G5 is P12, A4 down to C3 is -M13). `parse-interval` and `interval-name` round-trip 161 names and reject the 22 impossible ones (P3, M4, …). The section shows `simple-interval`, `invert-interval` (numbers add up to 9) and `reverse-interval`. A unison is read as ascending, so reversing A1 gives d1.
5. **Transposition keeps the spelling.** F#4 up M2 is G#4, and up d3 it is Ab4. The C major scale goes up an A4 to F# major (with E#) and up a d5 to Gb major (with Cb): the same MIDI numbers, but no shared spelling. Twelve fifths up from C1 end on B#7, not C8. Moving p by `(interval-between p q)` gives q for all 11,025 pairs of 105 pitches.
6. **Conditions and restarts.** `invalid-pitch` carries the text and a reason. `handler-bind` answers `use-value nil` for bad input, and the interactive `use-value` reads replacement text from `*query-io*`. `parse-interval "M4"` is given `P4` through `use-value`. Transposing G# major up A1 fails on F##5, which would need three sharps (`invalid-pitch`, no restart). An interval with no name still transposes. One `music-theory-error` handler catches every case.

## Rough edges found

Filed on [egao1980/music-theory](https://github.com/egao1980/music-theory/issues):

- `parse-pitch "Cbbb4"` (also `"C♯♭4"` and `"Cx#4"`) gives the reason "missing octave", although the octave is there and the extra accidental is what is wrong. Section 6 shows it. ([#4](https://github.com/egao1980/music-theory/issues/4))
