# music-theory demo: pitches and intervals, meter, rhythm and tempo

`harness-demos-music-theory` takes a short tour of **music-theory 0.2.0** from GHCR. The library treats tonal music theory as values and exact arithmetic. Sections 1-6 cover slice MT0 (spelled pitches, the line of fifths and intervals) and sections 7-11 slice MT1, new in 0.2.0 (meters, the metric hierarchy, dots and tuplets, tie splitting, tempo). Durations and positions are rationals in whole notes: 1/4 is a quarter, 3/8 a dotted quarter. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9. Home and temp paths in it are replaced with `~` and `<tmp>`.

## Run

```bash
cd harness/music-theory
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-music-theory")' \
  --eval '(harness-demos-music-theory:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`, and nothing else: no network once the package is installed, and no model. It runs in well under a second. The demo checks each printed result that has a known answer (220 checks: 118 for MT0, 102 for MT1). `run-demos` returns `NIL` if any check fails or a section stops on an error, and CI then exits 1.

## What each section shows

1. **Spelled pitches.** `parse-pitch` and `pitch-name` in ASCII (`F#4`, `Bbb3`, `C-1`) and Unicode (`F♯4`, `B𝄫3`). The parser takes `x` for a double sharp, either letter case and surrounding blanks. A `-` is never a flat: in `B-1` it belongs to octave -1. The section also shows a round trip over all 35 spellings in octaves -1 to 9 (385 names), readable `#S(...)` printing, and `pitch-class-name`.
2. **MIDI and the scientific octave.** The octave belongs to the letter: B#3 and C4 are both MIDI 60, but they are not `equalp`. `pitch-enharmonic-p` says they sound the same, and `interval-between` C4 and B#3 is `-d2`, not a unison. The section also shows the staff position (`pitch-diatonic-index`) and the pitch class.
3. **The line of fifths.** A table of positions -15 to 19 (Fbb to B##), and the twelve pitch classes with their two or three spellings, twelve positions apart. Each interval is a fixed distance on the line (P5 +1, M2 +2, A4 +6, d5 -6). Over all 1225 pairs of spellings, the distance and the simple interval name determine each other.
4. **Intervals.** `interval-between` with its name, quality, number, direction, compound flag and simple form (E♭4 to A4 is A4, C4 to G5 is P12, A4 down to C3 is -M13). `parse-interval` and `interval-name` round-trip 161 names and reject the 22 impossible ones (P3, M4, …). The section shows `simple-interval`, `invert-interval` (numbers add up to 9) and `reverse-interval`. A unison is read as ascending, so reversing A1 gives d1.
5. **Transposition keeps the spelling.** F#4 up M2 is G#4, and up d3 it is Ab4. The C major scale goes up an A4 to F# major (with E#) and up a d5 to Gb major (with Cb): the same MIDI numbers, but no shared spelling. Twelve fifths up from C1 end on B#7, not C8. Moving p by `(interval-between p q)` gives q for all 11,025 pairs of 105 pitches.
6. **Conditions and restarts.** `invalid-pitch` carries the text and a reason. `handler-bind` answers `use-value nil` for bad input, and the interactive `use-value` reads replacement text from `*query-io*`. `parse-interval "M4"` is given `P4` through `use-value`. Transposing G# major up A1 fails on F##5, which would need three sharps (`invalid-pitch`, no restart). An interval with no name still transposes. One `music-theory-error` handler catches every case.
7. **Meters.** `parse-meter` reads `4/4`, `6/8`, `12/8`, the additive `2+3/8` and the symbols `C` and `C|` (printed `4/4` and `2/2` by `meter-string`). `meter-beats` gives two dotted-quarter beats for 6/8, three eighths for 3/8 (simple) and `(1/4 3/8)` for 2+3/8, whose `beat-unit` is `NIL`. `compound-p` holds for 6/8, 9/8, 12/8 and 6/4. A table covers twelve meters. `invalid-meter` gives a reason for each bad text and takes a `use-value`. A leading zero (`04/4`) is accepted, which [#3](https://github.com/egao1980/music-theory/issues/3) leaves open.
8. **The metric hierarchy.** `metric-weight` maps of one bar on the 1/16 grid for 4/4, 6/8 and 2+3/8, with a dot column per position (the downbeat 0, the half bar of 4/4 at 1, the beats at 2, eighths 3, sixteenths 4), then 3/4, 12/8 and 2/2 as rows of weights. The same offset 1/4 weighs 2 in 4/4, 3 in 6/8 and 1 in 2+3/8. Offsets are taken modulo the bar (pickups, later bars), and a triplet position weighs 9.
9. **Dots and tuplets.** `duration-dots` gives the base and dots (7/16 is a double-dotted quarter; 5/8, 1/12 and 1/20 are no single value), `tuplet-duration` the sounding length under n/m (a triplet eighth is 1/12, a quintuplet 16th 1/20, a duplet eighth in 6/8 3/16), and `duration-dots :tuplet` turns it back (180 of 180 bases, dots and ratios).
10. **Tie splitting.** `split-duration` on a syncopated two-bar 4/4 rhythm and a two-bar 6/8 rhythm, drawn on the 1/16 grid as given and as tied (4/4 1/2 at 1/8 → 1/8 + 1/4 + 1/8; 6/8 1/2 at 0 → 3/8 + 1/8; a quarter across beat 2 of 6/8 → two eighths). Then `max-dots`, barlines, a pickup, 12/8 and 2+3/8, and a sweep over every 1/16 onset and every duration up to 2 in eight meters (3904 notes): the pieces add up, each is one value with at most one dot, none crosses a barline, and nothing inside a piece is stronger than its start. A tuplet length signals `music-theory-error`.
11. **Tempo and phrase windows.** `make-tempo`, `duration-seconds` and `seconds-duration` at ♩ = 72 (a quarter is 5/6 s, three 4/4 bars 10 s) and for 6/8 with a dotted-quarter beat, exact round trips over 320 durations and tempi, and `max-window-duration`: with a breath at least every 8 seconds at ♩ = 96, a phrase on an eighth-note grid can last 25/8 (three bars and one eighth, 7.8125 s). A table gives the same window at ♩ = 60 to 144.

## Rough edges found

Filed on [egao1980/music-theory](https://github.com/egao1980/music-theory/issues):

- `parse-pitch "Cbbb4"` (also `"C♯♭4"` and `"Cx#4"`) gives the reason "missing octave", although the octave is there and the extra accidental is what is wrong. Section 6 shows it. ([#4](https://github.com/egao1980/music-theory/issues/4))
- In an additive meter, a beat group of five (or 7, 9, …) eighths is halved at 5/16, so its eighths weigh 9, off every grid, and `split-duration` rejects a plain eighth in `3+5/8` with "not notatable without a tuplet". The end of section 10 shows it. ([#7](https://github.com/egao1980/music-theory/issues/7))
