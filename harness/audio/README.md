# Audio demo: PCM buffers, mixing, WAV, resampling, MIDI rendering

`harness-demos-audio` is a tour of **audio-protocol 0.1.0** (slice AU0, batch audio) on **midi-protocol 0.1.0**, both from GHCR. Everything starts from code: a C-major triad of sine buffers, mixed and written to WAV, and a small Standard MIDI File rendered by the protocol's reference engine. No audio asset is read, and there is no device or real-time I/O (audio-protocol has none).

Every number comes from the run, and every result with a known answer is checked (75 checks). The mix is recomputed from `src/mix.lisp`'s formula and compared sample for sample, the WAV header is unpacked with binary-protocol and compared field by field, the round trip is compared byte for byte, and the render's peaks are compared with the reference engine's `A * v / 127`. `run-demos` returns `NIL` if a check fails or a section stops on an error. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, with home paths replaced by `~`.

## Run

```bash
cd harness/audio
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-audio")' \
  --eval '(harness-demos-audio:run-demos)' > demo-output.txt 2>&1
```

It needs SBCL and `cl-repo`. It has no native library, model, network or git step, and takes about five seconds once the dependencies are compiled. The SHA-256 sums come from crypto-protocol 0.2.0 on crypto-backend-ironclad 0.2.2, as in [`../pdf-render/`](../pdf-render/).

## What each section shows

1. **Three voices, one sine buffer each.** C4, E4 and G4 (MIDI 60, 64, 67) at `note-frequency` (261.6256, 329.6276, 391.9954 Hz), 1.5 s at 44.1 kHz (66,150 frames, `pcm-buffer-duration` 3/2), amplitude 0.5, with a 20 ms linear fade in and out. Each is a mono `pcm-buffer` made with `make-pcm-buffer :samples`.
2. **`mix` with per-voice gain and pan.** Gains −3, −6 and −6 dB, pans −0.6, 0 and 0.6. `pan-gains` for a mono input follows the constant-power sine law: left² + right² = 1 for each voice, 0.70711 (−3.01 dB) per side at the centre, and exactly (1 0) at hard left. The stereo mix peaks at 0.59054 left and 0.52420 right, so `:clip` clamps nothing. All 132,300 samples equal the file header's formula, worked out independently in the demo. Muting E4 and G4 gives the same buffer as soloing C4.
3. **A 16-bit WAV.** `write-wav … :encoding :pcm16` gives 264,644 bytes. The 44-byte header is unpacked with binary-protocol and printed field by field (RIFF size 264,636, PCM tag 1, 2 channels, 44,100 Hz, 176,400 bytes/s, block align 4, 16 bits, data 264,600). The sample checksum is the code count, the sum of the codes, the sum of their magnitudes, min and max, and the SHA-256 of the data chunk and of the whole file. `read-wav` gives back a buffer whose every sample is an exact code / 32768, with the file's codes, and at most half a code (1.526e-5) from the float mix. Writing that buffer again gives the same bytes, and reading those gives an `equalp` buffer. The file on disk holds the same bytes as the vector. As float32 the mix reads back `equalp`.
4. **`resample`, 44.1 kHz to 16 kHz.** The triad goes from 66,150 to 24,000 frames (still 3/2 s). The peaks go from 0.59055 / 0.52420 to 0.59014 / 0.52373, and the resample takes about 0.03 s. Then 0.5 s tones show the kernel. 1 kHz and 5 kHz pass unchanged (0.00 dB). 7 kHz is in the transition band (−0.77 dB). 10 kHz and 15 kHz are attenuated by 79 dB and 120 dB, past the 60 dB that `src/resample.lisp` promises.
5. **Rendering an SMF.** A type-1 SMF built with midi-protocol has a conductor track (120 bpm, 4/4), a melody (C4 E4 G4 C5 in quarters, velocity 100, channel 1) and a bass (C3 G2 in halves, velocity 80, channel 2). It is 155 bytes. `smf-duration` is exactly 2 s, and `render-frames` at 44.1 kHz with the default 1 s tail is 132,300. `reference-engine` (sine, amplitude 1/4):
   - `render-midi` from the bytes gives the same buffer as from the `smf` object: 3 s long, sounding until 2 s, peak 0.35432.
   - `render-stems` gives "Melody" and "Bass" (tracks 1 and 2), each 3 s long and sounding until 2 s. Their peaks, 0.19685 and 0.15748, are `A * v / 127`.
   - The stems mixed with `:clip :none` match the whole render within 2.98e-8.
   - A rehearsal mix puts the bass at −12 dB.
   - `:by :channel` at 16 kHz mono gives "Channel 1" and "Channel 2", 48,000 frames each.
   - With `*max-render-seconds*` set to 2, rendering signals `audio-limit-exceeded` (limit 2, value 3), and `continue` renders anyway.
6. **Conditions and restarts.**
   - **a. A truncated WAV.** The file is cut after 1,000 whole frames plus one byte. `read-wav` signals `audio-format-error` (a `parse-error`) at byte 36, the data chunk, with "the data chunk declares 264600 bytes and 4001 are there". The `use-partial-data` restart, taken from `handler-bind`, returns the 1,000 frames, which equal the start of the full read. A `wav-format-warning` "partial frame" at byte 4044 is muffled along the way. Outside a handler, the `use-partial-data` function returns `NIL`.
   - **b. A non-finite sample on write.** A +infinity at sample 37 and a NaN at sample 151 (both decoded by binary-protocol, so no float trap) each make `write-wav` signal `audio-encode-error` with that index, as `:pcm16` and as `:float32`. The whole buffer is checked before any byte is written, so no file is created. The error has no restart: there is no sample to write in place of the bad one.
   - **c. A non-finite sample on read.** A float32 file has +infinity in sample 2. `read-wav` signals `audio-format-error` at that sample's first byte (66), and `use-value 0` replaces it.
7. **The files written.** `triad.wav`, `triad-16k.wav` and `arpeggio.wav` come to 889,932 bytes together. That is over the 200 KB limit for committed WAV files, so they are written to a temporary directory, removed at the end, and printed with their sizes and SHA-256 sums. The renders are deterministic, so the sums are the same on every run of this version.

## Issues

None. audio-protocol 0.1.0 matched every check here.
