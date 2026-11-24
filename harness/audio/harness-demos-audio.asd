(defsystem "harness-demos-audio"
  :version "0.1.0"
  :description "Demo of audio-protocol 0.1.0 (slice AU0, batch audio): a C-major triad built in code as one sine buffer per voice, mixed with per-voice gain and pan, written to a 16-bit WAV and read back (header fields, a sample checksum, a byte-for-byte round trip), resampled from 44.1 kHz to 16 kHz, a hand-built SMF (midi-protocol 0.1.0) rendered by the reference engine to a mix and to stems, and the conditions and restarts: a truncated WAV with USE-PARTIAL-DATA, a non-finite float32 sample on read with USE-VALUE, and a non-finite sample on write as AUDIO-ENCODE-ERROR"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "audio-protocol" "0.1.0")
               (:version "midi-protocol" "0.1.0")
               (:version "binary-protocol" "0.1.1")
               ;; SHA-256 of the files written, as pdf-render does
               (:version "crypto-protocol" "0.2.0")
               (:version "crypto-backend-ironclad" "0.2.2")
               (:version "encoding-protocol" "0.3.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
