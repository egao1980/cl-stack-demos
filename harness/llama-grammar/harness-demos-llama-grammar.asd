(defsystem "harness-demos-llama-grammar"
  :version "0.1.0"
  :description "Demo of llama-cpp 0.2.1 (llama-cpp#17, #18, #20): grammar-constrained COMPLETE returns a sentence of the GBNF grammar instead of aborting the image (each token accepted once, EOS once the grammar is complete), PARSE-GRAMMAR reuse, streaming, temperature sampling, a :max-tokens cut, a C++ exception from llama.cpp as LLAMA-ERROR :RUNTIME, bad GBNF as :INVALID, and exiting with a live engine keeping its exit code (FREE-LIVE-HANDLES from the exit hooks)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "llama-cpp" "0.2.1"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
