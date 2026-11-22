`stats.lisp` in the current directory defines `mean`, which should return the arithmetic mean of a non-empty list of numbers, for example `(mean '(1 2 3 4))` → `5/2`. It returns the wrong value.

Fix `mean` in `stats.lisp`. The check is `sbcl --script` on a test file that loads `stats.lisp` and calls `mean`; keep the function's name and argument list.
