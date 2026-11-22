# fib — sample project of the ACP demo

`fib.lisp` has an off-by-one: `(loop repeat (1- n) …)` runs one step too few,
so `(fib 1)` is 0 and `(fib 10)` is 34. `sbcl --script test.lisp` shows it.
The demo agent (`../agent-main.lisp`) fixes it through ACP: it reads the file
with `fs/read_text_file`, proposes the diff, asks permission, writes it with
`fs/write_text_file` and runs the tests in a `terminal/*` terminal.
This copy stays buggy; demo runs work on a copy.
