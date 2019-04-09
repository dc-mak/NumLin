# NumLin: Linear Types for Linear Algebra

## Exploring

1. Login (password: osboxes.org), open up a terminal (Activites > Terminal),
   `cd NumLin`.

2. Run the tests: `dune runtest`. Not much should happen because all the tests
   should pass.  If you wish to see a test fail, then you may change/delete any
   text inside a `[%expect {| .. |}]` block and then `dune runtest` will show
   you the difference between the expected output and the actual output.

3. Run the benchmarks: `_build/default/bin/benchmark.exe --start 1 --limit 5
   --alg lin_reg --micro-quota 10 --macro-runs 1`. You may also provide the
   `-help` flag for details about the options.
   * `--start` and `--limit` control the size of square matrices from
     *5 &times; 5* to *5<sup>5</sup> &times; 5<sup>5</sup>*.
   * `--alg` chooses the example to run (kalman, l1_norm_min, lin_reg).
   * `--micro-quota` controls how many *seconds* smaller benchmarks (limit
     &le 3) are run for.
   * `--macro-runs` controls *N*, how many measurements are made for each
     implement.

4. Run the repl: `_build/default/bin/repl.exe`. Type in a NumLin expression,
   terminated by `;;` to see its AST and translation to OCaml. Ctrl-D to exit
   the application.

5. Run the transpiler: `_build/default/bin/transpile.exe -i <input-file> -o
   <output-file>`.  Transpile a file containing a NumLin expression, terminated
   by `;;`, into an OCaml file/module.

6. Explore the code itself: `pushd src && dune utop && popd`. This will open up
   an OCaml REPL with the library in `src` loaded in under the module name
   `Numlin`.

## Command Reference

| Command                    | Meaning                                       |
| ---                        | ----                                          |
| `dune build src/numlin.a`  | Build the library (everything inside `src`).  |
| `cd src && dune utop`      | As above + launches UTop with library loaded. |
| `dune build test/test.exe` | Build library & tests.                        |
| `dune runtest`             | Build library & tests _and_ run all tests.    |
| `dune build bin/*.exe`     | Build {repl,benchmark,transpile}.             |
| `dune clean`               | Delete `_build` directory of build artifacts. |
| `_build/default/bin/*.exe` | Launch {repl,transpile,benchmark}.exe         |

## Structure

| Directory  | Purpose                          |
| ---        | ---                              |
| `src`      | Library being developed.         |
| `test`     | Tests for the library.           |
| `bin`      | For executables, like the REPL.  |
| `old`      | First attempt at implementation. |
| `write-up` | My dissertation on all of this.  |

To understand this project, consider what happens when you use the REPL:

  1. An input string is taken and fed to `Eval.eval`.

  2. `bin/eval.ml` in turn uses `src/parse.ml`: this file needs four things to
     run a parser over an input: a lexical-token buffer, a way to handle a
     successful parse, a way to handle an error, and optionally, a way to
     handle a request for a new lexical-buffer. How these are implemented in
     `bin/eval.ml` is not relevant for the big-picture.

  3. `src/parse.ml` uses `lexer.mll` and `parser.mly` to drive an incremental
     parser.  Making it incremental allows for using `parser.messages` and
     `error_msg.ml` for better error-messages. Upon success, `src/parse.ml`
     returns a value of type `Ast.exp`, upon failure, a position, to be used by
     handle in showing a useful error-message.

  4. `src/ast.ml` defines fractional-capabilities, linear types and expressions,
     as well as pretty-printing code-generation. The code is generated on the
     assumption that it comes after either the contents of the file `template.ml`
     or an `open Numlin.Template` statement.
     - `template.ml[i]` is a full implementation of NumLin's primitives in OCaml.

  5. Given a value of type `Ast.exp`, the `accept` function in `bin/eval.ml`
     passes it to `check_expr` in `src/checker.ml`.

  6. `src/checker.ml` checks types, linearity and scoping. It uses operations
     defined in `src/check_monad.ml` to implement typing rules.
     - `check_monad.ml[i]` hides implementation details of functions used in
       the type-checker as well as constraining how those functions are used. For
       example, `wf_lin` is how the type-checker ensures fractional-capabilities
       and linear-types are "well-formed" with respect to the environment.
       Similarly, `not_used` is a proof that a variable is not used: it is
       returned only by `lookup` and accepted only by `use_var`: you cannot
       extract the type of a linear variable unless you mark it as used first.

  7. Regardless of whether checking is successful or not, `accept` in
     `bin/eval.ml` does two things (1) output OCaml code representing a
     translation of the expression entered (2) output the full AST in
     s-expression form.
     If the checking is successful, then the type is output, otherwise an error.

  8. Similarly, `bin/transpile.ml` reads an entire file as input rather than an
     interactive, line-by-line entry. It acts as a thin-wrapper around
     `src/transpile.ml` which can take either `in_channel`/`out_channel` pairs
     or file names to translate DSL expressions to OCaml.

### Library

At its core, it's just an Abstract Syntax Tree and a Checker. I use
[ppx_let](https://github.com/janestreet/ppx_let) and try and keep everything
pure functional, returning `'a Or_error.t` for informative error messages.

`State_or_error` is a Error-monad with state as the result-type. `Check_monad`
uses it to provide very constrained "mutation" interface to the checker, and
attempts to use OCaml's type system to prevent invalid use of the interfaces
(see `Check_monad.use_var`).

`src/ast.ml[i]` also contains the pretty-printing/code-generation for the AST.
They assume/use the functions provided by `src/template.ml`.

Parsing and lexing are available for convenience in writing expressions (see
the REPL for details).

### Benchmarking

Something like `_build/default/bin/benchmark.exe --start 1 --limit 4 --alg kalman
--micro-quota 10 --macro-runs 10` should be a good place to get started. It will
take at least *150s* for just the micro-benchmarks, and more for the
macro-benchmarks. If you have time `--micro-quota 20` I've found to be more
than enough; you should increase `--macro-runs` carefully. Please do check that
*R<sup>2</sup>*, when given, is usally 1 or very close (0.95 or above),
otherwise it means something not quite right with this set of
measurements/regressions performed by Core_bench.

For profiling, I advise `--no-analyse` to skip data analysis/printing and just
run the algorithm.  Something like `--alg none --micro-quota 1 --macro-runs 1`
for equal `--start` and `--limit` will help in getting a baseline without
running any of the tests themselves.

Full usage is given by `-help`.  The benchmarks loads (or generates if they
don't exist) matrices for a size depending on `--start` and `--limit`. For *n=5*,
*k=3*, matrix sizes grow exponentially from *n<sup>start</sup>* to
*n<sup>limit</sup>* (inclusive) with *k = 3n<sup>i-1</sup>*.

For small exponents (i &le; 3), Jane Street's Core_bench runs the benchmark
for `--micro-quota` seconds each (so for `--alg all` and `--micro-quota 10` it
will take *10 &times; 5 algorithms = 50 s + data processing time*) to run.

For bigger exponents, it's just a bunch of repeated iterations, the number of
which is specified by `--macro-runs`. Be aware that for limits more than 4,
this can be pretty slow.

There are (currently) 4 implementations of a Kalman filter: Python/NumPy,
OCaml/Owl, OCaml/NumLin, C/CBLAS-LAPACKE. There are also 3 implementation each
of L1-norm minimisation and "linear regression" in Python/NumPy, OCaml/Owl, and
OCaml/NumLin.

