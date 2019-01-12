# NumLin: Linear Types for Linear Algebra
---
## What is NumLin? 

NumLin is a small, experimental programming language I wrote (under the guidance of Stephen
Dolan and Neel Krishnaswami) to explore the feasiblity and applicability of using linear
types and fractional-permissions to model the APIs of low-level linear algebra libraries
such as CBLAS/LAPACKE. It compiles to OCaml and this allows using optimized NumLin alongside
existing code, as well as integration with tools like Merlin.

Any issues or errors, please let me know! [![Build Status](https://travis-ci.com/dc-mak/lt4la.svg?token=gu5ZosGxNsQr8WuxN7by&branch=master)](https://travis-ci.com/dc-mak/lt4la)

## Try It! Installation

The easy way is to 
  1. [Install Docker](https://docs.docker.com/engine/installation)
  2. Clone the repo `git clone https://github.com/dc-mak/lt4la.git`
  3. `[sudo] docker build -t <tag> <path-to-repo>`

Once it is built type:
```
[sudo] docker run -it <tag>
```
to fire up an interactive shell. There, you can launch `utop` and interact with LT4LA.

The difficult way is to basically execute the Dockerfile (or equivalent for your platform)
by hand. Note, that I'm working from an Ubuntu 16.04 with Opam 2 and OCaml 4.06.0 base image,
so you'll need that (and all its dependencies) first (see [Owl's
Dockerfile](https://github.com/ryanrhymes/owl/blob/master/Dockerfile) for
details).

**Note** that `[sudo] docker build -t <tag> <path-to-repo>` _copies_ the files in the current
directory into the image, so you **must** rebuild if you change the source, not just restart the
container. Luckily, the image is cached so only the project stuff will be rebuilt, not everything.

## Quickstart

| Command                                         | Meaning                                       |
| ---                                             | ----                                          |
| `dune build src/lt4la.a`                        | Build the library (everything inside `src`).  |
| `pushd src && dune utop && popd`                | As above + launches UTop with library loaded. |
| `dune build test/test.exe`                      | Build library & tests.                        |
| `dune runtest`                                  | Build library & tests _and_ run all tests.    |
| `dune build bin/{repl,benchmark,transpile}.exe` | Build library, REPL and transpiler.           |
| `dune clean`                                    | Delete `_build` directory of build artifacts. |
| `_build/default/bin/*.exe`                      | Launch {repl,transpile,benchmark}.exe         |

### Roadmap (in _rough_ order of priority)

**Future:**
 - ?Documentation with [MkDocs](http://www.mkdocs.org/)
 - ?Performance: less pure-functional implementations behind `State_or_error` or `Check_monad`.
 - Combinators interface
 - PPX extension
 - Staging, preferably using [ppx_stage](https://github.com/stedolan/ppx_stage?files=1)
 - Size types

**Done:**
 - Fixed `dune utop` crash
 - Semantics written in Ott
 - Well-formed types
 - (Some) Owl/Level 1 BLAS Primitives
 - Parser/REPL
 - Code generation
 - Scalars and arithmetic expressions
 - Recursion, conditionals and !-types
 - Elaboration/inference
 - Matrices (some Level 3 BLAS/LAPACK primitives)
 - Syntactic sugar/parsing grammar
 - Matrix expression pattern matching
 - Benchmarking

## Development

[Code of conduct is here.](https://github.com/dc-mak/lt4la/blob/master/CODE_OF_CONDUCT.md) 
Overview of the project structure is in the table below.

| Directory  | Purpose                                      |
| ---        | ---                                          |
| `src`      | Library being developed.                     |
| `test`     | Tests for the library.                       |
| `bin`      | For executables, like the REPL.              |
| `docs`     | Documentation.                               |
| `write-up` | My dissertation on all of this.              |

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
     or an `open Lt4la.Template` statement.
     - `template.ml[i]` is a full implementation of LT4LA's DSL's primitives in OCaml.
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

### Formatting conventions
I'm sticking to to following conventions (except for `.mli` files and small modules)
 - Max 100 characters line-width
 - Jane Street preset for my Ocp-indent
 - At the top-level: 1 line for binding, 1 or more lines for body/expression, 1
   line for `;;` (makes for cleaner diffs)
 - Nested match statements: `begin match ...` on top and solo `end` at the bottom.

### Developing with a Container

I'm not very well-versed with Docker, but I think the first step is to _mount_
the source directory inside Docker to have mutable access to the directory.
Then, to get your tools you can either
 - append to the Dockerfile get install all your development gear inside it
 - OR create a new image based on the existing one for development
 - OR use existing tools across the container
 
I think the last is covered here, in [how to run Merlin from outside of a
container](https://gist.github.com/pbiggar/cce9b958704c6cadd9597b717bc18c4d).
See Myths 10-7 [in this
article](https://derickbailey.com/2017/01/30/10-myths-about-docker-that-stop-developers-cold/)
for more information.

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

Combinators is the starting point of the user-visible interface to this library.

### Tests

**Please write tricky tests for the checker in `test/checker_test.ml`.**

There are tests in each module in `src` for white-box/unit testing modules from
the inside and tests in `test` for black-box/interface/integration testing
components used together.  Writing a test should be straightforward enough from
the code already there. **Let me know if it isn't and I will update this
README.** As an overview, from
[ppx_inline_test](https://github.com/janestreet/ppx_inline_test/blob/master/README.md):

```ocaml
let%test "name" = <boolean expr> (* true means ok, false or exn means broken *)
let%test_unit "name" = <unit expr> (* () means ok, exn means broken *)
let%test_module "name" = (module <module-expr>)  (* to group tests/share setup *)
```

and from [ppx_expect](https://github.com/janestreet/ppx_expect):

```ocaml
let%expect_test "addition" =
  printf "%d" (1 + 2);
  [%expect {| 3 |}]
```
Almost all types can be printed with `printf !"%{sexp: <type>}" <value>"`. You
can simply write `[%expect {||}]` when first writing the test, run `dune
runtest` then `dune promote` to update the test file with the _if_ it is
correct/what you expect.

### Benchmarking

Run `dune build bin/benchmark.exe`.

Something `_build/default/bin/benchmark.exe --start 1 --limit 4 --alg kalman
--micro-quota 10 --macro-runs 10` maybe a good place to get started. It will
take at least 150s for just the micro-benchmarks, and more for the
macro-benchmarks. If you have time `--micro-quota 20` I've found to be more
than enough; you should increase `--macro-runs` carefully. Please do check that
R^2, when given, is usally 1 or very close (0.95 or above), otherwise it means
something not quite right with this set of measurements/regressions performed
by Core_bench.

For profiling, I advise `--no-analyse` to skip data analysis/printing and just
run the algorithm.  Something like `--alg none --micro-quota 1 --macro-runs 1`
for equal `--start` and `--limit` will help in getting a baseline without
running any of the tests themselves.

Full usage is given by `-help`.  The benchmarks loads (or generates if they
don't exist) matrices for a size depending on `--start` and `--limit`. For n=5,
k=3, matrix sizes grow exponentially from n^start to n^limit (inclusive) with k
= 3n^(i-1).

For small exponents (i <= 3), Jane Street's Core_bench runs the benchmark for
`--micro-quota` seconds each (so for `--alg all` and `--micro-quota 10` it will
take 10 * 5 algorithms = 50 s + data processing time) to run.

For bigger exponents, it's just a bunch of repeated iterations, the number of
which is specified by `--macro-runs`. Be aware that for limits more than 4,
this can be pretty slow.

There are (currently) 4 implementations of a Kalman filter: Python/NumPy,
OCaml/Owl, OCaml/NumLin, C/CBLAS-LAPACKE. There are also 3 implementation each of
L1-norm minimisation and "linear regression" in Python/NumPy, OCaml/Owl, and OCaml/NumLin.

### Continuous Integration

Just a Travis-CI system building the Dockerfile (which includes running tests).

Triggered with every push to the `master` branch.

### `.mli` Files
There is currently no straightforward way to "generate" `.mli` files from an `.ml`
file. There are [plans to update Merlin](https://github.com/ocaml/merlin/issues/538) with this
feature. So, in the meantime, say you are in directory `src` and you want to generate `ast.mli`.
 - `dune clean`
 - `dune build lt4la.a --verbose`
 - Find command ending in `-impl ast.pp.ml` and copy it
 - Replace `-impl` with `-i`
 - At the end of it all, append ` > /path/to/file/ast.mli`.

## History

This project started off life as "LT4LA", a Part III project for my M.Eng. degree in
Computer Science from Trinity College, University of Cambridge.

Like many things in my life, I only got the implementation "right" on the second try,
so for posterity, the first implementation is kept in the `old` directory if anyone
wishes to see even humbler origins of this code base.
