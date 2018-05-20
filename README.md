# LT4LA: Linear Types for Linear Algebra
---

Any issues or errors, please let me know! [![Build Status](https://travis-ci.com/dc-mak/lt4la.svg?token=gu5ZosGxNsQr8WuxN7by&branch=master)](https://travis-ci.com/dc-mak/lt4la)

## Installation

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

| Command                                   | Meaning                                       |
| ---                                       | ----                                          |
| `jbuilder build src/lt4la.a`              | Build the library (everything inside `src`).  |
| `pushd src && jbuilder utop && popd`      | As above + launches UTop with library loaded. |
| `jbuilder build test/test.exe`            | Build library & tests.                        |
| `jbuilder runtest`                        | Build library & tests _and_ run all tests.    |
| `jbuilder build bin/{repl,transpile}.exe` | Build library, REPL and transpiler.           |
| `jbuilder clean`                          | Delete `_build` directory of build artifacts. |
| `_build/default/bin/*.exe`                | Launch {repl,transpile}.exe                   |

### Roadmap (in _rough_ order of priority)

**Future:**
 - ?Documentation with [MkDocs](http://www.mkdocs.org/)
 - ?Performance: less pure-functional implementations behind `State_or_error` or `Check_monad`.
 - Combinators interface
 - PPX extension
 - Staging, preferably using [ppx_stage](https://github.com/stedolan/ppx_stage?files=1)
 - Size types

**Done:**
 - Fixed `jbuilder utop` crash
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
can simply write `[%expect {||}]` when first writing the test, run `jbuilder
runtest` then `jbuilder promote` to update the test file with the _if_ it is
correct/what you expect.

### Continuous Integration

Just a Travis-CI system building the Dockerfile (which includes running tests).

Triggered with every push to the `master` branch.

### `.mli` Files
There is currently no straightforward way to "generate" `.mli` files from an `.ml`
file. There are [plans to update Merlin](https://github.com/ocaml/merlin/issues/538) with this
feature. So, in the meantime, say you are in directory `src` and you want to generate `ast.mli`.
 - `jbuilder clean`
 - `jbuilder build lt4la.a --verbose`
 - Find command ending in `-impl ast.pp.ml` and copy it
 - Replace `-impl` with `-i`
 - At the end of it all, append ` > /path/to/file/ast.mli`.

## History

First, I installed [Owl](https://github.com/ryanrhymes/owl) on a fresh
ElementaryOS/Ubunutu 16.04 virtual machine (after setting up NeoVim, TMux, SSH,
Zsh, Git, Tig). To do so, I followed [Owl's
Dockerfile](https://hub.docker.com/r/ryanrhymes/owl/~/dockerfile) up until
building eigen. My attempt to install eigen failed (something to do with
ocamlbuild cmi version for too new a version of OCaml, I tried reinstalling the
switch/ocamlbuild but to no result), so:
 - I skipped over it
 - Pinned gsl to version 1.20.0 (`opam pin gsl 1.20.0`)
 - Installed eigen from opam
 - Continued with the rest of the Dockerfile instructions.

I can't tell if [this Wiki page recommending
Cairo](https://github.com/ryanrhymes/owl/wiki/Tutorial:-How-to-Plot-in-Owl%3F)
is important, so I didn't bother.

After a while, I ran into [problems with
ppx\_deriving](https://github.com/ocaml-ppx/ppx_deriving/issues/153) and
[ppx\_type\_conv](https://discuss.ocaml.org/t/ppx-deriving-ppx-type-conv-and-jbuilder-things-should-be-better-now/1212)
and the main point is that I upgraded to 4.06 so I could get [ppx\_type\_conv
v0.9.1](https://github.com/ocaml/opam-repository/pull/10885).

### Eigen Build Error
```
+ ocamlfind ocamlopt unix.cmxa -I /home/dhruv/.opam/4.04.0/lib/ocamlbuild /home/dhruv/.opam/4.04.0/lib/ocamlbuild/ocamlbuildlib.cmxa -linkpkg myocamlbuild.ml /home/dhruv/.opam/4.04.0/lib/ocamlbuild/ocamlbuild.cmx -o myocamlbuild
File "myocamlbuild.ml", line 1:
Error: /home/dhruv/.opam/4.04.0/lib/ocamlbuild/ocamlbuild_plugin.cmi
is not a compiled interface for this version of OCaml.
It seems to be for a newer version of OCaml.
Command exited with code 2.
Compilation unsuccessful after building 1 target (0 cached) in 00:00:00.
E: Failure("Command ''/home/dhruv/.opam/4.04.0/bin/ocamlbuild' lib/libeigen_stubs.a lib/dlleigen_stubs.so lib/eigen.cma lib/eigen.cmxa lib/eigen.a lib/eigen.cmxs -use-ocamlfind -tag debug' terminated with error code 10")
Makefile:2: recipe for target 'all' failed
make: *** [all] Error 1
```

