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

| Command                       | Where                     | Meaning                                       |
| ---                           | ---                       | ----                                          |
| `jbuilder build lt4la.a`      | `src`                     | Build the library (everything inside `src`).  |
| `jbuilder utop`               | `src`                     | As above + launches UTop with library loaded. |
| `jbuilder build test.exe`     | `test`                    | Build library & tests.                        |
| `jbuilder runtest`            | top, `test`               | Build library & tests _and_ run all tests.    |
| `jbuilder build repl.exe`     | `bin`                     | Build library & REPL.                         |
| `jbuilder clean`              | top, `src`, `test`, `bin` | Delete `_build` directory of build artifacts. |
| `_build/default/bin/repl.exe` | top                       | Launch the REPL.                              |

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

Almost all types can be printed with `printf !"%{sexp: <type>}" <value>"`. You can simply write
`[%expect {||}]` when first writing the test, run `jbuilder runtest` then 

```
cp <path-to-repo>/_build/default/test/test.ml.corrected <path-to-repo>/test/test.ml
```

to update the test file with the _if_ it is correct/what you expect.

### Continuous Integration

Just a Travis-CI system building the Dockerfile (which includes running tests).

Triggered with every push to the `master` branch.

### Roadmap (in _rough_ order of priority)

**In progress:**
 - Parser/REPL
 - Code generation
 - Scalars and arithmetic expressions

**Future:**
 - Array sizes in types.
 - Tough: Elaboration/inference.
 - Tough: staging, preferably using [ppx_stage](https://github.com/stedolan/ppx_stage?files=1)
 - ?Documentation with [Read the Docs](https://readthedocs.org/)
 - ?Performance: benchmark, less pure-functional implementations behind `State_or_error` or `Check_monad`.

**Done:**
 - Fixed `jbuilder utop` crash
 - Semantics written in Ott
 - Well-formed types
 - Level 1 BLAS Primitives

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
 - Pinned gls to version 1.20.0 (`opam pin gsl 1.20.0`)
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

