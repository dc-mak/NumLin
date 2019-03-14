# Questions:

## Questions common to multiple reviews

* Reviews A and C both ask how we got the NumLin types for the
  BLAS/LAPACK routines.
   
The documentation records the expected aliasing behaviour of each of
these routines, and whether or not the routine modifies or consumes
its argument in any way. We used that to derive the types we ascribed
to the low-level routines. Since most of these low-level routines are
very careful not to do any allocation themselves, it was generally
very easy to give them a NumLin type -- every argument that can
modify/consume its argument needs a full permission, and all others
can be fraction-polymorphic. Taking Fortran as an example, it has
a notion of `in`, `out` and `inout` parameters. The latter two would
need full permissions, the first would be fraction-polymorphic.

## Reviewer-specific questions

### Review A

* The exact research contribution of this paper is, however, unclear
  to me. Linear types as well as fractional permissions have been used
  before.  Is the contibution that these two type systems are applied
  together, or that they are applied to a new domain? If it is the
  latter, it would be helpful if the paper could point out differences
  to previous type systems, or challenges, if there are any.

We certainly agree that both linear types and fractional permissions
have been studied before. To our surprise, it did not appear that
linear types had been much applied to checking linear algebra
libraries. It was only when we tried to use it, that we realized that
we needed fractional permissions. 

In our view, this explains why it hadn't been used before: many prior
type systems for fractional permissions (eg, from Boyland's original
work to Aldrich and his students' work on Plural) are extremely
complex. This is because these type systems typically encode a complex
analysis to automatically infer how fractional permissions should be
split and rejoined.

By requiring sharing and merging to be made explicit, we were able to
drastically simplify the type system. Our formal system is very close
to standard presentations of linear logic, and the implementation
complexity is no worse than HM type inference.

* Finally, the related work focuses almost exclusively on type
  systems. Since ultimately the goal is to provide (memory) safe and
  fast linear algebra, I think it would be nice to also mention
  non-type based efforts, e.g. Spiral in Scala which compiles to
  efficient code [1], even though it does not use BLAS/LAPACK.  [1]
  Spiral in Scala: Towards the Systematic Construction of Generators
  for Performance Libraries. GPCE '13

We're happy to discuss this in the related work. However, NumLin is
more likely to be a consumer of efforts like Spiral than a competitor:
we want to check whether client code conforms to the aliasing rules
that low-level libraries (whether from platform libraries like BLAS or
generated via systems like Spiral, Cl1ck, FLAME) want clients to adhere to.


### Review B 

* One question I have is why OCaml code is generated? If the program
  only uses higher-order functions in certain ways, would it be
  possible to generate C code?

There's no technical obstacle to generating C code, but as we had
local access to the developer of the Owl numerical programming library
for OCaml, it made sense to use OCaml. Eventually we'd like to extend
Owl with something like NumLin to offer safe access to the low-level
platform numerics libraries.

* Also, could the aliasing requirements be checked dynamically? I.e.,
  provide the OCaml interface in Appendix G, but with dynamic aliasing
  checks? What overhead would that have?

It might be possible, but we're not sure how. Our system enforces full
linearity (to support explicit deallocation), and to our knowledge
contract systems for linearity checking (eg, Tov and Pucella's ESOP
2010 "Stateful Contracts for Affine Types") only enforce affine usage
(ie, a dynamic check can enforce at-most-one, but not exactly-once).

* I was confused by the desugaring `let !x = e1 in e2` => `let Many x
  = e1 in let Many x = Many (Many x) in e2`. Can you explain this
  more?

There are two aspects to answering this, one is the type-based justification
and the second is *why* that's even necessary. Starting with why, the primary
motivation for having this was for binary arithmetic operations (of type
`!int * !int --o !int`). For the sake of ergonomics, we'd like to be able to write
code like "let x = 5 * 5 in x * x"; this is morally right because integers and
their operations are !-types but this is non-linear in x. Intuitively, we wish
for a rule like the following to "just do the right thing" when
destructuring/evaluating expressions of a !-type; which we can derive as a
meta-rule using the desugaring in question.

```
Int; Lin |- e : !t 
Int, x : !t; Lin' |- e' : t'
 ---------------------------------------
Int; Lin, Lin' |- let !x = e in e' : t'
```

* I was initially confused by the presence (in Figure 11) of the
  seemling useless wrappers around banged values. Is this to make the
  API of the generated OCaml code easier to use correctly?

Yes, this is correct, we essentially embed the NumLin type system into the
OCaml one for a sanity check on the output code, ease of interop with OCaml and
tooling such as Merlin support.  Annotating with "[@@unboxed]" means OCaml's
underlying value representation does not allocate anything for unary datatypes,
so there is no performance penalty for using them like Haskell-style newtypes.

* The statement to line 302 talks about moving values from the linear
  environment to the intuitionistic one. But only variables live in
  environments?

Yes, that's correct, this is a typo, we meant to write "moves the variable
referring to that value from the linear environment to the intuitionistic one".

* I was confused by the comment on Linear Haskell starting on line
  610. What relevance does a system based on kinds have to NumLin
  (which isn't based on kinds either)?

We were trying to explain that (a) it seems like we can use GADTs with
natural-number indices to encode fractions a la NumLin in Linear
Haskell, but (b) we don't see how to support linear values (since
Linear Haskell only supports a linear arrow). We're happy to tighten
up the discussion in the final version. 


### Review C 

* Compared to the learning curve of Rust (which the authors cite) what
  is the learning curve of NumLin, in the authors' estimation?

We conjecture that the learning curve of NumLin is less steep than that of
Rust, for two reaons: Rust supports more than just linear types and is smarter
about inferring the appropriate ownership. We suspect the explicit nature of
NumLin makes it easier to pick up and sets up an easier transition to Rust's
more complex system. We will add a mention of Weiss et. al. (2018/19) which
proposes a fraction-based framework to understand/model Rust's borrow checker.

* Could the authors sketch the evolution of linear types from its
  initial classical introduction (say in Wadler's "Linear types can
  change the world" paper) to its current sophisticated form? Is this
  really an "off the shelf" linear type system?

We'll add this historical discussion to the final paper. Our system is
indeed fairly close to off-the-shelf, and as we mentioned to reviewer
A, it was one of the surprising discoveries of this paper that this
works.

* Can they explain their soundness theorem once in English?

We'll add an explanation of the following terse explanation to the final paper.
If an expression is syntactically type-checked, then for an arbitrary number of
steps (k), under *any* substitution of free fractional-permission variables,
linear variables (with a suitable heap) and intuitionistic variables, that
suitable heap and substituted expression are in the computational
interpretation (explained in English in the paper) of the (substituted) type of
that expression.

* My biggest comment is concerning concurrency. These sort of routines
  will be used in a setting where multicore concurrency will be
  employed to perform concurrent updates on arrays. In that setting,
  one might employ separation logic to deal with safe concurrent
  accesses. Now, separation logic also employs fractional
  permissions. Would NumLin be usable in that setting? Would the use
  of Separation Logic and NumLin (Linear Types) be orthogonal? Or do
  the authors feel the need to work out a concurrency extension of
  NumLin?

NumLin's type system enforces a concurrent-reader/exclusive-writer
pattern through the use of fractions, and our heap model was
deliberately chosen to be quite similar to the work on fractions from
separation logic. As a result, we would expect that proving that it
works in the presence of concurrency would be relatively
straightforward -- though without doing the work we don't want to say
that it would definitely work.

That said, developing a variant of separation logic suitable for
NumLin would be interesting, because it would let us prove things
about (eg) array bounds without having to throw them into the type
system.
