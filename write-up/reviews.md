ECOOP '19 Paper #35 Reviews and Comments
===========================================================================
Paper #35 NumLin: Linear Types for Linear Algebra


Review #35A
===========================================================================

Overall merit
-------------
C. Weak reject: I think this paper should be rejected but I will not argue
   strongly against it

Reviewer expertise
------------------
Y. Knowledgeable

Paper summary
-------------
The paper presents a language and type system called NumLin for writing linear algebra programs which use BLAS/LAPACK routines.  NumLin's type system is based on linear and fractional permission types and ensures correct memory usage and aliasing. The performance achieved is comparable to an unsafe C implementation.

Comments for author
-------------------
The paper targets an important domain; high-performance linear algebra routines are used in many different applications.  However, common libraries such as BLAS or LAPACK are written in low-level languages and require careful memory management. Providing more (memory) safety without sacrificing performance is thus of practical importance.  
The paper demonstrates how to provide this safety using a type system, and provides a prototype solution in the form of a  domain-specific language which compiles down to OCaml. Thus, it provides a practical implementation which is of potential interest to a wide audience.

The exact research contribution of this paper is, however, unclear to me. Linear types as well as fractional permissions have been used before.  Is the contibution that these two type systems are applied together, or that they are applied to a new domain? If it is the latter, it would be helpful if the paper could point out differences to previous type systems, or challenges, if there are any.

My lack of understanding of the contribution is probably also due to the fact that I found the paper hard to read:
- For instance, the exact usage of the language was unclear to me initiatially. The beginning of the paper talks about expressing the APIs of BLAS/LAPACK libraries, but much what follows is about writing programs (using those APIs), but the APIs are hidden. 
- Similarly, the overview of NumLin in section 2 read like a background section, but did not really explain to me how this fits together in NumLin.
- The examples in section 2.2 hinted at different features, but left too many details for later to help understanding. I found the examples were trying to introduce too much at the same time: syntax of the language, syntactic sugar, different BLAS routines which are supported, as well as the type system. For me, it would have been clearer to discuss these (more) individually, and I believe this would make the paper more accessible to a wider audience.

I have an additional question: how were the types of the BLAS/LAPACK routines obtained? Did you just annotate the APIs based on assumptions documented somewhere, or where these APIs also type-checked?

Finally, the related work focuses almost exclusively on type systems. Since ultimately the goal is to provide (memory) safe and fast linear algebra, I think it would be nice to also mention non-type based efforts, e.g. Spiral in Scala which compiles to efficient code [1], even though it does not use BLAS/LAPACK.
[1] Spiral in Scala: Towards the Systematic Construction of Generators for Performance Libraries. GPCE ’13



Review #35B
===========================================================================

Overall merit
-------------
A. Accept: I will champion this paper

Reviewer expertise
------------------
X. Expert

Paper summary
-------------
The APIs of low level linear algebra libraries, like LAPACK and BLAS go to great lengths to allow users full flexibility in how memory to store matricies is allocated, freed, and reused. However, this comes at the cost of potential memory safety bugs as the API does not automatically enforce the (non-)aliasing assumed by the implementations of the routines. High-level wrappers around these APIs ensure memory safety, but at the cost of additional temporary storage and defensive copying.

This paper proposes a "middle way": a high level programming language that directly uses the low-level routines, and uses a linear type system to ensure that the (non-)aliasing constraints required by the low-level routines are satisfied. The system is proved sound using a step-indexed logical relation. An implementation has been constructed that generates OCaml code, and it has been benchmarked against high-level code in Owl (an OCaml library) and NumPy (a Python library). The benchmark results show a convincing speedup in cases where the execution time is not dominated by the low level routines themselves.

Comments for author
-------------------
This paper introduces a non-trivial system to solve a real problem, provides both a proof of soundness and an implementation, and uses that implementation to run a convincing evaluation. I recommend this paper for acceptance.

One question I have is why OCaml code is generated? If the program only uses higher-order functions in certain ways, would it be possible to generate C code?

Also, could the aliasing requirements be checked dynamically? I.e., provide the OCaml interface in Appendix G, but with dynamic aliasing checks? What overhead would that have?

Points in favour of this paper:

  - The type system is introduced in a clear and easy to follow way, with the linear aspects of the type system introduced in a logical order. Several examples are given, progressing from fibonacci to relatively involved examples, showing that the library is actually usable for real work.

  - There is a soundness proof, carried out using step-indexed logical relations. While this is an adaptation of previous work by Morrisett et al., it is non-trivial to apply it to this system, and it includes the novel feature (for a type system like this) of using fractional permissions.

  - The inclusion of fractional permissions into a unification based type inference system appears to be novel, and while the paper does take pains to not claim that this feature has been formally proved complete, it does make the implementation feasible to use for real examples.

  - The existence of an implementation means that a good empirical evaluation can be carried out, and this shows that the NumLin system does indeed have the claimed benefits of reducing the overhead of generating temporary arrays and low-level copying.

Things that could be improved / suggestions:

  - The type system appears to be based on Andrew Barber's DILL (Dual Intuitionistic Linear Logic), but this is not referenced.

  - I was confused by the desugaring "let !x = e1 in e2" => `let Many x = e1 in let Many x = Many (Many x) in e2`. Can you explain this more?

  - The statement to line 302 talks about moving values from the linear environment to the intuitionistic one. But only variables live in environments?

  - The "value" restriction in the !-introduction rule should be explained more, with an example that shows a "bad" use. I guess the restriction arises from the existence of allocating primtivies in the language, such as `array`, and allowing these in the !-introduction rule would permit unrestricted duplication of references to newly allocated elements (another way round this would be to have a token passed around encapsulating the ability to allocate). Ahmed et al. (2007) [1] also have a similar value restriction for the same reason (I also think this would be a better reference than the Morrisett et al. TLCA2005 paper).

  - The types of some of the primitive should be given in the paper's main body, so that it is possible to see how to type the aliasing restrictions required for a given routine. As given, the reader has to reverse engineer the NumLin type from the OCaml type given in the appendix.

  - I was initially confused by the presence (in Figure 11) of the seemling useless wrappers around banged values. Is this to make the API of the generated OCaml code easier to use correctly?

  - I was confused by the comment on Linear Haskell starting on line 610. What relevance does a system based on kinds have to NumLin (which isn't based on kinds either)?

Typos:

  - Line 327: "follow" => "follows"

  - Line 494: Missing words at the start of the line.

[1] Amal Ahmed, Matthew Fluet, Greg Morrisett: L3: A Linear Language with Locations. Fundam. Inform. 77(4): 397-449 (2007)



Review #35C
===========================================================================

Overall merit
-------------
A. Accept: I will champion this paper

Reviewer expertise
------------------
Z. Outsider

Paper summary
-------------
This paper is on the use of linear types in a new programming language NUMLIN to express a large array of important algorithms involving aggregate data types (arrays primarily). Existing array based programs end up introducing many errors that are hard to detect including incorrect aliases. There also are sources of inefficiency including useless temporaries. The use of linear types that these authors propose elegantly characterizes the usage disciplines intended and also helps catch these bugs.

Comments for author
-------------------
While I am an outsider, I really find the use of linear types to be quite fascinating in terms of how it has advanced since the 80s and now applies to practical routines. The authors do a fantastic job covering a whole gamut of challenging case studies. 

NUMLIN appears to impose some modest burden on part of the programmer to "correctly" annotate a program as to the intended uses of the underlying array types. Given this, the typing system can infer permissions and check for correct usage of aggregate data structures. For instance, "free" or "set" need full permissions. Multiple read uses of an array are supported via fractional permissions.

In terms of results, the slowdown is not that pronounced even compared to a C implementation. They also detected an incorrect usage in the setting of the Kalman filter wrt Fortran conventions.

Here are my suggestions and questions:

1) How exactly could they incorporate this knowledge of what is legal/illegal wrt Fortran in the Numlin framework? Or was it spotted incidentally when they examined the permissions under which an external routine was attempted to be invoked?

2) Page 2: achieving parity with C -> explain this as parity wrt runtime efficiency (no other parity with C, I suppose)

3) Compared to the learning curve of Rust (which the authors cite) what is the learning curve of Numlin, in the authors' estimation? 

4) Could the authors sketch the evolution of linear types from its initial classical introduction (say in Wadler's "Linear types can change the world" paper) to its current sophisticated form? Is this really an "off the shelf" linear type system? Giving a history of the evolution of linear types to the kind used in Numlin would be very valuable

5) Can they explain their soundness theorem once in English? This can help novices read the paper better

6) My biggest comment is concerning concurrency. These sort of routines will be used in a setting where multicore concurrency will be employed to perform concurrent updates on arrays. In that setting, one might employ separation logic to deal with safe concurrent accesses. Now, separation logic also employs fractional permissions. Would Numlin be usable in that setting? Would the use of Separation Logic and Numlin (Linear Types) be orthogonal? Or do the authors feel the need to work out a concurrency extension of Numlin? This would be highly relevant to discuss, as otherwise it is unclear how we are going to employ multicores on these class of routines (run on a single core, the performance would be terrible).
