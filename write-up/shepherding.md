# Shepherding instructions
---

We hope that you will take on board all the reviewer suggestions that you agree
with, but in terms of shepherding, final acceptance of your paper is
conditional on addressing the following points:

 *  Introduce the syntax, syntactic sugar and supported operations more
    explicitly (i.e. not only with examples), in order to make the paper
    more accessible.

 *  Explain the types of some of the primitives in the main body of the
    paper, and your process for deriving the NumLin types from the BLAS API
    types and documentation.

 *  Improve the discussion of the desugaring of let !x = e1 in e2 as explained
    in the response.

## Done

 *  Improve the introduction of NumLin to a) emphasise that the main
    contribution of this paper is the definition of the NumLin language and its
    application as a safe language for invoking BLAS routines, and b) to
    clarify the relationship to previous work on linear types and fractional
    permissions (as explained in the response).

 *  Provide an accessible description of your soundness result.

