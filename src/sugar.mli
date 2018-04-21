type var = string
val sexp_of_var : var -> Base.Sexp.t

type fc = Z | S of fc | V of var | U of var
val sexp_of_fc : fc -> Base.Sexp.t
val ds_fc : fc -> Ast.fc

type lin =
    Unit
  | Bool
  | Int
  | Elt
  | Arr of fc
  | Mat of fc
  | Pair of lin * lin
  | Bang of lin
  | Fun of lin * lin
  | All of var * lin
val sexp_of_lin : lin -> Base.Sexp.t
val ds_lin : lin -> Ast.lin

type prim =
    Not_
  | Share
  | Unshare
  | Free
  | Array
  | Copy
  | Sin
  | Hypot
  | Asum
  | Axpy
  | Dot
  | Rotmg
  | Scal
  | Amax
  | Share_mat
  | Unshare_mat
  | Free_mat
  | Matrix
  | Copy_mat
  | Symv
  | Gemv
  | Trmv
  | Trsv
  | Ger
  | Gemm
  | Trmm
  | Trsm
val sexp_of_prim : prim -> Base.Sexp.t

type bang_var = NotB of var | Bang of var
val sexp_of_bang_var : bang_var -> Base.Sexp.t

type pat = Base of bang_var | Many of pat | Pair of pat * pat
val sexp_of_pat : pat -> Base.Sexp.t
val ds_pat : pat -> Ast.exp -> var * Ast.exp

type matrix_atom =
    Plain of var
  | Transp of var
  | Inv of var
  | InvTransp of var

type op =
    Or
  | And
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Lt
  | PlusDot
  | MinusDot
  | TimesDot
  | DivDot
  | EqDot
  | LtDot
val sexp_of_op : op -> Base.Sexp.t

type matrix_exp = Atom of matrix_atom | Op of matrix_exp * op * matrix_exp

type 'a non_empty = { first : 'a; rest : 'a list; }
val sexp_of_non_empty :
  ('a -> Base.Sexp.t) -> 'a non_empty -> Base.Sexp.t

type annot_arg = { pat : pat; lin : lin; }
val sexp_of_annot_arg : annot_arg -> Base.Sexp.t

type arg_like = Underscore | Fc of fc | Exp of exp
and exp =
    Prim of prim
  | Var of var
  | Unit_I
  | True
  | False
  | Int_I of int
  | Elt_I of float
  | Pair_I of exp * exp
  | Bang_I of exp
  | AppLike of exp * arg_like non_empty
  | If of exp * exp * exp
  | Lambda of (annot_arg, var) Base.Either.t non_empty * exp
  | Index of var * exp * exp option
  | Assign of var * exp * exp option * exp
  | Infix of exp * op * exp
  | LetAnnot of bang_var * lin * exp * exp
  | LetPat of (pat, pat * pat) Base.Either.t * exp * exp
  | LetFun of bang_var * (annot_arg, var) Base.Either.t non_empty * lin *
      exp * exp
  | LetRecFun of var * annot_arg * (annot_arg, var) Base.Either.t list *
      lin * exp * exp
val sexp_of_arg_like : arg_like -> Base.Sexp.t
val sexp_of_exp : exp -> Base.Sexp.t
val ds_exp : exp -> Ast.exp
