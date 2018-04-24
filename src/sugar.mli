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

type loc = Lexing.position
val sexp_of_loc : loc -> Base.Sexp.t

type pat = Base of loc * bang_var | Many of loc * pat | Pair of loc * pat * pat
val sexp_of_pat : pat -> Base.Sexp.t
val ds_pat : pat -> Ast.exp -> loc * Ast.var * Ast.exp

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

type 'a non_empty = { first : 'a; rest : 'a list; }
val sexp_of_non_empty :
  ('a -> Base.Sexp.t) -> 'a non_empty -> Base.Sexp.t

type annot_arg = { pat : pat; lin : lin; }
val sexp_of_annot_arg : annot_arg -> Base.Sexp.t

type arg_like = Underscore of loc | Fc of loc * fc | Exp of exp
and exp =
    Prim of loc * prim
  | Var of loc * var
  | Unit_I of loc
  | True of loc
  | False of loc
  | Int_I of loc * int
  | Elt_I of loc * float
  | Pair_I of loc * exp * exp
  | Bang_I of loc * exp
  | AppLike of loc * exp * arg_like non_empty
  | If of loc * exp * exp * exp
  | Lambda of loc * (annot_arg, loc * var) Base.Either.t non_empty * exp
  | Index of loc * var * exp * exp option
  | Assign of loc * var * exp * exp option * exp
  | Infix of loc * exp * op * exp
  | LetAnnot of loc * bang_var * lin * exp * exp
  | LetPat of loc * (pat, pat * pat) Base.Either.t * exp * exp
  | LetFun of loc * bang_var * (annot_arg, loc * var) Base.Either.t non_empty
              * lin * exp * exp
  | LetRecFun of loc * var * annot_arg * (annot_arg, loc * var) Base.Either.t list
                 * lin * exp * exp
val sexp_of_arg_like : arg_like -> Base.Sexp.t
val sexp_of_exp : exp -> Base.Sexp.t
val loc : exp -> loc
val ds_exp : exp -> Ast.exp
