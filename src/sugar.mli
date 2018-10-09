type var = string
val sexp_of_var : var -> Base.Sexp.t

type fc = Z | S of fc | V of var | U of var
[@@deriving sexp_of]
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
[@@deriving sexp_of]
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
  | Eye
  | Copy_mat
  | Copy_mat_to
  | Size_mat
  | Transpose
  | Symm
  | Gemm
  | Gesv
  | Posv
  | Potrs
  | Syrk
[@@deriving sexp_of]

type bang_var = NotB of var | Bang of var
[@@deriving sexp_of]

type loc = Lexing.position
[@@deriving sexp_of]

type pat = Unit of loc | Base of loc * bang_var | Many of loc * pat | Pair of loc * pat * pat
[@@deriving sexp_of]
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
[@@deriving sexp_of]

type mat_var =
  | Just of loc * var
  | Symm of loc * var
  | Trsp of loc * var
[@@deriving sexp_of]

type 'a non_empty = { first : 'a; rest : 'a list; }
[@@deriving sexp_of]

type annot_arg = { pat : pat; lin : lin; }
[@@deriving sexp_of]

type arg_like = Underscore of loc | Fc of loc * fc | Exp of exp
and mat_exp =
    Copy_mat of loc * var
  | Copy_mat_to of loc * var
  | New_AB of exp * exp * loc * float * mat_var * mat_var
  | AB_C of loc * float * mat_var * mat_var * loc * float * loc * var
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
  | AppLike of exp * arg_like non_empty
  | If of loc * exp * exp * exp
  | Lambda of (annot_arg, loc * var) Base.Either.t non_empty * exp
  | Index of loc * var * loc * exp * exp option
  | Assign of loc * var * loc * exp * exp option * exp
  | Infix of loc * exp * op * exp
  | LetAnnot of loc * bang_var * lin * exp * exp
  | LetPat of loc * pat * exp * exp
  | LetFun of loc * bang_var * (annot_arg, loc * var) Base.Either.t non_empty * exp * exp
  | LetRecFun of loc * var * annot_arg * (annot_arg, loc * var) Base.Either.t list * lin * exp * exp
  | LetMat of loc * var * loc * mat_exp * exp
[@@deriving sexp_of]
val ds_exp : exp -> (Ast.exp, string) result
