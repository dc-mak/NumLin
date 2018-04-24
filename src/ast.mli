(** Utilities *)
val string_of_pp :
  ?size:int -> (Caml.Format.formatter -> 'a -> unit) -> 'a -> string

(** Variables *)
type var = string
val sexp_of_var : var -> Base.Sexp.t
val compare_var : var -> var -> int
type comparator_witness
val comparator : (var, comparator_witness) Base.Comparator.t

(** Fractional Capabilities *)
type fc = Z | S of fc | V of var | U of var
val sexp_of_fc : fc -> Base.Sexp.t
val string_of_fc : fc -> string

(** Linear types *)
type lin =
  | Unit
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
val pp_lin : Caml.Format.formatter -> lin -> unit
val substitute_in : lin -> var:var -> replace:fc -> lin
val substitute_unify : lin -> var:var -> replace:fc -> lin
(** [same_lin] [ (x,x) | x is a free-variable] t1 t2 determines whether two
    types are the same up to alpha-equivalence. *)
val same_lin : (var * var) list -> lin -> lin -> (var * fc) list Base.Or_error.t

(** Arithmetic expressions *)
type arith = Add | Sub | Mul | Div | Eq | Lt
val sexp_of_arith : arith -> Base.Sexp.t

(** Primitives *)
type prim =
  (** Boolean *)
  | Not_
  (** Arithmetic *)
  | IntOp of arith
  | EltOp of arith
  (** Arrays *)
  | Set
  | Get
  | Share
  | Unshare
  | Free
  (** Owl - no polymorphism so no Mapi :'( *)
  | Array
  | Copy
  | Sin
  | Hypot
  (** Level 1 BLAS *)
  | Asum
  | Axpy
  | Dot
  | Rotmg
  | Scal
  | Amax
  (* matrix *)
  | Get_mat
  | Set_mat
  | Share_mat
  | Unshare_mat
  | Free_mat
  | Matrix
  | Copy_mat
  (* Level 2/3 BLAS *)
  | Symv
  | Gemv
  | Trmv
  | Trsv
  | Ger
  | Gemm
  | Trmm
  | Trsm
val sexp_of_prim : prim -> Base.Sexp.t
val string_of_prim : prim -> string

(** Expressions *)
type loc =
  Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
val sexp_of_loc : loc -> Base.Sexp.t
val dummy : loc
val line_col : loc -> string

type exp =
  | Prim of loc * prim
  | Var of loc * var
  | Unit_I of loc
  | True of loc
  | False of loc
  | Int_I of loc * int
  | Elt_I of loc * float
  | Pair_I of loc * exp * exp
  | Bang_I of loc * exp
  | Spc of loc * exp * fc
  | App of loc * exp * exp
  | Bang_E of loc * var * exp * exp
  | Pair_E of loc * var * var * exp * exp
  | Fix of loc * var * var * lin * lin * exp
  | If of loc * exp * exp * exp
  | Gen of loc * var * exp
  | Lambda of loc * var * lin * exp

val loc : exp -> loc
val is_value : exp -> bool
val sexp_of_exp : exp -> Base.Sexp.t
val prec : exp -> int
val pp_exp : Caml.Format.formatter -> exp -> unit
