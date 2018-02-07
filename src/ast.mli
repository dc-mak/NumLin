(* Dhruv Makwana *)
(* LT4LA Abstract Syntax Tree *)
(* -------------------------- *)

type variable = { id : int; name : string; }
val sexp_of_variable : variable -> Sexplib.Sexp.t
val compare_variable : variable -> variable -> int

(** We (will) have two kinds in this type system, fractional capabilities of an array
    and sizes of an array.  Fractional capabilities keep track of the linearity
    of the array, whether or not it is aliased. They can be interpreted as
    2^(-[frac_cap]). Hence, a whole (unshared) array has a capability 1
    capability (2^-[Zero]). Either 1 or 1/2 or 1/4, etc... or a variable. *)
type frac_cap = Zero | Succ of frac_cap | Var of variable
val sexp_of_frac_cap : frac_cap -> Sexplib.Sexp.t
val compare_frac_cap : frac_cap -> frac_cap -> int
val pp_frac_cap : frac_cap -> string

(** Standard Linear type system, using fractional capabilities
    and extensions for Linear Algebra. *)
type linear_t =
    Unit
  | Pair of linear_t * linear_t
  | Fun of linear_t * linear_t
  | ForAll_frac_cap of variable * linear_t
  | Array_t of frac_cap
val sexp_of_linear_t : linear_t -> Sexplib.Sexp.t
val compare_linear_t : linear_t -> linear_t -> int
val pp_linear_t : Caml.Format.formatter -> linear_t -> unit
val substitute_in : linear_t -> var:variable -> replacement:frac_cap -> linear_t Base.Or_error.t
val same_linear_t : linear_t -> linear_t -> unit Base.Or_error.t

(** For now, arrays will be interpreted as/implemented using this,
    due to issues with Dune, linking and Owl. *)
(* type array_type = float array *)
type array_type = Owl.Arr.arr
val sexp_of_array_type : array_type -> Sexplib.Sexp.t

(** Expressions of the language. Right now, I've made my life much easier by
    having type-directed abstract-syntax. Can hopefully elaborate to this later. *)
type expression =
    Var of variable
  | Unit_Intro
  | Unit_Elim of expression * expression
  | Pair_Intro of expression * expression
  | Pair_Elim of variable * variable * expression * expression
  | Lambda of variable * linear_t * expression
  | App of expression * expression
  | ForAll_frac_cap of variable * expression
  | Specialise_frac_cap of expression * frac_cap
  | Array_Intro of array_type
  | Array_Elim of variable * expression * expression
  | Primitive of primitive

and primitive =
  (* Operators *)
  | Split_Permission
  | Merge_Permission
  | Free
  | Copy (* xCOPY *)
  | Swap (* xSWAP *)

  (* Routines/Functions *)
  | Sum_Mag (* xASUM *)
  | Scalar_Mult_Then_Add (* xAXPY *)
  | DotProd (* xDOT *)
  | Norm2 (* xNRM2 *)
  | Plane_Rotation (* xROT *)
  | Givens_Rotation (* xROTG *)
  | GivensMod_Rotation (* xROTM *)
  | Gen_GivensMod_Rotation (* xROTMG *)
  | Scalar_Mult (* xSCAL *)
  | Index_of_Max_Abs (* IxAMAX *)
  | Index_of_Min_Abs (* IxAMIN -- Intel only *)

val sexp_of_expression : expression -> Sexplib.Sexp.t
val sexp_of_primitive : primitive -> Sexplib.Sexp.t
