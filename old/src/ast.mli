(* Dhruv Makwana *)
(* LT4LA Abstract Syntax Tree *)
(* -------------------------- *)

(** Make a pretty-printer output a string. *)
val string_of_pp : ?size:int -> (Base.Formatter.t -> 'a -> unit) -> 'a -> string

(** Variables have a [name] for pretty-printing and an [id] for uniqueness. *)
type variable = { id : int; name : string; }
val sexp_of_variable : variable -> Base.Sexp.t
val compare_variable : variable -> variable -> int
val string_of_variable : variable -> string

(** We (will) have two kinds in this type system, fractional capabilities of an array
    and sizes of an array.  Fractional capabilities keep track of the linearity
    of the array, whether or not it is aliased. They can be interpreted as
    2^(-[frac_cap]). Hence, a whole (unshared) array has a capability 1
    capability (2^-[Zero]). Either 1 or 1/2 or 1/4, etc... or a variable. *)
type frac_cap = Zero | Succ of frac_cap | Var of variable
val sexp_of_frac_cap : frac_cap -> Base.Sexp.t
val compare_frac_cap : frac_cap -> frac_cap -> int
val string_of_frac_cap : frac_cap -> string

(** [bind_fc_fc var fc] replaces any [Var var'] in [fc] with [Var var] if [var.name = var'.name]. *)
val bind_fc_fc : variable -> frac_cap -> frac_cap

(** Standard Linear type system, using fractional capabilities
    and extensions for Linear Algebra. *)
type linear_t =
  | Unit
  | Int
  | Float64
  | Pair of linear_t * linear_t
  | Fun of linear_t * linear_t
  | ForAll_frac_cap of variable * linear_t
  | Array_t of frac_cap
val sexp_of_linear_t : linear_t -> Base.Sexp.t
val compare_linear_t : linear_t -> linear_t -> int
val pp_linear_t : Base.Formatter.t -> linear_t -> unit
val substitute_in : linear_t -> var:variable -> replacement:frac_cap -> linear_t Base.Or_error.t
val same_linear_t : linear_t -> linear_t -> unit Base.Or_error.t

(** [bind_fc_lt var lt] replaces any [Var var'] in [lt] with [Var var] if [var.name = var'.name]. *)
val bind_fc_lt : variable -> linear_t -> linear_t

(** Primitives/extensions
    Intel Level 1: software.intel.com/en-us/mkl-developer-reference-c-blas-level-1-routines-and-functions
    BLAS Reference: www.netlib.org/blas/blasqr.pdf
    Not included: xxDOT (derivable), xDOTU, xDOTC (Complex Float32/64) *)
type primitive =
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

val sexp_of_primitive : primitive -> Base.Sexp.t

val string_of_primitive : primitive -> string

(** Expressions of the language. Right now, I've made my life much easier by
    having type-directed abstract-syntax. Can hopefully elaborate to this later.
    Elimination rules for [Int]s and [Float64]s will come later, after an
    arithmetic expression language is fixed. *)
type expression =
  | Var of variable
  | Int_Intro of int
  | Float64_Intro of float
  | Unit_Intro
  | Unit_Elim of expression * expression
  | Pair_Intro of expression * expression
  | Pair_Elim of variable * variable * expression * expression
  | Lambda of variable * linear_t * expression
  | App of expression * expression
  | ForAll_frac_cap of variable * expression
  | Specialise_frac_cap of expression * frac_cap
  | Array_Intro of expression
  | Array_Elim of variable * expression * expression
(*| ForAll_Size of variable * expression *)
  | Primitive of primitive

val sexp_of_expression : expression -> Base.Sexp.t

val pp_expression : Base.Formatter.t -> expression -> unit

(** [bind_fc_exp var exp] replaces any [Var var' : frac_cap] in [exp] with [Var
    var] if [var.name = var'.name]. *)
val bind_fc_exp : variable -> expression -> expression

(** [bind_exp var exp] replaces any [Var var' : expression] in [exp] with [Var
    var] if [var.name = var'.name]. *)
val bind_exp : variable -> expression -> expression
