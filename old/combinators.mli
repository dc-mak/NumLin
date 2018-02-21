(* Dhruv Makwana *)
(* LT4LA Combinator Interface *)
(* -------------------------- *)

(** Type-level zero for fractional-capabilities. *)
type z = Z

(** Type-level successor for fractional-capbalities. *)
type 'a s = S of 'a

(** Fractional capabilities keep track of the linearity of the array,
    whether or not it is aliased. They can be interpreted as
    2^(-[frac_cap]). Hence, a whole (unshared) array has a capability 1
    capability (2^-[Zero]). Either 1 or 1/2 or 1/4, etc... or a variable. *)
type _ fc
val z : z fc
val s : 'a fc -> 'a s fc

(** Array-types are parameterised over fractional-capabilities. *)
type _ arr

(** Module containing combinators to build linear-types. *)
module Type :
  sig
    (** Standard Linear type system, using fractional capabilities
        and extensions for Linear Algebra. *)
    type _ t
    val extract : 'a t -> Ast.linear_t
    val unit : unit t
    val int : int t
    val f64 : float t
    val arr : 'fc fc -> 'fc arr t
    val pair : 'a t -> 'b t -> ('a * 'b) t
    val func : 'a t -> 'b t -> ('a -> 'b) t
    val all : ('fc fc -> 'a t) -> ('fc fc -> 'a) t
    module Ops :
      sig
        val ( * ) : 'a t -> 'b t -> ('a * 'b) t
        val ( @-> ) : 'a t -> 'b t -> ('a -> 'b) t
        val ( !! ) : ('fc fc -> 'a t) -> ('fc fc -> 'a) t
      end
  end

(** Expressions of the language. *)
module Code :
  sig
    type _ t
    val extract : 'a t -> Ast.expression
    val unit : unit t
    val letU : unit t -> 'res t -> 'res t
    val int : int -> int t
    val f64 : float -> float t
    val pair : 'a t -> 'b t -> ('a * 'b) t
    val app : ('a -> 'b) t -> 'a t -> 'b t
    val arr : int t -> z arr t
    val spc : ('fc fc -> 'a) t -> 'fc fc -> 'a t
    val letP : ('a * 'b) t -> ('a t -> 'b t -> 'res t) -> 'res t
    val lambda : 'a Type.t -> ('a t -> 'b t) -> ('a -> 'b) t
    val letA : 'a arr t -> ('a arr t -> 'res t) -> 'res t
    val all : ('fc fc -> 'a t) -> ('fc fc -> 'a) t
    module Ops :
      sig
        val ( %% ) : ('a -> 'b) t -> 'a t -> 'b t
        val ( & ) : 'a t -> 'b t -> ('a * 'b) t
        val ( // ) : ('a fc -> 'b) t -> 'a fc -> 'b t
      end

    val split_perm : ('x fc -> 'x arr -> 'x s arr * 'x s arr) t
    val merge_perm : ('x fc -> 'x s arr * 'x s arr -> 'x arr) t
    val free : (z arr -> unit) t
    val copy : ('x fc -> 'x arr -> 'x arr * z arr) t
    val swap : (z arr * z arr -> z arr * z arr) t
    val asum : ('x arr -> 'x arr * float) t
    val axpy : (float -> 'vec fc -> 'vec arr -> z arr -> 'vec arr * z arr) t
    val dot : ('x fc -> 'x arr -> 'y fc -> 'y arr -> ('x arr * 'y arr) * float) t
    val nrm2 : ('x fc -> 'x arr -> 'x arr * float) t
    val rot : (float -> float -> (float * float) * (float * float)) t
    val rotm : (z arr -> z arr -> 'p fc -> 'p arr -> (z arr * z arr) * 'p arr) t
    val rotmg : (float * float -> float * float -> (float * float) * (float * z arr)) t
    val scal : (float -> z arr -> z arr) t
    val amax : ('x fc -> 'x arr -> int * 'x arr) t

  end
