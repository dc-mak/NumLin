(* Dhruv Makwana *)
(* LT4LA Check Monad *)
(* ----------------- *)
(* This is a wrapper arround the monad defined in state_or_error.mli.
   This is so that (1) I don't have arbitrary access to the state during
   typechecking and (2) I can enforce invariants such as only marking those
   linear variables which have been found and not used as used in [use_var]
   (3) I can change how the monad and how the state are implemented
   independently. Right now, (almost) everything is pure functional. *)


(** Proof that a type is not used *)
type not_used

val sexp_of_not_used : not_used -> Sexplib.Sexp.t

(** Track usage of linear types *)
type tagged_linear_t = Not_used of not_used | Used of Ast.linear_t

val sexp_of_tagged_linear_t : tagged_linear_t -> Sexplib.Sexp.t

(** The theory has two (eventually three) sorts: variables can map to either
    - Linear Types, with an 'a (e.g. usage)
    - Fractional capabilities
    - Sizes/dimensions or arrays and matrices *)
type state

val sexp_of_state : state -> Sexplib.Sexp.t

(** Sequence computations based on a given state. *)
type 'a t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

module Monad_infix : sig

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

end

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val join : 'a t t -> 'a t

val ignore_m : 'a t -> unit t

val all : 'a t list -> 'a list t

val all_ignore : unit t list -> unit t

module Let_syntax : sig

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Let_syntax : sig

    val return : 'a -> 'a t

    val bind : 'a t -> f:('a -> 'b t) -> 'b t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end

  end

end

(** Ignore the state and return error message with erroneous value. *)
val fail : ?strict:unit -> string -> 'a -> ('a -> Sexplib.Sexp.t) -> 'a t

(** Ignore the state and return error message *)
val fail_string : string -> 'a t

(** Ignore the state and return a formatted message *)
val failf : ('a, unit, string, 'b t) format4 -> 'a

(* Create a fresh variable and update the state *)
(* val create_fresh : ?name:string -> unit -> Ast.variable t *)

(** Look for given variable in linear_vars *)
val lookup : Ast.variable -> tagged_linear_t option t

(** Replace any variables in the fractional capability with what
    it is mapped to in frac_cap_vars in the environment *)
val normal_form : Ast.frac_cap -> Ast.frac_cap t

(** Marks the [not_used] variable and linear_t as used and returns latter. *)
val use_var : not_used -> Ast.linear_t t

(** Applies substitutions to the environment *)
val apply : (Ast.variable * Ast.frac_cap) list -> unit t

(** Evaluates the given monadic value in the context extended with the given
    mappings and then removes those mappings from the environment, checking that
    each variable has been used once.
    - Mappings must be globally unique. *)
val with_linear_t :
  (Ast.variable * Ast.linear_t) list -> Ast.linear_t t -> Ast.linear_t t

(** Evaluates the given monadic value in the context extended with the given
    mappings and then removes those mappings from the environment.
    - Mappings must be globally unique. *)
val with_frac_cap :
  (Ast.variable * Ast.frac_cap) list -> Ast.linear_t t -> Ast.linear_t t

(** Evaluates the monadic value. Must ensure [counter] = #variables in expression + 1. *)
val run : Ast.linear_t t -> counter:int -> Ast.linear_t Core_kernel.Or_error.t
