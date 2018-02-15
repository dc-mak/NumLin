(* Dhruv Makwana *)
(* LT4LA State_or_error *)
(* -------------------- *)
(* Basic monad for expressing stateful computation with terminating errors. *)

module Make :
  functor (State : sig type t end) ->
  sig
    type 'a t
    val get : State.t t
    val put : State.t -> unit t
    val fail : ?strict:unit -> string -> 'b -> ('b -> Sexplib.Sexp.t) -> 'a t
    val fail_string : string -> 'a t
    val failf : ('a, unit, string, 'b t) format4 -> 'a
    val run : 'a t -> State.t -> ('a * State.t) Base.Or_error.t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    module Monad_infix :
    sig
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
    module Let_syntax :
    sig
      val return : 'a -> 'a t
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
      module Let_syntax :
      sig
        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t
        module Open_on_rhs : sig  end
      end
    end
  end
