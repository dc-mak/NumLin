(** Proof a type is well-formed w.r.t environment. *)
type wf_fc = private WFC of Ast.fc [@@ocaml.unboxed]
type wf_var = private WFV of Ast.var [@@ocaml.unboxed]
type wf_lin = private WFL of Ast.lin [@@ocaml.unboxed]
[@@deriving sexp_of]

(** Well-formed constructors *)
val wf_Unit : wf_lin
val wf_Bool : wf_lin
val wf_Int : wf_lin
val wf_Elt : wf_lin
val wf_Pair : wf_lin -> wf_lin -> wf_lin
val wf_Bang : wf_lin -> wf_lin
val wf_Arr_Z : wf_lin
val wf_Mat_Z : wf_lin
val wf_Fun : wf_lin -> wf_lin -> wf_lin
val wf_All : Ast.var -> wf_lin -> wf_lin

(** Proof that a type is not used. *)
type not_used
[@@deriving sexp_of]

(** A type is either used, not used, or intuitionistic. *)
type tagged = private Not_used of not_used | Used of Ast.loc * wf_lin | Intuition of wf_lin
[@@deriving sexp_of]

(** Type checker state *)
type state
[@@deriving sexp_of]

(** State-or-error monad values. *)
type 'a t
val get : state t
val put : state -> unit t
val fail : ?strict:unit -> string -> 'b -> ('b -> Base.Sexp.t) -> 'a t
val fail_string : string -> 'a t
val failf : ('a, unit, string, 'b t) format4 -> 'a
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
val all_unit : unit t list -> unit t
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

(** Checker utilities *)
val create_fresh : ?name:string -> unit -> Ast.var t
val lookup : Ast.var -> tagged option t
val use_var : Ast.loc -> not_used -> wf_lin t
val same_lin : wf_lin -> wf_lin ->
  (string * Ast.fc, string * Ast.lin) Base.Either.t list Base.Or_error.t t
val apply : (string * Ast.fc, string * Ast.lin) Base.Either.t list -> wf_lin -> wf_lin t
val return_lin : Ast.var -> wf_lin -> 'a t -> ('a * wf_lin) t
val with_lin : Ast.var -> wf_lin -> 'a t -> 'a t
val return_int : Ast.var -> wf_lin -> 'a t -> ('a * wf_lin) t
val with_int : Ast.var -> wf_lin -> 'a t -> 'a t
val with_fc : Ast.var -> 'a t -> 'a t
val run : wf_lin t -> counter:int -> Ast.lin Base.Or_error.t
val in_empty : 'a t -> 'a t
val same_resources : ('a t * Ast.loc) -> ('b t * Ast.loc) -> ('a * 'b) t

(** Well-formed destructors *)
val if_wf : Ast.fc -> then_:(wf_fc -> 'a t) -> else_:(Ast.fc -> 'a t) -> 'a t
val wf_substitute_fc : wf_lin -> wf_var -> wf_fc -> wf_lin
val split_wf_Pair :
  wf_lin t ->
  if_pair:(wf_lin -> wf_lin -> 'a t) ->
  not_pair:(Ast.lin -> 'a t) -> 'a t
val split_wf_Bang :
  wf_lin t ->
  if_bang:(wf_lin -> 'a t) -> not_bang:(Ast.lin -> 'a t) -> 'a t
val split_wf_All :
  wf_lin t ->
  if_all:(wf_var -> wf_lin -> 'a t) ->
  not_all:(Ast.lin -> 'a t) -> 'a t
val split_wf_Fun :
  wf_lin t ->
  if_fun:(wf_lin -> wf_lin -> 'a t) ->
  not_fun:(Ast.lin -> 'a t) -> 'a t
val wf_lin :
  fmt:('a -> Ast.loc -> Ast.lin t, unit, string, 'b t) format4 ->
  arg:'a -> loc:Ast.loc -> Ast.lin -> wf_lin t
