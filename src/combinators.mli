type z = Z
type 'a s = S of 'a
type _ arr
module Type :
  sig
    type _ fc
    val z : z fc
    val s : 'a fc -> 'a s fc
    type _ t
    val extract : 'a t -> Ast.linear_t
    val unit : unit t
    val int : int t
    val f64 : float t
    val arr : 'fc fc -> 'fc arr t
    val pair : 'a t -> 'b t -> ('a * 'b) t
    val func : 'a t -> 'b t -> ('a -> 'b) t
    val all : ('fc fc -> 'a t) -> 'a t
    module Ops :
      sig
        val ( * ) : 'a t -> 'b t -> ('a * 'b) t
        val ( @-> ) : 'a t -> 'b t -> ('a -> 'b) t
      end
  end
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
    val spc : ('fc Type.fc -> 'a) t -> 'fc Type.fc -> 'a t
    val letP : ('a * 'b) t -> ('a t -> 'b t -> 'res t) -> 'res t
    val lambda : 'a Type.t -> ('a t -> 'b t) -> ('a -> 'b) t
    val letA : 'a arr t -> ('a arr t -> 'res t) -> 'res t
    val all : ('fc Type.fc -> 'a t) -> ('fc Type.fc -> 'a) t
    module Ops :
      sig
        val ( %% ) : ('a -> 'b) t -> 'a t -> 'b t
        val ( & ) : 'a t -> 'b t -> ('a * 'b) t
        val ( // ) : ('a Type.fc -> 'b) t -> 'a Type.fc -> 'b t
      end
    val dot : ('x Type.fc -> 'x arr -> 'y Type.fc -> 'y arr -> ('x arr * 'y arr) * float) t
  end
