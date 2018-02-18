(* Dhruv Makwana *)
(* LT4LA Combinators *)
(* ----------------- *)
(* Allow a user to build up a term that is fairly well-typed. *)

type z = Z

type 'a s = S of 'a

let new_var = 
  let i = ref 0 in
  fun () -> let x = !i in (i := x + 1; Ast.{name="gen";id=(x)})

type _ arr = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

module Type =
struct

  open Ast

  type _ fc = frac_cap
  let z :  z fc = Zero
  let s (x : ('a) fc) : ('a s) fc = Succ x

  type _ t = linear_t
  let extract (x : _ t) : linear_t = x

  let unit : unit t = Unit
  let int : int t = Int
  let f64 : float t = Float64
  let arr (x : 'fc fc) : 'fc arr t = Array_t x
  let pair (fst : 'a t) (snd : 'b t) : ('a * 'b) t = Pair (fst, snd)
  let func (arg : 'a t) (res : 'b t) : ('a -> 'b) t = Fun (arg, res)

  (* impure *)
  let all (f : 'fc fc -> 'a t) : 'a t =
    let a = new_var () in ForAll_frac_cap (a, f (Var a))

  module Ops =
  struct
    let ( * ) = pair
    let ( @-> ) = func
  end

end

module Code =
struct

  open Ast

  type (_) t = expression
  let extract (x : _ t) : expression = x

  let unit : unit t = Unit_Intro
  let letU (exp : unit t) (res : 'res t) : 'res t = Unit_Elim (exp, res)
  let int (i : int) : int t = Int_Intro i
  let f64 (f : float) : float t = Float64_Intro f
  let pair (fst : 'a t) (snd : 'b t) : ('a * 'b) t = Pair_Intro (fst, snd)
  let app (f : ('a -> 'b) t) (arg : 'a t) : 'b t = App (f, arg)
  let arr (exp : int t) : z arr t = Array_Intro exp
  let spc (exp : ('fc Type.fc -> 'a) t) (fc : 'fc Type.fc) : 'a t =
    Specialise_frac_cap (exp, fc)

  (* impure *)
  let letP (p : ('a * 'b) t) (elim : 'a t -> 'b t -> 'res t) : 'res t =
    let a, b = new_var (), new_var () in Pair_Elim (a, b, p, elim (Var a) (Var b))

  (* impure *)
  let lambda (t : 'a Type.t) (f : 'a t -> 'b t) : ('a -> 'b) t =
    let a = new_var () in Lambda (a, t, f (Var a))

  (* impure *)
  let letA (exp : 'a arr t) (elim : 'a arr t -> 'res t) : 'res t =
    let a = new_var () in Array_Elim (a, exp, elim (Var a))

  (* impure *)
  let all (f : 'fc Type.fc -> 'a t) : ('fc Type.fc -> 'a) t =
    let a = new_var () in ForAll_frac_cap (a, f (Var a))

  module Ops =
  struct
    let ( %% ) = app
    let ( & ) = pair
    let ( // ) = spc
  end

  (* Primitives *)
  let dot : ('x Type.fc -> 'x arr -> 'y Type.fc -> 'y arr -> ('x arr * 'y arr) * float) t =
    Primitive DotProd

end
