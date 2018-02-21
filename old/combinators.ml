(* Dhruv Makwana *)
(* LT4LA Combinators *)
(* ----------------- *)
(* Allow a user to build up a term that is fairly well-typed. *)

type z = Z

type 'a s = S of 'a

let new_var = 
  let i = ref 0 in
  fun () -> let x = !i in (i := x + 1; Ast.{name="gen";id=(x)})

type _ fc = Ast.frac_cap
let z :  z fc = Zero
let s (x : ('a) fc) : ('a s) fc = Succ x

type _ arr = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

module Type =
struct

  open Ast

  type _ t = linear_t
  let extract (x : _ t) : linear_t = x

  let unit : unit t = Unit
  let int : int t = Int
  let f64 : float t = Float64
  let arr (x : 'fc fc) : 'fc arr t = Array_t x
  let pair (fst : 'a t) (snd : 'b t) : ('a * 'b) t = Pair (fst, snd)
  let func (arg : 'a t) (res : 'b t) : ('a -> 'b) t = Fun (arg, res)

  (* impure *)
  let all (f : 'fc fc -> 'a t) : ('fc fc -> 'a) t =
    let a = new_var () in ForAll_frac_cap (a, f (Var a))

  module Ops =
  struct
    let ( * ) = pair
    let ( @-> ) = func
    let ( !! ) = all
  end

end

module Code =
struct

  open Ast

  type _ t = expression
  let extract (x : _ t) : expression = x

  let unit : unit t = Unit_Intro
  let letU (exp : unit t) (res : 'res t) : 'res t = Unit_Elim (exp, res)
  let int (i : int) : int t = Int_Intro i
  let f64 (f : float) : float t = Float64_Intro f
  let pair (fst : 'a t) (snd : 'b t) : ('a * 'b) t = Pair_Intro (fst, snd)
  let app (f : ('a -> 'b) t) (arg : 'a t) : 'b t = App (f, arg)
  let arr (exp : int t) : z arr t = Array_Intro exp
  let spc (exp : ('fc fc -> 'a) t) (fc : 'fc fc) : 'a t =
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
  let all (f : 'fc fc -> 'a t) : ('fc fc -> 'a) t =
    let a = new_var () in ForAll_frac_cap (a, f (Var a))

  module Ops =
  struct
    let ( %% ) = app
    let ( & ) = pair
    let ( // ) = spc
    let ( !! ) = all
  end

  let split_perm : ('x fc -> 'x arr -> 'x s arr * 'x s arr) t = Primitive Split_Permission
  let merge_perm : ('x fc -> 'x s arr * 'x s arr -> 'x arr) t = Primitive Merge_Permission
  let free : (z arr -> unit) t = Primitive Free
  let copy : ('x fc -> 'x arr -> 'x arr * z arr) t = Primitive Copy
  let swap : (z arr * z arr -> z arr * z arr) t = Primitive Swap
  let asum : ('x arr -> 'x arr * float) t = Primitive Sum_Mag
  let axpy : (float -> 'vec fc -> 'vec arr -> z arr -> 'vec arr * z arr) t = Primitive Scalar_Mult_Then_Add
  let dot : ('x fc -> 'x arr -> 'y fc -> 'y arr -> ('x arr * 'y arr) * float) t = Primitive DotProd
  let nrm2 : ('x fc -> 'x arr -> 'x arr * float) t = Primitive Norm2
  let rot : (float -> float -> (float * float) * (float * float)) t = Primitive Plane_Rotation
  let rotm : (z arr -> z arr -> 'p fc -> 'p arr -> (z arr * z arr) * 'p arr) t = Primitive GivensMod_Rotation
  let rotmg : (float * float -> float * float -> (float * float) * (float * z arr)) t = Primitive Gen_GivensMod_Rotation
  let scal : (float -> z arr -> z arr) t = Primitive Scalar_Mult
  let amax : ('x fc -> 'x arr -> int * 'x arr) t = Primitive Index_of_Max_Abs

end
