(* Dhruv Makwana *)
(* LT4LA - Prototype combinators *)
(* ------------------------------- *)
(* This module consists of experiments with GADTs to figure out a balance between
   an ergonomic interface for combinators versus one that is type-safe by construction. *)

module Try3 =
struct

  type z = Z

  type 'a s = S of 'a

  type _ arr = Arr

  type _ fc =
    | Z : z fc
    | S : 'a fc -> 'a s fc

  type _ lin =
    | I : unit lin
    | Int : int lin
    | F64 : float lin
    | Arr : 'fc fc -> 'fc arr lin
    | Pair : 'a lin * 'b lin -> ('a * 'b) lin
    | Fun : 'a lin * 'b lin -> ('a -> 'b) lin
    | All : ('fc fc -> 'a lin) -> ('fc fc -> 'a) lin

  let t = All (fun x -> Arr x)
  let t = All (fun x -> Arr Z)

  let ( * ) x y = Pair (x,y)
  let ( @-> ) x y = Fun (x, y)

  let dotProd_t =
    (All (fun a -> Arr a @-> All (fun b -> Arr b @-> (Arr a * Arr b) * F64)))

  type 'a exp =
    | I : unit exp
    | LetI : unit exp * 'res exp -> 'res exp
    | Int : int -> int exp
    | F64 : float -> float exp
    | Pair : 'a exp * 'b exp -> ('a * 'b) exp
    | LetP : ('a * 'b) exp * ('a exp * 'b exp -> 'res exp) -> 'res exp
    | Fun : ('a lin * 'a exp -> 'b exp) -> ('a -> 'b) exp
    | App : (('a -> 'b) exp * 'a exp) ->  'b exp
    | Arr : int exp -> z arr exp
    | LetA : 'a arr exp * ('a arr exp -> 'res exp) -> 'res exp
    | All : ('fc fc -> 'a exp) -> ('fc fc -> 'a) exp
    | Spc : ('fc fc -> 'a) exp * 'fc fc -> 'a exp
end

module Try2 =
struct

  (* (void, 'a) means environment is empty, term has no free variables. *)
  (* E.g. translate: (void, 'a) -> 'a                                   *)
  type void = { absurd : 'a. 'a }

  type z = Z

  type 'a s = S of 'a

  type _ nat =
    | Z : z nat
    | S : 'a nat -> 'a s nat

  type (_, 'a, _) index =
    | Fst : ('a * 'env, 'a, 'env) index
    | Nxt : ('env, 'a, 'env_r) index -> ('b * 'env, 'a, 'b * 'env_r) index

  type _ arr = Arr

  type (_, _) fc =
    | Z : (_, z) fc
    | S : ('env, 'a) fc -> ('env, 'a s) fc
    | V : ('env, 'a, _) index -> ('env, 'a) fc

  type (_,_) contains =
    | Old : ('a, 'a) contains
    | Ext : ('a, 'b) contains -> ('a1 * 'a, 'b) contains

  type (_, _) lin =
    | I : (_, unit) lin
    | Int : (_, int) lin
    | F64 : (_,float) lin
    | Arr : ('env, 'fc) fc -> ('env, 'fc nat arr) lin
    | Pair : ('env, 'a) lin * ('env, 'b) lin -> ('env, 'a * 'b) lin
    | Fun : ('env, 'a) lin * ('env, 'b) lin -> ('env, 'a -> 'b) lin
    | All : (('env, 'fc, 'res) index * ('env, 'env2) contains) * (('env2, 'fc) fc -> ('env2, 'a) lin) -> ('res, 'a) lin

  (* New *)
  let (n,p) as one = Fst, Old
  let (n,p) as two = Nxt n, Ext p
  let (n,p) as three = Nxt n, Ext p

  let t = All (one, fun x -> Arr x)
  let t = All (one, fun x -> Arr Z)

  let ( * ) x y = Pair (x,y)
  let ( @-> ) x y = Fun (x, y)

  let dotProd_t =
    let rest one = fun two -> Arr two @-> (Arr one * Arr two) * F64 in
    let fst = fun one -> Arr one @-> All (two, rest one) in
    All (one, fst)

  type (_,_) exp =
    | I : (_,unit) exp
    | LetI : ('env,unit) exp * ('env,'res) exp -> ('env, 'res) exp
    | Int : int -> (_, int) exp
    | F64 : float -> (_,float) exp
    | Pair : ('env,'a) exp * ('env,'b) exp -> ('env, 'a * 'b) exp
    | LetP : ('env,'a * 'b) exp * (('env, 'a) exp * ('env,'b) exp -> ('env,'res) exp) -> ('env, 'res) exp
    | Fun : (('env,'a) lin * ('env,'a) exp -> ('env,'b) exp) -> ('env, 'a -> 'b) exp
    | App : (('env,'a -> 'b) exp * ('env,'a) exp) -> ('env, 'b) exp
    | Arr : ('env,int) exp -> ('env, z nat arr) exp
    | LetA : ('env, 'a nat arr) exp * (('env, 'a nat arr) exp -> ('env, 'res) exp) -> ('env,'res) exp
    | All : ('env, 'fc, 'env2) index * ('env, 'a) exp -> ('env2, 'a) exp
    | Spc : ('env, 'a) exp * ('env2, 'env) contains * ('env2, 'fc) fc -> ('env2, 'a) exp

end

module Try1 =
struct

  (* (void, 'a) means environment is empty, term has no free variables. *)
  (* E.g. translate: (void, 'a) -> 'a *)
  type void = { absurd : 'a. 'a }

  type z = Z

  type 'a s = S of 'a

  type _ nat =
    | Z : z nat
    | S : 'a nat -> 'a s nat

  type (_, 'a, _) index =
    | Fst : ('a * 'env, 'a, 'env) index
    | Nxt : ('env, 'a, 'env_r) index -> ('b * 'env, 'a, 'b * 'env_r) index

  type _ arr = Arr

  type (_, _) fc =
    | Z : (_, z) fc
    | S : ('env, 'a) fc -> ('env, 'a s) fc
    | V : ('env, 'a, _) index -> ('env, 'a) fc

  type (_,_) contains =
    | Old : ('a, 'a) contains
    | Ext : ('a, 'b) contains -> ('a1 * 'a, 'b) contains

  type (_, _) lin =
    | I : (_, unit) lin
    | Int : (_, int) lin
    | F64 : (_,float) lin
    | Arr : ('env, 'fc) fc -> ('env, 'fc nat arr) lin
    | Pair : ('env, 'a) lin * ('env, 'b) lin -> ('env, 'a * 'b) lin
    | Fun : ('env, 'a) lin * ('env, 'b) lin -> ('env, 'a -> 'b) lin
    | All : ('env, 'fc, 'env2) index * ('env, 'a) lin -> ('env2, 'a) lin

  (* Old *)
  let t = All (Fst, Arr (V Fst))
  let t = All (Fst, Arr Z)

  let one = Fst 
  let two = Nxt one
  let three = Nxt two

  let ( * ) x y = Pair (x,y)
  let ( @-> ) x y = Fun (x, y)

  (* let dotProd_t =                                                                           *)
  (*   All (one, Arr (V one) @-> All (two, Arr (V two) @-> (Arr (V one) * Arr (V two)) * F64)) *)

  type (_,_) exp =
    | I : (_,unit) exp
    | LetI : ('env,unit) exp * ('env,'res) exp -> ('env, 'res) exp
    | Int : int -> (_, int) exp
    | F64 : float -> (_,float) exp
    | Pair : ('env,'a) exp * ('env,'b) exp -> ('env, 'a * 'b) exp
    | LetP : ('env,'a * 'b) exp * (('env, 'a) exp * ('env,'b) exp -> ('env,'res) exp) -> ('env, 'res) exp
    | Fun : (('env,'a) lin * ('env,'a) exp -> ('env,'b) exp) -> ('env, 'a -> 'b) exp
    | App : (('env,'a -> 'b) exp * ('env,'a) exp) -> ('env, 'b) exp
    | Arr : ('env,int) exp -> ('env, z nat arr) exp
    | LetA : ('env, 'a nat arr) exp * (('env, 'a nat arr) exp -> ('env, 'res) exp) -> ('env,'res) exp
    | All : ('env, 'fc, 'env2) index * ('env, 'a) exp -> ('env2, 'a) exp
    | Spc : ('env, 'a) exp * ('env2, 'env) contains * ('env2, 'fc) fc -> ('env2, 'a) exp
end
