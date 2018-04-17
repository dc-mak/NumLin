(* Dhruv Makwana *)
(* LT4LA Compiler Output Header *)
(* ---------------------------- *)
(* This file is a header/wrapper around Owl's CBLAS bindings. *)

(* begin template *)
open Owl
;;

module Arr =
  Owl.Dense.Ndarray.D
;;

type z =
    Z
;;

type 'a s =
    Succ
;;

type 'a arr = 
  A of Arr.arr
[@@ocaml.unboxed]
;;

type 'a bang =
  Many of 'a
[@@ocaml.unboxed]
;;

module Prim =
struct

  let extract (Many x) =
    x
  ;;

  (* Boolean *)
  let not_ (Many x) =
    Many (not x)
  ;;

  (* IntOp *)
  let addI (Many x) (Many y) =
    Many (x + y)
  ;;

  let subI (Many x) (Many y) =
    Many (x - y)
  ;;

  let mulI (Many x) (Many y) =
    Many (x * y)
  ;;

  let divI (Many x) (Many y) =
    Many (x / y)
  ;;

  let eqI (Many x : int bang) (Many y : int bang) =
    Many (x = y)
  ;;

  let ltI (Many x : int bang) (Many y : int bang) =
    Many (x < y)
  ;;

  (* EltOp *)
  let addE (Many x) (Many y) =
    Many (x +. y)
  ;;

  let subE (Many x) (Many y) =
    Many (x -. y)
  ;;

  let mulE (Many x) (Many y) =
    Many (x *. y)
  ;;

  let divE (Many x) (Many y) =
    Many (x /. y)
  ;;

  let eqE (Many x : int bang) (Many y : int bang) =
    Many (x = y)
  ;;

  let ltE (Many x : int bang) (Many y : int bang) =
    Many (x < y)
  ;;

  (* Array operations *)
  let set (A arr : z arr) (Many i : int bang) (Many v : float bang)  : z arr = 
    Arr.set arr [|i|] v;
    A arr
  ;;

  let get (A arr : 'a arr) (Many i : int bang) =
    (Many (Arr.get arr [|i|]), A arr)
  ;;

  let share (A arr : 'a arr) : 'a s arr *  'a s arr =
    (A arr, A arr)
  ;;

  let unshare (A arr1 : 'a s arr) (A arr2 : 'a s arr) : 'a arr =
    assert (Base.phys_equal arr1 arr2);
    A arr1
  ;;

  let free (A _ : z arr) =
    ()
  ;;

  (* Owl *)
  let array (Many n : int bang) : z arr =
    A Arr.(empty [| n |])
  ;;

  let copy (A arr : 'a arr) : 'a arr * z arr =
    (A arr, A Arr.(copy arr))
  ;;

  let sin (A arr : z arr) : z arr =
    Arr.sin_ arr;
    A arr
  ;;

  let hypot (A arr1 : z arr) (A arr2 : 'a arr) : z arr * 'a arr =
    Arr.hypot_ arr1 arr2;
    (A arr1, A arr2)
  ;;

  (* BLAS helper *)
  let same_dim_exn read_a read_b =
      let n_a, n_b = Arr.(numel read_a, numel read_b) in
      let () = assert (n_a = n_b) in
      n_a
  ;;

  let conv =
    Bigarray.array1_of_genarray
  ;;

  let inv =
    Bigarray.genarray_of_array1
  ;;

  (* BLAS *)
  let asum : 'a. 'a arr -> 'a arr * float =
    fun (A read) ->
      let result = Cblas.asum (Arr.numel read) (conv read) 1 in
      (A read , result)
  ;;

  let axpy scalar (A read: 'a arr) (A write: z arr) : 'a arr * z arr =
      let n = same_dim_exn read write in
      let () = Cblas.axpy n scalar (conv read) 1 (conv write) 1 in
      (A read, A write)
  ;;

  let dot : 'a 'b. 'a arr -> 'b arr -> (('a arr * 'b arr) * float) = 
    fun (A fst) (A snd) ->
      let n = same_dim_exn fst snd in
      let result = Cblas.dot n (conv fst) 1 (conv snd) 1 in
      ((A fst, A snd), result)
  ;;

  let rotmg : float * float -> float * float -> (float * float) * (float * z arr) =
    fun (d1, d2) (b1, b2) ->
      let (d1, d2, b1, p) = Cblas.rotmg Bigarray.Float64 d1 d2 b1 b2 in
      ((d1, d2), (b1, A (inv p)))
  ;;

  let scal : float -> z arr -> z arr =
    fun scal (A write) ->
      let () = Cblas.scal (Arr.numel write) scal (conv write) 1 in
      A write
  ;;

  let amax : 'a. 'a arr -> int * 'a arr =
    fun (A read) ->
      let result = Cblas.amax (Arr.numel read) (conv read) 1 in
      (result, A read)
  ;;
      
  end
;;

(* end template *)
