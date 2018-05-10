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

type 'a mat =
    M of Arr.arr
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

  let eqE (Many x : float bang) (Many y : float bang) =
    Many (x = y)
  ;;

  let ltE (Many x : float bang) (Many y : float bang) =
    Many (x < y)
  ;;

  (* Array operations *)
  let set (A arr : z arr) (Many i) (Many v) = 
    Arr.set arr [|i|] v;
    A arr
  ;;

  let get (A arr) (Many i) =
    (A arr, Many (Arr.get arr [|i|]))
  ;;

  let share (type a) (A arr : a arr) : a s arr  * a s arr =
    (A arr, A arr)
  ;;

  let unshare (type a) (A arr1 : a s arr) (A arr2 : a s arr) : a arr =
    assert (Base.phys_equal arr1 arr2);
    A arr1
  ;;

  let free (A _ : z arr) =
    ()
  ;;

  (* Owl *)
  let array (Many n) : z arr =
    A Arr.(empty [| n |])
  ;;

  let copy (type a) (A arr : a arr) : a arr * z arr =
    (A arr, A Arr.(copy arr))
  ;;

  let sin (A arr : z arr) : z arr =
    Arr.sin_ arr;
    A arr
  ;;

  let hypot (type a) (A write : z arr) (A read : a arr) : a arr * z arr =
    Arr.hypot_ write read;
    (A read, A write)
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

  (* Level 1 BLAS *)
  let asum (type a) (A read : a arr) =
    let result = Cblas.asum (Arr.numel read) (conv read) 1 in
    (A read , Many result)
  ;;

  let axpy (type a) (Many scalar) (A read : a arr) (A write : z arr) =
    let n = same_dim_exn read write in
    let () = Cblas.axpy n scalar (conv read) 1 (conv write) 1 in
    (A read, A write)
  ;;

  let dot (type a b) (A fst : a arr) (A snd : b arr) = 
    let n = same_dim_exn fst snd in
    let result = Cblas.dot n (conv fst) 1 (conv snd) 1 in
    ((A fst, A snd), Many result)
  ;;

  let rotmg (Many d1, Many d2) (Many b1, Many b2) =
    let (d1, d2, b1, p) = Cblas.rotmg Bigarray.Float64 d1 d2 b1 b2 in
    ((Many d1, Many d2), (Many b1, (A (inv p) : z arr)))
  ;;

  let scal (Many scal) (A write : z arr) : z arr =
    let () = Cblas.scal (Arr.numel write) scal (conv write) 1 in
    A write
  ;;

  let amax (type a) (A read : a arr) =
    let result = Cblas.amax (Arr.numel read) (conv read) 1 in
    (A read, Many result)
  ;;

  (* Level 3 BLAS/LAPACK helpers *)
  let dim ?(transp=false) mat =
    let (rows, cols) as result = Arr.(row_num mat, col_num mat) in
    if transp then (cols, rows) else result
  ;;

  let mult_dims (a, transp_a) (b, transp_b) c =
    let (m1, n1) = dim ~transp:transp_a a
    and (n2, k1) = dim ~transp:transp_b b
    and (m2, k2) = dim c in
    let () = assert (m1 = m2 && n1 = n2 && k1 = k2) in
    (m1, n1, k1)
  ;;

  (* Matrix *)
  let get_mat (type a) (M mat : a mat) (Many i : int bang) (Many j : int bang) =
    (M mat, Many (Arr.get mat [| i ; j |]))
  ;;

  let set_mat (M mat : z mat) (Many i) (Many j) (Many v)  : z mat = 
    Arr.set mat [| i ; j |] v;
    M mat
  ;;

  let share_mat (type a) (M a : a mat) : a s mat * a s mat =
    (M a, M a)
  ;;

  let unshare_mat (type a) (M a : a s mat) (M a' : a s mat) : a mat =
    let () = assert (Base.phys_equal a a') in
    M a
  ;;

  let free_mat (M _ :  z mat) =
    ()
  ;;

  let copy_mat (type a) (M mat : a mat) : a mat * z mat =
    (M mat, M (Arr.copy mat))
  ;;

  let copy_mat_to (type a) (M read) (M write) : a mat * z mat =
    let () = Arr.copy_to read write in
    (M read, M write)
  ;;

  let size_mat (type a) (M mat : a mat) =
    let (rows, cols) = dim mat in
    (M mat, (Many rows, Many cols))
  ;;

  let matrix (Many rows) (Many cols) : z mat =
    M (Arr.empty [| rows; cols |])
  ;;

  (* Level 3 BLAS/LAPACK *)

  let gemm (type a b)
        (Many alpha) ((M a : a mat), Many tr_a) ((M b : b mat), Many tr_b)
        (Many beta) (M c : z mat) =
    let (m, k, n) = mult_dims (a, tr_a) (b, tr_b) c in
    let _ = Cblas.(gemm CblasRowMajor
                     (if tr_a then CblasTrans else CblasNoTrans)
                     (if tr_b then CblasTrans else CblasNoTrans)
                     m n k
                     alpha (conv a) (if tr_a then k else m) (conv b) (if tr_b then n else k)
                     beta (conv c) m) in
    ((M a, M b), M c)
  ;;

  let symm (Many flip) (Many alpha) (M a) (M b) (Many beta) (M c) = 
    let (m, k, n) = mult_dims (a, false) (b, false) c in
    let () = Cblas.(symm CblasRowMajor (if flip then CblasRight else CblasLeft) CblasUpper
                      m n
                      alpha (conv a) m (conv b) k
                      beta (conv c) m) in
    ((M a, M b), M c)
  ;;

  let posv (M a : z mat) (M b : z mat) =
    let (a',b') = Owl_lapacke.(posv ~uplo:'U' ~a ~b) in
    let () = assert (Base.(phys_equal a a' && phys_equal b b')) in
    (M a, M b)
  ;;

  let potrs (type a) (M a : a mat) (M b : z mat) =
    let b' = Owl_lapacke.(potrs ~uplo:'U' ~a ~b) in
    let () = assert (Base.phys_equal b b') in
    (M a, M b)
  ;;

end
;;

(* end template *)
