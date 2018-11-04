(* Dhruv Makwana *)
(* LT4LA Compiler Output Header *)
(* ---------------------------- *)
(* This file is a header/wrapper around Owl's CBLAS bindings. *)

(* begin template *)

module Arr =
  Owl.Dense.Ndarray.D
;;

module Cblas =
  Owl_cblas_basic
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
    match Arr.shape mat with
    | [| rows; cols |] -> if transp then (cols, rows) else (rows, cols)
    |  _ -> raise (Invalid_argument "Not a matrix!")
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
    let () = Arr.copy_ read ~out:write in
    (M read, M write)
  ;;

  let size_mat (type a) (M mat : a mat) =
    let (rows, cols) = dim mat in
    (M mat, (Many rows, Many cols))
  ;;

  let transpose (type a) (M mat : a mat) =
    (M mat, M (Owl.Mat.transpose mat))
  ;;

  let matrix (Many rows) (Many cols) : z mat =
    M (Arr.empty [| rows; cols |])
  ;;

  let eye (Many n) : z mat =
    M (Owl.Mat.eye n)
  ;;

  (* Level 3 BLAS/LAPACK *)

  let mult_dims (a, transp_a) (b, transp_b) c =
    let (m1, n1) = dim ~transp:transp_a a
    and (n2, k1) = dim ~transp:transp_b b
    and (m2, k2) = dim c in
    let () = assert (m1 = m2 && n1 = n2 && k1 = k2) in
    (m1, n1, k1)
  ;;

  let conv x =
    let (m, n) = dim x in
    Bigarray.reshape_1 x (m * n)
  ;;

  let symm (Many flip) (Many alpha) (M a) (M b) (Many beta) (M c) =
    let side, m, n, lda, ldb, ldc =
      if flip then
        let m, k, n = mult_dims (b, false) (a, false) c in
        let () = assert (k = n) (* snd/a is square *) in
        Cblas.CblasRight, m, n, n, k, n
      else
        let m, k, n = mult_dims (a, false) (b, false) c in
        let () = assert (m = k) (* fst/a is square *) in
        Cblas.CblasLeft,  m, n, k, n, n in
    let () = Cblas.(symm CblasRowMajor side CblasUpper
                      m n
                      alpha (conv a) lda (conv b) ldb
                      beta (conv c) ldc) in
    ((M a, M b), M c)
  ;;

  let gemm (type a b)
        (Many alpha) ((M a : a mat), Many tr_a) ((M b : b mat), Many tr_b)
        (Many beta) (M c : z mat) =
    let (m, k, n) = mult_dims (a, tr_a) (b, tr_b) c in
    let _ = Cblas.(gemm CblasRowMajor
                     (if tr_a then CblasTrans else CblasNoTrans)
                     (if tr_b then CblasTrans else CblasNoTrans)
                     m n k
                     alpha
                     (conv a) (if tr_a then m else k)
                     (conv b) (if tr_b then k else n)
                     beta (conv c) n) in
    ((M a, M b), M c)
  ;;


  let gesv (M a : z mat) (M b : z mat) =
    (* FIXME: To re-use a/use getrf/getrs we need the ignored [_ipiv] parameter *)
    let (a',b', _ipiv) = Owl_lapacke.(gesv ~a ~b) in
    let () = assert (Base.(phys_equal a a' && phys_equal b b')) in
    (M a, M b)
  ;;

  let posv (M a : z mat) (M b : z mat) =
    let (a',b') = Owl_lapacke.(posv ~uplo:'U' ~a ~b) in
    let () = assert (Base.(phys_equal a a' && phys_equal b b')) in
    (M a, M b)
  ;;

  (* Compute X = B A^-1 using A'X' = B' / X = (A'^-1 B')'
     But make it so transposing is just a Row/Column order flipping *)
  let posv_flip (M a : z mat) (M b : z mat) =
    let (n,_) = Owl.Mat.shape a in
    let (k, n') = Owl.Mat.shape b in
    let () = assert (n = n') in
    let lda = n in
    let ldb = n and nrhs = k in
    let layout = 102 (* Interpret as Fortran layout *) in
    let conv = Ctypes.bigarray_start  Ctypes_static.Genarray in
    let ret = Owl_lapacke_generated.dposv ~layout ~uplo:'U'
                    ~n ~nrhs ~a:(conv a) ~lda ~b:(conv b) ~ldb in
    let () =
      if ret < 0 then
        raise @@ Invalid_argument (Printf.sprintf "posv_flip: parameter %d illegal value" (-ret))
      else if ret > 0 then
        raise @@ Invalid_argument (Printf.sprintf "posv_flip: leading minor order %d not pos. def." ret)
    in
    (M a, M b)
  ;;

  let potrs (type a) (M a : a mat) (M b : z mat) =
    let b' = Owl_lapacke.(potrs ~uplo:'U' ~a ~b) in
    let () = assert (Base.phys_equal b b') in
    (M a, M b)
  ;;

  let syrk (type a) (Many trans) (Many alpha) (M a : a mat) (Many beta) (M c : z mat) =
    let () = Owl_cblas.syrk ~uplo:Cblas.CblasUpper ~trans ~alpha ~beta ~a ~c in
    (M a, M c)
  ;;

end
;;

module Ops =
struct
  let (||) (Many x) y = if x then Many true else Lazy.force y
  let (&&) (Many x) y = if x then Lazy.force y else Many false
end

(* end template *)
