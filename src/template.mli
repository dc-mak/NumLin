module Arr = Owl.Dense.Ndarray.D
type z = Z
type 'a s = Succ
type 'a arr = A of Arr.arr [@@unboxed]
type 'a mat = M of Arr.arr [@@unboxed]
type 'a bang = Many of 'a [@@unboxed]
module Ops :
  sig
    val (||) : bool bang -> bool bang lazy_t -> bool bang
    val (&&) : bool bang -> bool bang lazy_t -> bool bang
  end
module Prim :
sig
  val extract : 'a bang -> 'a
  (** Boolean *)
  val not_ : bool bang -> bool bang
  (** Arithmetic *)
  val addI : int bang -> int bang -> int bang
  val subI : int bang -> int bang -> int bang
  val mulI : int bang -> int bang -> int bang
  val divI : int bang -> int bang -> int bang
  val ltI : int bang -> int bang -> bool bang
  val eqI : int bang -> int bang -> bool bang
  val addE : float bang -> float bang -> float bang
  val subE : float bang -> float bang -> float bang
  val mulE : float bang -> float bang -> float bang
  val divE : float bang -> float bang -> float bang
  val eqE : float bang -> float bang -> bool bang
  val ltE : float bang -> float bang -> bool bang
  (** Arrays *)
  val set : z arr -> int bang -> float bang -> z arr
  val get : 'a arr -> int bang -> 'a arr * float bang
  val share : 'a arr -> 'a s arr * 'a s arr
  val unshare : 'a s arr -> 'a s arr -> 'a arr
  val free : z arr -> unit
  (** Owl *)
  val array : int bang -> z arr
  val copy : 'a arr -> 'a arr * z arr
  val sin : z arr -> z arr
  val hypot : z arr -> 'a arr -> 'a arr * z arr
  (** Level 1 BLAS *)
  val asum : 'a arr -> 'a arr * float bang
  val axpy : float bang -> 'a arr -> z arr -> 'a arr * z arr
  val dot : 'a arr -> 'b arr -> ('a arr * 'b arr) * float bang
  val rotmg : float bang * float bang -> float bang * float bang -> (float bang * float bang) * (float bang * z arr)
  val scal : float bang -> z arr -> z arr
  val amax : 'a arr -> 'a arr * int bang
  (* Matrix *)
  val get_mat : 'a mat -> int bang -> int bang -> 'a mat * float bang
  val set_mat : z mat -> int bang -> int bang -> float bang -> z mat
  val share_mat : 'a mat -> 'a s mat * 'a s mat
  val unshare_mat : 'a s mat -> 'a s mat -> 'a mat
  val free_mat : z mat -> unit
  val matrix : int bang -> int bang -> z mat
  val eye : int bang -> z mat
  val copy_mat : 'a mat -> 'a mat * z mat
  val copy_mat_to : 'a mat -> z mat -> 'a mat * z mat
  val size_mat : 'a mat -> 'a mat * (int bang * int bang)
  val transpose : 'a mat -> 'a mat * z mat
  (* Level 3 BLAS/LAPACK *)
  val gemm : float bang -> ('a mat * bool bang) -> ('b mat * bool bang) -> float bang -> z mat -> ('a mat * 'b mat) * z mat
  val symm : bool bang -> float bang -> 'a mat -> 'b mat -> float bang -> z mat -> ('a mat * 'b mat) * z mat
  val gesv : z mat -> z mat -> z mat * z mat
  val posv : z mat -> z mat -> z mat * z mat
  val potrs : 'a mat -> z mat -> 'a mat * z mat
  val syrk : bool bang -> float bang -> 'a mat -> float bang -> z mat -> 'a mat * z mat
end
