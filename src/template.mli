module Arr = Owl.Dense.Ndarray.D
type z = Z
type 'a s = Succ
type 'a arr = A of Arr.arr [@@unboxed]
type 'a bang = Many of 'a [@@unboxed]
module Prim :
sig
  val extract : 'a bang -> 'a
  val and_ : bool bang -> bool bang -> bool bang
  val or_ : bool bang -> bool bang -> bool bang
  val not_ : bool bang -> bool bang
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
  val eqE : int bang -> int bang -> bool bang
  val ltE : int bang -> int bang -> bool bang
  val set : z arr -> int bang -> float bang -> z arr
  val get : 'a arr -> int bang -> float bang * 'a arr
  val share : 'a arr -> 'a s arr * 'a s arr
  val unshare : 'a s arr -> 'a s arr -> 'a arr
  val free : z arr -> unit
  val array : int bang -> z arr
  val copy : 'a arr -> 'a arr * z arr
  val sin : z arr -> z arr
  val hypot : z arr -> 'a arr -> z arr * 'a arr
  val asum : 'a arr -> 'a arr * float
  val axpy : float -> 'a arr -> z arr -> 'a arr * z arr
  val dot : 'a arr -> 'b arr -> ('a arr * 'b arr) * float
  val rotmg :
    float * float -> float * float -> (float * float) * (float * z arr)
  val scal : float -> z arr -> z arr
  val amax : 'a arr -> int * 'a arr
end
