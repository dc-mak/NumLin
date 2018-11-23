open Base
;;

let numpy, numpy_measure =
  let l1_norm_min_bytecode =
    Python_compile.f `Exec ~optimize:`Normal ~filename:"l1_norm_min.py" ~source:"
import gc
import resource
import numpy as np
from numpy.linalg import inv

def l1_norm_min(q, u):
    q_inv_u = np.dot(inv(q), u)
    i = np.identity(u.shape[1])
    return np.dot(np.dot(q_inv_u, inv(i + np.dot(u.T, q_inv_u))), u.T)

def measure(q, u):
    gc.collect()
    start = resource.getrusage(resource.RUSAGE_SELF).ru_utime
    result = l1_norm_min(q, u)
    end = resource.getrusage(resource.RUSAGE_SELF).ru_utime
    return ((end - start) * 1000000.0)
" in
  let l1_norm_min_module = Py.Import.exec_code_module "l1_norm_min" l1_norm_min_bytecode in
  let measure = Py.Module.get_function l1_norm_min_module "measure" in
  let l1_norm_min = Py.Module.get_function l1_norm_min_module "l1_norm_min" in
  l1_norm_min, measure
;;

let numpy ~q ~u =
  [| q; u |]
  |> Array.map ~f:Numpy.of_bigarray
  |> numpy
  |> Numpy.to_bigarray Bigarray.float64 Bigarray.c_layout
;;

let numpy_measure ~q ~u =
  [| q; u |]
  |> Array.map ~f:Numpy.of_bigarray
  |> numpy_measure
  |> Py.Float.to_float
;;

let owl ~q ~u =
  let open Owl.Mat in
  let ( * ) = dot in
  let u' = transpose u in
  let q_inv_u = inv q * u in
  let i = Owl.Mat.(eye @@ snd @@ shape u) in
  q_inv_u * inv ( i +  u' * q_inv_u ) * u'
;;

let lt4la ~q ~u =
  let Lt4la.Template.M x = Gen.L1_norm_min.it (M q) (M u) in
  x
;;

type o_mat =
  Owl.Mat.mat
;;

type _ t =
  | Owl : (q:o_mat -> u:o_mat -> o_mat) t
  | LT4LA : (q:o_mat -> u:o_mat -> o_mat) t
  | NumPy : ((q:o_mat -> u:o_mat -> float) * (q:o_mat -> u:o_mat -> o_mat)) t
;;

type wrap =
  | W : _ t -> wrap
[@@ocaml.unboxed]
;;

let get : type a . a t -> a = function
  | Owl -> owl
  | LT4LA -> lt4la
  | NumPy -> (numpy_measure, numpy)
;;

let name : wrap -> string = function
  | W Owl -> "Owl"
  | W LT4LA -> "LT4LA"
  | W NumPy -> "NumPy"
;;

let all =
  [W NumPy; W Owl; W LT4LA]
;;
