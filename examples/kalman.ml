open Base
;;

let owl ~sigma ~h ~mu ~r ~data =
  let open Owl.Mat in
  let ( * ) = dot in
  let h' = transpose h in
  let sigma_h' = sigma * h' in
  let x = sigma_h' * (inv @@ r + h * sigma_h') in
  let new_mu = mu + x * (h * mu - data) in
  let new_sigma = sigma - x * h * sigma in
  new_sigma, new_mu
;;

let numpy, numpy_measure =
  let kalman_bytecode =
    Python_compile.f `Exec ~optimize:`Normal ~filename:"kalman.py" ~source:"
import gc
import resource
import numpy as np
from numpy.linalg import inv

def kalman(sigma, h, mu, r, data):
    sigma_hT = np.dot(sigma, h.T)
    x = np.dot(sigma_hT, inv(r + np.dot(h, sigma_hT)))
    new_mu = mu + np.dot(x, np.dot(h, mu) - data)
    new_sigma = sigma - np.dot(np.dot(x,h), sigma)
    return (new_sigma, new_mu)

def measure(sigma, h, mu, r, data):
    gc.collect()
    start = resource.getrusage(resource.RUSAGE_SELF).ru_utime
    (new_sigma, new_mu) = kalman(sigma, h, mu, r, data)
    end = resource.getrusage(resource.RUSAGE_SELF).ru_utime
    return ((end - start) * 1000000.0)
" in
  let kalman_module = Py.Import.exec_code_module "kalman" kalman_bytecode in
  let measure = Py.Module.get_function kalman_module "measure" in
  let kalman = Py.Module.get_function kalman_module "kalman" in
  kalman, measure
;;

let numpy ~sigma ~h ~mu ~r ~data =
  let [| new_sigma; new_mu |] =
    [| sigma; h; mu; r; data |]
    |> Array.map ~f:Numpy.of_bigarray
    |> numpy
    |> Py.Tuple.to_array
    |> Array.map ~f:(Numpy.to_bigarray Bigarray.float64 Bigarray.c_layout)
  [@@ocaml.warning "-8" (* inexhaustive pattern match *) ] in
  (new_sigma, new_mu)
;;

let numpy_measure ~sigma ~h ~mu ~r ~data =
  [| sigma; h; mu; r; data |]
  |> Array.map ~f:Numpy.of_bigarray
  |> numpy_measure
  |> Py.Float.to_float
;;

let numlin ~sigma ~h ~mu ~r ~data =
  Gen.Kalman.it (M sigma) (M h) (M mu) (M r) (M data)
;;

let cblas ~n ~k ~sigma ~h ~mu ~r ~data =
  let open Kalman_c_ffi.Bind.C in
  let module Bind = Kalman_c_ffi.Bind in
  let gen, f64 = Ctypes_static.Genarray, Bigarray.float64 in
  let f x = bigarray_start gen x [@@ocaml.inline] in
  let new_sigma = Bind.result n k (f sigma) (f h) (f mu) (f r) (f data) in
  let new_sigma = bigarray_of_ptr gen [| n; n |]  f64 new_sigma in
  new_sigma
;;

let cblas_measure ~n ~k ~sigma ~h ~mu ~r ~data =
  let open Kalman_c_ffi in
  let f x = Bind.C.(bigarray_start Ctypes_static.Genarray x) [@@ocaml.inline] in
  Bind.measure n k (f sigma) (f h) (f mu) (f r) (f data)
;;

let lazy_ =
  let module Lazy_Nd = Owl.Lazy.Make (Owl.Dense.Ndarray.D) in
  let open Lazy_Nd in
  let sigma = var_arr "sigma"
  and h = var_arr "h"
  and h' = var_arr "h'"
  and mu = var_arr "mu"
  and r = var_arr "r"
  and data = var_arr "data"
  in
  fun ~sigma:sigma_ ~h:h_ ~mu:mu_ ~r:r_ ~data:data_ ->
    let ( := ) = assign_arr in
    sigma := sigma_;
    h := h_ ;
    h' := Owl.Mat.transpose h_;
    mu := mu_;
    r := r_;
    data := data_;
    let ( * ) = dot and ( + ) = add and ( - ) = sub in
    let sigma_h' = sigma * h' in
    let x = sigma_h' * (inv @@ r + h * sigma_h') in
    let new_mu = mu + x * (h * mu - data) in
    let new_sigma = sigma - x * h * sigma in
    let graph =
      let input = Array.map ~f:arr_to_node [| sigma; h; h'; mu; r; data |] in
      let output = Array.map ~f:arr_to_node [| new_mu; new_sigma |] in
      make_graph ~input ~output "lazy_kalman" in
    Owl_io.write_file "lazy_kalman.dot" @@ graph_to_dot graph;
    eval_graph graph;
    (unpack_arr new_sigma, unpack_arr new_mu)
;;

(* Uniform interface *)
type o_mat =
  Owl.Mat.mat
;;

type 'a l_mat =
  'a Numlin.Template.mat
;;

type l_z =
  Numlin.Template.z
;;

type 'a from_input =
  sigma:o_mat -> h:o_mat -> mu:o_mat -> r:o_mat -> data:o_mat -> 'a
;;

type numlin =
  { f : 'a 'b.
          (('a l_mat * 'b l_mat) *
           (l_z l_mat * (l_z l_mat * (l_z l_mat * l_z l_mat)))) from_input }
;;

type _ t =
  | NumPy : (float from_input * (o_mat * o_mat) from_input) t
  | Owl : ((o_mat * o_mat) from_input) t
  | CBLAS : ((n:int -> k:int -> float from_input) * (n:int -> k:int -> o_mat from_input)) t
  | NumLin : numlin t
;;

type wrap =
  | W : _ t -> wrap
[@@ocaml.unboxed]
;;

let get : type a . a t -> a = function
  | NumPy -> (numpy_measure, numpy)
  | Owl -> owl
  | NumLin -> { f = numlin }
  | CBLAS -> (cblas_measure, cblas)
;;

let name : wrap -> string = function
  | W NumPy -> "NumPy"
  | W Owl -> "Owl"
  | W NumLin -> "Numlin"
  | W CBLAS -> "CBLAS"
;;

let all =
  [W CBLAS; W NumLin; W Owl; W NumPy]
;;

