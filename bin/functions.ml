type o_mat =
  Owl.Mat.mat
;;

type 'a l_mat =
  'a Lt4la.Template.mat
;;

type l_z =
  Lt4la.Template.z
;;

type lt4la =
  { f : 'a 'b 'c .
          (sigma:o_mat -> h:o_mat -> mu:o_mat ->
           r:o_mat -> data:o_mat ->
           ('a l_mat * ('b l_mat * ('c l_mat * (l_z l_mat * l_z l_mat)))) *
           (l_z l_mat * l_z l_mat)) }
;;

type _ t =
  | Chol : (sigma:o_mat -> h:o_mat -> mu:o_mat ->
            r:o_mat -> data:o_mat ->
            (o_mat * (o_mat * (o_mat * (o_mat * o_mat)))) *
            (o_mat * o_mat)) t
  | Owl : (sigma:o_mat -> h:o_mat -> mu:o_mat ->
           r:o_mat -> data:o_mat ->
           (o_mat * (o_mat * (o_mat * (o_mat * o_mat)))) *
           (o_mat * o_mat)) t
  | CBLAS :
      (n:int -> k:int ->
       sigma:o_mat -> h:o_mat -> mu:o_mat ->
       r:o_mat -> data:o_mat -> float) t
  | LT4LA : lt4la t
  | TRANSP : lt4la t
;;

type wrap =
  | W : _ t -> wrap
[@@ocaml.unboxed]
;;

(* t to benchmark *)
let get : type a . a t -> a = function
  | Chol -> Test.chol_kalman
  | Owl -> Test.owl_kalman
  | LT4LA -> { f = Test.lt4la_kalman }
  | TRANSP -> { f = Test.transp_kalman }
  | CBLAS ->
    fun ~n ~k ~sigma ~h ~mu ~r ~data ->
      let open Kalman_c_ffi in
      let f x = Bind.C.(bigarray_start Ctypes_static.Genarray x) [@@ocaml.inline] in
      Bind.measure n k (f sigma) (f h) (f mu) (f r) (f data)
;;

let name : wrap -> string = function
  | W Chol -> "Chol"
  | W Owl -> "Owl"
  | W LT4LA -> "LT4LA"
  | W TRANSP -> "TRANSP"
  | W CBLAS -> "CBLAS"
;;


let all =
  [W Owl; W Chol; W LT4LA; W TRANSP; W CBLAS]
;;
