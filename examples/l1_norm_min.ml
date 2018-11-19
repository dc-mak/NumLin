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
;;

type wrap =
  | W : _ t -> wrap
[@@ocaml.unboxed]
;;

let get : type a . a t -> a = function
  | Owl -> owl
  | LT4LA -> lt4la
;;

let name : wrap -> string = function
  | W Owl -> "Owl"
  | W LT4LA -> "LT4LA"
;;

let all =
  [W Owl; W LT4LA]
;;
