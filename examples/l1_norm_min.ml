let owl ~q ~u =
  let open Owl.Mat in
  let ( * ) = dot in
  let u' = transpose u in
  let q_inv_u = inv q * u in
  let i = Owl.Mat.(eye @@ snd @@ shape u) in
  q_inv_u * inv ( i +  u' * q_inv_u ) * u'
;;

let lt4la ~q ~u =
  Gen.L1_norm_min.it (M q) (M u)
;;
