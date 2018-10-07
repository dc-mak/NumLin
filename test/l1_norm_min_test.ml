(*
open Base
;;

open Lt4la.Template
;;
*)

let n, k =
  5, 3
;;

let duplicate ~row ~col arr =
  let arr = Owl.Mat.of_array arr row col in
  arr, Owl.Mat.copy arr
;;

let q, q_copy =
  let q, q_copy = duplicate ~row:n ~col:n [|
    1.682490; 0.621964; 0.959947; 1.228820; 1.029410;
    0.621964; 0.631446; 0.551902; 0.723342; 0.756674;
    0.959947; 0.551902; 1.100060; 0.908402; 1.032840;
    1.228820; 0.723342; 0.908402; 1.212400; 1.011350;
    1.029410; 0.756674; 1.032840; 1.011350; 1.302410;
  |] in
  (* let () = assert Owl.Linalg.D.(is_posdef q && is_symmetric q) in *)
  q, q_copy
;;

let u', u'_copy =
  let u', u'_copy = duplicate ~row:k ~col:n [|
    0.4621110; 0.833041; 0.0395867; 0.529315; 0.241678;
    0.0507828; 0.340120; 0.8726660; 0.836114; 0.571528;
    0.7779080; 0.541655; 0.8691540; 0.286846; 0.265820;
  |] in
  u', u'_copy
;;

let i =
  Owl.Mat.eye k
;;

let owl_l1_norm_min ~q ~u' =
  let open Owl.Mat in
  let ( * ) = dot in
  let u (* 5, 3 *) = transpose u' (* 3, 5 *) in
  let q_inv_u (* 5, 3, *) = inv q (* 5, 5 *) * (* 5, 3 *) u in
  q_inv_u (* 5, 3 *) * inv ( i (* 3, 3 *) +  u' (* 3, 5 *) * q_inv_u (* 5, 3 *) ) * u' (* 3, 5 *)
;;

let%expect_test "l1_norm_min" =
  Owl.Mat.print ~header:false (owl_l1_norm_min ~q ~u');
  [%expect {|
     0.163738 -0.0580065 -0.236211 -0.378285 -0.237876
     0.803372   0.920085  0.357245  0.437528  0.240099
     0.463474   0.278753   0.97436  0.367974  0.341635
    -0.421418 0.00261993 0.0966232   0.51514  0.291141
    -0.584246  -0.609541 -0.733115 -0.505515  -0.35633 |}]
;;
