open Base
;;

open Lt4la.Template
;;

let%expect_test "factorial" =
  let fact = Examples.Gen.Factorial.it in
  for i = -2 to 10 do
    Stdio.printf "%d\n" @@ Prim.extract @@ fact (Many i);
  done;
  [%expect {|
    1
    1
    1
    1
    2
    6
    24
    120
    720
    5040
    40320
    362880
    3628800 |}]
;;

let%expect_test "sum_array" =
  let sum_array = Examples.Gen.Sum_array.it in
  let n = 20 in
  let row : z arr = A Owl.Arr.(mapi (fun x _ -> Int.to_float x) (empty [| n |])) in
  let (_, Many sum) = sum_array (Many 0) (Many n) (Many 0.) row in
  Stdio.printf "%f\n" sum;
  [%expect {| 190.000000 |}]
;;

let make_arr () =
  let n = 7 in
  A (Owl.Arr.of_array [| 10.; 50.; 60.; 10.; 20.; 30.; 40. |] [| n |])
;;

(* It's not quite but close enough... *)
let%expect_test "one_d_conv" =
  let one_d_conv = Examples.Gen.Weighted_avg.it in
  let (A row) : z arr = make_arr () in
  let n = Owl.Arr.numel row in
  let weights : z arr = A (Owl.Arr.(init [| n |] (fun _ -> 1. /. 3.))) in
  let (_, A row) = one_d_conv (Many 1) (Many (n-1)) (Many 10.) (A row) weights in
  Stdio.printf !"%{sexp: float array}" (Owl.Arr.to_array row);
  [%expect {| (10 40 40 30 19.999999999999996 30 40) |}]
;;

(* With inferred fractional capabilities *)
let%expect_test "one_d_conv" =
  let one_d_conv = Examples.Gen.Weighted_avg_infer.it in
  let (A row) : z arr = make_arr () in
  let n = Owl.Arr.numel row in
  let weights : z arr = A (Owl.Arr.(init [| n |] (fun _ -> 1. /. 3.))) in
  let (_ ,A row) = one_d_conv (Many 1) (Many (n-1)) (Many 10.) (A row) weights in
  Stdio.printf !"%{sexp: float array}" (Owl.Arr.to_array row);
  [%expect {| (10 40 40 30 19.999999999999996 30 40) |}]
;;

let%expect_test "sugar" =
  let (f, g) = Examples.Gen.Sugar.it in
  let row : _ arr = A Owl.Arr.(ones [| 3 |]) in
  let (A row, Many x) = f row (Many 1, Many (Many 2)) in
  let Many y = g (Many (Many 4)) (Many 1) in
  Stdio.printf !"%{sexp: float array} and %d and %d" (Owl.Arr.to_array row) x y;
  [%expect {| (1 1 1) and 3 and 9 |}]
;;

let%test "square" =
  let square = Examples.Gen.Square.it in
  let rand = Owl.Mat.uniform 5 5 in
  let (_, M answer) = square (M rand) in
  let answer2 = Owl.Mat.dot rand rand in
  Owl.Mat.(answer = answer2)
;;

let%test_module "Kalman" = 
  (module Kalman_test)
;;

let%test_module "L1_norm_min" =
  (module L1_norm_min_test)
;;

let%test_module "Lin_reg" =
  (module Lin_reg_test)
;;

