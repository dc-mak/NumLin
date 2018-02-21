open Base
;;

open Lt4la.Template
;;

let%expect_test "factorial" =
  let fact = Examples.Factorial.it in
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
  let sum_array = Examples.Sum_array.it in
  let n = 20 in
  let row : z arr = A Owl.Arr.(mapi (fun x _ -> Int.to_float x) (empty [| n |])) in
  let (Many sum, _) = sum_array (Many 0) (Many n) (Many 0.) row in
  Stdio.printf "%f\n" sum;
  [%expect {| 190.000000 |}]
;;

(* It's not quite but close enough... *)
let%expect_test "one_d_conv" =
  let one_d_conv = Examples.Weighted_avg.it in
  let n = 7 in
  let f i _ = match i with
    | 0 -> 10. | 1 -> 50. | 2 -> 60. | 3 -> 10.
    | 4 -> 20. | 5 -> 30. | 6 -> 40. | _ -> assert false in
  let row : z arr = A Owl.Arr.(mapi f (empty [| n |])) in
  let f _ _ = 1. /. 3. in
  let weights : z arr = A Owl.Arr.(mapi f @@ empty [| n |]) in
  let (A row, _) = one_d_conv (Many 1) (Many (n-1)) (Many 10.) row weights in
  Stdio.printf !"%{sexp: float array}" (Owl.Arr.to_array row);
  [%expect {| (10 40 40 30 19.999999999999996 30 40) |}]
;;
