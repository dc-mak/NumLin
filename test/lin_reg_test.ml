open Base [@@warning "-33"]
;;

open Lt4la.Template
;;

let n, k =
  5, 3
;;

let duplicate ~row ~col arr =
  let arr = Owl.Mat.of_array arr row col in
  arr, Owl.Mat.copy arr
;;

let x, x_copy =
  duplicate ~row:n ~col:k [|
    0.4621110; 0.0507828; 0.7779080;
    0.833041 ; 0.340120 ; 0.541655 ;
    0.0395867; 0.8726660; 0.8691540;
    0.529315 ; 0.836114 ; 0.286846 ;
    0.241678 ; 0.571528 ; 0.265820 ;
  |]
;;

let params =
  Owl.Mat.of_array [|
    0.2;
    0.3;
    0.5;
  |] k 1
;;

let y, y_copy =
  let y = Owl.Mat.(x *@ params ) in
  y, Owl.Mat.copy y
;;

let naive_lin_reg ~x ~y =
  let open Owl.Mat in
  let ( * ) = dot in
  let x' = transpose x in
  inv (x' * x) * x' * y
;;

let owl_lin_reg ~x ~y =
  Owl.Regression.D.ols x y
;;

let lt4la_lin_reg ~x ~y =
  Examples.Lin_reg.it (M x) (M y)
;;

let%expect_test "l1_norm_min" =

  let naive = naive_lin_reg ~x ~y in
  let [| owl |] = owl_lin_reg ~x ~y [@@warning "-8"] in
  let (M lt4la, _ ) = lt4la_lin_reg ~x ~y in
  let () = assert Owl.Mat.(x = x_copy && y = y_copy) in
  let (=) = Owl.Mat.(=~) in
  Stdio.printf "Naive and Owl: %b\n\
                Naive and LT4LA: %b\n\
                Owl and LT4LA: %b\n"
    (naive = owl) (naive = lt4la) (owl = lt4la);
  Owl.Mat.print ~header:false naive;

  [%expect {|
    Naive and Owl: true
    Naive and LT4LA: true
    Owl and LT4LA: true


    0.2
    0.3
    0.5 |}]
;;
