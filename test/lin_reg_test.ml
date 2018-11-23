open Base
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

let%expect_test "lin_reg" =

  let naive_res = Examples.Lin_reg.naive ~x ~y in
  let results = Examples.Lin_reg.[
    ("Naive", naive_res);
    ("NumPy", numpy ~x ~y);
    ("Owl", (owl ~x ~y).(0));
    ("LT4LA", let _, M res = lt4la ~x ~y in res);
  ] in
  let () = assert Owl.Mat.(x = x_copy && y = y_copy) in

  let pair_up x rest = List.map rest ~f:(fun y -> (x,y)) in
  let all_pairs xs = fst @@ List.fold xs ~init:([], []) ~f:(fun (pairs, rest) x ->
    pair_up x rest @ pairs, x :: rest) in

  let pairs = all_pairs results in
  let same x = if x then "same" else " NOT" in
  let () = List.iter pairs ~f:(fun ((a, res_a), (b, res_b)) ->
    Stdio.printf !"%5s and %5s: (%{same})\n" a b Owl.Mat.(res_a =~ res_b)) in

  Owl.Mat.print ~header:false naive_res;

  [%expect {|
    LT4LA and   Owl: (same)
    LT4LA and NumPy: (same)
    LT4LA and Naive: (same)
      Owl and NumPy: (same)
      Owl and Naive: (same)
    NumPy and Naive: (same)


    0.2
    0.3
    0.5 |}]
;;
(*
*)
