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

let make_arr () =
  let n = 7 in
  A (Owl.Arr.of_array [| 10.; 50.; 60.; 10.; 20.; 30.; 40. |] [| n |])
;;

(* It's not quite but close enough... *)
let%expect_test "one_d_conv" =
  let one_d_conv = Examples.Weighted_avg.it in
  let (A row) : z arr = make_arr () in
  let n = Owl.Arr.numel row in
  let weights : z arr = A (Owl.Arr.(init [| n |] (fun _ -> 1. /. 3.))) in
  let (A row, _) = one_d_conv (Many 1) (Many (n-1)) (Many 10.) (A row) weights in
  Stdio.printf !"%{sexp: float array}" (Owl.Arr.to_array row);
  [%expect {| (10 40 40 30 19.999999999999996 30 40) |}]
;;

(* With inferred fractional capabilities *)
let%expect_test "one_d_conv" =
  let one_d_conv = Examples.Weighted_avg_infer.it in
  let (A row) : z arr = make_arr () in
  let n = Owl.Arr.numel row in
  let weights : z arr = A (Owl.Arr.(init [| n |] (fun _ -> 1. /. 3.))) in
  let (A row, _) = one_d_conv (Many 1) (Many (n-1)) (Many 10.) (A row) weights in
  Stdio.printf !"%{sexp: float array}" (Owl.Arr.to_array row);
  [%expect {| (10 40 40 30 19.999999999999996 30 40) |}]
;;

let%expect_test "sugar" =
  let (f, g) = Examples.Sugar.it in
  let row : _ arr = A Owl.Arr.(ones [| 3 |]) in
  let (A row, Many x) = f row (Many 1, Many (Many 2)) in
  let Many y = g (Many (Many 4)) (Many 1) in
  Stdio.printf !"%{sexp: float array} and %d and %d" (Owl.Arr.to_array row) x y;
  [%expect {| (1 1 1) and 3 and 9 |}]
;;

(* Matrices *)
let matrices () =

  let open Owl in
  (** [sigma] and [r] must be PD and Symmetric *)
  let n, k = 5, 3 in

  let sigma = Mat.of_array [|
    1.682490; 0.621964; 0.959947; 1.228820; 1.029410;
    0.621964; 0.631446; 0.551902; 0.723342; 0.756674;
    0.959947; 0.551902; 1.100060; 0.908402; 1.032840;
    1.228820; 0.723342; 0.908402; 1.212400; 1.011350;
    1.029410; 0.756674; 1.032840; 1.011350; 1.302410;
  |] n n in
  let () = assert Linalg.D.(is_posdef sigma && is_symmetric sigma) in
  let sigma2 = Mat.copy sigma in

  let r = Mat.of_array [|
    0.880164; 0.676823; 0.802738;
    0.676823; 0.650806; 0.958725;
    0.802738; 0.958725; 1.745970;
  |] k k in
  let () = assert Linalg.D.(is_posdef r && is_symmetric r) in

  let h = Mat.of_array [|
    0.4621110; 0.833041; 0.0395867; 0.529315; 0.241678;
    0.0507828; 0.340120; 0.8726660; 0.836114; 0.571528;
    0.7779080; 0.541655; 0.8691540; 0.286846; 0.265820;
  |] k n in
  let h2 = Mat.copy h in

  let mu = Mat.of_array [|
    0.8015420;
    0.8585870;
    0.0950306;
    0.8101720;
    0.3491810;
  |] n 1 in
  let mu2 = Mat.copy mu in

  let data = Mat.of_array [| 0.551922; 0.673854; 0.259412 |] k 1 in

  sigma, sigma2, r, h, h2, mu, mu2, data

;;

let potrs ~uplo a b =
  let b = Owl.Mat.copy b in
  Owl.Lapacke.potrs ~uplo ~a ~b
;;

let chol_kalman sigma h mu r data =
  let open Owl.Mat in
  let ( * ) = dot in
  let h' = transpose h in
  let sigma_h' = sigma * h' in
  let chol = Owl.Linalg.D.chol (r + h * sigma_h') in
  let sigma_h'_inv rest = sigma_h' * potrs ~uplo:'U' chol rest in
  let new_sigma = sigma - sigma_h'_inv (h * sigma) in
  let new_mu = mu + sigma_h'_inv (h * mu - data) in
  ((sigma, (h, (mu, (r, data)))), (new_mu, new_sigma))
;;

let%expect_test "chol kalman" =

  let sigma, _, r, h, _, mu, _, data = matrices () in

  let (_, (new_mu, new_sigma)) =
    chol_kalman sigma h mu r data in

  Owl.Mat.print ~header:false new_sigma;
  Owl.Mat.print ~header:false new_mu;

  [%expect {|
       0.541272 -0.00852694    0.133997   0.234808   0.0897324
    -0.00852694     0.17944  -0.0357339  0.0665866    0.078525
       0.133997  -0.0357339    0.100837  0.0120868 -0.00196882
       0.234808   0.0665866   0.0120868   0.227933  0.00138223
      0.0897324    0.078525 -0.00196882 0.00138223     0.18484


       1.40304
      0.983331
    -0.0586492
       1.06233
      0.313462 |}]
;;

let owl_kalman sigma h mu r data =
  let open Owl.Mat in
  let ( * ) = dot in
  let h' = transpose h in
  let sigma_h' = sigma * h' in
  let x = sigma_h' * (inv @@ r + h * sigma_h') in
  let new_mu = mu + x * (h * mu - data) in
  let new_sigma = sigma - x * h * sigma in
  ((sigma, (h, (mu, (r, data)))), (new_mu, new_sigma))
;;

let%expect_test "owl kalman" =

  let sigma, _, r, h, _, mu, _, data = matrices () in

  let (_, (new_mu, new_sigma)) =
    owl_kalman sigma h mu r data in

  Owl.Mat.print ~header:false new_sigma;
  Owl.Mat.print ~header:false new_mu;

  [%expect {|
       0.541272 -0.00852694    0.133997   0.234808   0.0897324
    -0.00852694     0.17944  -0.0357339  0.0665866    0.078525
       0.133997  -0.0357339    0.100837  0.0120868 -0.00196882
       0.234808   0.0665866   0.0120868   0.227933  0.00138223
      0.0897324    0.078525 -0.00196882 0.00138223     0.18484


       1.40304
      0.983331
    -0.0586492
       1.06233
      0.313462 |}]
;;

module Lazy_Nd =
  Owl.Lazy.Make (Owl.Dense.Ndarray.D)
;;

let lazy_kalman =
  let open Lazy_Nd in
  let sigma = variable ()
  and h = variable ()
  and h' = variable ()
  and mu = variable ()
  and r = variable ()
  and data = variable ()
  in
  fun sigma_ h_ mu_ r_ data_ ->
  let ( := ) = assign_arr in
  sigma := sigma_;
  h := h_ ;
  h' := Owl.Mat.transpose h_;
  mu := mu_;
  r := r_;
  data := data_;
  let ( * ) = dot and ( + ) = add and ( - ) = sub in
  let sigma_h' = sigma * h' in
  let to_inv = r + h * sigma_h' in
  let x = sigma_h' * (of_arr @@ Owl.Mat.inv @@ to_arr @@ (eval to_inv; to_inv)) in
  let new_mu = mu + x * (h * mu - data) in
  let new_sigma = sigma - x * h * sigma in
  eval new_sigma;
  eval new_mu;
  ((sigma_, (h_, (mu_, (r_, data_)))), (to_arr new_mu, to_arr new_sigma))
;;

let%expect_test "lazy kalman" =

  let sigma, _, r, h, _, mu, _, data = matrices () in

  let (_, (new_mu, new_sigma)) =
    lazy_kalman sigma h mu r data in

  Owl.Mat.print ~header:false new_sigma;
  Owl.Mat.print ~header:false new_mu;

  [%expect {|
       0.541272 -0.00852694    0.133997   0.234808   0.0897324
    -0.00852694     0.17944  -0.0357339  0.0665866    0.078525
       0.133997  -0.0357339    0.100837  0.0120868 -0.00196882
       0.234808   0.0665866   0.0120868   0.227933  0.00138223
      0.0897324    0.078525 -0.00196882 0.00138223     0.18484


       1.40304
      0.983331
    -0.0586492
       1.06233
      0.313462 |}]
;;

let%expect_test "lt4la kalman" =

  let sigma, sigma2, r, h, h2, mu, mu2, data = matrices () in

  let ((M sigma, (M h, (M mu, (M _, M _)))), (M new_mu, M new_sigma)) =
    Examples.Kalman.it (M sigma) (M h) (M mu) (M r) (M data) in

  Stdio.printf !"Same: %{sexp:bool}\n" (Owl.Mat.(sigma = sigma2 && h = h2 && mu = mu2));
  Owl.Mat.print ~header:false new_sigma;
  Owl.Mat.print ~header:false new_mu;

  [%expect {|
    Same: true

       0.541272 -0.00852694    0.133997   0.234808   0.0897324
    -0.00852694     0.17944  -0.0357339  0.0665866    0.078525
       0.133997  -0.0357339    0.100837  0.0120868 -0.00196882
       0.234808   0.0665866   0.0120868   0.227933  0.00138223
      0.0897324    0.078525 -0.00196882 0.00138223     0.18484


       1.40304
      0.983331
    -0.0586492
       1.06233
      0.313462 |}]
;;

