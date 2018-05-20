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

(* Matrices. Must resest before every test. *)
let n, k =
  5, 3
;;

let duplicate ~row ~col arr =
  let arr = Owl.Mat.of_array arr row col in
  arr, Owl.Mat.copy arr
;;

let sigma, sigma_copy =
  let sigma, sigma_copy = duplicate ~row:n ~col:n [|
    1.682490; 0.621964; 0.959947; 1.228820; 1.029410;
    0.621964; 0.631446; 0.551902; 0.723342; 0.756674;
    0.959947; 0.551902; 1.100060; 0.908402; 1.032840;
    1.228820; 0.723342; 0.908402; 1.212400; 1.011350;
    1.029410; 0.756674; 1.032840; 1.011350; 1.302410;
  |] in
  let () = assert Owl.Linalg.D.(is_posdef sigma && is_symmetric sigma) in
  sigma, sigma_copy
;;

let h, h_copy =
  let h, h_copy = duplicate ~row:k ~col:n [|
    0.4621110; 0.833041; 0.0395867; 0.529315; 0.241678;
    0.0507828; 0.340120; 0.8726660; 0.836114; 0.571528;
    0.7779080; 0.541655; 0.8691540; 0.286846; 0.265820;
  |] in
  h, h_copy
;;

let mu, mu_copy =
  let mu, mu_copy = duplicate ~row:n ~col:1 [|
    0.8015420;
    0.8585870;
    0.0950306;
    0.8101720;
    0.3491810;
  |] in
  mu, mu_copy
;;

let data, data_copy =
  let data, data_copy = duplicate ~row:k ~col:1 [|
    0.551922;
    0.673854;
    0.259412
  |] in
  data, data_copy
;;

let r, r_copy =
  let r, r_copy = duplicate ~row:k ~col:k [|
    0.880164; 0.676823; 0.802738;
    0.676823; 0.650806; 0.958725;
    0.802738; 0.958725; 1.745970;
  |] in
  let () = assert Owl.Linalg.D.(is_posdef r && is_symmetric r) in
  r, r_copy
;;

let reset () =
  List.iter ~f:(fun (orig, copy) -> Owl.Mat.copy_to copy orig) [
    (sigma, sigma_copy);
    (h, h_copy);
    (mu, mu_copy);
    (r, r_copy);
    (data, data_copy);
  ]
;;

let potrs ~uplo a b =
  let b = Owl.Mat.copy b in
  Owl.Lapacke.potrs ~uplo ~a ~b
;;

let chol_kalman ~sigma ~h ~mu ~r ~data =
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

let owl_kalman ~sigma ~h ~mu ~r ~data =
  let open Owl.Mat in
  let ( * ) = dot in
  let h' = transpose h in
  let sigma_h' = sigma * h' in
  let x = sigma_h' * (inv @@ r + h * sigma_h') in
  let new_mu = mu + x * (h * mu - data) in
  let new_sigma = sigma - x * h * sigma in
  ((sigma, (h, (mu, (r, data)))), (new_mu, new_sigma))
;;

let lt4la_kalman ~sigma ~h ~mu ~r ~data =
    Examples.Kalman.it (M sigma) (M h) (M mu) (M r) (M data)
;;

let cblas_kalman ~n ~k ~sigma ~h ~mu ~r ~data =
  let open Kalman_c_ffi.Bind.C in
  let module Bind = Kalman_c_ffi.Bind in
  let gen, f64 = Ctypes_static.Genarray, Bigarray.float64 in
  let f x = bigarray_start gen x in
  let returned =
    Bind.results n k (f sigma) (f h) (f mu)
      (f @@ Owl.Mat.copy r) (f @@ Owl.Mat.copy data) in
  let new_sigma, new_mu =  getf returned Bind.new_sigma, getf returned Bind.new_mu in
  let new_sigma = bigarray_of_ptr gen [| n; n |]  f64 new_sigma
  and new_mu = bigarray_of_ptr gen [| n; 1 |]  f64 new_mu in
  new_mu, new_sigma
;;

let%expect_test "Kalman" =

  let (_, (chol_mu, chol_sigma)) = reset (); chol_kalman ~sigma ~h ~mu ~r ~data in
  let (_, (owl_mu, owl_sigma)) = reset (); owl_kalman ~sigma ~h ~mu ~r ~data in

  let same x = if x then "same" else " NOT" in

  let (_, (M lt4la_mu, M lt4la_sigma)) = reset (); lt4la_kalman ~sigma ~h ~mu ~r ~data in
  let () = Owl.Mat.(Stdio.printf !"LT4LA - sigma? %{same} | h? %{same} | mu? %{same}\n"
                      (sigma = sigma_copy) (h = h_copy) (mu = mu_copy)) in

  let cblas_mu, cblas_sigma = reset (); cblas_kalman ~n ~k ~sigma ~h ~mu ~r ~data in
  let () = Owl.Mat.(Stdio.printf !"CBLAS - sigma? %{same} | h? %{same} | mu? %{same}\n"
                      (sigma = sigma_copy) (h = h_copy) (mu = mu_copy)) in

  let results = [
    ("Chol", chol_mu, chol_sigma);
    ("Owl", owl_mu, owl_sigma);
    ("LT4LA", lt4la_mu, lt4la_sigma);
    ("CBLAS", cblas_mu, cblas_sigma);
  ] in

  let pair_up x rest = List.map rest ~f:(fun y -> (x,y)) in
  let all_pairs xs = fst @@ List.fold xs ~init:([], []) ~f:(fun (pairs, rest) x ->
    pair_up x rest @ pairs, x :: rest) in

  let pairs = all_pairs results in
  let () = List.iter pairs ~f:(fun ((a, mu_a, sigma_a), (b, mu_b, sigma_b)) ->
    let mu_res = Owl.Mat.(mu_a =~ mu_b) and sigma_res = Owl.Mat.(sigma_a =~ sigma_b) in
    Stdio.printf !"%5s and %5s: Mu (%{same}) Sigma (%{same})\n" a b mu_res sigma_res) in

  Owl.Mat.print ~header:false chol_sigma;
  Owl.Mat.print ~header:false chol_mu;

  [%expect {|
    LT4LA - sigma? same | h? same | mu? same
    CBLAS - sigma? same | h? same | mu? same
    CBLAS and LT4LA: Mu (same) Sigma (same)
    CBLAS and   Owl: Mu (same) Sigma (same)
    CBLAS and  Chol: Mu (same) Sigma (same)
    LT4LA and   Owl: Mu (same) Sigma (same)
    LT4LA and  Chol: Mu (same) Sigma (same)
      Owl and  Chol: Mu (same) Sigma (same)

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
  let to_inv = r + h * sigma_h' in
  let x = sigma_h' * (of_arr @@ Owl.Mat.inv @@ to_arr @@ (eval to_inv; to_inv)) in
  let new_mu = mu + x * (h * mu - data) in
  let new_sigma = sigma - x * h * sigma in
  eval new_sigma;
  eval new_mu;
  ((sigma_, (h_, (mu_, (r_, data_)))), (to_arr new_mu, to_arr new_sigma))
;;

let%expect_test "lazy kalman" =

  let (_, (new_mu, new_sigma)) =
    reset (); lazy_kalman ~sigma ~h ~mu ~r ~data in

  Owl.Mat.print ~header:false new_sigma;
  Owl.Mat.print ~header:false new_mu;

  [%expect {|
     -0.541272 0.00852694  -0.133997   -0.234808  -0.0897324
    0.00852694   -0.17944  0.0357339  -0.0665866   -0.078525
     -0.133997  0.0357339  -0.100837  -0.0120868  0.00196882
     -0.234808 -0.0665866 -0.0120868   -0.227933 -0.00138223
    -0.0897324  -0.078525 0.00196882 -0.00138223    -0.18484


       1.40304
      0.983331
    -0.0586492
       1.06233
      0.313462 |}]
;;

