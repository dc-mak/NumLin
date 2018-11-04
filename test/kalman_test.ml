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

(* Matrices. Must reset before every test. *)
let reset () =
  List.iter ~f:(fun (orig, copy) -> Owl.Mat.copy_ copy ~out:orig) [
    (sigma, sigma_copy);
    (h, h_copy);
    (mu, mu_copy);
    (r, r_copy);
    (data, data_copy);
  ]
;;

let owl_kalman ~sigma ~h ~mu ~r ~data =
  let open Owl.Mat in
  let ( * ) = dot in
  let h' = transpose h in
  let sigma_h' = sigma * h' in
  let x = sigma_h' * (inv @@ r + h * sigma_h') in
  let new_mu = mu + x * (h * mu - data) in
  let new_sigma = sigma - x * h * sigma in
  new_sigma, new_mu
;;

let lt4la_kalman ~sigma ~h ~mu ~r ~data =
  Examples.Kalman.it (M sigma) (M h) (M mu) (M r) (M data)
;;

let cblas_kalman ~n ~k ~sigma ~h ~mu ~r ~data =
  let open Kalman_c_ffi.Bind.C in
  let module Bind = Kalman_c_ffi.Bind in
  let gen, f64 = Ctypes_static.Genarray, Bigarray.float64 in
  let f x = bigarray_start gen x [@@ocaml.inline] in
let new_sigma = Bind.result n k (f sigma) (f h) (f mu) (f r) (f data) in
let new_sigma = bigarray_of_ptr gen [| n; n |]  f64 new_sigma in
new_sigma
;;

let%expect_test "Kalman" =

  let same x = if x then "same" else " NOT" in

  let lt4la_sigma, lt4la_mu =
    reset ();
    let (_, (M lt4la_sigma, (M lt4la_mu, _))) =
      lt4la_kalman ~sigma ~h ~mu ~r ~data in
    lt4la_sigma, Owl.Mat.copy lt4la_mu in
  let () = Owl.Mat.(Stdio.printf !"LT4LA - sigma? %{same} | h? %{same}\n"
                      (sigma = sigma_copy) (h = h_copy)) in

  let owl_sigma, owl_mu =
    reset ();
    owl_kalman ~sigma ~h ~mu ~r ~data in

  let cblas_sigma, cblas_mu =
    reset ();
    let cblas_sigma = cblas_kalman ~n ~k ~sigma ~h ~mu ~r ~data in
    cblas_sigma, Owl.Mat.copy mu in
  let () = Owl.Mat.(Stdio.printf !"CBLAS - sigma? %{same} | h? %{same}\n"
                      (sigma = sigma_copy) (h = h_copy)) in

  let results = [
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

  Owl.Mat.print ~header:false cblas_sigma;
  Owl.Mat.print ~header:false cblas_mu;

  [%expect {|
    LT4LA - sigma? same | h? same
    CBLAS - sigma? same | h? same
    CBLAS and LT4LA: Mu (same) Sigma (same)
    CBLAS and   Owl: Mu (same) Sigma (same)
    LT4LA and   Owl: Mu (same) Sigma (same)

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
  let sigma = var_arr "sigma"
  and h = var_arr "h"
  and h' = var_arr "h'"
  and mu = var_arr "mu"
  and r = var_arr "r"
  and data = var_arr "data"
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
    let x = sigma_h' * (inv @@ r + h * sigma_h') in
    let new_mu = mu + x * (h * mu - data) in
    let new_sigma = sigma - x * h * sigma in
    let graph =
      let input = Array.map ~f:arr_to_node [| sigma; h; h'; mu; r; data |] in
      let output = Array.map ~f:arr_to_node [| new_mu; new_sigma |] in
      make_graph ~input ~output "lazy_kalman" in
    Owl_io.write_file "lazy_kalman.dot" @@ graph_to_dot graph;
    eval_graph graph;
    (unpack_arr new_sigma, unpack_arr new_mu)
;;

let%expect_test "lazy kalman" =

  let lazy_sigma, lazy_mu =
    reset (); lazy_kalman ~sigma ~h ~mu ~r ~data in

  Owl.Mat.print ~header:false lazy_sigma;
  Owl.Mat.print ~header:false lazy_mu;

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
