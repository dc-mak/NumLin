open Base
;;

open Numlin.Template
;;

module Ex =
  Examples.Kalman
;;

(* Test set-up *)
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

let%expect_test "Kalman" =

  let same x = if x then "same" else " NOT" in

  (* NumLin *)
  let numlin_sigma, numlin_mu =
    reset ();
    let (_, (M numlin_sigma, (M numlin_mu, _))) =
      Ex.numlin ~sigma ~h ~mu ~r ~data in
    numlin_sigma, Owl.Mat.copy numlin_mu in
  let () = Owl.Mat.(Stdio.printf !"NumLin - sigma? %{same} | h? %{same}\n" (sigma = sigma_copy) (h = h_copy)) in

  (* Owl *)
  let owl_sigma, owl_mu =
    reset ();
    Ex.owl ~sigma ~h ~mu ~r ~data in

  (* NumPy *)
  let numpy_sigma, numpy_mu =
    reset ();
    Ex.numpy ~sigma ~h ~mu ~r ~data in
  let () = Owl.Mat.(Stdio.printf !"NumPy - sigma? %{same} | h? %{same}\n" (sigma = sigma_copy) (h = h_copy)) in

  (* CBLAS *)
  let cblas_sigma, cblas_mu =
    reset ();
    let cblas_sigma = Ex.cblas ~n ~k ~sigma ~h ~mu ~r ~data in
    cblas_sigma, Owl.Mat.copy mu in
  let () = Owl.Mat.(Stdio.printf !"CBLAS - sigma? %{same} | h? %{same}\n" (sigma = sigma_copy) (h = h_copy)) in

  (* Lazy *)
  let lazy_sigma, lazy_mu =
    reset ();
    Ex.lazy_ ~sigma ~h ~mu ~r ~data in

  let results = [
    ("NumPy", numpy_mu, numpy_sigma);
    ("Owl", owl_mu, owl_sigma);
    ("NumLin", numlin_mu, numlin_sigma);
    ("CBLAS", cblas_mu, cblas_sigma);
    ("Lazy", lazy_mu, lazy_sigma);
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
    NumLin - sigma? same | h? same
    NumPy - sigma? same | h? same
    CBLAS - sigma? same | h? same
     Lazy and CBLAS: Mu (same) Sigma (same)
     Lazy and NumLin: Mu (same) Sigma (same)
     Lazy and   Owl: Mu (same) Sigma (same)
     Lazy and NumPy: Mu (same) Sigma (same)
    CBLAS and NumLin: Mu (same) Sigma (same)
    CBLAS and   Owl: Mu (same) Sigma (same)
    CBLAS and NumPy: Mu (same) Sigma (same)
    NumLin and   Owl: Mu (same) Sigma (same)
    NumLin and NumPy: Mu (same) Sigma (same)
      Owl and NumPy: Mu (same) Sigma (same)

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
