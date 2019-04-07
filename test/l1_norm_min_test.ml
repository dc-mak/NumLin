let n, k =
  5, 3
;;

let duplicate ~row ~col arr =
  let arr = Owl.Mat.of_array arr row col in
  arr, Owl.Mat.copy arr
;;

let q, q_copy =
  duplicate ~row:n ~col:n [|
    1.682490; 0.621964; 0.959947; 1.228820; 1.029410;
    0.621964; 0.631446; 0.551902; 0.723342; 0.756674;
    0.959947; 0.551902; 1.100060; 0.908402; 1.032840;
    1.228820; 0.723342; 0.908402; 1.212400; 1.011350;
    1.029410; 0.756674; 1.032840; 1.011350; 1.302410;
  |]
;;

let u, u_copy =
  duplicate ~row:n ~col:k [|
    0.4621110; 0.0507828; 0.7779080;
    0.833041 ; 0.340120 ; 0.541655 ;
    0.0395867; 0.8726660; 0.8691540;
    0.529315 ; 0.836114 ; 0.286846 ;
    0.241678 ; 0.571528 ; 0.265820 ;
  |]
;;

let i =
  Owl.Mat.eye k
;;

let%expect_test "l1_norm_min" =

  let numpy = Examples.L1_norm_min.numpy ~q ~u in
  let owl = Examples.L1_norm_min.owl ~q ~u in
  (* run last *)
  let numlin = Examples.L1_norm_min.numlin ~q ~u in

  if not Owl.Mat.(owl =~ numlin) then Stdio.printf "Owl/NumLin: NOT SAME!\n";
  if not Owl.Mat.(owl =~ numpy) then Stdio.printf "Owl/NumPy: NOT SAME!\n";
  if not Owl.Mat.(numlin =~ numpy) then Stdio.printf "NumLin/NumPy: NOT SAME!\n";
  Owl.Mat.print ~header:false numpy;
  Owl.Mat.print ~header:false numlin;

  [%expect {|
     0.163738 -0.0580065 -0.236211 -0.378285 -0.237876
     0.803372   0.920085  0.357245  0.437528  0.240099
     0.463474   0.278753   0.97436  0.367974  0.341635
    -0.421418 0.00261993 0.0966232   0.51514  0.291141
    -0.584246  -0.609541 -0.733115 -0.505515  -0.35633


     0.163738 -0.0580065 -0.236211 -0.378285 -0.237876
     0.803372   0.920085  0.357245  0.437528  0.240099
     0.463474   0.278753   0.97436  0.367974  0.341635
    -0.421418 0.00261993 0.0966232   0.51514  0.291141
    -0.584246  -0.609541 -0.733115 -0.505515  -0.35633 |}]
;;
