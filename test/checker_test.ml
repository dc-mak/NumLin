(* Dhruv Makwana *)
(* Lt4la.Checker External Tests *)
(* These will be easier to write with a parser. *)

open Core_kernel
;;

open Lt4la
;;

open Vars
;;

let check_expr = 
  Checker.check_expr ~counter:1719
;;

let string_of_linear_t linear_t =
  let buffer = Buffer.create 80 in
  Ast.pp_linear_t (Caml.Format.formatter_of_buffer buffer) linear_t;
  Buffer.contents buffer
;;

let pretty x =
  printf !"%{sexp: string Or_error.t}\n" (Or_error.map x ~f:string_of_linear_t)
;;

let arr : Ast.expression =
  Array_Intro (Owl.Arr.zeros [| 5 |] )
;;

let%expect_test "checker_unit_intro" =
  check_expr Unit_Intro
  |> pretty;
  [%expect {| (Ok "I\n") |}]
;;

let%expect_test "checker_var_unbound" =
  check_expr (Var one)
  |> pretty;
  [%expect {| (Error "Unbound variable one (not found in environment)") |}]
;;

let unit_elim : Ast.expression =
  Unit_Elim (Unit_Intro, arr)
;;

let%expect_test "checker_unit_elim" =
  check_expr unit_elim
  |> pretty;
  [%expect {| (Ok "Arr[0]\n") |}]
;;

let pair : Ast.expression =
  Pair_Intro (arr, Unit_Intro)
;;

let%expect_test "checker_pair_intro" =
  check_expr pair
  |> pretty;
  [%expect {| (Ok "Arr[0] * I\n") |}]
;;

let pair_elim : Ast.expression =
  Pair_Elim (one, two, pair, Pair_Intro (Var two, Var one))
;;

let%expect_test "checker_pair_elim" = 
  check_expr pair_elim
  |> pretty;
  [%expect {| (Ok "I * Arr[0]\n") |}]
;;

let%expect_test "checker_pair_elim" = 
  check_expr (Pair_Elim (one, two, pair, Var one))
  |> pretty;
  [%expect {| (Error "Variable two not used.") |}]
;;

let unit_lambda : Ast.expression =
  Lambda (one, Unit, Var one)
;;

let%expect_test "checker_lambda" =
  check_expr unit_lambda
  |> pretty;
  [%expect {| (Ok "I --o I\n") |}]
;;

let app : Ast.expression =
  App (unit_lambda, Unit_Intro)
;;

let%expect_test "checker_app" =
  check_expr app
  |> pretty;
  [%expect {| (Ok "I\n") |}]
;;

let forall : Ast.expression =
  ForAll_frac_cap (one, Lambda (two, Array_t (Var one), Var two))
;;

let%expect_test "checker_forall" =
  check_expr forall
  |> pretty;
  [%expect {| (Ok "\226\136\128 one. Arr[one] --o Arr[one]\n") |}]
;;

let specialise : Ast.expression =
  Specialise_frac_cap(forall, Succ Zero)
;;

let%expect_test "checker_specialise" =
  check_expr specialise
  |> pretty;
  [%expect {| (Ok "Arr[1] --o Arr[1]\n") |}]
;;

let%expect_test "checker_specialise" =
  check_expr (Specialise_frac_cap(forall, Succ (Var three)))
  |> pretty;
  [%expect {|
    (Error
     "Specialise_frac_cap: (Succ (Var ((id 3) (name three)))) not found in environment.") |}]
;;

let%expect_test "checker_array_elim" =
  check_expr (Array_Elim (one, arr, Var one))
  |> pretty;
  [%expect {| (Ok "Arr[0]\n") |}] 
;;

let prims : Ast.primitive list =
   (* Operators *)
  [ Split_Permission
  ; Merge_Permission
  ; Free
  ; Copy (* xCOPY *)
  ; Swap (* xSWAP *)

   (* Routines/Functions *)
  ; Sum_Mag (* xASUM *)
  ; Scalar_Mult_Then_Add (* xAXPY *)
  ; DotProd (* xDOT *)
  ; Norm2 (* xNRM2 *)
  ; Plane_Rotation (* xROT *)
  ; Givens_Rotation (* xROTG *)
  ; GivensMod_Rotation (* xROTM *)
  ; Gen_GivensMod_Rotation (* xROTMG *)
  ; Scalar_Mult (* xSCAL *)
  ; Index_of_Max_Abs (* IxAMAX *)
  ; Index_of_Min_Abs (* IxAMIN -- Intel only *)
  ]
;;

let%expect_test "check_array_elim" =
  List.map ~f:(fun x -> check_expr (Primitive x)) prims
  |> List.iter ~f:pretty;
  [%expect {|
    (Ok
      "\226\136\128 split_perm_1719.\
     \n  Arr[split_perm_1719] --o Arr[split_perm_1719+1] * Arr[split_perm_1719+1]\
     \n")
    (Ok
      "\226\136\128 merge_perm_1719.\
     \n  Arr[merge_perm_1719+1] * Arr[merge_perm_1719+1] --o Arr[merge_perm_1719]\
     \n")
    (Ok "Arr[0] --o I\n")
    (Ok "\226\136\128 copy_1719. Arr[copy_1719] --o Arr[copy_1719] * Arr[0]\n")
    (Ok "Arr[0] * Arr[0] --o Arr[0] * Arr[0]\n")
    (Ok
     "\226\136\128 sum_mag_1719. Arr[sum_mag_1719] --o Arr[sum_mag_1719] * Arr[0]\n")
    (Ok
      "\226\136\128 sum_mag_scalar_1719.\
     \n  Arr[sum_mag_scalar_1719] --o \226\136\128 sum_mag_vec_1720.\
     \n    Arr[sum_mag_vec_1720]\
     \n      --o Arr[0]\
     \n          --o ( Arr[sum_mag_scalar_1719] * Arr[sum_mag_vec_1720] ) * Arr[0]\
     \n")
    (Ok
      "\226\136\128 dot_prod_x_1719.\
     \n  Arr[dot_prod_x_1719] --o \226\136\128 dot_prod_y_1720.\
     \n    Arr[dot_prod_y_1720]\
     \n    --o ( Arr[dot_prod_x_1719] * Arr[dot_prod_y_1720] ) * Arr[0]\
     \n")
    (Ok
     "\226\136\128 norm2_1719. Arr[norm2_1719] --o Arr[norm2_1719] * Arr[0]\n")
    (Ok
      "\226\136\128 plane_rot_x_1719.\
     \n  Arr[0] * Arr[plane_rot_x_1719] --o \226\136\128 plane_rot_y_1720.\
     \n    Arr[0] * Arr[plane_rot_y_1720] --o \226\136\128 plane_rot_s_1721.\
     \n      Arr[plane_rot_s_1721] --o \226\136\128 plane_rot_c_1722.\
     \n        Arr[plane_rot_c_1722]\
     \n        --o ( ( Arr[plane_rot_x_1719] * Arr[plane_rot_y_1720] )\
     \n              * ( Arr[plane_rot_s_1721] * Arr[plane_rot_c_1722] ) )\
     \n            * ( Arr[0] * Arr[0] )\
     \n")
    (Ok
      "Arr[0] --o Arr[0] --o Arr[0]\
     \n  --o Arr[0] --o ( Arr[0] * Arr[0] ) * ( Arr[0] * Arr[0] )\
     \n")
    (Ok
      "\226\136\128 plane_rot_x_1719.\
     \n  Arr[0] * Arr[plane_rot_x_1719] --o \226\136\128 plane_rot_y_1720.\
     \n    Arr[0] * Arr[plane_rot_y_1720] --o \226\136\128 plane_rot_s_1721.\
     \n      Arr[plane_rot_s_1721]\
     \n      --o ( ( Arr[plane_rot_x_1719] * Arr[plane_rot_y_1720] )\
     \n            * Arr[plane_rot_s_1721] )\
     \n          * ( Arr[0] * Arr[0] )\
     \n")
    (Ok
      "Arr[0] --o Arr[0]\
     \n  --o Arr[0] --o \226\136\128 mod_givens_y_1719.\
     \n        Arr[mod_givens_y_1719]\
     \n          --o Arr[0]\
     \n              --o Arr[mod_givens_y_1719]\
     \n                  * ( ( Arr[0] * Arr[0] ) * ( Arr[0] * Arr[0] ) )\
     \n")
    (Ok
      "\226\136\128 scalar_mult_sc_1719.\
     \n  Arr[scalar_mult_sc_1719] --o \226\136\128 scalar_mult_inc_1720.\
     \n    Arr[scalar_mult_inc_1720]\
     \n      --o Arr[0]\
     \n          --o ( Arr[scalar_mult_sc_1719] * Arr[scalar_mult_inc_1720] )\
     \n              * Arr[0]\
     \n")
    (Ok
      "\226\136\128 index_max_inc_1719.\
     \n  Arr[index_max_inc_1719] --o \226\136\128 index_max_x_1720.\
     \n    Arr[index_max_x_1720]\
     \n    --o ( Arr[index_max_inc_1719] * Arr[index_max_x_1720] ) * Arr[0]\
     \n")
    (Ok
      "\226\136\128 index_max_inc_1719.\
     \n  Arr[index_max_inc_1719] --o \226\136\128 index_max_x_1720.\
     \n    Arr[index_max_x_1720]\
     \n    --o ( Arr[index_max_inc_1719] * Arr[index_max_x_1720] ) * Arr[0]\
     \n") |}]
;;
