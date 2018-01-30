(* Dhruv Makwana *)
(* Lt4la.Checker External Tests *)

open Core_kernel
;;

open Lt4la
;;

open Vars
;;

let check_expr = 
  Checker.check_expr ~counter:1719
;;

(*
 * let t1 =
 *   Ast.(Fun(Unit, ForAll_frac_cap (nine, ForAll_frac_cap(three, fc, Unit))))
 * ;;
*)

(* Base cases *)
let%expect_test "checker_unit" =
  check_expr Unit_Intro
  |> printf !"%{sexp: Ast.linear_t Or_error.t}";
  [%expect {| (Ok Unit) |}]
;;

let%expect_test "checker_var_unbound" =
  check_expr (Var one)
  |> printf !"%{sexp: Ast.linear_t Or_error.t}";
  [%expect {| (Error "Unbound variable one (not found in environment)") |}]
;;

(*
* let%expect_test "checker_var_pass" =
*   check_in (Var one) [(one, First (Unit, Never))]
*   |> printf !"%{sexp: type_or_kind answer}";
*   [%expect {| (Ok ((First Unit) ((((id 1) (name one)) (First (Unit Once)))))) |}]
* ;;
*)

(*
 * let%expect_test "checker_var_used" =
 *   check_in (Var one) [(one, First (Unit, Once))]
 *   |> printf !"%{sexp: type_or_kind answer}";
 *   [%expect {| (Error "Variable one used twice (or more).") |}]
 * ;;
*)

(*
 * let%expect_test "checker_var_used" =
 *   check_in (Var one) [(one, Second fc)]
 *   |> printf !"%{sexp: type_or_kind answer}";
 *   [%expect {|
 *     (Ok
 *      ((Second Fractional_capability)
 *       ((((id 1) (name one)) (Second Fractional_capability))))) |}]
 * ;;
*)

(*
 *     (* Generation *)
 *     let rec generate height =
 *       let open Ast in
 *       if height <= 0 then
 *         [Unit_Intro]
 *       else
 *         let smaller = generate (height - 1) in
 *         let pairs = List.cartesian_product smaller smaller in
 *         let make, make' = List.map smaller, List.map pairs in
 *         List.concat
 *           [ smaller
 *           ; make (fun x -> Lambda (one, Unit, x))
 *           ; make (fun x -> ForAll (one, fc, x))
 *           ; make' (fun (x,y) -> Pair_Intro (x,y))
 *           ; make' (fun (x,y) -> Pair_Elim (one, two, x, y))
 *           ; make' (fun (x,y) -> App (x,y))
 *           ; make' (fun (x,y) -> Unit_Elim (x,y))
 *           ]
 *     ;;
 *
 *     let expressions =
 *       generate 1
 *     ;;
 *
 *     let%expect_test "checker" =
 *       let open Ast in
 *       List.map ~f:check_expr expressions
 *       |> printf !"%{sexp: linear_type Or_error.t list}";
 *       [%expect {|
 *       ((Ok Unit) (Ok (Fun Unit Unit))
 *        (Ok (ForAll_T ((id 1) (name one)) Fractional_capability Unit))
 *        (Ok (Pair Unit Unit))
 *        (Error "Pair_Elim expression does not have type Pair(_, _) ")
 *        (Error "Function in App expression does not have type Fun(_, _)") (Ok Unit)) |}]
 *     ;;
 *)
