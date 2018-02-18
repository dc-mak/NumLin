(* Dhruv Makwana *)
(* Lt4la.Combinators External Tests *)

open Base
;;

open Lt4la
;;

open Combinators
;;

let pretty f x =
  Stdio.printf "%s\n" (Ast.string_of_pp f x)
;;

let%expect_test "all" =
  let open Type in
  let x = all (fun x -> arr x) in
  extract x
  |> pretty Ast.pp_linear_t;
  [%expect {| ∀ gen_0. Arr[gen_0] |}]
;;

(* xDOT: ∀x. Arr[x] --o ∀y. Arr[y] --o (Arr[x] * Arr[y]) * f64 *)
let%expect_test "swap" =
  let open Type in
  let x = Ops.(all @@ fun a -> (arr a) @-> all @@ fun b -> arr b) in
  extract x
  |> pretty Ast.pp_linear_t;
  [%expect {| ∀ gen_1. Arr[gen_1] --o ∀ gen_2. Arr[gen_2] |}]
;;

let%expect_test "dot" =
  let open Code in
  let x = Ops.(dot // Type.z %% (arr (int 5)) // Type.z %% (arr (int 5))) in
  extract x
  |> pretty Ast.pp_expression;
  [%expect {| Prim.dot (* [0] *) (array_intro 5) (* [0] *) (array_intro 5) |}]
;;
