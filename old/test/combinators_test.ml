(* Dhruv Makwana *)
(* Old.Combinators External Tests *)

open Base
;;

module Ast =
  Old.Ast
;;

open Old.Combinators
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

(* Advantage of GADTs: Proto_comb.Try3.dotProd_t is syntactically a value. *)
let x () =
  Type.(Ops.(all @@ fun a -> arr a @-> all @@ fun b -> (arr a * arr b) * f64))
;;

(* xDOT: ∀x. Arr[x] --o ∀y. Arr[y] --o (Arr[x] * Arr[y]) * f64 *)
let%expect_test "swap" =
  x ()
  |> Type.extract
  |> pretty Ast.pp_linear_t;
  [%expect {| ∀ gen_1. Arr[gen_1] --o ∀ gen_2. ( Arr[gen_1] * Arr[gen_2] ) * f64 |}]
;;

let apply_dot () =
  Code.(Ops.(dot // z %% (arr (int 5)) // z %% (arr (int 5))))
;;

let%expect_test "dot" =
  apply_dot ()
  |> Code.extract
  |> pretty Ast.pp_expression;
  [%expect {| Prim.dot (* [0] *) (array_intro 5) (* [0] *) (array_intro 5) |}]
;;
