(* Dhruv Makwana *)
(* Lt4la.Ast External Tests *)

open Core_kernel
;;

open Lt4la
;;

open Vars
;;

(* unify_frac_cap (and pp_frac_cap) *)
let%expect_test "unify_frac_cap" =
  let open Ast in
  unify_frac_cap Zero (Succ Zero)
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {| (Error "Could not unify 0 with 1.") |}]
;;

let%expect_test "unify_frac_cap" =
  let open Ast in
  unify_frac_cap  (Succ (Succ (Var one))) (Var one)
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {| (Error "Variable: ((id 1) (name one)) occurs in one+2") |}]
;;

let%expect_test "unify_frac_cap" =
  let open Ast in
  unify_frac_cap (Var one) (Succ (Succ (Var three)))
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {| (Ok ((((id 1) (name one)) (Succ (Succ (Var ((id 3) (name three)))))))) |}]
;;

(* Generate a sample of linear_types of height at most i *)
let rec generate i =
  let open Ast in
  let var' = {id=i; name= String.of_char (Char.of_int_exn (Char.to_int 'a' + i)) } in
  let var = {id=i+1; name= String.of_char (Char.of_int_exn (Char.to_int 'a' + i + 1)) } in
  let base = [Unit; Array_t (Succ(Succ(Var var'))) ] in
  if i <= 0 then
    base
  else 
    let rest = generate (i-1) in
    let cart = List.take (List.permute (List.cartesian_product rest rest)) 10 in
    let rest = List.take (List.permute rest) 10 in
    base
    @ List.map ~f:(fun x : linear_t -> ForAll_frac_cap (var, x)) rest
    @ List.concat_map ~f:(fun (x,y) -> [Fun(x,y); Pair (x, y)]) cart
;;

let%expect_test "pp_linear_type" =
  (* Maximum height 7, drop first two (the base cases) *)
  generate 7 |> Fn.flip List.drop 2 |> Fn.flip List.take 3
  |> List.iter ~f:(Ast.pp_linear_t Caml.Format.std_formatter);
  [%expect {|
        ∀ i. ∀ h.
          ( ∀ e. ∀ d. I * Arr[a+2] ) * ( Arr[c+2] * ( ∀ d. I ) )
          --o ( ( ( Arr[a+2] --o Arr[a+2] ) --o Arr[b+2] ) * ( I * Arr[a+2] --o I ) )
              * ( ∀ e. ( I --o I ) * ( Arr[a+2] * Arr[a+2] ) )
        ∀ i.
          ( Arr[e+2] --o ( Arr[c+2] --o ∀ d. I )
            --o ( Arr[a+2] --o Arr[a+2] ) * ( I * I ) --o ( ∀ c. I )
            --o Arr[a+2] --o Arr[a+2] )
          * ( ( ( ∀ e. ∀ d. I * Arr[a+2] ) * ( Arr[c+2] * ( ∀ d. I ) ) )
              * ( ( ( ( Arr[a+2] --o Arr[a+2] ) --o Arr[b+2] )
                    * ( I * Arr[a+2] --o I ) )
                  * ( ∀ e. ( I --o I ) * ( Arr[a+2] * Arr[a+2] ) ) ) )
        ∀ i. ∀ h. ∀ g. ∀ f. ∀ e. I |}]
;;

(* unify_linear_t *)
let%expect_test "unify_linear_t" =
  let open Ast in
  unify_linear_t (Array_t (Var one)) (Array_t (Succ Zero))
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {| (Ok ((((id 1) (name one)) (Succ Zero)))) |}]
;;

let%expect_test "unify_linear_t" = 
  let open Ast in
  unify_linear_t (ForAll_frac_cap (one, Array_t (Succ (Succ (Var one)))))
    (ForAll_frac_cap (two, Array_t (Var two)))
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {|
        (Ok
         ((((id 1) (name one)) (Var ((id 2) (name two))))
          (((id 2) (name two)) (Succ (Succ (Var ((id 1) (name one)))))))) |}]
;;

let%expect_test "unify_linear_t" = 
  let open Ast in 
  unify_linear_t
    (Pair (ForAll_frac_cap (one, Array_t (Succ (Var one))), Unit))
    (Pair (ForAll_frac_cap (one, Array_t (Var one)), Unit))
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {|
        (Error
          "INTERNAL ERROR: binding variables are not unique.\
         \nBody 1: (Array_t (Succ (Var ((id 1) (name one)))))\
         \nBody 2: (Array_t (Var ((id 1) (name one))))") |}]
;;

let%expect_test "unify_linear_t" =
  let open Ast in
  unify_linear_t Unit (Fun (Unit, Unit))
  |> printf !"%{sexp: (variable * frac_cap) list Or_error.t}";
  [%expect {|
        (Error  "Couldn't unify\
               \n    I\
               \nwith\
               \n    I --o I\
               \n") |}]
;;
