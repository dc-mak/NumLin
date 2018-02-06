(* Dhruv Makwana *)
(* Lt4la.Ast External Tests *)

open Core_kernel
;;

open Lt4la
;;

open Vars
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

(* substitute_in *)
let%expect_test "substitute_in" =
  let open Ast in
  substitute_in Unit ~var:four ~replacement:(Succ Zero)
  |> printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok Unit) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (Array_t (Var four)) ~var:four ~replacement:(Succ Zero)
  |> printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok (Array_t (Succ Zero))) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (Array_t (Var three)) ~var:four ~replacement:(Succ Zero)
  |> printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok (Array_t (Var ((id 3) (name three))))) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in
    (ForAll_frac_cap (three, Array_t (Var four)))
    ~var:four
    ~replacement:(Succ Zero)
  |> printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok (ForAll_frac_cap ((id 3) (name three)) (Array_t (Succ Zero)))) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in
    (ForAll_frac_cap (four, Array_t (Var four)))
    ~var:four
    ~replacement:(Succ Zero)
  |> printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Error "INTERNAL ERROR: binding variables are not unique.") |}]
;;

(* same_linear_t *)
let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t (Array_t (Var one)) (Array_t (Succ Zero))
  |> printf !"%{sexp: unit Or_error.t}";
  [%expect {|
    (Error
     ( "Could not show equality:\
      \n    Arr[one]\
      \nwith\
      \n    Arr[1]\
      \n" "Could not show one and 1 are equal.")) |}]
;;

let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t (ForAll_frac_cap (one, Array_t (Succ (Succ (Var one)))))
    (ForAll_frac_cap (two, Array_t (Var two)))
  |> printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    \226\136\128 one. Arr[one+2]\
          \nwith\
          \n    \226\136\128 two. Arr[two]\
          \n" "Could not show one+2 and two are equal.")) |}]
;;

let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t
    (Pair (ForAll_frac_cap (one, Array_t (Succ (Var one))), Unit))
    (Pair (ForAll_frac_cap (one, Array_t (Var one)), Unit))
  |> printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    ( \226\136\128 one. Arr[one+1] ) * I\
          \nwith\
          \n    ( \226\136\128 one. Arr[one] ) * I\
          \n"
           "INTERNAL ERROR: binding variables are not unique.\
          \nBody 1: (Array_t (Succ (Var ((id 1) (name one)))))\
          \nBody 2: (Array_t (Var ((id 1) (name one))))")) |}]
;;

(* joke unintended *)
let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t
    (Pair (ForAll_frac_cap (one, Array_t (Succ (Var one))), Unit))
    (Pair (ForAll_frac_cap (two, Array_t (Var two)), Unit))
  |> printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    ( \226\136\128 one. Arr[one+1] ) * I\
          \nwith\
          \n    ( \226\136\128 two. Arr[two] ) * I\
          \n" "Could not show one+1 and two are equal.")) |}]
;;

let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t Unit (Fun (Unit, Unit))
  |> printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    I\
          \nwith\
          \n    I --o I\
          \n"
           "Specifically, could not show this equality:\
          \n    I\
          \nwith\
          \n    I --o I\
          \n")) |}]
;;

(* Without alpha-equivalence, this test would pass *)
let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t
    (ForAll_frac_cap (one, ForAll_frac_cap (two, Fun (Array_t (Var one), Fun (
       Array_t (Var two), Pair (Array_t (Var one), Array_t (Var two)))))))
    (ForAll_frac_cap (three, ForAll_frac_cap (four, Fun (Array_t (Var three), Fun (
       Array_t (Var four), Pair (Array_t (Var four), Array_t (Var three)))))))
  |> printf !"%{sexp: unit Or_error.t}";
  [%expect {|
    (Error
     ( "Could not show equality:\
      \n    \226\136\128 one. \226\136\128 two. Arr[one] --o Arr[two] --o Arr[one] * Arr[two]\
      \nwith\
      \n    \226\136\128 three. \226\136\128 four. Arr[three] --o Arr[four] --o Arr[four] * Arr[three]\
      \n" "Could not show one and four and alpha-equivalent.")) |}]
;;
