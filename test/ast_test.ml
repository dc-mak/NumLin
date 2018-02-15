(* Dhruv Makwana *)
(* Lt4la.Ast External Tests *)

open Base
;;

open Lt4la
;;

open Vars
;;

let%expect_test "bind_fc_fc" =
  let open Ast in
  bind_fc_fc one Zero
  |> Stdio.printf !"%{sexp: frac_cap}";
  [%expect {| Zero |}]
;;

let%expect_test "bind_fc_fc" =
  let open Ast in
  bind_fc_fc one (Succ (Var {one with id=(-1)}))
  |> Stdio.printf !"%{sexp: frac_cap}";
  [%expect {| (Succ (Var ((id 1) (name one)))) |}]
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
  |> (let f lt =
        Ast.pp_linear_t Caml.Format.std_formatter lt;
        Stdio.Out_channel.(newline stdout) in
      List.iter ~f);
  [%expect {|
        ∀ i_8. Arr[g_6+2]
        ∀ i_8. ∀ h_7.
          ( ( ( Arr[a_0+2] * I ) * ( I * Arr[a_0+2] ) --o
              ( I --o Arr[a_0+2] ) --o Arr[a_0+2] --o Arr[a_0+2] )
            * ( ( ( Arr[a_0+2] * Arr[a_0+2] ) * ( I * I ) )
                * ( ( Arr[a_0+2] * Arr[a_0+2] ) * ( Arr[a_0+2] --o I ) ) ) )
          * ( ( ( Arr[a_0+2] * Arr[a_0+2] ) * ( I * I ) --o
              ( Arr[a_0+2] * Arr[a_0+2] ) * ( Arr[a_0+2] --o I ) )
              * ( ( ( Arr[a_0+2] * I ) * ( I * Arr[a_0+2] ) )
                  * ( ( I --o Arr[a_0+2] ) --o Arr[a_0+2] --o Arr[a_0+2] ) ) )
        ∀ i_8.
          ( ( ∀ e_4. Arr[a_0+2] * Arr[a_0+2] --o Arr[a_0+2] --o I )
            * ( ( Arr[a_0+2] * I --o ∀ c_2. Arr[a_0+2] )
                * ( Arr[a_0+2] * I --o I * Arr[a_0+2] ) ) --o
            ( ∀ e_4. Arr[b_1+2] * ( I * Arr[a_0+2] ) ) --o ∀ e_4. Arr[c_2+2] ) --o
          ( ( Arr[a_0+2] * Arr[a_0+2] --o Arr[a_0+2] * I ) --o
          ( Arr[a_0+2] * Arr[a_0+2] ) * ( Arr[a_0+2] --o I ) )
          * ( ( ( I * Arr[a_0+2] ) * ( Arr[a_0+2] * I ) ) * ( ∀ d_3. I * I ) ) --o
          ( ∀ e_4. Arr[b_1+2] * ( I * Arr[a_0+2] ) ) --o ∀ e_4. Arr[c_2+2] |}]
;;

(* substitute_in *)
let%expect_test "substitute_in" =
  let open Ast in
  substitute_in Unit ~var:four ~replacement:(Succ Zero)
  |> Stdio.printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok Unit) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (Array_t (Var four)) ~var:four ~replacement:(Succ Zero)
  |> Stdio.printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok (Array_t (Succ Zero))) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (Array_t (Var three)) ~var:four ~replacement:(Succ Zero)
  |> Stdio.printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok (Array_t (Var ((id 3) (name three))))) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in
    (ForAll_frac_cap (three, Array_t (Var four)))
    ~var:four
    ~replacement:(Succ Zero)
  |> Stdio.printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Ok (ForAll_frac_cap ((id 3) (name three)) (Array_t (Succ Zero)))) |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in
    (ForAll_frac_cap (four, Array_t (Var four)))
    ~var:four
    ~replacement:(Succ Zero)
  |> Stdio.printf !"%{sexp: linear_t Or_error.t}";
  [%expect {| (Error "INTERNAL ERROR: binding variables are not unique.") |}]
;;

(* same_linear_t *)
let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t (Array_t (Var one)) (Array_t (Succ Zero))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
    (Error
     ( "Could not show equality:\
      \n    Arr[one_1]\
      \nwith\
      \n    Arr[1]\
      \n" "Could not show one_1 and 1 are equal.\n")) |}]
;;

let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t (ForAll_frac_cap (one, Array_t (Succ (Succ (Var one)))))
    (ForAll_frac_cap (two, Array_t (Var two)))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    \226\136\128 one_1. Arr[one_1+2]\
          \nwith\
          \n    \226\136\128 two_2. Arr[two_2]\
          \n" "Could not show one_1+2 and two_2 are equal.\n")) |}]
;;

let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t
    (Pair (ForAll_frac_cap (one, Array_t (Succ (Var one))), Unit))
    (Pair (ForAll_frac_cap (one, Array_t (Var one)), Unit))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    ( \226\136\128 one_1. Arr[one_1+1] ) * I\
          \nwith\
          \n    ( \226\136\128 one_1. Arr[one_1] ) * I\
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
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
         ( "Could not show equality:\
          \n    ( \226\136\128 one_1. Arr[one_1+1] ) * I\
          \nwith\
          \n    ( \226\136\128 two_2. Arr[two_2] ) * I\
          \n" "Could not show one_1+1 and two_2 are equal.\n")) |}]
;;

let%expect_test "same_linear_t" =
  let open Ast in
  same_linear_t Unit (Fun (Unit, Unit))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
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
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
    (Error
     ( "Could not show equality:\
      \n    \226\136\128 one_1. \226\136\128 two_2. Arr[one_1] --o Arr[two_2] --o Arr[one_1] * Arr[two_2]\
      \nwith\
      \n    \226\136\128 three_3. \226\136\128 four_4.\
      \n      Arr[three_3] --o Arr[four_4] --o Arr[four_4] * Arr[three_3]\
      \n" "Could not show one_1 and four_4 and alpha-equivalent.\n")) |}]
;;
