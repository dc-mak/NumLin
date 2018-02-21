(* Dhruv Makwana *)
(* Lt4la.Ast External Tests *)

open Base
;;

module Ast =
  Lt4la.Ast
;;

open Vars
;;

(* Fractional capabilities: no tests *)

(* Linear types *)

(* Generate a sample of linear types of height at most i *)
let rec generate i =
  let open Ast in
  let v' = String.of_char Char.(of_int_exn (to_int 'a' + i))  in
  let v = String.of_char Char.(of_int_exn (to_int 'a' + i + 1)) in
  let base = [Unit; Arr (S(S(V v'))); Bool; Int; Elt ] in
  if i <= 0 then
    base
  else
    let rest = generate (i-1) in
    let cart = List.take (List.permute (List.cartesian_product rest rest)) 10 in
    let rest = List.take (List.permute rest) 10 in
    base
    @ List.concat_map ~f:(fun x -> [All (v, x); Bang x]) rest
    @ List.concat_map ~f:(fun (x,y) -> [Fun(x,y); Pair (x, y)]) cart
;;

let%expect_test "pp_lin" =
  (* Maximum height 7, drop first five (the base cases) *)
  generate 7 |> Fn.flip List.drop 5 |> Fn.flip List.take 3
  |> List.iter ~f:(fun lt ->
       Ast.pp_lin Caml.Format.std_formatter lt;
       Stdio.Out_channel.(newline stdout));
  [%expect {|
        'i.
          ( ( ( 'e. !( 'c. unit ) ) --o !float ) --o unit )
          * ( ( 'f. int ) --o float * ( 'c s s arr * int ) )
        !( ( ( ( 'e. !( 'c. unit ) ) --o !float ) --o unit )
           * ( ( 'f. int ) --o float * ( 'c s s arr * int ) ) )
        'i.
          ( ( ( 'e. ( 'c. float ) * ( int * unit ) ) * ( 'c s s arr * int ) )
            * !float )
          * !( ( float --o 'd. float --o int ) --o int ) |}]
;;

(* substitute_in *)
let pretty x =
  Stdio.printf !"%{sexp: Ast.lin}\n%s" x Ast.(string_of_pp pp_lin x)
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in Unit ~var:four ~replace:(S Z)
  |> pretty;
  [%expect {|
    Unit
    unit |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (Arr (V four)) ~var:four ~replace:(S Z)
  |> pretty;
  [%expect {|
    (Arr (S Z))
    z s arr |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (Arr (V three)) ~var:four ~replace:(S Z)
  |> pretty;
  [%expect {|
    (Arr (V three))
    'three arr |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (All (three, Arr (V four))) ~var:four ~replace:(S Z)
  |> pretty;
  [%expect {|
    (All three (Arr (S Z)))
    'three. z s arr |}]
;;

let%expect_test "substitute_in" =
  let open Ast in
  substitute_in (All (four, Arr (V four))) ~var:four ~replace:(S Z)
  |> pretty;
  [%expect {|
    (All four (Arr (V four)))
    'four. 'four arr |}]
;;

(* same_lin *)
let%expect_test "same_lin" =
  let open Ast in
  same_lin [] (Arr (V one)) (Arr (S Z))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
    (Error
      "Could not show equality:\
     \n    'one arr\
     \nwith\
     \n    z s arr\
     \n\
     \nCould not show 'one and z s are equal.\
     \n") |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (All (one, Arr (S (S (V one)))))
    (All (two, Arr (V two)))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
          "Could not show equality:\
         \n    'one. 'one s s arr\
         \nwith\
         \n    'two. 'two arr\
         \n\
         \nCould not show 'one s s and 'two are equal.\
         \n") |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (Pair (All (one, Arr (S (V one))), Unit))
    (Pair (All (one, Arr (V one)), Unit))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
          "Could not show equality:\
         \n    ( 'one. 'one s arr ) * unit\
         \nwith\
         \n    ( 'one. 'one arr ) * unit\
         \n\
         \nCould not show 'one s and 'one are equal.\
         \n") |}]
;;

(* joke unintended *)
let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (Pair (All (one, Arr (S (V one))), Unit))
    (Pair (All (two, Arr (V two)), Unit))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
          "Could not show equality:\
         \n    ( 'one. 'one s arr ) * unit\
         \nwith\
         \n    ( 'two. 'two arr ) * unit\
         \n\
         \nCould not show 'one s and 'two are equal.\
         \n") |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin [] Unit (Fun (Unit, Unit))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
        (Error
          "Could not show equality:\
         \n    unit\
         \nwith\
         \n    unit --o unit\
         \n\
         \nSpecifically, could not show this equality:\
         \n    unit\
         \nwith\
         \n    unit --o unit\
         \n") |}]
;;

(* Without alpha-equivalence, this test would pass *)
let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (All (one, All (two, Fun (Arr (V one), Fun (
       Arr (V two), Pair (Arr (V one), Arr (V two)))))))
    (All (three, All (four, Fun (Arr (V three), Fun (
       Arr (V four), Pair (Arr (V four), Arr (V three)))))))
  |> Stdio.printf !"%{sexp: unit Or_error.t}";
  [%expect {|
    (Error
      "Could not show equality:\
     \n     'one. 'two. 'one arr --o 'two arr --o 'one arr * 'two arr\
     \nwith\
     \n     'three. 'four. 'three arr --o 'four arr --o 'four arr * 'three arr\
     \n\
     \nCould not show 'one and 'four and alpha-equivalent.\
     \n") |}]
;;
