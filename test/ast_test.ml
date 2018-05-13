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
          'f s s arr --o 'g.
            ( ( !( 'a s s arr ) --o !( 'a s s arr ) ) --o 'd. int * unit ) --o 'e.
              'b s s arr * ( 'a s s arr * bool )
        !( 'f s s arr --o 'g.
          ( ( !( 'a s s arr ) --o !( 'a s s arr ) ) --o 'd. int * unit ) --o 'e.
            'b s s arr * ( 'a s s arr * bool ) )
        'i.
          int --o !( ( 'd. bool * int ) * ( 'd. int --o int ) ) --o
          ( ( !( 'a s s arr ) --o !( 'a s s arr ) ) --o 'd. int * unit )
          * ( 'e. 'b s s arr * ( 'a s s arr * bool ) ) |}]
;;

(* substitute_in, substitute_unify *)
let pretty msg x =
  Stdio.printf !"%s:\t%{sexp: Ast.lin}\t%s\n" msg x Ast.(string_of_pp pp_lin x)
;;

let both lin ~var ~replace = 
  pretty "Normal" @@ Ast.substitute_in lin ~var ~replace;
  pretty "Unify" @@ Ast.substitute_unify lin ~var ~replace;
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both Unit ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	Unit	unit
    Unify:	Unit	unit |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (Arr (V four)) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(Arr (S Z))	z s arr
    Unify:	(Arr (S Z))	z s arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (Arr (U four)) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(Arr (S Z))	z s arr
    Unify:	(Arr (S Z))	z s arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (Arr (V three)) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(Arr (V three))	'three arr
    Unify:	(Arr (V three))	'three arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (Arr (U three)) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(Arr (U three))	?three arr
    Unify:	(Arr (U three))	?three arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (All (three, Arr (V four))) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(All three (Arr (S Z)))	'three. z s arr
    Unify:	(All three (Arr (S Z)))	'three. z s arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (All (three, Arr (U four))) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(All three (Arr (S Z)))	'three. z s arr
    Unify:	(All three (Arr (S Z)))	'three. z s arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (All (four, Arr (V four))) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(All four (Arr (V four)))	'four. 'four arr
    Unify:	(All four (Arr (S Z)))	'four. z s arr |}]
;;

let%expect_test "substitute_{in,unify}" =
  let open Ast in
  both (All (four, Arr (U four))) ~var:four ~replace:(S Z);
  [%expect {|
    Normal:	(All four (Arr (U four)))	'four. ?four arr
    Unify:	(All four (Arr (S Z)))	'four. z s arr |}]
;;

(* same_lin *)
let pretty =
  Stdio.printf !"%{sexp: (Ast.var * Ast.fc) list Or_error.t}"
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin [] (Arr (V one)) (Arr (S Z))
  |> pretty;
  [%expect {|
    (Ok ()) |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (All (one, Arr (S (S (V one)))))
    (All (two, Arr (V two)))
  |> pretty;
  [%expect {|
        (Error
          "Could not show equality:\
         \n    'one. 'one s s arr\
         \nwith\
         \n    'two. 'two arr\
         \n\
         \nOccurs check failed: 'two found in 'one s s, under alpha-equivalences:\
         \n((one two))\
         \n") |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (Pair (All (one, Arr (S (V one))), Unit))
    (Pair (All (one, Arr (V one)), Unit))
  |> pretty;
  [%expect {|
        (Error
          "Could not show equality:\
         \n    ( 'one. 'one s arr ) * unit\
         \nwith\
         \n    ( 'one. 'one arr ) * unit\
         \n\
         \nOccurs check failed: 'one found in 'one s, under alpha-equivalences:\
         \n((one one))\
         \n") |}]
;;

(* joke unintended *)
let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (Pair (All (one, Arr (S (V one))), Unit))
    (Pair (All (two, Arr (V two)), Unit))
  |> pretty;
  [%expect {|
        (Error
          "Could not show equality:\
         \n    ( 'one. 'one s arr ) * unit\
         \nwith\
         \n    ( 'two. 'two arr ) * unit\
         \n\
         \nOccurs check failed: 'two found in 'one s, under alpha-equivalences:\
         \n((one two))\
         \n") |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin [] Unit (Fun (Unit, Unit))
  |> pretty;
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
  |> pretty;
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

let%expect_test "same_lin" =
  let open Ast in
  same_lin [] (Arr (U one)) (Arr (S Z))
  |> pretty;
  [%expect {|
    (Ok ((one (S Z)))) |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (All (one, Arr (S (S (U one)))))
    (All (one, Arr (U one)))
  |> pretty;
  [%expect {|
        (Error
          "Could not show equality:\
         \n    'one. ?one s s arr\
         \nwith\
         \n    'one. ?one arr\
         \n\
         \nOccurs check failed: ?one found in ?one s s.\
         \n") |}]
;;

let%expect_test "same_lin" =
  let open Ast in
  same_lin []
    (Pair (All (one, Arr (S (U one))), Unit))
    (Pair (All (one, Arr (U two)), Unit))
  |> pretty;
  [%expect {|
        (Ok ((two (S (U one))))) |}]
;;
