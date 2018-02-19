(* Dhruv Makwana *)
(* Lt4la.Check_monad External Tests *)

open Base
;;

open Vars
;;

module Ast =
  Lt4la.Ast
;;

open Lt4la.Check_monad
;;

let execute x =
  run ~counter:1719 x
  |> Stdio.printf !"%{sexp: Ast.linear_t Or_error.t}\n"
;;

let wf_lt =
  well_formed_lt ~fmt:"Not well-formed%s" ~arg:""
;;

let wf_array_t =
  with_frac_cap [three] (wf_lt (Ast.(Array_t (Var three))))
;;

(* create_fresh *)
let%expect_test "create_fresh" =
  let open Let_syntax in
  begin
    let%bind fresh = create_fresh () in
    Stdio.printf !"%{sexp:Ast.variable}\n" fresh;
    let%bind fresh = create_fresh ~name:"test" () in
    Stdio.printf !"%{sexp:Ast.variable}\n" fresh;
    return wf_Unit
  end
  |> run ~counter:1719 |> ignore;
  [%expect {|
    ((id 1719) (name gen))
    ((id 1720) (name test)) |}]
;;

(* with_frac_cap *)
let%expect_test "with_frac_cap" =
  with_frac_cap [four] (return wf_Unit)
  |> execute;
  [%expect {| (Ok Unit) |}]
;;

(* with_linear_t *)
let%expect_test "with_linear_t" =
  let open Let_syntax in
  let%bind wf = wf_array_t in
  with_linear_t [(four,  wf)] (return wf_Unit);
  |> execute;
  [%expect {|
        (Error "Variable four_4 not used.") |}]
;;

(* lookup *)
let%expect_test "lookup None (no vars)" =
  let open Let_syntax in
  let%bind var = lookup four in
  Stdio.printf !"%{sexp: tagged_linear_t option}\n" var;
  return wf_Unit;
  |> run ~counter:1719 |> ignore;
  [%expect {| () |}]
;;

let%expect_test "lookup None (with_frac_cap)" =
  let open Let_syntax in
  with_frac_cap [four] begin
    let%bind var = lookup four in
    Stdio.printf !"%{sexp: tagged_linear_t option}\n" var;
    (return wf_Unit)
  end
  |> execute;
  [%expect {|
        ()
        (Ok Unit) |}]
;;

let%expect_test "lookup None (with_linear_t)" =
  let open Let_syntax in
  let%bind wf = wf_array_t in
  with_linear_t [(five, wf)] begin
    let%bind var = lookup four in
    Stdio.printf !"%{sexp: tagged_linear_t option}\n" var;
    (return wf_Unit)
  end;
  |> execute;
  [%expect {|
        ()
        (Error "Variable five_5 not used.") |}]
;;

let%expect_test "lookup (Some (Not_used _))" =
  let open Let_syntax in
  let%bind wf = wf_array_t in
  with_linear_t [(four, wf)] begin
    let%bind (Some (Not_used four)) = lookup four in
    Stdio.printf !"%{sexp: not_used}\n" four;
    return wf_Unit
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (((id 4) (name four)) (WF (Array_t (Var ((id 3) (name three))))))
        (Error "Variable four_4 not used.") |}]
;;

let%expect_test "lookup (Some (Used _))" =
  let open Let_syntax in
  let%bind wf = wf_array_t in
  with_linear_t [(four, wf)] begin
    let%bind (Some (Not_used four')) = lookup four in
    let%bind _ = use_var four' in
    let%bind (Some (Used linear_t)) = lookup four in
    return linear_t
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok (Array_t (Var ((id 3) (name three))))) |}]
;;

(* if_well_formed *)
let%expect_test "if_well_formed" =
  if_well_formed (Succ Zero)
    ~then_:(Fn.const (fail_string "Yay"))
    ~else_:(Fn.const (fail_string "Nay"))
  |> execute;
  [%expect {|
     (Error Yay) |}]
;;

let%expect_test "if_well_formed" =
  if_well_formed (Var one)
    ~then_:(Fn.const (fail_string "Yay"))
    ~else_:(Fn.const (fail_string "Nay"))
  |> execute;
  [%expect {|
     (Error Nay) |}]
;;

let%expect_test "if_well_formed" =
  with_frac_cap [one] begin
    if_well_formed (Var one)
      ~then_:(Fn.const (fail_string "Yay"))
      ~else_:(Fn.const (fail_string "Nay"))
  end
  |> execute;
  [%expect {|
     (Error Yay) |}]
;;

(* well_formed_lt *)
let%expect_test "well_formed" =
  wf_lt Ast.(Array_t Zero) >>= return
  |> execute;
  [%expect {|
     (Ok (Array_t Zero)) |}]
;;

let%expect_test "well_formed" =
  wf_lt Ast.(Array_t (Var one)) >>= return
  |> execute;
  [%expect {|
     (Error "Not well-formed") |}]
;;

(* well_formed/with_frac_cap *)
let%expect_test "well_formed/with_frac_cap" =
  with_frac_cap [one] (wf_lt Ast.(Array_t (Var one)) >>= return)
  |> execute;
  [%expect {|
     (Ok (Array_t (Var ((id 1) (name one))))) |}]
;;

let%expect_test "well_formed/with_frac_cap" =
  with_frac_cap [four] (wf_lt Ast.(Array_t (Succ (Var one))) >>= return)
  |> execute;
  [%expect {|
     (Error "Not well-formed") |}]
;;

let%expect_test "well_formed/with_frac_cap" =
  with_frac_cap [one] (wf_lt Ast.(Array_t (Succ (Var one))) >>= return)
  |> execute;
  [%expect {|
     (Ok (Array_t (Succ (Var ((id 1) (name one)))))) |}]
;;

(* with_linear_t/use_var *)
let%expect_test "with_linear_t/use_var" =
  let open Let_syntax in
  with_linear_t [(four, wf_Unit)] begin
    let%bind (Some (Not_used four')) = lookup four in
    use_var four'
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok Unit) |}]
;;

let%expect_test "with_linear_t/use_var" =
  let open Let_syntax in
  let%bind wf = wf_array_t in
  (* Observable: fractional capabiities and linearly-typed variables are kept
     in separate environments. *)
  with_linear_t [(one, wf_Unit); (three, wf)] begin
    (* Boiler plate *)
    let%bind (Some (Not_used one)) = lookup one in
    let%bind WF linear_t = use_var one in
    Stdio.printf !"%{sexp: Ast.linear_t}\n" linear_t;
    (* Test *)
    let%bind (Some (Not_used three)) = lookup three in
    let%bind WF linear_t = use_var three in
    Stdio.printf !"%{sexp: Ast.linear_t}\n" linear_t;
    return wf_Unit
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> run ~counter:1719 |> ignore;
  [%expect {|
        Unit
        (Array_t (Var ((id 3) (name three)))) |}]
;;

let%expect_test "with_linear_t/use_var" =
  let open Let_syntax in
  let%bind wf_arr = wf_array_t in
  with_frac_cap [five] begin
    let%bind wf_ForAll = wf_lt Ast.(ForAll_frac_cap (four, Array_t (Succ (Var (four))))) in
    with_linear_t
    [ (one, wf_ForAll)
    ; (three, wf_arr) ]
    (return wf_Unit)
  end;
  |> execute;
  [%expect {|
    (Error  "Variable three_3 not used.\
           \nVariable one_1 not used.") |}]
;;
