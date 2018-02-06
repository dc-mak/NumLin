(* Dhruv Makwana *)
(* Lt4la.Check_monad External Tests *)

open Core_kernel
;;

open Lt4la
;;

open Vars
;;

open Check_monad
;;

let execute x =
  run ~counter:1719 x
  |> printf !"%{sexp: Ast.linear_t Or_error.t}\n"
;;

(* create_fresh *)
let%expect_test "create_fresh" =
  let open Let_syntax in
  begin
    let%bind fresh = create_fresh () in
    printf !"%{sexp:Ast.variable}\n" fresh;
    let%bind fresh = create_fresh ~name:"test" () in
    printf !"%{sexp:Ast.variable}\n" fresh;
    return Ast.Unit
  end
  |> run ~counter:1719 |> ignore;
  [%expect {|
    ((id 1719) (name 1719))
    ((id 1720) (name test_1720)) |}]
;;

(* with_frac_cap *)
let%expect_test "with_frac_cap" =
  with_frac_cap [four] (return Ast.Unit)
  |> execute;
  [%expect {| (Ok Unit) |}]
;;

(* with_linear_t *)
let%expect_test "with_linear_t" =
  let open Let_syntax in
  with_linear_t [(four, Ast.Array_t (Ast.Var three) )] (return Ast.Unit)
  |> execute;
  [%expect {|
        (Error "Variable four not used.") |}]
;;

(* lookup *)
let%expect_test "lookup None (no vars)" =
  let open Let_syntax in
  begin
    let%bind var = lookup four in
    printf !"%{sexp: tagged_linear_t option}\n" var;
    return Ast.Unit
  end
  |> run ~counter:1719 |> ignore;
  [%expect {| () |}]
;;

let%expect_test "lookup None (with_frac_cap)" =
  let open Let_syntax in
  with_frac_cap [four] begin
    let%bind var = lookup four in
    printf !"%{sexp: tagged_linear_t option}\n" var;
    (return Ast.Unit)
  end
  |> execute;
  [%expect {|
        ()
        (Ok Unit) |}]
;;

let%expect_test "lookup None (with_linear_t)" =
  let open Let_syntax in
  let linear_t = Ast.Array_t (Ast.Var three) in
  with_linear_t [(five, linear_t)] begin
    let%bind var = lookup four in
    printf !"%{sexp: tagged_linear_t option}\n" var;
    (return Ast.Unit)
  end
  |> execute;
  [%expect {|
        ()
        (Error "Variable five not used.") |}]
;;

let%expect_test "lookup (Some (Not_used _))" =
  let open Let_syntax in
  let linear_t = Ast.Array_t (Ast.Var three) in
  with_linear_t [(four, linear_t)] begin
    let%bind (Some (Not_used four)) = lookup four in
    printf !"%{sexp: not_used}\n" four;
    return Ast.Unit
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (((id 4) (name four)) (Array_t (Var ((id 3) (name three)))))
        (Error "Variable four not used.") |}]
;;

let%expect_test "lookup (Some (Used _))" =
  let open Let_syntax in
  let linear_t = Ast.Array_t (Ast.Var three) in
  with_linear_t [(four, linear_t)] begin
    let%bind (Some (Not_used four')) = lookup four in
    let%bind _ = use_var four' in
    let%bind (Some (Used linear_t)) = lookup four in
    return linear_t
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok (Array_t (Var ((id 3) (name three))))) |}]
;;

(* well_formed *)
let%expect_test "well_formed" =
  let open Let_syntax in
  begin
    let%bind wf = well_formed (Ast.Zero) in
    if wf then return Ast.Unit else (fail_string "Not well-formed")
  end
  |> execute;
  [%expect {|
     (Ok Unit) |}]
;;

let%expect_test "well_formed" =
  let open Let_syntax in
  begin
    let%bind wf = well_formed (Ast.Var one) in
    if wf then return Ast.Unit else (fail_string "Not well-formed")
  end
  |> execute;
  [%expect {|
     (Error "Not well-formed") |}]
;;

(* well_formed/with_frac_cap *)
let%expect_test "well_formed/with_frac_cap" =
  let open Let_syntax in
  with_frac_cap [one] begin
    let%bind wf = well_formed (Ast.Var one) in
    if wf then return Ast.Unit else (fail_string "Not well-formed")
  end
  |> execute;
  [%expect {|
     (Ok Unit) |}]
;;

let%expect_test "well_formed/with_frac_cap" =
  let open Let_syntax in
  with_frac_cap [four] begin
    let%bind wf = well_formed (Ast.Succ (Var one)) in
    if wf then return Ast.Unit else (fail_string "Not well-formed")
  end
  |> execute;
  [%expect {|
     (Error "Not well-formed") |}]
;;

let%expect_test "well_formed/with_frac_cap" =
  let open Let_syntax in
  with_frac_cap [one] begin
    let%bind wf = well_formed (Ast.Succ (Var one)) in
    if wf then return Ast.Unit else (fail_string "Not well-formed")
  end
  |> execute;
  [%expect {|
     (Ok Unit) |}]
;;

(* with_linear_t/use_var *)
let%expect_test "with_linear_t/use_var" =
  let open Let_syntax in
  with_linear_t [(four, Ast.Unit)] begin
    let%bind (Some (Not_used four')) = lookup four in
    use_var four'
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok Unit) |}]
;;

let%expect_test "with_linear_t/use_var" =
  let open Let_syntax in
  with_linear_t [(one, Ast.Unit); (three, Ast.Array_t (Var four))] begin
    (* Boiler plate *)
    let%bind (Some (Not_used one)) = lookup one in
    let%bind linear_t = use_var one in
    printf !"%{sexp: Ast.linear_t}\n" linear_t;
    (* Test *)
    let%bind (Some (Not_used three)) = lookup three in
    let%bind linear_t = use_var three in
    printf !"%{sexp: Ast.linear_t}\n" linear_t;
    return Ast.Unit
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> run ~counter:1719 |> ignore;
  [%expect {|
        Unit
        (Array_t (Var ((id 4) (name four)))) |}]
;;

let%expect_test "with_linear_t/use_var" =
  let open Let_syntax in
  with_linear_t
    [ (one, Ast.ForAll_frac_cap (four, Ast.Array_t (Succ (Var (four)))))
    ; (three, Ast.Array_t (Var four))]
    (return Ast.Unit)
  |> execute;
  [%expect {|
    (Error  "Variable three not used.\
           \nVariable one not used.") |}]
;;
