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

(* When create_fresh was still around *)
    (*
 * let%expect_test "create_fresh" =
 *   let open Let_syntax in
 *   begin
 *     let%bind fresh = create_fresh in
 *     printf !"%{sexp:Ast.variable}\n" fresh;
 *     let%bind fresh = create_fresh in
 *     printf !"%{sexp:Ast.variable}\n" fresh;
 *     return Ast.Unit
 *   end
 *   |> run ~counter:1719 |> ignore;
 *   [%expect {|
 *     ((id 1719) (name 1719))
 *     ((id 1720) (name 1720)) |}]
 * ;;
*)

(* with_frac_cap *)
let%expect_test "with_frac_cap" =
  with_frac_cap [(four, Ast.(Succ(Var three)))] (return Ast.Unit)
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
  with_frac_cap [(four, Ast.(Succ(Var three)))] begin
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

(* normal_form *)
let%expect_test "normal_form" =
  let open Let_syntax in
  begin
    let%bind frac_cap = normal_form (Ast.Zero) in
    (return Ast.(Array_t frac_cap))
  end
  |> execute;
  [%expect {|
        (Ok (Array_t Zero)) |}]
;;

let%expect_test "normal_form" =
  let open Let_syntax in
  begin
    let%bind frac_cap = normal_form (Ast.(Succ (Succ (Var four)))) in
    (return Ast.(Array_t frac_cap))
  end
  |> execute;
  [%expect {|
        (Ok (Array_t (Succ (Succ (Var ((id 4) (name four))))))) |}]
;;

let%expect_test "normal_form/with_frac_cap" =
  let open Let_syntax in
  with_frac_cap [(four, Ast.(Succ(Var three)))] begin
    let%bind frac_cap = normal_form (Ast.(Succ (Succ (Var four)))) in
    (return Ast.(Array_t frac_cap))
  end
  |> execute;
  [%expect {|
        (Ok (Array_t (Succ (Succ (Succ (Var ((id 3) (name three)))))))) |}]
;;

let%expect_test "normal_form/with_linear_t/use_var" =
  let open Let_syntax in
  with_linear_t [(four, Ast.Unit)] begin
    let%bind (Some (Not_used four')) = lookup four in
    let%bind _ = use_var four' in
    let%bind frac_cap = normal_form (Ast.(Succ (Succ (Var four)))) in
    (return Ast.(Array_t frac_cap))
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok (Array_t (Succ (Succ (Var ((id 4) (name four))))))) |}]
;;

(* apply *)
let%expect_test "apply/with_linear_t/use_var" =
  let open Let_syntax in
  with_linear_t [(one, Ast.Unit); (three, Ast.Array_t (Var four))] begin
    let%bind () = apply [(four, Succ (Succ Zero))] in
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
        (Array_t (Succ (Succ Zero))) |}]
;;

let%expect_test "apply/with_linear_t/use_var" =
  let open Let_syntax in
  (try
     with_linear_t
       [ (one, Ast.ForAll_frac_cap (four, Ast.Array_t (Succ (Var (four)))))
       ; (three, Ast.Array_t (Var four))]
       ( let%bind () = apply [(four, Succ (Succ Zero))] in
         return Ast.Unit )
     |> run ~counter:1719 |> ignore;
   with
   | Failure msg ->
     print_string msg);
  [%expect {|
        INTERNAL ERROR: Variable ((id 4) (name four)) not unique in linear_t.
        Ensure you call unify_linear_t before sub_frac_cap_exn/apply |}]
;;
