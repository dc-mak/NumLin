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
  let x = run ~counter:1719 x in
  Stdio.printf !"%{sexp: Ast.lin Or_error.t}\n%s" x
    begin match x with
    | Ok v -> Ast.(string_of_pp pp_lin v)
    | Error _ -> ""
    end
;;

let wf_lin =
  wf_lin ~fmt:"Not well-formed%s" ~arg:""
;;

let wf_arr =
  with_fc three (wf_lin Ast.(Arr (V three)))
;;

(* create_fresh *)
let%expect_test "create_fresh" =
  let open Let_syntax in
  begin
    let%bind fresh = create_fresh () in
    Stdio.printf !"%{sexp:Ast.var}\n" fresh;
    let%bind fresh = create_fresh ~name:"test" () in
    Stdio.printf !"%{sexp:Ast.var}\n" fresh;
    return wf_Unit
  end
  |> run ~counter:1719 |> ignore;
  [%expect {|
    gen_1719
    test_1720 |}]
;;

(* with_fc *)
let%expect_test "with_fc" =
  with_fc four (return wf_Unit)
  |> execute;
  [%expect {|
    (Ok Unit)
    unit |}]
;;

(* with_lin *)
let%expect_test "with_lin" =
  let open Let_syntax in
  let%bind wf = wf_arr in
  with_lin four wf (return wf_Unit);
  |> execute;
  [%expect {|
        (Error "Variable four not used.\n") |}]
;;

(* lookup *)
let%expect_test "lookup None (no vars)" =
  let open Let_syntax in
  let%bind var = lookup four in
  Stdio.printf !"%{sexp: tagged option}\n" var;
  return wf_Unit;
  |> run ~counter:1719 |> ignore;
  [%expect {| () |}]
;;

let%expect_test "lookup None (with_fc)" =
  let open Let_syntax in
  with_fc four begin
    let%bind var = lookup four in
    Stdio.printf !"%{sexp: tagged option}\n" var;
    (return wf_Unit)
  end
  |> execute;
  [%expect {|
        ()
        (Ok Unit)
        unit |}]
;;

let%expect_test "lookup None (with_lin)" =
  let open Let_syntax in
  let%bind wf = wf_arr in
  with_lin five wf begin
    let%bind var = lookup four in
    Stdio.printf !"%{sexp: tagged option}\n" var;
    (return wf_Unit)
  end;
  |> execute;
  [%expect {|
        ()
        (Error "Variable five not used.\n") |}]
;;

let%expect_test "lookup (Some (Not_used _))" =
  let open Let_syntax in
  let%bind wf = wf_arr in
  with_lin four wf begin
    let%bind (Some (Not_used four)) = lookup four in
    Stdio.printf !"%{sexp: not_used}\n" four;
    return wf_Unit
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        ((var four) (t (WFL (Arr (V three)))))
        (Error "Variable four not used.\n") |}]
;;

let%expect_test "lookup (Some (Used _))" =
  let open Let_syntax in
  let%bind wf = wf_arr in
  with_lin four wf begin
    let%bind Some (Not_used four') = lookup four in
    let%bind lin = use_var four' in
    let%bind Some Used = lookup four in
    return lin
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok (Arr (V three)))
        'three arr |}]
;;

(* if_wf *)
let%expect_test "if_wf" =
  if_wf (S Z)
    ~then_:(Fn.const (fail_string "Yay"))
    ~else_:(Fn.const (fail_string "Nay"))
  |> execute;
  [%expect {|
     (Error Yay) |}]
;;

let%expect_test "if_wf" =
  if_wf (V one)
    ~then_:(Fn.const (fail_string "Yay"))
    ~else_:(Fn.const (fail_string "Nay"))
  |> execute;
  [%expect {|
     (Error Nay) |}]
;;

let%expect_test "if_wf" =
  with_fc one begin
    if_wf (V one)
      ~then_:(Fn.const (fail_string "Yay"))
      ~else_:(Fn.const (fail_string "Nay"))
  end
  |> execute;
  [%expect {|
     (Error Yay) |}]
;;

(* wf_lin *)
let%expect_test "wf_lin" =
  wf_lin Ast.(Arr Z) >>= return
  |> execute;
  [%expect {|
     (Ok (Arr Z))
     z arr |}]
;;

let%expect_test "wf_lin" =
  wf_lin Ast.(Arr (V one)) >>= return
  |> execute;
  [%expect {|
     (Error "Not well-formed") |}]
;;

(* wf_lin/with_fc *)
let%expect_test "wf_lin/with_fc" =
  with_fc one (wf_lin Ast.(Arr (V one)) >>= return)
  |> execute;
  [%expect {|
     (Ok (Arr (V one)))
     'one arr |}]
;;

let%expect_test "wf_lin/with_fc" =
  with_fc four (wf_lin Ast.(Arr (S (V one))) >>= return)
  |> execute;
  [%expect {|
     (Error "Not well-formed") |}]
;;

let%expect_test "wf_lin/with_fc" =
  with_fc one (wf_lin Ast.(Arr (S (V one))) >>= return)
  |> execute;
  [%expect {|
     (Ok (Arr (S (V one))))
     'one s arr |}]
;;

(* with_lin/use_var *)
let%expect_test "with_lin/use_var" =
  let open Let_syntax in
  with_lin four wf_Unit begin
    let%bind Some (Not_used four') = lookup four in
    use_var four'
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Ok Unit)
        unit |}]
;;

let%expect_test "with_lin (scoping)/use_var" =
  let open Let_syntax in
  with_lin four wf_Unit @@
  with_lin four (wf_Bang wf_Int) begin
    let%bind Some (Not_used four') = lookup four in
    let%bind (WFL t) as res = use_var four' in
    Stdio.printf !"%{sexp:Ast.lin}\n" t;
    return res
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
        (Bang Int)
        (Error "Variable four not used.\n") |}]
;;

let%expect_test "with_lin/use_var" =
  let open Let_syntax in
  let%bind wf = wf_arr in
  (* Observable: fractional capabiities and linear/intuitionistic *)
  (*  variables are kept in separate environments.                *)
  with_int one wf @@
  with_lin three wf begin
    (* Intuition *)
    let%bind Some (Intuition (WFL one)) = lookup one in
    Stdio.printf !"%{sexp: Ast.lin}\n" one;
    (* Linear *)
    let%bind Some (Not_used three) = lookup three in
    let%bind WFL lin = use_var three in
    Stdio.printf !"%{sexp: Ast.lin}\n" lin;
    return wf_Unit
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> run ~counter:1719 |> ignore;
  [%expect {|
        (Arr (V three))
        (Arr (V three)) |}]
;;

let%expect_test "with_lin/use_var" =
  let open Let_syntax in
  let%bind wf_arr = wf_arr in
  with_fc five begin
    let%bind wf_ForAll = wf_lin Ast.(All (four, Arr (S (V (four))))) in
    with_lin one wf_ForAll @@
    with_lin three wf_arr  @@
    return wf_Unit
  end;
  |> execute;
  [%expect {|
    (Error "Variable three not used.\n") |}]
;;

(* with_int *)
let%expect_test "with_int" =
  let open Let_syntax in
  with_int one wf_Unit @@ return wf_Unit
  |> execute;
  [%expect {|
    (Ok Unit)
    unit |}]
;;

let%expect_test "with_int" =
  let open Let_syntax in
  with_int one wf_Unit begin
    let%bind Some (Intuition t) = lookup one in
    return t
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Ok Unit)
    unit |}]
;;

let%expect_test "with_int" =
  let open Let_syntax in
  with_int one wf_Unit begin
    let%bind None = lookup two in
    return @@ wf_Bang wf_Int
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Ok (Bang Int))
    !int |}]
;;

let%expect_test "with_int (scoping)" =
  let open Let_syntax in
  with_int one wf_Unit @@
  with_int one (wf_Bang wf_Bool) begin
    let%bind Some (Intuition t) = lookup one in
    return t
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Ok (Bang Bool))
    !bool |}]
;;

(* Scoping: with_int/with_lin *)
let%expect_test "wtih_lin/with_int" =
  let open Let_syntax in
  with_lin one wf_Unit @@
  with_int one (wf_Bang wf_Int) @@
  return wf_Unit
  |> execute;
  [%expect {|
    (Error "Variable one not used.\n") |}]
;;

let%expect_test "with_int/with_lin" =
  let open Let_syntax in
  with_int one (wf_Bang wf_Int) @@
  with_lin one wf_Unit begin
    let%bind Some (Not_used t) = lookup one in
    let%bind t = use_var t in
    return t
  end [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Ok Unit)
    unit |}]
;;

(* in_empty *)
let%expect_test "in_empty" =
  let open Let_syntax in
  let%bind wf_arr = wf_arr in
  in_empty @@
  with_lin one wf_arr @@
  begin
    let%bind Some (Not_used t) = lookup one in
    let%bind t = use_var t in
    return t
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Ok (Arr (V three)))
    'three arr |}]
;;

let%expect_test "in_empty" =
  let open Let_syntax in
  with_lin one (wf_Bang wf_Int) @@
  in_empty @@ begin
    let%bind Some (Not_used t) = lookup one in
    let%bind t = use_var t in
    return t
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Error  "Cannot use linearly-typed variables in Fix/Many\
           \n    one\
           \n") |}]
;;

let%expect_test "in_empty" =
  let open Let_syntax in
  in_empty @@ return @@ wf_Bang wf_Elt
  |> execute;
  [%expect {|
    (Ok (Bang Elt))
    !float |}]
;;

(* same_resources *)
let%expect_test "same_resources" =
  let open Let_syntax in
  with_int two (wf_Bang wf_Int) @@
  with_lin one wf_Arr_Z begin
    let prog_one =
      let%bind Some (Not_used one) = lookup one in
      let%bind t1 = use_var one in
      let%bind Some (Intuition t2) = lookup two in
      return @@ wf_Pair t1 t2 in
    let prog_two =
      let%bind Some (Not_used one) = lookup one in
      let%bind t1 = use_var one in
      let%bind Some (Intuition t2) = lookup two in
      return @@ wf_Pair t1 t2 in
    let%bind (a,b) = same_resources prog_one prog_two in
    return @@ wf_Pair a b
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Ok (Pair (Pair (Arr Z) (Bang Int)) (Pair (Arr Z) (Bang Int))))
    ( z arr * !int ) * ( z arr * !int ) |}]
;;

let%expect_test "same_resources" =
  let open Let_syntax in
  with_int two (wf_Bang wf_Int) @@
  with_lin one wf_Arr_Z begin
    let prog_one =
      let%bind Some (Intuition t2) = lookup two in
      return @@ wf_Pair (wf_Arr_Z) t2 in
    let prog_two =
      let%bind Some (Not_used one) = lookup one in
      let%bind t1 = use_var one in
      let%bind Some (Intuition t2) = lookup two in
      return @@ wf_Pair t1 t2 in
    let%bind (a,b) = same_resources prog_one prog_two in
    return @@ wf_Pair a b
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Error  "Second term used these variables not used by the first:\
           \n  one\
           \n") |}]
;;

let%expect_test "same_resources" =
  let open Let_syntax in
  with_lin two (wf_Bang wf_Int) @@
  with_int three (wf_Bang wf_Elt) @@
  with_lin one wf_Arr_Z begin
    let prog_one =
      let%bind Some (Not_used one) = lookup one in
      let%bind t1 = use_var one in
      let%bind Some (Intuition t3) = lookup three in
      return @@ wf_Pair t1 t3 in
    let prog_two =
      let%bind Some (Not_used two) = lookup two in
      let%bind t2 = use_var two in
      let%bind Some (Intuition t3) = lookup three in
      return @@ wf_Pair t2 t3 in
    let%bind (a,b) = same_resources prog_two prog_one in
    return @@ wf_Pair a b
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Error
      "First term used these variables not used by the second:\
     \n  two\
     \nSecond term used these variables not used by the first:\
     \n  one\
     \n") |}]
;;

let%expect_test "same_resources" =
  let open Let_syntax in
  with_lin two wf_Arr_Z @@
  with_int three (wf_Bang wf_Elt) @@
  with_lin one wf_Arr_Z begin
    let prog_one =
      let%bind Some (Not_used one) = lookup one in
      let%bind t1 = use_var one in
      let%bind Some (Intuition t3) = lookup three in
      return @@ wf_Pair t1 t3 in
    let prog_two =
      let%bind Some (Not_used two) = lookup two in
      let%bind t2 = use_var two in
      let%bind Some (Intuition t3) = lookup three in
      return @@ wf_Pair t2 t3 in
    let%bind (a,b) = same_resources prog_two prog_one in
    return @@ wf_Pair a b
  end; [@ocaml.warning "-8" (* Non-exhaustive patterns *) ]
  |> execute;
  [%expect {|
    (Error
      "First term used these variables not used by the second:\
     \n  two\
     \nSecond term used these variables not used by the first:\
     \n  one\
     \n") |}]
;;
