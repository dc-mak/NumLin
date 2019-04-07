(* Dhruv Makwana *)
(* LT4LA.Checker External Tests *)

open Base
;;

open Vars
;;

module Ast =
  Numlin.Ast
;;

let check_expr =
  Numlin.Checker.check_expr ~counter:1719
;;

let pretty x =
  Stdio.printf
    !"%{sexp: string Or_error.t}\n"
    (Or_error.map x ~f:(Ast.(string_of_pp pp_lin)))
;;

let arr : Ast.exp =
  App (Ast.dummy, Prim (Ast.dummy, Array), (Int_I (Ast.dummy, 5)))
;;

let%expect_test "Unit_I" =
  check_expr (Unit_I  Ast.dummy)
  |> pretty;
  [%expect {| (Ok unit) |}]
;;

let%expect_test "Elt_I" =
  check_expr (Elt_I (Ast.dummy, 4.0))
  |> pretty;
  [%expect {| (Ok !float) |}]
;;

let%expect_test "Var (unbound)" =
  check_expr (Var (Ast.dummy, one))
  |> pretty;
  [%expect {|
    (Error
      "Unbound variable one (not found in enviornment)\
     \nIn dummy, at line: 0 and column: 1\
     \n") |}]
;;

let pair : Ast.exp =
  Pair_I (Ast.dummy, arr, Unit_I Ast.dummy)
;;

let%expect_test "Pair_I" =
  check_expr pair
  |> pretty;
  [%expect {| (Ok "z arr * unit") |}]
;;

let%expect_test "Bang_I (not value)" =
  check_expr (Bang_I (Ast.dummy, pair))
  |> pretty;
  [%expect {|
    (Error
      "Can only call 'Many' on values.\
     \nIn dummy, at line: 0 and column: 1\
     \n") |}]
;;

let%expect_test "Bang_I (value)" =
  check_expr (Bang_I (Ast.dummy, Pair_I (Ast.dummy, Unit_I Ast.dummy, Unit_I Ast.dummy)))
  |> pretty;
  [%expect {| (Ok "!( unit * unit )") |}]
;;

let%expect_test "Fix/App" =
  check_expr @@
  Fix (Ast.dummy, one, two, Bang Int, Bang Bool, App (Ast.dummy, App (Ast.dummy, Prim (Ast.dummy, IntOp Eq), Var (Ast.dummy, two)), Int_I (Ast.dummy, 0)))
  |> pretty;
  [%expect {| (Ok "!int --o !bool") |}]
;;

let gen : Ast.exp =
  Gen (Ast.dummy, one, Lambda (Ast.dummy, two, Arr (V one), Var (Ast.dummy, two)))
;;

let%expect_test "Gen" =
  check_expr gen
  |> pretty;
  [%expect {| (Ok "'one. 'one arr --o 'one arr") |}]
;;

let spc : Ast.exp =
  Spc (Ast.dummy, gen, S Z)
;;

let%expect_test "Spc" =
  check_expr spc
  |> pretty;
  [%expect {| (Ok "z s arr --o z s arr") |}]
;;

let%expect_test "Spc" =
  check_expr (Spc (Ast.dummy, gen, S (V three)))
  |> pretty;
  [%expect {|
    (Error
      "Spc: (S (V three)) not found in environment.\
     \nIn dummy, at line: 0 and column: 1\
     \n") |}]
;;

let%expect_test "Bang_E" =
  check_expr @@
  Lambda (Ast.dummy, one, Bang Elt,
    Bang_E (Ast.dummy, one, Var (Ast.dummy, one),
    Bang_E (Ast.dummy, one, Bang_I (Ast.dummy, Bang_I (Ast.dummy, Var (Ast.dummy, one))),
    App (Ast.dummy, App (Ast.dummy, Prim (Ast.dummy, EltOp Add), Var (Ast.dummy, one)), Var (Ast.dummy, one)))))
  |> pretty;
  [%expect {| (Ok "!float --o !float") |}]
;;

let pair_e : Ast.exp =
  Pair_E (Ast.dummy, one, two, pair, Pair_I (Ast.dummy, Var (Ast.dummy, two), Var (Ast.dummy, one)))
;;

let%expect_test "Pair_E" =
  check_expr pair_e
  |> pretty;
  [%expect {| (Ok "unit * z arr") |}]
;;

let%expect_test "Pair_E" =
  check_expr (Pair_E (Ast.dummy, one, two, pair, Var (Ast.dummy, one)))
  |> pretty;
  [%expect {| (Error "Variable two not used.\n") |}]
;;

let unit_lambda : Ast.exp =
  Lambda (Ast.dummy, one, Unit, Var (Ast.dummy, one))
;;

let%expect_test "Lambda" =
  check_expr unit_lambda
  |> pretty;
  [%expect {| (Ok "unit --o unit") |}]
;;

let app : Ast.exp =
  App (Ast.dummy, unit_lambda, Unit_I Ast.dummy)
;;

let%expect_test "App" =
  check_expr app
  |> pretty;
  [%expect {| (Ok unit) |}]
;;

let%expect_test "If" =
  check_expr @@
  Lambda (Ast.dummy, one, Bang Bool,
  Lambda (Ast.dummy, two, Bang Int,
    If (Ast.dummy, Var (Ast.dummy, one), Var (Ast.dummy, two), Unit_I Ast.dummy)))
  |> pretty;
  [%expect {|
    (Error
      "Then-branch (0:1) used these variables not used by else-branch:\
     \n  two (0:1)\
     \n") |}]
;;

let%expect_test "If" =
  check_expr @@
  Lambda (Ast.dummy, one, Bang Bool,
  Lambda (Ast.dummy, two, Bang Int,
    If (Ast.dummy, Var (Ast.dummy, one), Var (Ast.dummy, two), Var (Ast.dummy, two))))
  |> pretty;
  [%expect {| (Ok "!bool --o !int --o !int") |}]
;;

let%expect_test "Unit_E" =
  check_expr @@ Unit_E (Ast.dummy, app, unit_lambda)
  |> pretty;
  [%expect {| (Ok "unit --o unit") |}]
;;

let%expect_test "Let" =
  check_expr @@
  Let (Ast.dummy, "unit", app, unit_lambda)
  |> pretty;
  [%expect {| (Error "Variable unit not used.\n") |}]
;;

let%expect_test "Let" =
  check_expr @@
  Let (Ast.dummy, "unit", app,
       App(Ast.dummy, unit_lambda, Var (Ast.dummy, "unit")))
  |> pretty;
  [%expect {| (Ok unit) |}]
;;

let pretty (x,t) =
  Stdio.printf "%s: %s\n"
    Ast.(string_of_prim x)
    (Ast.(string_of_pp pp_lin @@ Or_error.ok_exn t))
;;

let%expect_test "check_array_elim" =
  List.map ~f:(fun x -> x, check_expr (Prim (Ast.dummy, x))) Ast.prims
  |> List.iter ~f:pretty;
  [%expect {|
    not_: !bool --o !bool
    addI: !int --o !int --o !int
    subI: !int --o !int --o !int
    mulI: !int --o !int --o !int
    divI: !int --o !int --o !int
    eqI: !int --o !int --o !bool
    ltI: !int --o !int --o !bool
    addE: !float --o !float --o !float
    subE: !float --o !float --o !float
    mulE: !float --o !float --o !float
    divE: !float --o !float --o !float
    eqE: !float --o !float --o !bool
    ltE: !float --o !float --o !bool
    set: z arr --o !int --o !float --o z arr
    get: 'x. 'x arr --o !int --o 'x arr * !float
    share: 'x. 'x arr --o 'x s arr * 'x s arr
    unshare: 'x. 'x s arr --o 'x s arr --o 'x arr
    free: z arr --o unit
    array: !int --o z arr
    copy: 'x. 'x arr --o 'x arr * z arr
    sin: z arr --o z arr
    hypot: z arr --o 'x. 'x arr --o 'x arr * z arr
    asum: 'x. 'x arr --o 'x arr * !float
    axpy: z arr --o !float --o 'x. 'x arr --o 'x arr * z arr
    dot: 'x. 'x arr --o 'y. 'y arr --o ( 'x arr * 'y arr ) * !float
    rotmg: !float * !float --o !float * !float --o
    ( !float * !float ) * ( !float * z arr )
    scal: !float --o z arr --o z arr
    amax: 'x. 'x arr --o 'x arr * !int
    get_mat: 'x. 'x mat --o !int --o !int --o 'x mat * !float
    set_mat: z mat --o !int --o !int --o !float --o z mat
    share_mat: 'x. 'x mat --o 'x s mat * 'x s mat
    unshare_mat: 'x. 'x s mat --o 'x s mat --o 'x mat
    free_mat: z mat --o unit
    matrix: !int --o !int --o z mat
    eye: !int --o z mat
    copy_mat: 'x. 'x mat --o 'x mat * z mat
    copy_mat_to: 'x. 'x mat --o z mat --o 'x mat * z mat
    size_mat: 'x. 'x mat --o 'x mat * ( !int * !int )
    transpose: 'x. 'x mat --o 'x mat * z mat
    symm: !bool --o !float --o 'x.
      'x mat --o 'y. 'y mat --o !float --o z mat --o ( 'x mat * 'y mat ) * z mat
    gemm: !float --o 'x.
      'x mat * !bool --o 'y.
        'y mat * !bool --o !float --o z mat --o ( 'x mat * 'y mat ) * z mat
    gesv: z mat --o z mat --o z mat * z mat
    posv: z mat --o z mat --o z mat * z mat
    posv_flip: z mat --o z mat --o z mat * z mat
    potrs: 'x. 'x mat --o z mat --o 'x mat * z mat
    syrk: !bool --o !float --o 'x. 'x mat --o !float --o z mat --o 'x mat * z mat |}]
;;
