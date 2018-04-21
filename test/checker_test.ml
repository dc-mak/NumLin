(* Dhruv Makwana *)
(* LT4LA.Checker External Tests *)

open Base
;;

open Vars
;;

module Ast =
  Lt4la.Ast
;;

let check_expr = 
  Lt4la.Checker.check_expr ~counter:1719
;;

let pretty x =
  Stdio.printf
    !"%{sexp: string Or_error.t}\n"
    (Or_error.map x ~f:(Ast.(string_of_pp pp_lin)))
;;

let arr : Ast.exp =
  (App (Prim Array, (Int_I 5)))
;;

let%expect_test "Unit_I" =
  check_expr Unit_I
  |> pretty;
  [%expect {| (Ok unit) |}]
;;

let%expect_test "Elt_I" =
  check_expr (Elt_I 4.0)
  |> pretty;
  [%expect {| (Ok !float) |}]
;;

let%expect_test "Var (unbound)" =
  check_expr (Var one)
  |> pretty;
  [%expect {| (Error "Unbound variable one (not found in environment)\n") |}]
;;

let pair : Ast.exp =
  Pair_I (arr, Unit_I)
;;

let%expect_test "Pair_I" =
  check_expr pair
  |> pretty;
  [%expect {| (Ok "z arr * unit") |}]
;;

let%expect_test "Bang_I (not value)" =
  check_expr (Bang_I pair)
  |> pretty;
  [%expect {| (Error "Can only call 'Many' on values.\n") |}]
;;

let%expect_test "Bang_I (value)" =
  check_expr (Bang_I (Pair_I (Unit_I, Unit_I)))
  |> pretty;
  [%expect {| (Ok "!( unit * unit )") |}]
;;

let%expect_test "Fix/App" =
  check_expr @@
  Fix (one, two, Bang Int, Bang Bool, App (App (Prim (IntOp Eq), Var two), Int_I 0))
  |> pretty;
  [%expect {| (Ok "!( !int --o !bool )") |}]
;;

let gen : Ast.exp =
  Gen (one, Lambda (two, Arr (V one), Var two))
;;

let%expect_test "Gen" =
  check_expr gen
  |> pretty;
  [%expect {| (Ok "'one. 'one arr --o 'one arr") |}]
;;


let spc : Ast.exp =
  Spc(gen, S Z)
;;

let%expect_test "Spc" =
  check_expr spc
  |> pretty;
  [%expect {| (Ok "z s arr --o z s arr") |}]
;;

let%expect_test "Spc" =
  check_expr (Spc(gen, S (V three)))
  |> pretty;
  [%expect {|
    (Error "Spc: (S (V three)) not found in environment.\n") |}]
;;

let%expect_test "Bang_E" =
  check_expr @@
  Lambda (one, Bang Elt,
    Bang_E (one, Var one,
    Bang_E (one, Bang_I (Bang_I (Var one)),
    App (App (Prim (EltOp Add), Var one), Var one))))
  |> pretty;
  [%expect {| (Ok "!float --o !float") |}]
;;

let pair_e : Ast.exp =
  Pair_E (one, two, pair, Pair_I (Var two, Var one))
;;

let%expect_test "Pair_E" = 
  check_expr pair_e
  |> pretty;
  [%expect {| (Ok "unit * z arr") |}]
;;

let%expect_test "Pair_E" = 
  check_expr (Pair_E (one, two, pair, Var one))
  |> pretty;
  [%expect {| (Error "Variable two not used.\n") |}]
;;

let unit_lambda : Ast.exp =
  Lambda (one, Unit, Var one)
;;

let%expect_test "Lambda" =
  check_expr unit_lambda
  |> pretty;
  [%expect {| (Ok "unit --o unit") |}]
;;

let app : Ast.exp =
  App (unit_lambda, Unit_I)
;;

let%expect_test "App" =
  check_expr app
  |> pretty;
  [%expect {| (Ok unit) |}]
;;

let%expect_test "If" =
  check_expr @@ 
  Lambda (one, Bang Bool,
  Lambda (two, Bang Int,
    If (Var one, Var two, Unit_I)))
  |> pretty;
  [%expect {|
    (Error  "First term used these variables not used by the second:\
           \n  two\
           \n") |}]
;;

let%expect_test "If" =
  check_expr @@ 
  Lambda (one, Bang Bool,
  Lambda (two, Bang Int,
    If (Var one, Var two, Var two)))
  |> pretty;
  [%expect {| (Ok "!bool --o !int --o !int") |}]
;;

let arith : Ast.arith list =
  [ Add
  ; Sub
  ; Mul
  ; Div
  ; Eq
  ; Lt
  ]
;;

let ops =
  List.concat_map ~f:(fun x -> Ast.[IntOp x; EltOp x]) arith
;;

let prims : Ast.prim list =
  (* Arithmetic *)
  ops @
   (* Operators *)
  [ Not_
  (* Arrays *)
  ; Set
  ; Get
  ; Share
  ; Unshare
  ; Free
  (* Owl - no polymorphism so no Mapi :'( *)
  ; Array
  ; Copy
  ; Sin
  ; Hypot
  (* Level 1 BLAS *)
  ; Asum
  ; Axpy
  ; Dot
  ; Rotmg
  ; Scal
  ; Amax
  (* matrix *)
  ; Get_mat
  ; Set_mat
  ; Share_mat
  ; Unshare_mat
  ; Free_mat
  ; Matrix
  ; Copy_mat
  (* Level 2/3 BLAS *)
  ; Symv
  ; Gemv
  ; Trmv
  ; Trsv
  ; Ger
  ; Gemm
  ; Trmm
  ; Trsm
  ]
;;

let pretty (x,t) =
  Stdio.printf "%s: %s\n"
    Ast.(string_of_prim x)
    (Ast.(string_of_pp pp_lin @@ Or_error.ok_exn t))
;;

let%expect_test "check_array_elim" =
  List.map ~f:(fun x -> x, check_expr (Prim x)) prims
  |> List.iter ~f:pretty;
  [%expect {|
    addI: !int --o !int --o !int
    addE: !float --o !float --o !float
    subI: !int --o !int --o !int
    subE: !float --o !float --o !float
    mulI: !int --o !int --o !int
    mulE: !float --o !float --o !float
    divI: !int --o !int --o !int
    divE: !float --o !float --o !float
    eqI: !int --o !int --o !bool
    eqE: !float --o !float --o !bool
    ltI: !int --o !int --o !bool
    ltE: !float --o !float --o !bool
    not_: !bool --o !bool
    set: z arr --o !int --o !float --o z arr
    get: 'x. 'x arr --o !int --o !float * 'x arr
    share: 'x. 'x arr --o 'x s arr * 'x s arr
    unshare: 'x. 'x s arr --o 'x s arr --o 'x arr
    free: z arr --o unit
    array: !int --o z arr
    copy: 'x. 'x arr --o 'x arr * z arr
    sin: z arr --o z arr
    hypot: z arr --o 'x. 'x arr --o z arr * 'x arr
    asum: 'x. 'x arr --o !float * 'x arr
    axpy: z arr --o !float --o 'x. 'x arr --o z arr * 'x arr
    dot: 'x. 'x arr --o 'y. 'y arr --o !float * ( 'x arr * 'y arr )
    rotmg: !float * !float --o !float * !float --o
    ( !float * !float ) * ( !float * z arr )
    scal: !float --o z arr --o z arr
    amax: 'x. 'x arr --o !int * 'x arr
    get_mat: 'x. 'x arr --o !int --o !int --o !float * 'x mat
    set_mat: z mat --o !int --o !int --o !float --o z mat
    share_mat: 'x. 'x mat --o 'x s mat * 'x s mat
    unshare_mat: 'x. 'x s mat --o 'x s mat --o 'x mat
    free_mat: z mat --o unit
    matrix: !int --o !int --o z mat
    copy_mat: 'x. 'x mat --o 'x mat * z mat
    symv: !float --o 'x.
      'x mat --o 'y. 'y arr --o !float --o z arr --o ( 'x mat * 'y arr ) * z arr
    gemv: !float --o 'x.
      'x mat * !bool --o 'y.
        'y arr --o !float --o z arr --o ( 'x mat * 'y arr ) * z arr
    trmv: 'x. 'x mat * !bool --o z arr --o 'x mat * z arr
    trsv: 'x. 'x mat * !bool --o z arr --o 'x mat * z arr
    ger: !float --o 'x.
      'x arr --o 'y. 'y arr --o z mat --o ( 'x arr * 'y arr ) * z mat
    gemm: !float --o 'x.
      'x mat * !bool --o 'y.
        'y mat * !bool --o !float --o z mat --o ( 'x mat * 'y mat ) * z mat
    trmm: !float --o !bool --o z mat --o 'x. 'x mat * !bool --o 'x mat * z mat
    trsm: !float --o !bool --o z mat --o 'x. 'x mat * !bool --o 'x mat * z mat |}]
;;
