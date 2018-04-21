(* Dhruv Makwana *)
(* LT4LA Syntactic Sugar/Parsing AST *)
(* --------------------------------- *)

open Base
;;

(* Variables *)
type var =
  string
[@@deriving sexp_of]
;;

(* Fractional capabilities *)
type fc =
  | Z
  | S of fc
  | V of var
  | U of var
[@@deriving sexp_of]
;;

let rec ds_fc : fc -> Ast.fc = function
    | Z -> Z
    | V v -> V v
    | U u -> U u
    | S fc -> S (ds_fc fc)
;;

(* Linear types *)
type lin =
  | Unit
  | Bool
  | Int
  | Elt
  | Arr of fc
  | Mat of fc
  | Pair of lin * lin
  | Bang of lin
  | Fun of lin * lin
  | All of var * lin
[@@deriving sexp_of]
;;

let rec ds_lin : lin -> Ast.lin = function
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Elt -> Elt
  | Arr fc -> Arr (ds_fc fc)
  | Mat fc -> Mat (ds_fc fc)
  | Pair (a, b) -> Pair(ds_lin a, ds_lin b)
  | Bang lin -> Bang (ds_lin lin)
  | Fun (func, arg) -> Fun (ds_lin func, ds_lin arg)
  | All (var, lin) -> All (var, ds_lin lin)
;;

type prim =
  (* Boolean *)
  | Not_
  (* Arrays *)
  | Share
  | Unshare
  | Free
  (* Owl - no polymorphism so no Mapi :'( *)
  | Array
  | Copy
  | Sin
  | Hypot
  (* Level 1 BLAS *)
  | Asum
  | Axpy
  | Dot
  | Rotmg
  | Scal
  | Amax
  (* matrix *)
  | Share_mat
  | Unshare_mat
  | Free_mat
  | Matrix
  | Copy_mat
  (* Level 2/3 BLAS *)
  | Symv
  | Gemv
  | Trmv
  | Trsv
  | Ger
  | Gemm
  | Trmm
  | Trsm
[@@deriving sexp_of]
;;

type bang_var =
  | NotB of var
  | Bang of var
[@@deriving sexp_of]
;;

type pat =
  | Base of bang_var
  | Many of pat
  | Pair of pat * pat
[@@deriving sexp_of]
;;

let rec ds_pat pat (body : Ast.exp) : Ast.var * Ast.exp =
  match pat with
  | Base (NotB var) ->
    (var, body)

  | Base (Bang var) ->
    (var, Bang_E (var, Var var, Bang_E (var, Bang_I (Bang_I (Var var)), body)))

  | Many pat ->
    let (var, body) = ds_pat pat body in
    (var, Bang_E (var, Var var, body))

  | Pair (a, b) ->
    let (var_b, body) = ds_pat b body in
    let (var_a, body) = ds_pat a body in
    let var = "_p_" ^ var_a ^ "_" ^ var_b ^ "_p_" in
    (var, Pair_E(var_a, var_b, Var var, body))
;;

type matrix_atom =
  | Plain of var
  | Transp of var
  | Inv of var
  | InvTransp of var
;;

type op =
  | Or
  | And
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Lt
  | PlusDot
  | MinusDot
  | TimesDot
  | DivDot
  | EqDot
  | LtDot
[@@deriving sexp_of]
;;

type matrix_exp =
  | Atom of matrix_atom
  | Op of matrix_exp * op * matrix_exp
;;

type 'a non_empty =
  { first : 'a
  ; rest : 'a list
  }
[@@deriving sexp_of]
;;

type annot_arg =
  { pat : pat
  ; lin : lin
  }
[@@deriving sexp_of]
;;

type arg_like =
  | Underscore
  | Fc of fc
  | Exp of exp

and exp =
  | Prim of prim
  | Var of var
  | Unit_I
  | True
  | False
  | Int_I of int
  | Elt_I of float
  | Pair_I of exp * exp
  | Bang_I of exp
  | AppLike of exp * arg_like non_empty
  | If of exp * exp * exp
  | Lambda of (annot_arg, var) Either.t non_empty * exp
  | Index of var * exp * exp option
  | Assign of var * exp * exp option * exp
  | Infix of exp * op * exp
  | LetAnnot of bang_var * lin * exp * exp
  | LetPat of (pat, pat * pat) Either.t * exp * exp
  | LetFun of bang_var * (annot_arg, var) Either.t non_empty * lin * exp * exp
  | LetRecFun of var * annot_arg * (annot_arg, var) Either.t list * lin * exp * exp
[@@deriving sexp_of]
;;

let reset, inc =
  let init = 0 in
  let x = ref init in
  (fun () -> x := init),
  (fun () -> let y = !x in (x := y + 1; y))
;;

let unify_var () =
  Printf.sprintf "__unify%d" (inc ())
;;

let rec ds_exp : exp -> Ast.exp = function
  | Prim p ->
    Prim begin match p with
      (* Boolean *)
      | Not_ -> Not_
      (* Arrays *)
      | Share -> Share
      | Unshare -> Unshare
      | Free -> Free
      (* Owl - no polymorphism so no Mapi :'( *)
      | Array -> Array
      | Copy -> Copy
      | Sin -> Sin
      | Hypot -> Hypot
      (* Level 1 BLAS *)
      | Asum -> Asum
      | Axpy -> Axpy
      | Dot -> Dot
      | Rotmg -> Rotmg
      | Scal -> Scal
      | Amax -> Amax
      (* matrix *)
      | Share_mat -> Share_mat
      | Unshare_mat -> Unshare_mat
      | Free_mat -> Free_mat
      | Matrix -> Matrix
      | Copy_mat -> Copy_mat
      (* Level 2/3 BLAS *)
      | Symv -> Symv
      | Gemv -> Gemv
      | Trmv -> Trmv
      | Trsv -> Trsv
      | Ger -> Ger
      | Gemm -> Gemm
      | Trmm -> Trmm
      | Trsm -> Trsm
    end

  | Var x -> Var x
  | Unit_I -> Unit_I
  | True -> True
  | False -> False
  | Int_I i -> Int_I i
  | Elt_I e -> Elt_I e
  | Pair_I (a, b) -> Pair_I (ds_exp a, ds_exp b)
  | Bang_I exp -> Bang_I (ds_exp exp)

  | AppLike (func, {first=arg; rest=args}) ->

    List.fold (arg :: args) ~init:(ds_exp func)
      ~f:(fun acc -> function
        | Underscore -> Spc(acc, U (unify_var()))
        | Fc fc -> Spc(acc, ds_fc fc)
        | Exp exp-> App (acc, ds_exp exp))

  | If (cond, true_, false_) ->
    If (ds_exp cond, ds_exp true_, ds_exp false_)

  | Lambda ({first=arg;rest=args}, body) ->

    List.fold_right (arg :: args) ~init:(ds_exp body)
      ~f:(fun arg body -> match arg with
        | Either.First {pat;lin} ->
          let (var, body) = ds_pat pat body in
          Lambda(var, ds_lin lin, body)
        | Either.Second fc_var ->
          Gen(fc_var, body))

  | Index (var, fst, snd) ->
    let one get : Ast.exp =
      App(App(Spc(Prim get, U (unify_var ())), Var var), ds_exp fst) in
    begin match snd with
    | None -> one Get
    | Some snd -> App(one Get_mat, ds_exp snd)
    end

  | Assign (var, fst, snd, exp) ->
    let one set : Ast.exp = App(App(Prim set, Var var), ds_exp fst) in
    begin match snd with
    | None -> App (one Set, ds_exp exp)
    | Some snd -> App(App(one Set_mat, ds_exp snd), ds_exp exp)
    end

  | Infix (fst, op, snd) ->
    let fst = ds_exp fst and snd = ds_exp snd in
    begin match op with
    | Or       -> If (fst, True, snd)
    | And      -> If (fst, snd, False)
    | Plus     -> App(App(Prim (IntOp Add), fst), snd)
    | Minus    -> App(App(Prim (IntOp Sub), fst), snd)
    | Times    -> App(App(Prim (IntOp Mul), fst), snd)
    | Div      -> App(App(Prim (IntOp Div), fst), snd)
    | Eq       -> App(App(Prim (IntOp Eq), fst), snd)
    | Lt       -> App(App(Prim (IntOp Lt), fst), snd)
    | PlusDot  -> App(App(Prim (EltOp Add), fst), snd)
    | MinusDot -> App(App(Prim (EltOp Sub), fst), snd)
    | TimesDot -> App(App(Prim (EltOp Mul), fst), snd)
    | DivDot   -> App(App(Prim (EltOp Div), fst), snd)
    | EqDot    -> App(App(Prim (EltOp Eq), fst), snd)
    | LtDot    -> App(App(Prim (EltOp Lt), fst), snd)
    end

  | LetAnnot (bang_var, lin, exp, body) ->
    let (var, body) = ds_pat (Base bang_var) @@ ds_exp body in
    App (Lambda (var, ds_lin lin, body), ds_exp exp)

  | LetPat (pat, exp, body) ->
    let exp = ds_exp exp and body = ds_exp body in
    begin match pat with
    | First pat ->
      let (var, body) = ds_pat pat body in
      Bang_E (var, exp, body)
    | Second (a, b) ->
      let (var_b, body) = ds_pat b body in
      let (var_a, body) = ds_pat a body in
      Pair_E(var_a, var_b, exp, body)
    end

  | LetFun (fun_var, {first=arg;rest=args}, fun_lin, fun_exp, in_body) ->

    let in_body = ds_exp in_body
    and (fun_lin, fun_exp) =
      List.fold_right (arg :: args) ~init:(ds_lin fun_lin, ds_exp fun_exp)
        ~f:(fun arg (fun_lin, fun_exp) -> match arg with

          | Either.First {pat;lin=arg_lin} ->
            let arg_lin = ds_lin arg_lin in
            (Ast.Fun (arg_lin, fun_lin),
             let (arg_var, fun_exp) = ds_pat pat fun_exp in
             Lambda (arg_var, arg_lin, fun_exp))

          | Either.Second fc_var ->
            (All (fc_var, fun_lin), Gen (fc_var, fun_exp))) in

    begin match fun_var with
    | NotB fun_var ->
      App (Lambda (fun_var, fun_lin, in_body), fun_exp)
    | Bang fun_var ->
      App (Lambda (fun_var, Bang fun_lin, Bang_E (fun_var, Var fun_var, in_body)), Bang_I fun_exp)
    end

  | LetRecFun (fun_var, {pat;lin=arg_lin}, args, res_lin, res_exp, in_body) ->

    let (res_lin, res_exp) =
      List.fold_right args ~init:(ds_lin res_lin, ds_exp res_exp)
        ~f:(fun arg (res_lin, res_exp) -> match arg with

          | Either.First {pat;lin=arg_lin} ->
            let arg_lin = ds_lin arg_lin in
            (Ast.Fun (arg_lin, res_lin),
             let (arg_var, res_exp) = ds_pat pat res_exp in
             Lambda (arg_var, arg_lin, res_exp))

          | Either.Second fc_var ->
            (All (fc_var, res_lin), Gen (fc_var, res_exp))) in

    let (arg_var, res_exp) = ds_pat pat res_exp
    and arg_lin = ds_lin arg_lin in
    App (Lambda (fun_var, Bang(Fun(arg_lin, res_lin)),
                 Bang_E (fun_var, Var fun_var, ds_exp in_body)),
         Fix (fun_var, arg_var, arg_lin, res_lin, res_exp))
;;

let ds_exp exp =
  (reset(); ds_exp exp)
;;
