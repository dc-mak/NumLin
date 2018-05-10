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
  | Pair (a, b) -> Pair (ds_lin a, ds_lin b)
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
  | Copy_mat_to
  | Size_mat
  (* Level 2/3 BLAS *)
  | Symm
  | Gemm
  | Posv
  | Potrs
[@@deriving sexp_of]
;;

type bang_var =
  | NotB of var
  | Bang of var
[@@deriving sexp_of]
;;

type loc =
   Lexing.position sexp_opaque
[@@deriving sexp_of]
;;

type pat =
  | Unit of loc
  | Base of loc * bang_var
  | Many of loc * pat
  | Pair of loc * pat * pat
[@@deriving sexp_of]
;;

let rec ds_pat pat (body : Ast.exp) : loc * Ast.var * Ast.exp =
  match pat with
  | Unit loc ->
    let var = "unit" in (loc, var, Unit_E (loc, Var (loc, var), body))

  | Base (loc, NotB var) ->
    (loc, var, body)

  | Base (loc, Bang var) ->
    (loc, var, Bang_E (loc, var, Var (loc, var),
                  Bang_E (loc, var, Bang_I (loc, Bang_I (loc, Var (loc, var))), body)))

  | Many (loc, pat) ->
    let (_, var, body) = ds_pat pat body in
    (loc, var, Bang_E (loc, var, Var (loc, var), body))

  | Pair (loc, a, b) ->
    let (_, var_b, body) = ds_pat b body in
    let (_, var_a, body) = ds_pat a body in
    let var = "_p_" ^ var_a ^ "_" ^ var_b ^ "_p_" in
    (loc, var, Pair_E (loc, var_a, var_b, Var (loc, var), body))
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
  | Underscore of loc
  | Fc of loc * fc
  | Exp of exp

and exp =
  | Prim of loc * prim
  | Var of loc * var
  | Unit_I of loc
  | True of loc
  | False of loc
  | Int_I of loc * int
  | Elt_I of loc * float
  | Pair_I of loc * exp * exp
  | Bang_I of loc * exp
  | AppLike of loc * exp * arg_like non_empty
  | If of loc * exp * exp * exp
  | Lambda of loc * (annot_arg, loc * var) Either.t non_empty * exp
  | Index of loc * var * exp * exp option
  | Assign of loc * var * exp * exp option * exp
  | Infix of loc * exp * op * exp
  | LetAnnot of loc * bang_var * lin * exp * exp
  | LetPat of loc * pat * exp * exp
  | LetFun of loc * bang_var * (annot_arg, loc * var) Either.t non_empty * lin * exp * exp
  | LetRecFun of loc * var * annot_arg * (annot_arg, loc * var) Either.t list * lin * exp * exp
[@@deriving sexp_of]
;;

let loc : exp -> loc = function
  | Prim (loc, _)
  | Var (loc, _)
  | Unit_I loc
  | True loc
  | False loc
  | Int_I (loc, _)
  | Elt_I (loc, _)
  | Pair_I (loc, _, _)
  | Bang_I (loc, _)
  | AppLike (loc, _, _)
  | If (loc, _, _, _)
  | Lambda (loc, _, _)
  | Index (loc, _, _, _)
  | Assign (loc, _, _, _, _)
  | Infix (loc, _, _, _)
  | LetAnnot (loc, _, _, _, _)
  | LetPat (loc, _, _, _)
  | LetFun (loc, _, _, _, _, _)
  | LetRecFun (loc, _, _, _, _, _, _) -> loc
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
  | Prim (loc, p) ->
    Prim (loc, begin match p with
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
      | Copy_mat_to -> Copy_mat_to
      | Size_mat -> Size_mat
      (* Level 2/3 BLAS *)
      | Symm -> Symm
      | Gemm -> Gemm
      | Posv -> Posv
      | Potrs -> Potrs
    end)

  | Var (loc, x) -> Var (loc, x)
  | Unit_I loc -> Unit_I loc
  | True loc -> True loc
  | False loc -> False loc
  | Int_I (loc, i) -> Int_I (loc, i)
  | Elt_I (loc, e) -> Elt_I (loc, e)
  | Pair_I (loc, a, b) -> Pair_I (loc, ds_exp a, ds_exp b)
  | Bang_I (loc, exp) -> Bang_I (loc, ds_exp exp)

  | AppLike (_, func, {first=arg; rest=args}) ->

    List.fold (arg :: args) ~init:(ds_exp func)
      ~f:(fun acc -> function
        | Underscore loc -> Spc(loc, acc, U (unify_var()))
        | Fc (loc, fc) -> Spc (loc, acc, ds_fc fc)
        | Exp exp-> App (loc exp, acc, ds_exp exp))

  | If (loc, cond, true_, false_) ->
    If (loc, ds_exp cond, ds_exp true_, ds_exp false_)

  | Lambda (_, {first=arg;rest=args}, body) ->

    List.fold_right (arg :: args) ~init:(ds_exp body)
      ~f:(fun arg body -> match arg with
        | First {pat;lin} ->
          let (loc, var, body) = ds_pat pat body in
          Lambda (loc, var, ds_lin lin, body)
        | Second (loc, fc_var) ->
          Gen (loc, fc_var, body))

  | Index (loc, var, fst, snd) ->
    let one get : Ast.exp =
      App (loc, App (loc, Spc (loc, Prim (loc, get), U (unify_var ())), Var (loc, var)), ds_exp fst) in
    begin match snd with
    | None -> one Get
    | Some snd -> App (loc, one Get_mat, ds_exp snd)
    end

  | Assign (loc, var, fst, snd, exp) ->
    let one set : Ast.exp = App (loc, App (loc, Prim (loc, set), Var (loc, var)), ds_exp fst) in
    begin match snd with
    | None -> App (loc, one Set, ds_exp exp)
    | Some snd -> App (loc, App (loc, one Set_mat, ds_exp snd), ds_exp exp)
    end

  | Infix (op_loc, fst, op, snd) ->
    let fst = ds_exp fst and snd = ds_exp snd
    and fst_loc = loc fst and snd_loc = loc snd in
    begin match op with
    | Or       -> If (op_loc, fst, True op_loc, snd)
    | And      -> If (op_loc, fst, snd, False op_loc)
    | Plus     -> App (snd_loc, App (fst_loc, Prim (op_loc, IntOp Add), fst), snd)
    | Minus    -> App (snd_loc, App (fst_loc, Prim (op_loc, IntOp Sub), fst), snd)
    | Times    -> App (snd_loc, App (fst_loc, Prim (op_loc, IntOp Mul), fst), snd)
    | Div      -> App (snd_loc, App (fst_loc, Prim (op_loc, IntOp Div), fst), snd)
    | Eq       -> App (snd_loc, App (fst_loc, Prim (op_loc, IntOp Eq), fst), snd)
    | Lt       -> App (snd_loc, App (fst_loc, Prim (op_loc, IntOp Lt), fst), snd)
    | PlusDot  -> App (snd_loc, App (fst_loc, Prim (op_loc, EltOp Add), fst), snd)
    | MinusDot -> App (snd_loc, App (fst_loc, Prim (op_loc, EltOp Sub), fst), snd)
    | TimesDot -> App (snd_loc, App (fst_loc, Prim (op_loc, EltOp Mul), fst), snd)
    | DivDot   -> App (snd_loc, App (fst_loc, Prim (op_loc, EltOp Div), fst), snd)
    | EqDot    -> App (snd_loc, App (fst_loc, Prim (op_loc, EltOp Eq), fst), snd)
    | LtDot    -> App (snd_loc, App (fst_loc, Prim (op_loc, EltOp Lt), fst), snd)
    end

  | LetAnnot (loc, bang_var, lin, exp, body) ->
    let (loc, var, body) = ds_pat (Base (loc, bang_var)) @@ ds_exp body in
    App (loc, Lambda (loc, var, ds_lin lin, body), ds_exp exp)

  | LetPat (loc, pat, exp, body) ->
    let exp = ds_exp exp and body = ds_exp body in
    (* Unroll ds_pat one level to prevent unnecessary 'Let's *)
    begin match pat with
    | Unit _ ->
      Unit_E (loc, exp, body)

    | Base (_, NotB var) ->
      Let (loc, var, exp, body)

    | Base (var_loc, Bang var) ->
      Bang_E (loc, var, exp,
              Bang_E (loc, var, Bang_I (loc, Bang_I (loc, Var (var_loc, var))), body))

    | Many (_, pat) ->
      let (_, var, body) = ds_pat pat body in
      Bang_E (loc, var, exp, body)

    | Pair (_, a, b) ->
      let (_, var_b, body) = ds_pat b body in
      let (_, var_a, body) = ds_pat a body in
      Pair_E (loc, var_a, var_b, exp, body)
    end

  | LetFun (loc, fun_var, {first=arg;rest=args}, fun_lin, fun_exp, in_body) ->

    let in_body = ds_exp in_body
    and (fun_lin, fun_exp) =
      List.fold_right (arg :: args) ~init:(ds_lin fun_lin, ds_exp fun_exp)
        ~f:(fun arg (fun_lin, fun_exp) -> match arg with

          | First {pat;lin=arg_lin} ->
            let arg_lin = ds_lin arg_lin in
            (Ast.Fun (arg_lin, fun_lin),
             let (loc, arg_var, fun_exp) = ds_pat pat fun_exp in
             Lambda (loc, arg_var, arg_lin, fun_exp))

          | Second (loc, fc_var) ->
            (All (fc_var, fun_lin), Gen (loc, fc_var, fun_exp))) in

    begin match fun_var with
    | NotB fun_var ->
      App (loc, Lambda (loc, fun_var, fun_lin, in_body), fun_exp)
    | Bang fun_var ->
      App (loc,
           Lambda (loc, fun_var, Bang fun_lin,
                   Bang_E (loc, fun_var, Var (loc, fun_var), in_body)),
           Bang_I (Ast.loc fun_exp, fun_exp))
    end

  | LetRecFun (loc, fun_var, {pat;lin=arg_lin}, args, res_lin, res_exp, in_body) ->

    let (res_lin, res_exp) =
      List.fold_right args ~init:(ds_lin res_lin, ds_exp res_exp)
        ~f:(fun arg (res_lin, res_exp) -> match arg with

          | First {pat;lin=arg_lin} ->
            let arg_lin = ds_lin arg_lin in
            (Ast.Fun (arg_lin, res_lin),
             let (loc, arg_var, res_exp) = ds_pat pat res_exp in
             Lambda (loc, arg_var, arg_lin, res_exp))

          | Second (loc, fc_var) ->
            (All (fc_var, res_lin), Gen (loc, fc_var, res_exp))) in

    let (_, arg_var, res_exp) = ds_pat pat res_exp
    and arg_lin = ds_lin arg_lin in
    App (loc, Lambda (loc, fun_var, Bang (Fun (arg_lin, res_lin)),
                      Bang_E (loc, fun_var, Var (loc, fun_var), ds_exp in_body)),
         Fix (loc, fun_var, arg_var, arg_lin, res_lin, res_exp))
;;

let ds_exp exp =
  (reset(); ds_exp exp)
;;
