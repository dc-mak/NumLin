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
  | Eye
  | Copy_mat
  | Copy_mat_to
  | Size_mat
  | Transpose
  (* Level 2/3 BLAS *)
  | Symm
  | Gemm
  | Gesv
  | Posv
  | Potrs
  | Syrk
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

type mat_var =
  | Just of loc * var
  | Symm of loc * var
  | Trsp of loc * var
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

and mat_exp =
  | Copy_mat of loc * var
  | Copy_mat_to of loc * var
  | New_AB of exp * exp * loc * float * mat_var * mat_var
  | AB_C of loc * float * mat_var * mat_var * loc * float * loc * var
  | ArrIndex of var * loc * (exp * exp option)

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
  | AppLike of exp * arg_like non_empty
  | If of loc * exp * exp * exp
  | Lambda of (annot_arg, loc * var) Either.t non_empty * exp
  | Index of loc * var * loc * exp * exp option
  | Assign of loc * var * loc * exp * exp option * exp
  | Infix of loc * exp * op * exp
  | LetAnnot of loc * bang_var * lin * exp * exp
  | LetPat of loc * pat * exp * exp
  | LetFun of loc * bang_var * (annot_arg, loc * var) Either.t non_empty * exp * exp
  | LetRecFun of loc * bang_var * annot_arg * (annot_arg, loc * var) Either.t list * lin * exp * exp
  | LetMat of loc * bang_var * loc * mat_exp * exp
[@@deriving sexp_of]
;;

let reset, inc =
  let init = 0 in
  let x = ref init in
  (fun () -> x := init),
  (fun () -> let y = !x in (x := y + 1; y))
;;

let fresh name =
  Printf.sprintf "__%s%d" name (inc())
;;

let unify_var () =
  fresh "unify"
;;

let rec ds_pat pat (body : Ast.exp) : loc * Ast.var * Ast.exp =
  match pat with
  | Unit loc ->
    let var = fresh "unit" in (loc, var, Unit_E (loc, Var (loc, var), body))

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


exception MatrixPat of string
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
      | Eye -> Eye
      | Copy_mat -> Copy_mat
      | Copy_mat_to -> Copy_mat_to
      | Size_mat -> Size_mat
      | Transpose -> Transpose
      (* Level 2/3 BLAS *)
      | Symm -> Symm
      | Gemm -> Gemm
      | Gesv -> Gesv
      | Posv -> Posv
      | Potrs -> Potrs
      | Syrk -> Syrk
    end)

  | Var (loc, x) -> Var (loc, x)
  | Unit_I loc -> Unit_I loc
  | True loc -> True loc
  | False loc -> False loc
  | Int_I (loc, i) -> Int_I (loc, i)
  | Elt_I (loc, e) -> Elt_I (loc, e)
  | Pair_I (loc, a, b) -> Pair_I (loc, ds_exp a, ds_exp b)
  | Bang_I (loc, exp) -> Bang_I (loc, ds_exp exp)

  | AppLike (func, {first=arg; rest=args}) ->

    List.fold (arg :: args) ~init:(ds_exp func)
      ~f:(fun acc -> function
        | Underscore loc -> Spc(loc, acc, U (unify_var()))
        | Fc (loc, fc) -> Spc (loc, acc, ds_fc fc)
        | Exp exp-> let exp = ds_exp exp in App (Ast.loc exp, acc, exp))

  | If (loc, cond, true_, false_) ->
    If (loc, ds_exp cond, ds_exp true_, ds_exp false_)

  | Lambda ({first=arg;rest=args}, body) ->

    List.fold_right (arg :: args) ~init:(ds_exp body)
      ~f:(fun arg body -> match arg with
        | First {pat;lin} ->
          let (loc, var, body) = ds_pat pat body in
          Lambda (loc, var, ds_lin lin, body)
        | Second (loc, fc_var) ->
          Gen (loc, fc_var, body))

  | Index (loc, var, prim_loc, fst, snd) ->
    let one get : Ast.exp =
      let fst = ds_exp fst in
      App (Ast.loc fst, App (loc, Spc (loc,
        Prim (prim_loc, get), U (unify_var ())), Var (loc, var)), fst) in

    begin match snd with
    | None -> one Get
    | Some snd -> let snd = ds_exp snd in App (Ast.loc snd, one Get_mat, snd)
    end

  | Assign (loc, var, prim_loc, fst, snd, exp) ->
    let one set : Ast.exp =
      let fst = ds_exp fst in
      App (Ast.loc fst, App (loc, Prim (prim_loc, set), Var (loc, var)), fst) in
    begin match snd with
    | None ->
      let exp = ds_exp exp in
      App (Ast.loc exp, one Set, exp)
    | Some snd ->
      let snd = ds_exp snd and exp = ds_exp exp in
      App (Ast.loc exp, App (Ast.loc snd, one Set_mat, snd), exp)

    end

  | Infix (op_loc, fst, op, snd) ->
    let fst = ds_exp fst and snd = ds_exp snd in
    let fst_loc = Ast.loc fst and snd_loc = Ast.loc snd in
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

  | LetFun (loc, fun_var, {first=arg;rest=args}, fun_exp, in_body) ->

    let in_body = ds_exp in_body
    and fun_exp =
      List.fold_right (arg :: args) ~init:(ds_exp fun_exp)
        ~f:(fun arg fun_exp -> match arg with

          | First {pat;lin=arg_lin} ->
            let arg_lin = ds_lin arg_lin in
            let (loc, arg_var, fun_exp) = ds_pat pat fun_exp in
            Lambda (loc, arg_var, arg_lin, fun_exp)

          | Second (loc, fc_var) ->
            Gen (loc, fc_var, fun_exp)) in

    begin match fun_var with
    | NotB fun_var ->
      Let (loc, fun_var, fun_exp, in_body)
    | Bang fun_var ->
      Bang_E (loc, fun_var, Bang_I (Ast.loc fun_exp, fun_exp), in_body)
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
    begin match fun_var with
    | Bang fun_var ->
      Bang_E (loc, fun_var, Bang_I(loc, Fix (loc, fun_var, arg_var, arg_lin, res_lin, res_exp)), ds_exp in_body)
    | NotB fun_var ->
      Let (loc, fun_var, Fix (loc, fun_var, arg_var, arg_lin, res_lin, res_exp), ds_exp in_body)
    end

  | LetMat (var_loc, bang_var, arr_loc, ArrIndex (arr, index_loc, (index1, index2)), in_body) ->
    ds_exp @@
    LetPat (
      var_loc,
      Pair (var_loc, Base (arr_loc, NotB arr) , Base (var_loc, bang_var)),
      Index (arr_loc, arr, index_loc, index1, index2),
      in_body
    )

  | LetMat (new_loc, Bang new_var, _, (Copy_mat _ | Copy_mat_to _ | New_AB _ | AB_C _), _) ->
    raise @@
    MatrixPat
      (Printf.sprintf !"Can't use ! on %s at %{Ast.line_col}, make aliases \
                        of arrays & matrices with share and unshare.\n"  new_var new_loc)

  | LetMat (new_loc, NotB new_var, prim_loc, (Copy_mat _ | Copy_mat_to _ | New_AB _ | AB_C _ as mat_exp), body) ->

    let match_on ~alpha_loc ~alpha ~a ~b ~beta_loc ~beta ~c_loc ~c : Ast.exp =

      let trsp = function
        | Trsp (loc,_) -> True loc
        | Just (loc,_) -> False loc
        | Symm _ -> assert false in

      let gemm ~a_loc ~a ~trsp_a ~b_loc ~b ~trsp_b =

        let first = Exp (Elt_I (alpha_loc, alpha))
        and rest = [
          Underscore a_loc;
          Exp (Pair_I (a_loc, Var (a_loc, a), trsp_a));
          Underscore b_loc;
          Exp (Pair_I (b_loc, Var (b_loc, b), trsp_b));
          Exp (Elt_I (beta_loc, beta));
          Exp (Var (c_loc, c));
        ]

        and tmp_var = "_p_" ^ a ^ "_" ^ "_p_" in
        let exp = ds_exp @@ AppLike (Prim (prim_loc, Gemm), {first; rest}) in
        Ast.Pair_E (new_loc, tmp_var, new_var, exp,
                    (* using prim_loc is arbitrary here *)
                    Pair_E (prim_loc, a, b, Var (prim_loc, tmp_var), ds_exp body))
      in

      match a, b with

      (* syrk or gemm *)
      | (Just (a_loc, a) as a'), (Trsp (b_loc, b) as b')
      | (Trsp (a_loc, a) as a'), (Just (b_loc, b) as b') ->

        if not String.(a = b) then
          gemm ~a_loc ~a ~trsp_a:(trsp a') ~b_loc ~b ~trsp_b:(trsp b')
        else
          (* syrk *)
          let first = Exp (trsp a')
          and rest = [
            Exp (Elt_I (alpha_loc, alpha));
            Underscore a_loc;
            Exp (Var (a_loc, a));
            Exp (Elt_I (beta_loc, beta));
            Exp (Var (c_loc, c));
          ] in

          let exp = ds_exp @@ AppLike (Prim (prim_loc, Syrk), {first; rest}) in
          Pair_E (prim_loc, a, c, exp, ds_exp body)

      (* gemm *)
      | (Just (a_loc, a) as a'), (Just (b_loc, b) as b')
      | (Trsp (a_loc, a) as a'), (Trsp (b_loc, b) as b') ->

        gemm ~a_loc ~a ~trsp_a:(trsp a') ~b_loc ~b ~trsp_b:(trsp b')

      (* symm *)
      | (Symm (a_loc, a) as a'), Just (b_loc, b)
      | (Just (a_loc, a) as a'), Symm (b_loc, b) ->

        let (first, a_loc, a, b_loc, b) =
          match a' with
          | Symm (loc, _) -> (Exp (False loc), a_loc, a, b_loc, b)
          | Just (loc, _) -> (Exp (True loc), b_loc, b, a_loc, a)
          | Trsp _ -> assert false
        in
        let rest = [
          Exp (Elt_I (alpha_loc, alpha));
          Underscore a_loc;
          Exp (Var (a_loc, a));
          Underscore b_loc;
          Exp (Var (b_loc, b));
          Exp (Elt_I (beta_loc, beta));
          Exp (Var (c_loc, c));
        ]

        and tmp_var = "_p_" ^ a ^ "_" ^ "_p_" in
        let exp = ds_exp @@ AppLike (Prim (prim_loc, Symm), {first; rest}) in
        Pair_E (new_loc, tmp_var, new_var, exp,  
                (* using prim_loc is arbitrary here *)
                Pair_E (prim_loc, a, b, Var (prim_loc, tmp_var), ds_exp body))

      (* illegal *)
      | Symm (a_loc, _), Symm (b_loc, _) ->
        raise @@
        MatrixPat
          (Printf.sprintf !"Need only one 'sym' annotation, \
                            either %{Ast.line_col} or %{Ast.line_col}.\n" a_loc b_loc)

      | Symm (a_loc, _), Trsp (b_loc, _)
      | Trsp (a_loc, _), Symm (b_loc, _) ->
        raise @@
        MatrixPat
          (Printf.sprintf !"Cannot use 'sym' annotation with transpose, at \
                            %{Ast.line_col} and %{Ast.line_col}.\n" a_loc b_loc)

    in

    begin match mat_exp with

    | ArrIndex _ -> assert false

    | Copy_mat (reb_loc, rebound) ->
      let unify = unify_var () in
      Pair_E (new_loc, rebound, new_var,
              App (reb_loc, Spc (reb_loc, Prim (prim_loc, Copy_mat), U unify),
                   Var (reb_loc, rebound)),
              ds_exp body)

    | Copy_mat_to (reb_loc, rebound) ->
      let unify = unify_var () in
      Pair_E (new_loc, rebound, new_var,
              App (new_loc, App(reb_loc, Spc (reb_loc, Prim (prim_loc, Copy_mat_to), U unify),
                    Var (reb_loc, rebound)), Var (new_loc, new_var)),
              ds_exp body)

    | New_AB (row, col, alpha_loc, alpha, a, b) ->
      let row = ds_exp row and col = ds_exp col in
      Let (new_loc, new_var,
           App (Ast.loc col, App (Ast.loc row, Prim (prim_loc, Matrix), row), col),
           match_on ~alpha_loc ~alpha ~a ~b ~beta_loc:prim_loc ~beta:0. ~c_loc:prim_loc ~c:new_var)

    | AB_C (alpha_loc, alpha, a, b, beta_loc, beta, c_loc, c) ->
      match_on ~alpha_loc ~alpha ~a ~b ~beta_loc ~beta ~c_loc ~c

    end
;;

(* Don't let impure implementation result in impure interface/behaviour *)
let ds_exp exp =
  let () = reset () in
  try Ok (ds_exp exp)
  with MatrixPat msg -> Error msg
;;
