(* Dhruv Makwana *)
(* LT4LA (Type) Checker *)
(* --------------------- *)

open Base
;;

(* For simplicity in these types, I'm always assuming that (1) lengths `n` can
   be retrieved at runtime and (2) incx = incy = 1. *)

let check_prim =
  let open Ast in

  let x = "x" and y = "y" in

  let elt, int, bool = Bang Elt, Bang Int, Bang Bool in
  let binary x = function
    | Add | Sub | Mul | Div -> Fun(x, Fun (x, x))
    | Eq | Lt -> Fun(x, Fun (x, bool)) in

  function
  | Not_ -> Fun (bool, bool)
  | IntOp op -> binary int op
  | EltOp op -> binary elt op
  | Set -> Fun (Arr Z, Fun (int, Fun (elt, Arr Z)))
  | Get -> All(x, Fun (Arr (V x), Fun (int, Pair (elt, Arr (V x)))))
  | Share -> All (x, Fun (Arr (V x), Pair (Arr (S (V x)), Arr (S (V x)))))
  | Unshare -> All (x, Fun (Arr (S (V x)), Fun (Arr (S (V x)), Arr (V x))))
  | Free -> Fun (Arr Z, Unit)
  | Array -> Fun (int, Arr Z)
  | Copy -> All (x, Fun (Arr (V x), Pair(Arr (V x), Arr Z)))
  | Sin -> Fun (Arr Z, Arr Z)
  | Rotmg -> Fun (Pair (elt, elt), Fun (Pair (elt, elt), Pair (Pair (elt, elt), Pair (elt, Arr Z))))
  | Scal -> Fun (elt, Fun (Arr Z, Arr Z))
  | Amax -> All (x, Fun (Arr (V x), Pair (int, Arr (V x))))
  | Asum -> All (x, Fun (Arr (V x), Pair (elt, Arr (V x))))
  | Hypot -> Fun (Arr Z, All (x, Fun (Arr (V x), Pair (Arr Z, Arr (V x)))))
  | Axpy -> Fun (Arr Z, Fun (elt, All (x, Fun (Arr (V x), Pair (Arr Z, Arr (V x))))))
  | Dot -> All (x, Fun (Arr (V x), All (y, Fun (Arr (V y), Pair (elt, Pair (Arr (V x), Arr (V y)))))))
  (* matrix *)
  | Get_mat -> All(x, Fun (Arr (V x), Fun (int, Fun(int, Pair (elt, Mat (V x))))))
  | Set_mat -> Fun (Mat Z, Fun (int, (Fun (int, Fun (elt, Mat Z)))))
  | Share_mat -> All (x, Fun (Mat (V x), Pair (Mat (S (V x)), Mat (S (V x)))))
  | Unshare_mat -> All (x, Fun (Mat (S (V x)), Fun (Mat (S (V x)), Mat (V x))))
  | Free_mat -> Fun(Mat Z, Unit)
  | Matrix -> Fun (int, Fun (int, Mat Z))
  | Copy_mat -> All (x, Fun (Mat (V x), Pair(Mat (V x), Mat Z)))
  | Symv -> Fun (elt, All (x, Fun (Mat (V x), All (y, Fun (Arr (V y), Fun (elt, Fun (Arr Z, Pair (Pair (Mat (V x), Arr(V y)), Arr Z))))))))
  | Gemv -> Fun (elt, All (x, Fun (Pair (Mat (V x), bool), All (y, Fun (Arr (V y), Fun (elt, Fun (Arr Z, Pair (Pair (Mat (V x), Arr(V y)), Arr Z))))))))
  | (Trmv | Trsv) -> All (x, Fun (Pair (Mat (V x), bool), Fun (Arr Z, Pair (Mat (V x), Arr Z))))
  | Ger -> Fun (elt, All (x, Fun (Arr (V x), All (y, Fun (Arr (V y), Fun (Mat Z, Pair (Pair (Arr (V x), Arr (V y)), Mat Z)))))))
  | Gemm -> Fun (elt, All(x, Fun (Pair(Mat (V x), bool), All (y, Fun (Pair (Mat (V y), bool), Fun (elt, Fun (Mat Z, Pair (Pair (Mat (V x), Mat (V y)), Mat Z))))))))
  | (Trmm | Trsm) -> Fun (elt, Fun(bool, Fun (Mat Z, All (x, Fun (Pair (Mat (V x), bool), Pair (Mat (V x), Mat Z))))))
;;

let error ~expected inferred =
  Check_monad.failf !"%s\nActual: %s\n" expected @@ Ast.(string_of_pp pp_lin) inferred
;;

(* The actual checking algorithm *)
let rec check =
  let open Ast in
  let open Check_monad in
  let open Let_syntax in
  function
  | Prim prim ->
    wf_lin (check_prim prim)
      ~fmt:!"Internal Error: Primitive is not well-formed.\n%{sexp: Ast.prim}"
      ~arg:prim

  | Unit_I -> return @@ (* wf_Bang ?? *) wf_Unit
  | True -> return @@ wf_Bang wf_Bool
  | False -> return @@ wf_Bang wf_Bool
  | Var var ->
    let%bind typ = lookup var in
    begin match typ with
    | Some (Not_used var) ->
      let%bind lin = use_var var in
      return lin
    | Some (Intuition lin) ->
      return lin
    | Some Used ->
      failf !"Variable %s used twice (or more).\n" var
    | None ->
      fail_string ("Unbound variable " ^ var ^ " (not found in environment)\n")
    end

  | Int_I _ -> return @@ wf_Bang wf_Int
  | Elt_I _ -> return @@ wf_Bang wf_Elt
  | Pair_I (fst, snd) ->
    let%bind fst = check fst in
    let%bind snd = check snd in
    return @@ wf_Pair fst snd

  | Bang_I exp ->
    if Ast.is_value exp then
      let%bind res = in_empty @@ check exp in
      return @@ wf_Bang res
    else
      failf "Can only call 'Many' on values.\n"

  | Fix (f, x, tx, res, body) ->
    let fmt, arg = !"Type is not well-formed:\n%{sexp:lin}", Fun (tx, res) in
    split_wf_Fun (wf_lin ~fmt ~arg arg)
      ~if_fun:
        (fun tx res ->
           let%bind actual =
             in_empty (check body |> with_lin x tx |> with_int f (wf_Fun tx res)) in
           match%bind same_lin res actual with
           | Ok subs ->
             return @@ apply subs @@ wf_Bang @@ wf_Fun tx res
           | Error err ->
             fail_string @@ Error.to_string_hum err)

      ~not_fun:
        (failf !"Internal Error: passed in\n    %{sexp:lin}\nbut got out\n    %{sexp:lin}.\n" arg)

  | Spc (exp, fc) ->
    split_wf_All (check exp)
      ~if_all:
        (fun var lin ->
           if_wf fc
             ~then_:(fun fc -> return @@ wf_substitute_in lin var fc)
             ~else_:(failf !"Spc: %{sexp:fc} not found in environment.\n"))
      ~not_all:
        (error ~expected:"Spc: expected All(_,_)")

  | App (func, arg) ->
    split_wf_Fun (check func)
      ~if_fun:
        (fun expected body_t ->
           let%bind actual = check arg in
           match%bind same_lin expected actual with
           | Ok subs ->
             return @@ apply subs body_t
           | Error err ->
             fail_string @@ Error.to_string_hum err)
      ~not_fun:
        (error ~expected:"App: expected Fun(_,_)")

  | Bang_E (x, exp, body) ->
    split_wf_Bang (check exp)
      ~if_bang:
        (fun exp ->
          check body |> with_int x exp)
      ~not_bang:
        (error ~expected:"Bang_E: expect Bang _")

  | Pair_E (a, b, pair, body) ->
    split_wf_Pair (check pair)
      ~if_pair:
        (fun ta tb ->
           check body |> with_lin a ta |> with_lin b tb)
      ~not_pair:
        (error ~expected:"Pair_E: expected Pair (_, _)")

  | If (cond, true_, false_) ->
    begin match%bind check cond with
    | WFL (Bang Bool) ->
      let%bind (t, f) = same_resources (check true_) (check false_) in
      begin match%bind same_lin t f with
      | Ok subs -> return @@ apply subs t
      | Error err -> fail_string @@ Error.to_string_hum err
      end

    | WFL inferred ->
      error ~expected:"If: expected Bool" inferred
    end

  | Gen (var, exp) ->
    let%bind lin = check exp |> with_fc var in
    return @@ wf_All var lin

  | Lambda (var, t, body) ->
    let fmt, arg = !"Type is not well-formed:\n%{sexp:lin}", t in
    let%bind t = wf_lin ~fmt ~arg t in
    let%bind body_t = check body |> with_lin var t in
    return @@ wf_Fun t body_t

;;

let check_expr expr =
  Check_monad.run (check expr)
;;

let%test_module "Test" =
  (module struct
  end)
;;
