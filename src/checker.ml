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
  (* Boolean *)
  | Not_ -> Fun (bool, bool)
  (* Arithmetic *)
  | IntOp op -> binary int op
  | EltOp op -> binary elt op
  (* Arrays *)
  | Set -> Fun (Arr Z, Fun (int, Fun (elt, Arr Z)))
  | Get -> All(x, Fun (Arr (V x), Fun (int, Pair (Arr (V x), elt))))
  | Share -> All (x, Fun (Arr (V x), Pair (Arr (S (V x)), Arr (S (V x)))))
  | Unshare -> All (x, Fun (Arr (S (V x)), Fun (Arr (S (V x)), Arr (V x))))
  | Free -> Fun (Arr Z, Unit)
  (* Owl *)
  | Array -> Fun (int, Arr Z)
  | Copy -> All (x, Fun (Arr (V x), Pair (Arr (V x), Arr Z)))
  | Sin -> Fun (Arr Z, Arr Z)
  | Hypot -> Fun (Arr Z, All (x, Fun (Arr (V x), Pair (Arr (V x), Arr Z))))
  (* Level 1 BLAS *)
  | Asum -> All (x, Fun (Arr (V x), Pair (Arr (V x), elt)))
  | Axpy -> Fun (Arr Z, Fun (elt, All (x, Fun (Arr (V x), Pair (Arr (V x), Arr Z)))))
  | Dot -> All (x, Fun (Arr (V x), All (y, Fun (Arr (V y), Pair (Pair (Arr (V x), Arr (V y)), elt)))))
  | Rotmg -> Fun (Pair (elt, elt), Fun (Pair (elt, elt), Pair (Pair (elt, elt), Pair (elt, Arr Z))))
  | Scal -> Fun (elt, Fun (Arr Z, Arr Z))
  | Amax -> All (x, Fun (Arr (V x), Pair (Arr (V x), int)))
  (* Matrix *)
  | Get_mat -> All(x, Fun (Mat (V x), Fun (int, Fun(int, Pair (Mat (V x), elt)))))
  | Set_mat -> Fun (Mat Z, Fun (int, (Fun (int, Fun (elt, Mat Z)))))
  | Share_mat -> All (x, Fun (Mat (V x), Pair (Mat (S (V x)), Mat (S (V x)))))
  | Unshare_mat -> All (x, Fun (Mat (S (V x)), Fun (Mat (S (V x)), Mat (V x))))
  | Free_mat -> Fun(Mat Z, Unit)
  | Matrix -> Fun (int, Fun (int, Mat Z))
  | Eye -> Fun (int, Mat Z)
  | Copy_mat -> All (x, Fun (Mat (V x), Pair (Mat (V x), Mat Z)))
  | Copy_mat_to -> All (x, Fun (Mat (V x), Fun (Mat Z, Pair (Mat (V x), Mat Z))))
  | Size_mat -> All (x, Fun (Mat (V x), Pair (Mat (V x), Pair (int, int))))
  | Transpose -> All (x, Fun (Mat (V x), Pair (Mat (V x), Mat Z)))
  (* Level 3 BLAS/LAPACK *)
  | Gemm -> Fun (elt, All(x, Fun (Pair (Mat (V x), bool), All (y, Fun (Pair (Mat (V y), bool), Fun (elt, Fun (Mat Z, Pair (Pair (Mat (V x), Mat (V y)), Mat Z))))))))
  | Symm -> Fun (bool, Fun (elt, All(x, Fun (Mat (V x), All (y, Fun (Mat (V y), Fun (elt, Fun (Mat Z, Pair (Pair (Mat (V x), Mat (V y)), Mat Z)))))))))
  (* FIXME: This is a lie because of IPIV parameter *)
  | Gesv -> Fun (Mat Z, Fun (Mat Z, Pair (Mat Z, Mat Z)))
  | Posv -> Fun (Mat Z, Fun (Mat Z, Pair (Mat Z, Mat Z)))
  | Potrs -> All (x, Fun (Mat (V x), Fun (Mat Z, Pair (Mat (V x), Mat Z))))
  | Syrk -> Fun (bool, Fun (elt, All (x, Fun (Mat (V x), Fun (elt, Fun (Mat Z, Pair (Mat (V x), Mat Z)))))))
;;

let string_of_loc (loc : Ast.loc) =
  Printf.sprintf "In %s, at line: %d and column: %d"
    loc.pos_fname
    loc.pos_lnum
    (loc.pos_cnum - loc.pos_bol + 1)
;;

let error (loc : Ast.loc) ~expected inferred =
  let lin_to_str = Ast.(string_of_pp pp_lin) in
  Check_monad.failf !"%s\nActual: %{lin_to_str}\n%{string_of_loc}\n"
    expected
    inferred
    loc
;;

(* The actual checking algorithm *)
let rec check =
  let open Ast in
  let open Check_monad in
  let open Let_syntax in
  function
  | Prim (loc, prim) ->
    wf_lin (check_prim prim)
      ~fmt:!"Internal Error: Primitive is not well-formed.\n%{sexp: Ast.prim}\n%{string_of_loc}\n"
      ~arg:prim
      ~loc

  | Unit_I _ -> return @@ wf_Unit
  | True _ -> return @@ wf_Bang wf_Bool
  | False _ -> return @@ wf_Bang wf_Bool
  | Var (loc, var) ->
    let%bind typ = lookup var in
    begin match typ with
    | Some (Not_used var) ->
      let%bind lin = use_var loc var in
      return lin
    | Some (Intuition lin) ->
      return lin
    | Some (Used (used, _)) ->
      failf !"Variable %s (first used %{Ast.line_col}) used again.\n%{string_of_loc}\n" var used loc
    | None ->
      failf !"Unbound variable %s (not found in enviornment)\n%{string_of_loc}\n" var loc
    end

  | Int_I _ -> return @@ wf_Bang wf_Int
  | Elt_I _ -> return @@ wf_Bang wf_Elt
  | Pair_I (_,fst, snd) ->
    let%bind fst = check fst in
    let%bind snd = check snd in
    return @@ wf_Pair fst snd

  | Bang_I (loc, exp) ->
    if Ast.is_value exp then
      let%bind res = in_empty @@ check exp in
      return @@ wf_Bang res
    else
      failf !"Can only call 'Many' on values.\n%{string_of_loc}\n" loc

  | Fix (loc, f, x, tx, res, body) ->
    let fmt, arg = !"Type is not well-formed:\n%{sexp:lin}\n%{string_of_loc}\n", Fun (tx, res) in
    split_wf_Fun (wf_lin ~fmt ~arg ~loc arg)
      ~if_fun:
        (fun tx res ->
           let%bind (actual, fun_t) =
             (* TODO Generalise free frac. cap. vars for t here *)
             in_empty (check body |> with_lin x tx |> return_int f (wf_Fun tx res)) in
           match%bind same_lin res actual with
           | Ok subs ->
             apply subs fun_t
           | Error err ->
             failf !"%{Error.to_string_hum}%{string_of_loc}\n" err loc)

      ~not_fun:
        (failf !"Internal Error: passed in\n    %{sexp:lin}\nbut got out\n    %{sexp:lin}.\n" arg)

  | Spc (loc, exp, fc) ->
    split_wf_All (check exp)
      ~if_all:
        (fun var lin ->
           if_wf fc
             ~then_:(fun fc -> return @@ wf_substitute_fc lin var fc)
             ~else_:(fun fc -> failf !"Spc: %{sexp:fc} not found in environment.\
                                       \n%{string_of_loc}\n" fc loc))
      ~not_all:
        (error  loc ~expected:"Spc: expected All(_,_)")

  | App (loc, func, arg) ->
    split_wf_Fun (check func)
      ~if_fun:
        (fun expected body_t ->
           let%bind actual = check arg in
           match%bind same_lin expected actual with
           | Ok subs ->
             apply subs body_t
           | Error err ->
             failf !"%{Error.to_string_hum}%{string_of_loc}\n" err loc)
      ~not_fun:
        (error loc ~expected:"App: expected Fun(_,_)")

  | Unit_E (loc, exp, body) ->
    begin match%bind check exp with
    | WFL Unit -> check body
    | WFL (Unk var) ->
      apply [Either.Second (var, Ast.Unit)] wf_Unit
    | WFL inferred -> error loc ~expected:"Unit_E: expect Unit" inferred
    end

  | Bang_E (loc, x, exp, body) ->
    split_wf_Bang (check exp)
      ~if_bang:
        (fun exp ->
          check body |> with_int x exp)
      ~not_bang:
        (error loc ~expected:"Bang_E: expect Bang _")

  | Pair_E (loc, a, b, pair, body) ->
    split_wf_Pair (check pair)
      ~if_pair:
        (fun ta tb ->
           check body |> with_lin a ta |> with_lin b tb)
      ~not_pair:
        (error loc ~expected:"Pair_E: expected Pair (_, _)")

  | If (loc, cond, true_, false_) ->
    begin match%bind check cond with
    | WFL (Bang Bool) ->
      let%bind (t, f) =
        same_resources
          (check true_, Ast.loc true_)
          (check false_, Ast.loc false_) in
      begin match%bind same_lin t f with
      | Ok subs ->
        apply subs t
      | Error err ->
        failf !"%{Error.to_string_hum}%{string_of_loc}\n" err loc
      end

    | WFL inferred ->
      error loc ~expected:"If: expected (Bang Bool)" inferred
    end

  | Gen (_, var, exp) ->
    let%bind lin = check exp |> with_fc var in
    return @@ wf_All var lin

  | Lambda (loc, var, t, body) ->
    let fmt, arg = !"Type is not well-formed:\n%{sexp:lin}\n%{string_of_loc}\n", t in
    let%bind t = wf_lin ~fmt ~arg ~loc t in
    let%bind (body_t, t) = check body |> return_lin var t in
    (* Generalise any free frac. cap. vars of the argument here *)
    let%bind t =
      begin match t with
      | WFL (Arr fc) | WFL (Mat fc) ->
        let rec free_var = function
          | Z -> return None
          | (U var | V var) as fc ->
            if_wf fc
              ~then_:(fun _ -> return None)
              ~else_:(fun _ -> return @@ Some var)
          | S fc -> free_var fc in
        begin match%bind free_var fc with
        | None -> return t
        | Some var -> return @@ wf_All var t
        end
      | t -> return t
      end in
    return @@ wf_Fun t body_t

  | Let (_, var, exp, body) ->
    let%bind lin = check exp in
    check body |> with_lin var lin

;;

let check_expr expr ~counter =
  Check_monad.run (check expr) ~counter
;;

let%test_module "Test" =
  (module struct
  end)
;;
