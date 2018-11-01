(* Dhruv Makwana *)
(* LT4LA Check Monad *)
(* ----------------- *)

open Base
;;

(*###############################################################################################*)
(*                                     (1) Type Constructors                                     *)
(*###############################################################################################*)

type wf_fc =
    WFC of Ast.fc
[@@ocaml.unboxed]
;;

type wf_var =
    WFV of Ast.var
[@@ocaml.unboxed]
;;

type wf_lin =
    WFL of Ast.lin
[@@ocaml.unboxed]
[@@deriving sexp_of]
;;

let wf_Unit =
  WFL Ast.Unit

and wf_Bool =
  WFL Ast.Bool

and wf_Int =
  WFL Ast.Int

and wf_Elt =
  WFL Ast.Elt

and wf_Pair (WFL x) (WFL y) =
  WFL Ast.(Pair (x, y))

and wf_Bang (WFL x) =
  WFL Ast.(Bang x)

and wf_Arr_Z =
  WFL Ast.(Arr Z)

and wf_Mat_Z =
  WFL Ast.(Mat Z)

and wf_Fun (WFL x) (WFL y) =
  WFL Ast.(Fun (x, y))

and wf_All var (WFL x) =
  WFL (Ast.All (var, x))
;;

type not_used =
  { var : Ast.var
  ; t   : wf_lin
  }
[@@deriving sexp_of]
;;

type tagged =
  | Not_used of not_used
  | Used of Ast.loc * wf_lin
  | Intuition of wf_lin
[@@deriving sexp_of]
;;

type state =
  { env : (Ast.var, tagged) List.Assoc.t
  ; used_vars : (Ast.var, Ast.loc, Ast.comparator_witness) Map.t sexp_opaque
  ; fc_vars  : Ast.var list
  ; counter : int
  }
[@@deriving sexp_of]
;;

let empty_used =
  Map.empty (module struct include Ast type t = var end)
;;

include State_or_error.Make (struct type t = state end)
;;

(*###############################################################################################*)
(*                                   (2)  Checker utilities                                      *)
(*###############################################################################################*)

let create_fresh ?(name="gen") () =
  let open Let_syntax in
  let%bind {counter=id;_} as state = get in
  let%bind () = put {state with counter=id+1} in
  return @@ "__" ^ name ^ Int.to_string id
;;

(* Environment manipulation. Invariants:                   *)
(* (1) never re-ordered, always push to and pop from front *)
(* (2) (var, Not_used (var', _)) ==> phys_equal var var'.  *)

let find =
  List.Assoc.find ~equal:[%compare.equal:Ast.var]
;;

let lookup var  =
  let open Let_syntax in
  let%bind {env; _} = get in
  return @@ find env var
;;

let mark_used loc var env used_vars =

  let rec find var accum = function
    | [] -> (accum, None, [])
    | ((var', t) as entry) :: env ->
      if [%compare.equal:Ast.var] var var' then
        (accum, Some t, env)
      else
        find var (entry :: accum) env in

  match find var [] env with
  | _, None, _ | _, Some (Used _), _ | _, Some Intuition _, _ ->
    Result.fail "INTERNAL ERROR: use_var invariant broken."

  | rev_front, Some (Not_used {var;t}), rest ->
    let rec loop rest = function
      | [] -> rest
      | x :: xs -> loop (x :: rest) xs in
    let env = loop ((var, Used (loc, t)) :: rest) rev_front in
    let used_vars = Map.set used_vars ~key:var ~data:loc in
    Result.return (t, env, used_vars)
;;

let use_var loc {var;t=_} =
  let open Let_syntax in
  let%bind {env; used_vars; _} as state = get in
  match mark_used loc var env used_vars with
  | Ok (var_t, env, used_vars) ->
    let%bind () = put { state with env; used_vars } in
    return var_t
  | Error err ->
    fail_string err
;;

let same_lin (WFL expected) (WFL actual) =
  let open Let_syntax in
  let%bind {fc_vars; _} = get in
  let fc_vars = List.map fc_vars ~f:(fun x -> (x,x)) in
  return @@ Ast.same_lin fc_vars expected actual
;;

let apply_env subs =
  let open Let_syntax in
  let sub_into init = List.fold subs ~init ~f:Ast.substitute_unify in
  let%bind {env; _} as state = get in
  put { state with env = List.map env ~f:(function
    | (var, Not_used ({t=WFL lin; _} as not_used)) ->
      (var, Not_used {not_used with t = WFL (sub_into lin)})
    | (var, Used (loc, WFL lin)) ->
      (var, Used (loc, WFL (sub_into lin)))
    | (var, Intuition (WFL lin)) ->
      (var, Intuition (WFL (sub_into lin)))
  ) }
;;

let apply subs (WFL init) =
  let open Let_syntax in
  let%bind () = apply_env subs in
  return @@ WFL (List.fold subs ~init ~f:Ast.substitute_unify)
;;

let with_var var wf_lin result =
  let open Let_syntax in
  let%bind {env; used_vars=prev; _} as state = get in

  (* FIRST update state *)
  let%bind () =
    let value = match wf_lin with
      | Either.First wf_lin -> Not_used {var; t=wf_lin}
      | Either.Second wf_lin -> Intuition wf_lin in
    put { state with env = (var, value) :: env } in

  (* THEN run computation *)
  let%bind result = result in

  (* THEN pop the variable off *)
  let%bind lin =
    let%bind {env; used_vars; _} as state = get in
    match env with
    | (_, Intuition lin) :: env ->
      let%bind () = put { state with env } in
      return lin
    | (_, Used (_, lin)) :: env ->
      (* Variable names are not unique *)
      let used_vars = if not @@ Map.mem prev var then Map.remove used_vars var else used_vars in
      let%bind () = put { state with env; used_vars } in
      return lin
    | (_, Not_used _) :: _ -> failf !"Variable %s not used.\n" var
    | [] -> failf !"INTERNAL ERROR: %{sexp:Ast.var} NOT FOUND IN vars\n" var in

  return (result, lin)
;;

let return_lin var wf_lin result =
  with_var var (Either.First wf_lin) result
;;

let with_lin var wf_lin result =
  map ~f:fst @@ return_lin var wf_lin result
;;

let return_int var wf_lin result =
  with_var var (Either.Second wf_lin) result
;;

let with_int var wf_lin result =
  map ~f:fst @@ return_int var wf_lin result
;;

let with_fc var lin =
  let open Let_syntax in
  let%bind {fc_vars=prev; _} as state = get in

  (* FIRST update state *)
  let%bind () = put { state with fc_vars =  var :: prev } in

  (* THEN run computation *)
  let%bind result = lin in

  (* THEN remove the frac. cap. variable and return *)
  let%bind () =
    let%bind {fc_vars; _} as state = get in
    match fc_vars with
    | _ :: fc_vars -> put { state with fc_vars }
    | [] -> failf !"INTERNAL ERROR: %{sexp:Ast.var} NOT FOUND IN fc_vars\n" var in

  return result
;;

let string_of_used_vars used_vars =
  used_vars
  |> Map.to_alist
  |> List.map ~f:(fun (var, (loc : Ast.loc)) ->
    Printf.sprintf !"%s (%{Ast.line_col})" var loc)
  |> String.concat ~sep:", "
;;

let in_empty wf_lin =
  let open Let_syntax in
  let%bind {used_vars=prev; _} as state = get in
  let%bind () = put { state with used_vars = empty_used } in
  let%bind res = wf_lin in
  let%bind {used_vars; _} as state = get in
  if Map.is_empty used_vars then
    let%bind () = put { state with used_vars = prev } in
    return res
  else
    failf !"Cannot use linearly-typed variables in Fix/Many\
            \n    %{string_of_used_vars}\n"
      used_vars
;;

let report_diff (used_a, loc_a) (used_b, loc_b) =
  let diff1 = Map.filter_keys used_a ~f:(fun x -> not @@ Map.mem used_b x)
  and diff2 = Map.filter_keys used_b ~f:(fun x -> not @@ Map.mem used_a x) in
  if Map.is_empty diff1 then
    failf !"Else-branch (%{Ast.line_col}) used these variables not used by then-branch:\
            \n  %{string_of_used_vars}\n"
      loc_b diff2
  else if Map.is_empty diff2 then
    failf !"Then-branch (%{Ast.line_col}) used these variables not used by else-branch:\
            \n  %{string_of_used_vars}\n"
      loc_a diff1
  else
    failf !"Then-branch (%{Ast.line_col}) used these variables not used by else-branch:\
            \n  %{string_of_used_vars}\n\
            Else-branch (%{Ast.line_col}) used these variables not used by then-branch:\
            \n  %{string_of_used_vars}\n"
      loc_a diff1
      loc_b diff2
;;

let same_resources (wf_a, loc_a) (wf_b, loc_b) =
  let open Let_syntax in
  (* Save state *)
  let%bind {used_vars=prev; env=old_env; _} as state = get in
  (* Reset, run a, save state *)
  let%bind () = put { state with used_vars = empty_used } in
  let%bind res_a = wf_a in
  let%bind {used_vars=used_a; _} as state = get in
  (* Reset, run b, save state *)
  let%bind () = put { state with used_vars = empty_used; env = old_env } in
  let%bind res_b = wf_b in
  let%bind {used_vars=used_b; _} as state = get in
  (* Check if same resources *)
  let keys_a, keys_b =
    let key_set x = Set.of_list (module struct include Ast type t = var end) @@ Map.keys x in
    key_set used_a, key_set used_b in
  if Set.equal keys_a keys_b then
    let used_vars = Map.merge prev used_b ~f:(fun ~key:_ -> function
      | `Left loc | `Right loc | `Both (_, loc) -> Some loc) in
    let%bind () = put { state with used_vars } in
    return (res_a, res_b)
  else
    report_diff (used_a, loc_a) (used_b, loc_b)
;;

let run wf_lin ~counter =
  let open Or_error.Let_syntax in
  let init = {env=[]; fc_vars=[]; counter; used_vars=empty_used} in
  let%bind (WFL result, state) = run wf_lin init in
  begin match state with
  | {env = []; fc_vars = []; used_vars; counter=_} when Map.is_empty used_vars ->
    return result
  | state ->
    Or_error.errorf
      !"INTERNAL ERROR: After checking (%{sexp:Ast.lin}), environment is not empty\n\
        %{sexp:state}\nused_vars:\n%{sexp:(Ast.var * Ast.loc) list}\n"
      result state (Map.to_alist state.used_vars)
  end
;;

(*###############################################################################################*)
(*                                 (3) Well-formed destructors                                   *)
(*                          Probably excessive but fun to experiment with.                       *)
(*###############################################################################################*)

(* Check if fractional-capability is well-formed w.r.t. given list of variables. *)
let rec wf_wrt fc_vars =
  let open Ast in function
    | Z | U _ -> true
    | S fc -> wf_wrt fc_vars fc
    | V var -> List.exists fc_vars ~f:([%compare.equal : Ast.var] var)
;;

(* Checking a linear type is well-formed. *)
let wf_lin ~fmt ~arg ~loc:pos lt =
  let open Let_syntax in
  let open Ast in
  let rec wf bindings = function
    | Unit | Int | Bool | Elt | Unk _ as e ->
      return e

    | Arr fc ->
      if wf_wrt bindings fc then
        return @@ Arr fc
      else if%bind get >>= fun x -> return @@ wf_wrt x.fc_vars fc then
        return @@ Arr fc
      else
        failf fmt arg pos

    | Mat fc ->
      if wf_wrt bindings fc then
        return @@ Mat fc
      else if%bind get >>= fun x -> return @@ wf_wrt x.fc_vars fc then
        return @@ Mat fc
      else
        failf fmt arg pos

    | Pair (fst, snd) ->
      let%bind fst = wf bindings fst and snd = wf bindings snd in
      return @@ Pair (fst, snd)

    | Bang lin ->
      let%bind lin = wf bindings lin in
      return @@ Bang lin

    | Fun (fun_, arg) ->
      let%bind fun_t = wf bindings fun_ and arg_t = wf bindings arg in
      return @@ Fun (fun_t, arg_t)

    | All (var, lin) ->
      let%bind lin = wf (var :: bindings) lin in
      return @@ All (var, lin) in

  let%bind result = wf [] lt in
  return @@ WFL result
;;

let if_wf fc ~then_ ~else_ =
  let open Let_syntax in
  let%bind {fc_vars; _} = get in
  if wf_wrt fc_vars fc then
    then_ @@ WFC fc
  else
    else_ fc
;;

let wf_substitute_fc (WFL lin) (WFV var) (WFC fc) =
  WFL (Ast.substitute_in lin (Either.First (var, fc)))
;;

let split_wf_Pair wfl ~if_pair ~not_pair =

  let open Let_syntax in
  match%bind wfl with
  | WFL (Pair (t1, t2)) ->
    if_pair (WFL t1) (WFL t2)

  | WFL (Unk lin) ->
    let%bind t1 = map ~f:(fun x -> Ast.Unk x) @@ create_fresh () in
    let%bind t2 = map ~f:(fun x -> Ast.Unk x) @@ create_fresh () in
    let%bind () = apply_env [Either.Second (lin, Ast.Pair (t1, t2))] in
    if_pair (WFL t1) (WFL t2)

  | WFL inferred -> not_pair inferred

and split_wf_Bang wfl ~if_bang ~not_bang =

  let open Let_syntax in
  match%bind wfl with
  | WFL (Bang t) ->
    if_bang @@ WFL t

  | WFL (Unk lin) ->
    let%bind t = map ~f:(fun x -> Ast.Unk x) @@ create_fresh () in
    let%bind () = apply_env [Either.Second (lin, Ast.Bang t)] in
    if_bang @@ WFL t

  | WFL inferred -> not_bang inferred

and split_wf_All wfl ~if_all ~not_all =

  let open Let_syntax in
  match%bind wfl with
  | WFL (All (var, t2)) ->
    if_all (WFV var) (WFL t2)

  (* Not sure about this *)
  | WFL (Unk lin) ->
    let%bind v = create_fresh () in
    let%bind t = map ~f:(fun x -> Ast.Unk x) @@ create_fresh () in
    let%bind () = apply_env [Either.Second (lin, Ast.All (v, t))] in
    if_all (WFV v) (WFL t)

  | WFL inferred -> not_all inferred

and split_wf_Fun wfl ~if_fun ~not_fun =

  let open Let_syntax in
  match%bind wfl with
  | WFL (Fun (t1, t2)) ->
    if_fun (WFL t1) (WFL t2)

  | WFL (Unk lin) ->
    let%bind t1 = map ~f:(fun x -> Ast.Unk x) @@ create_fresh () in
    let%bind t2 = map ~f:(fun x -> Ast.Unk x) @@ create_fresh () in
    let%bind () = apply_env [Either.Second (lin, Ast.Fun (t1, t2))] in
    if_fun (WFL t1) (WFL t2)

  | WFL inferred -> not_fun inferred
;;

(*###############################################################################################*)
(*                         (4) Type-directed Matrix-expression Elaboration                       *)
(*###############################################################################################*)

let%test_module "Test" =
  (module struct

    let two, three, six =
      ("two", "three", "six")
    ;;

    let env =
      [ (six, Used (Ast.dummy, WFL Unit))
      ; (two, Not_used {var=two;t=WFL(Fun (Unit, Unit))})
      ; (six, Not_used {var=six;t=WFL(Bang(Unit))})
      ; (three, Intuition (WFL (Bang Int)))
      ]
    ;;

    (* ignore irrelevant location information for these tests *)
    type nonrec tagged = tagged =
      | Not_used of not_used
      | Used of Ast.loc sexp_opaque * wf_lin
      | Intuition of wf_lin
    [@@deriving sexp_of]
    ;;

    let%expect_test "mark_used" =
      begin match mark_used Ast.dummy six env empty_used with
      | Ok (WFL var_t, env, set) ->
        Stdio.printf !"%{sexp:Ast.lin}\n%{sexp:(Ast.var * tagged) list}\n\
                       %{sexp:(Ast.var * Ast.loc sexp_opaque) list}\n"
          var_t env (Map.to_alist set)
      | Error str ->
        Stdio.printf "%s" str
      end;
      [%expect {| INTERNAL ERROR: use_var invariant broken. |}]
    ;;

    let%expect_test "mark_used" =
      begin match mark_used Ast.dummy two env empty_used with
      | Ok (WFL var_t, env, set) ->
        Stdio.printf !"Result: %{sexp:Ast.lin}\n\
                       Env: %{sexp:(Ast.var * tagged) list}\n\
                       Used: %{sexp:(Ast.var * Ast.loc sexp_opaque) list}\n"
          var_t env (Map.to_alist set)
      | Error str ->
        Stdio.printf "%s" str
      end;
      [%expect {|
        Result: (Fun Unit Unit)
        Env: ((six (Used <opaque> (WFL Unit))) (two (Used <opaque> (WFL (Fun Unit Unit))))
         (six (Not_used ((var six) (t (WFL (Bang Unit))))))
         (three (Intuition (WFL (Bang Int)))))
        Used: ((two <opaque>)) |}]
    ;;

  end)
;;
