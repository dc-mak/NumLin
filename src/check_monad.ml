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
  | Used
  | Intuition of wf_lin
[@@deriving sexp_of]
;;

type state =
  { env : (Ast.var, tagged) List.Assoc.t
  ; used_vars : (Ast.var, Ast.comparator_witness) Set.t sexp_opaque
  ; fc_vars  : Ast.var list
  ; counter : int
  }
[@@deriving sexp_of]
;;

let empty_used =
  Set.empty (module struct include Ast type t = var end)
;;

include State_or_error.Make (struct type t = state end)
;;

(*###############################################################################################*)
(*                                   (2)  Checker utilities                                      *)
(*###############################################################################################*)

let create_fresh ?(name="gen") () =
  let open Let_syntax in
  let%bind {counter=id;_} as state = get in
  let%bind () =  put {state with counter=id+1} in
  return Ast.(name ^ "_" ^ Int.to_string id)
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

let mark_used var env used_vars =

  let rec find var accum = function
    | [] -> (accum, None, [])
    | ((var', t) as entry) :: env ->
      if [%compare.equal:Ast.var] var var' then
        (accum, Some t, env)
      else
        find var (entry :: accum) env in 

  match find var [] env with
  | _, None, _ | _, Some Used, _ | _, Some Intuition _, _ ->
    Result.fail "INTERNAL ERROR: use_var invariant broken."

  | rev_front, Some (Not_used {var;t}), rest ->
    let rec loop rest = function
      | [] -> rest
      | x :: xs -> loop (x :: rest) xs in
    let env = loop ((var, Used) :: rest) rev_front in
    let used_vars = Set.add used_vars var in
    Result.return (t, env, used_vars)
;;

let use_var {var;t} =
  let open Let_syntax in
  let%bind {env; used_vars; _} as state = get in
  match mark_used var env used_vars with
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
  let%bind () =
    let%bind {env; used_vars; _} as state = get in
    match env with
    | (_, Intuition _) :: env ->
      put { state with env }
    | (_, Used) :: env ->
      (* Variable names are not unique *)
      let used_vars = if not @@ Set.mem prev var then Set.remove used_vars var else used_vars in
      put { state with env; used_vars }
    | (_, Not_used _) :: _ -> failf !"Variable %s not used.\n" var
    | [] -> failf !"INTERNAL ERROR: %{sexp:Ast.var} NOT FOUND IN vars\n" var in

  return result
;;

let with_lin var wf_lin result =
  with_var var (Either.First wf_lin) result
;;

let with_int var wf_lin result =
  with_var var (Either.Second wf_lin) result
;;

let with_fc var lin =
  let open Let_syntax in
  let%bind {fc_vars=prev; _} as state = get in

  (* FIRST update state *)
  let%bind () = put { state with fc_vars =  var :: prev } in

  (* THEN run computation *)
  let%bind result = lin in

  (* THEN remove the fraction_capability variables and return *)
  let%bind () =
    let%bind {fc_vars; _} as state = get in
    match fc_vars with
    | _ :: fc_vars -> put { state with fc_vars }
    | [] -> failf !"INTERNAL ERROR: %{sexp:Ast.var} NOT FOUND IN fc_vars\n" var in

  return result
;;

let in_empty wf_lin =
  let open Let_syntax in
  let%bind {used_vars=prev; _} as state = get in
  let%bind () = put { state with used_vars = empty_used } in
  let%bind res = wf_lin in
  let%bind {used_vars; _} as state = get in
  if Set.is_empty used_vars then
    let%bind () = put { state with used_vars = prev } in
    return res
  else
    failf !"Cannot use linearly-typed variables in Fix/Many\n    %s\n"
      (String.concat ~sep:", " @@ Set.to_list used_vars)
;;

let report_diff used_a used_b =
  let diff1 = Set.to_list @@ Set.diff used_a used_b
  and diff2 = Set.to_list @@ Set.diff used_b used_a in
  if List.is_empty diff1 then
    failf !"Second term used these variables not used by the first:\n  %s\n"
      (String.concat ~sep:", " diff2)
  else if List.is_empty diff2 then
    failf !"First term used these variables not used by the second:\n  %s\n"
      (String.concat ~sep:", " diff1)
  else
    failf !"First term used these variables not used by the second:\n  %s\n\
            Second term used these variables not used by the first:\n  %s\n"
      (String.concat ~sep:", " diff1)
      (String.concat ~sep:", " diff2)
;;

let same_resources wf_a wf_b =
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
  if Set.equal used_a used_b then
    let%bind () = put { state with used_vars = Set.union used_a prev } in
    return (res_a, res_b)
  else
    report_diff used_a used_b
;;

let run wf_lin ~counter =
  let open Or_error.Let_syntax in
  let init = {env=[]; fc_vars=[]; counter; used_vars=empty_used} in
  let%bind (WFL result, state) = run wf_lin init in
  begin match state with
  | {env = []; fc_vars = []; counter} ->
    return result
  | state ->
    Or_error.errorf
      !"INTERNAL ERROR: After checking (%{sexp:Ast.lin}), environment is not empty\n%{sexp:state}\n"
      result state
  end
;;

(*###############################################################################################*)
(*                                 (3) Well-formed destructors                                   *)
(*                          Probably excessive but fun to experiment with.                       *)
(*###############################################################################################*)

(* Check if fractional-capability is well-formed w.r.t. given list of variables. *)
let rec wf_wrt fc_vars =
  let open Ast in function
    | Z -> true
    | S fc -> wf_wrt fc_vars fc
    | V var -> List.exists fc_vars ([%compare.equal : Ast.var] var)
;;

(* Checking a linear type is well-formed. *)
let wf_lin ~fmt ~arg lt =
  let open Let_syntax in
  let open Ast in
  let rec wf bindings = function
    | Unit | Int | Bool | Elt as e ->
      return e

    | Arr fc ->
      if wf_wrt bindings fc then
        return @@ Arr fc
      else if%bind get >>= fun x -> return @@ wf_wrt x.fc_vars fc then
        return @@ Arr fc
      else
        failf fmt arg

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

let wf_substitute_in (WFL lin) (WFV var) (WFC fc) =
  WFL (Ast.substitute_in lin var fc)
;;

let split_wf_Pair wfl ~if_pair ~not_pair =
  let open Let_syntax in
  match%bind wfl with
  | WFL (Pair (t1, t2)) -> if_pair (WFL t1) (WFL t2)
  | WFL inferred -> not_pair inferred

let split_wf_Bang wfl ~if_bang ~not_bang =
  let open Let_syntax in
  match%bind wfl with
  | WFL (Bang t) -> if_bang @@ WFL t
  | WFL inferred -> not_bang inferred

and split_wf_All wfl ~if_all ~not_all =
  let open Let_syntax in
  match%bind wfl with
  | WFL (All (var, t2)) -> if_all (WFV var) (WFL t2)
  | WFL inferred -> not_all inferred

and split_wf_Fun wfl ~if_fun ~not_fun =
  let open Let_syntax in
  match%bind wfl with
  | WFL (Fun (t1, t2)) -> if_fun (WFL t1) (WFL t2)
  | WFL inferred -> not_fun inferred
;;

let%test_module "Test" =
  (module struct

    let one, two, three, four, five, six, seven, eight, nine, ten, eleven, sentinel =
      ( "one"   , "two"    , "three"
      , "four"  , "five"   , "six"
      , "seven" , "eight"  , "nine"
      , "ten"   , "eleven" , "sentinel" )
    ;;

    let env =
      [ (six, Used)
      ; (two, Not_used {var=two;t=WFL(Fun (Unit, Unit))})
      ; (six, Not_used {var=six;t=WFL(Bang(Unit))})
      ; (three, Intuition (WFL (Bang Int)))
      ]
    ;;

    let%expect_test "mark_used" =
      begin match mark_used six env empty_used with
      | Ok (WFL var_t, env, set) ->
        Stdio.printf !"%{sexp:Ast.lin}\n%{sexp:(Ast.var * tagged) list}\n%{sexp:Ast.var list}\n"
          var_t env (Set.to_list set)
      | Error str ->
        Stdio.printf "%s" str
      end;
      [%expect {| INTERNAL ERROR: use_var invariant broken. |}]
    ;;

    let%expect_test "mark_used" =
      begin match mark_used two env empty_used with
      | Ok (WFL var_t, env, set) ->
        Stdio.printf !"Result: %{sexp:Ast.lin}\n\
                       Env: %{sexp:(Ast.var * tagged) list}\n\
                       Used: %{sexp:Ast.var list}\n"
          var_t env (Set.to_list set)
      | Error str ->
        Stdio.printf "%s" str
      end;
      [%expect {|
        Result: (Fun Unit Unit)
        Env: ((six Used) (two Used) (six (Not_used ((var six) (t (WFL (Bang Unit))))))
         (three (Intuition (WFL (Bang Int)))))
        Used: (two) |}]
    ;;

  end)
;;

