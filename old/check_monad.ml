(* Dhruv Makwana *)
(* LT4LA Check Monad *)
(* ----------------- *)

open Base
;;

type well_formed =
  WF of Ast.linear_t
[@@deriving sexp_of]
;;

let wf_Array_t_Zero =
  WF (Ast.(Array_t Zero))

and wf_Unit =
  WF Ast.Unit

and wf_Int =
  WF Ast.Int

and wf_Float64 =
  WF Ast.Float64

and wf_Pair (WF x) (WF y) =
  WF (Ast.Pair (x, y))

and wf_Fun (WF x) (WF y) =
  WF (Ast.Fun (x, y))

and wf_ForAll var (WF x) =
  WF (Ast.ForAll_frac_cap (var, x))
;;

type not_used =
  Ast.variable * well_formed
[@@ deriving sexp_of]
;;

type tagged_linear_t =
  | Not_used of not_used
  | Used of well_formed
[@@deriving sexp_of]
;;

(* This could be a Map but kept it simple to start off. Kept the environments
   separate to simplify implementation at the cost of duplicating some logic.
   NOTE Should these be stacks? *)
type state =
  { linear_t_vars : (Ast.variable, tagged_linear_t) List.Assoc.t
  ; frac_cap_vars : Ast.variable list
  ; counter : int
  }
[@@deriving sexp_of]
;;

(* This will hopefully allow me to easily change the implementation later if necessary. *)
include State_or_error.Make (struct type t = state end)
;;

(* I thought I needed this but clearly not it seems *)
let create_fresh ?(name="gen") () =
  let open Let_syntax in
  let%bind {counter=id;_} as state = get in
  let%bind () =  put {state with counter=id+1} in
  return (Ast.({id; name}))
;;

(* Shorthands for common utilities *)
let add, remove, find =
  let equal = [%compare.equal: Ast.variable] in
  List.Assoc.(add ~equal, remove ~equal, find ~equal)
;;

let lookup var  =
  let open Let_syntax in
  let%bind {linear_t_vars; _} = get in
  return (find linear_t_vars var)
;;

(* Check if fractional-capability is well-formed w.r.t. given list of variables. *)
let rec wf_wrt frac_cap_vars = 
  let open Ast in function
    | Zero -> true
    | Succ frac_cap -> wf_wrt frac_cap_vars frac_cap
    | Var var -> List.exists frac_cap_vars ~f:([%compare.equal : Ast.variable] var)
;;

(* Well-formed manipulations. Probably execessive but fun to experiment with. *)
type wf_frac_cap =
  WFC of Ast.frac_cap
;;

let if_well_formed fc ~then_ ~else_ =
  let open Let_syntax in
  let%bind {frac_cap_vars; _} = get in
  if wf_wrt frac_cap_vars fc then
    then_ (WFC fc)
  else
    else_ fc
;;

type wf_variable =
  WFV of Ast.variable
;;

let wf_substitute_in (WF linear_t) (WFV var) (WFC fc) =
  let open Let_syntax in
  match Ast.substitute_in linear_t ~var ~replacement:fc with
  | Ok result ->
    return (WF result)
  | Error err ->
    fail_string (Error.to_string_hum err)
;;

(* Case analysis on well-formed types. *)
let split_wf_ForAll wf ~if_forall ~not_forall =
  let open Let_syntax in
  match%bind wf with
  | WF (ForAll_frac_cap (var, t2)) -> if_forall (WFV var) (WF t2)
  | WF inferred_t -> not_forall inferred_t

and split_wf_Pair wf ~if_pair ~not_pair =
  let open Let_syntax in
  match%bind wf with
  | WF (Pair (t1, t2)) -> if_pair (WF t1) (WF t2)
  | WF inferred_t -> not_pair inferred_t

and split_wf_Fun wf ~if_fun ~not_fun =
  let open Let_syntax in
  match%bind wf with
  | WF (Fun (t1, t2)) -> if_fun (WF t1) (WF t2)
  | WF inferred_t -> not_fun inferred_t
;;

(* Checking a linear type is well-formed. *)
let well_formed_lt ~fmt ~arg lt =
  let open Let_syntax in
  let open Ast in
  let rec wf bindings = function
    | Unit ->
      return Unit
    | Int ->
      return Int
    | Float64 ->
      return Float64
    | Pair (t1, t2) ->
      let%bind t1 = wf bindings t1 in
      let%bind t2 = wf bindings t2 in
      return (Pair (t1, t2))
    | Fun (fun_t, arg_t) ->
      let%bind fun_t = wf bindings fun_t in
      let%bind arg_t = wf bindings arg_t in
      return (Fun (fun_t, arg_t))
    | Array_t fc ->
      if wf_wrt bindings fc then
        return (Array_t fc)
      else if%bind get >>= fun x -> return (wf_wrt x.frac_cap_vars fc) then
        return (Array_t fc)
      else
        failf fmt arg
    | ForAll_frac_cap (var, linear_t) ->
      let%bind linear_t = wf (var :: bindings) linear_t in
      return (ForAll_frac_cap (var, linear_t) : Ast.linear_t) in
  let%bind result = wf [] lt in
  return (WF result)
;;

(* NOTE List.Assoc.add actually overrides silently, like a map. *)
let use_var (var, var_t) =
    let open Let_syntax in
    let%bind {linear_t_vars; _} as state = get in
    let%bind () = put { state with linear_t_vars = add linear_t_vars var (Used var_t) } in
    return var_t
;;

let with_linear_t bindings linear_t =
  let open Let_syntax in
  let%bind {linear_t_vars; _} as state = get in

  (* FIRST update state *)
  let%bind () =
    let bindings = List.map bindings ~f:(fun (var,lt) -> (var, Not_used (var, lt))) in
    (* Yes, I'm cheating by appending *)
    put { state with linear_t_vars =  bindings @ linear_t_vars } in

  (* THEN run computation *)
  let%bind result = linear_t in

  (* THEN for each linear variable, remove if it was used, and log if it was not *)
  let remove_linear_var err_log (var, _) =
    let str = Printf.sprintf in
    let%bind err_log = err_log in
    let%bind {linear_t_vars; _} as state = get in
    begin match find linear_t_vars var with
      | Some (Used _) ->
        let%bind () = put {state with linear_t_vars=remove linear_t_vars var} in
        return err_log
      | Some (Not_used _) ->
        return ((str "Variable %s not used." (Ast.string_of_variable var)) :: err_log)
      | None ->
        return ((str !"INTERNAL ERROR: %{sexp:Ast.variable} NOT FOUND IN linear_t_vars" var) :: err_log)
    end in

  (* If the log is not empty, fail *)
  let%bind () =
    begin match%bind List.fold_left bindings ~init:(return []) ~f:remove_linear_var with
    | [] ->
      return ()
    | _ :: _ as errs ->
      fail_string (String.concat ~sep:"\n" errs)
    end in

  (* Otherwise, return result *)
  return result
;;

let with_frac_cap bindings linear_t =
  let open Let_syntax in
  let%bind {frac_cap_vars; _} as state = get in

  (* FIRST update state *)
  let%bind () =
    (* Yes, I'm cheating by appending *)
    put { state with frac_cap_vars =  bindings @ frac_cap_vars } in

  (* THEN run computation *)
  let%bind result = linear_t in

  (* THEN remove the fraction_capability variables and return *)
  let%bind () =
    let remove_frac_cap_var var =
      let%bind {frac_cap_vars; _} as state = get in
      put { state with frac_cap_vars = List.filter frac_cap_vars ~f:(fun x ->
        not ( [%compare.equal : Ast.variable] var x ) ) } in
    List.map bindings ~f:remove_frac_cap_var |> all_unit in

  return result
;;

let run well_formed ~counter =
  let open Or_error.Let_syntax in
  let%bind (WF result, state) = run well_formed {linear_t_vars=[]; frac_cap_vars=[]; counter} in
  begin match state with
  | {linear_t_vars = []; frac_cap_vars = []; counter=_} ->
    return result
  | state ->
    Or_error.errorf
      !"INTERNAL ERROR: After checking, environment is not empty\n%{sexp:state}"
      state
  end
;;

let%test_module "Test" =
  (module struct
  end)
;;
