(* Dhruv Makwana *)
(* LT4LA Check Monad *)
(* ----------------- *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

(* Please read the .mli file for explanations. *)

open Core_kernel
;;

type not_used =
  Ast.variable * Ast.linear_t
[@@ deriving sexp_of]
;;

type tagged_linear_t =
  | Not_used of not_used
  | Used of Ast.linear_t
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
let create_fresh ?name () =
  let open Let_syntax in
  let%bind {counter=id;_} as state = get in
  let%bind () =  put {state with counter=id+1} in
  let default = Int.to_string id in
  let name = Option.value_map ~default ~f:(Fn.flip (^) ("_" ^ default)) name in
  return (Ast.({id; name}))
;;

let add, remove, find =
  let equal = [%compare.equal: Ast.variable] in
  List.Assoc.add ~equal,
  List.Assoc.remove ~equal,
  List.Assoc.find ~equal
;;

let lookup var  =
  let open Let_syntax in
  let%bind {linear_t_vars; frac_cap_vars; _} = get in
  return (find linear_t_vars var)
;;

let well_formed frac_cap =
  let open Let_syntax in
  let open Ast in
  let rec well_formed frac_cap_vars = function
  | Zero -> true
  | Succ frac_cap -> well_formed frac_cap_vars frac_cap
  | Var var -> List.exists frac_cap_vars ([%compare.equal : Ast.variable] var) in
  let%bind {frac_cap_vars; _} = get in
  return (well_formed frac_cap_vars frac_cap)
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
    let bindings = List.map bindings (fun (var,lt) -> (var, Not_used (var, lt))) in
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
        return ((str "Variable %s not used." var.Ast.name) :: err_log)
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
      let open Let_syntax in
      let%bind {frac_cap_vars; _} as state = get in
      put { state with frac_cap_vars = List.filter frac_cap_vars (fun x ->
        not ( [%compare.equal : Ast.variable] var x ) ) } in
    List.map bindings ~f:remove_frac_cap_var |> all_ignore in

  return result
;;

let run linear_t ~counter =
  let open Or_error.Let_syntax in
  let%bind (result, state) = run linear_t {linear_t_vars=[]; frac_cap_vars=[]; counter} in
  begin match state with
  | {linear_t_vars = []; frac_cap_vars = []; counter} ->
    return result
  | state ->
    Or_error.errorf
      !"INTERNAL ERROR: After checking, environment is not empty\n%{sexp:state}"
      state
  end
;;

(* TODO Internal tests *)
let%test_module "Test" =
  (module struct
    let%test "test" = true
  end)
;;
