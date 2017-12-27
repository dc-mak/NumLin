(* Dhruv Makwana *)
(* LT4LA (Type) Checker *)
(* --------------------- *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

(* Please read the .mli file for explanations. *)

open Core_kernel
;;

let error ~expected ~inferred_t =
  Check_monad.failf !"%s\nActual: %{sexp:Ast.linear_t}" expected inferred_t 
;;

(* The actual checking algorithm *)
let rec (check : Ast.expression -> Ast.linear_t Check_monad.t) =
  let open Check_monad in
  let open Let_syntax in
  function

  (* Introduction rules are easy to type *)
  | Unit_Intro ->
    return Ast.Unit

  | Pair_Intro (first, second) ->
    let%bind first_t = check first in
    let%bind second_t = check second in
    return (Ast.Pair (first_t, second_t))

  | Lambda (var, var_t, body) ->
    let%bind expression_t = check body |> with_linear_t [(var, var_t)] in
    return (Ast.Fun(var_t, expression_t))

  | ForAll_frac_cap (var, (expression : Ast.expression)) ->
    let%bind expression_t = (check expression) |> with_frac_cap [(var, Ast.Var var)] in
    return ((Ast.ForAll_frac_cap (var, expression_t)) : Ast.linear_t)

  | Var var ->
    begin match%bind lookup var with
    | None ->
      fail_string ("Unbound variable " ^ var.name ^ " (not found in environment)")
    | Some (Used _ ) ->
      fail_string ("Variable " ^ var.name ^ " used twice (or more).")
    | Some (Not_used var) ->
      let%bind linear_t = use_var var in
      return linear_t
    end

  | Array_Intro _ ->
    return (Ast.Array_t Zero)

  (* Elimination rules are more of a pain *)
  | Unit_Elim (expression, body) ->
    begin match%bind check expression with
    | Unit ->
      check body
    | inferred_t ->
      error "Unit_Elim: expected Unit" inferred_t
    end

  | Pair_Elim (first, second, expression, body) ->
    begin match%bind check expression with
    | Pair(first_t, second_t) -> 
      check body |> with_linear_t [(first, first_t); (second, second_t)]
    | inferred_t ->
      error "Pair_Elim: expected Pair(_,_)" inferred_t
    end

  | Specialise_frac_cap (expression, frac_cap) ->
    begin match%bind check expression with
    | ForAll_frac_cap (var, linear_t) ->
      (* Really important to call normal_form BEFORE Ast.unify_frac_cap. This
         invariant could/should be wrapped up into a stateful operation
         enforced by the type system and implementation. *)
      let%bind frac_cap = normal_form frac_cap in
      begin match Ast.unify_frac_cap (Var var) frac_cap with
      | Ok subs ->
        let%bind () = apply subs in
        return (Ast.apply subs linear_t)
      | Error err  -> 
        failf "%a" (Fn.const Error.to_string_hum) err
      end
    | inferred_t ->
      error "Specialise_frac_cap: expected ForAll_frac_cap(_,_)" inferred_t
    end
  
  | Array_Elim (var, expression, body) ->
    begin match%bind check expression with
    | Array_t frac_cap ->
      (* This may be unnecessary right now, but when I add primitives they
         could be not normalised w.r.t. the enviroment *)
      let%bind frac_cap = normal_form frac_cap in
      check expression |> with_linear_t [(var, Array_t frac_cap)]
    | inferred_t ->
      error "Array_Elim: expected Array_t(_)" inferred_t
    end

  | App (func, arg) ->
    begin match%bind check func with
    | Fun (expected_arg_t, body_t) ->
      let%bind actual_arg_t = check arg in
      begin match Ast.unify_linear_t expected_arg_t actual_arg_t with
      | Ok subs ->
        let%bind () = apply subs in
        return body_t
      | Error err -> 
        failf "%a" (Fn.const Error.to_string_hum) err
      end
    | inferred_t ->
      error "App: expected Fun(_,_)" inferred_t
    end

;;

let check_expr expr =
  Check_monad.run (check expr)
;;

(* TODO Internal tests *)
let%test_module "Test" =
  (module struct
    let%test "test" = true
  end)
;;
