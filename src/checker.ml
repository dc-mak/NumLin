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

(* For simplicity in these types, I'm always assuming that (1) lengths `n` can
   be retrieved at runtime and (2) incx = incy = 1. *)

let (check_prim : Ast.primitive -> Ast.linear_t Check_monad.t) =
  let open Check_monad in
  let open Let_syntax in

  let abstract_one name f =
    let%bind x = create_fresh ~name () in
    return (Ast.ForAll_frac_cap(x, f x) : Ast.linear_t) in

  let arr0 = Ast.(Array_t Zero) in

  function

  (* Operators *)

  (* ∀x. Arr[x] --o Arr[x+1] * Arr[x+1] *)
  | Split_Permission ->
    let func_t x =
      let component = Ast.Array_t (Succ (Var x)) in
      Ast.Fun (Array_t (Var x), Pair (component, component)) in
    abstract_one "split_perm" func_t

  (* ∀x. Arr[x+1] * Arr[x+1] --o Arr[x] *)
  | Merge_Permission ->
    let func_t x =
      let component = Ast.Array_t (Succ (Var x)) in
      Ast.Fun (Pair (component, component), Array_t (Var x)) in
    abstract_one "merge_perm" func_t

  (* Arr[0] --o I *)
  | Free ->
    return (Ast.Fun(arr0, Unit))

  (* xCOPY: ∀x. Arr[x] --o Arr[x] * Arr[0] *)
  | Copy ->
    let func_t x = Ast.Fun(Array_t (Var x), Pair (Array_t (Var x), arr0)) in
    abstract_one "copy" func_t

  (* xSWAP: Arr[0] * Arr[0] --o Arr[0] * Arr[0] *)
  | Swap ->
    return (Ast.Fun(Pair (arr0, arr0), Pair (arr0, arr0)))

  (* Routines/Functions *)

  (* xASUM: ∀x. Arr[x] --o Arr[x] * f64 *)
  | Sum_Mag ->
    let func_t x = Ast.Fun(Array_t (Var x), Pair(Array_t (Var x), Float64)) in
    abstract_one "sum_mag" func_t

  (* xAXPY: f64 --o ∀ vec. Arr[vec] --o Arr[0] --o Arr[vec] * Arr[0] *)
  | Scalar_Mult_Then_Add ->
    let func_t vec =
      let return_t = Ast.Pair (Array_t (Var vec), arr0) in
      Ast.Fun (Float64, Fun ( Array_t (Var vec), Fun ( arr0, return_t ))) in
    abstract_one "sum_mag_vec" func_t

  (* xDOT: ∀x. Arr[x] --o ∀y. Arr[y] --o (Arr[x] * Arr[y]) * f64 *)
  | DotProd ->
    let%bind x = create_fresh ~name:"dot_prod_x" () in
    let%bind y = create_fresh ~name:"dot_prod_y" () in
    let result_t = Ast.Pair (Pair (Array_t (Var x), Array_t (Var y)), Float64) in
    return (Ast.ForAll_frac_cap (x, Fun (Array_t (Var x),
                ForAll_frac_cap (y, Fun (Array_t (Var y),
                result_t)))) : Ast.linear_t)

  (* xNRM2: ∀x. Arr[x] --o Arr[x] * f64 *)
  | Norm2 ->
    let func_t x = Ast.Fun(Array_t (Var x), Pair(Array_t (Var x), Float64)) in
    abstract_one "norm2" func_t

  (* xROT: f64 --o f64 --o Arr[0] --o Arr[0] --o Arr[0] * Arr[0] *)
  | Plane_Rotation ->
    return Ast.(Fun (Float64, Fun(Float64, Fun(
      arr0, Fun(arr0, Pair(arr0, arr0))))))

  (* xROTG: f64 --o f64 --o (f64 * f64) * (f64 * f64) *)
  | Givens_Rotation ->
    return (Ast.Fun (Float64, Fun (Float64, Pair(Pair(Float64, Float64), Pair(Float64, Float64)))))

  (* xROTM: Arr[0] --o Arr[0] --o ∀p. Arr[p] --o ((Arr[0] * Arr[0]) * Arr[p] *)
  | GivensMod_Rotation ->
    let%bind rest = abstract_one "rotm" (fun p ->
      Fun (Array_t (Var p), Pair (Pair (arr0, arr0), Array_t (Var p)))) in
    return Ast.(Fun (arr0, Fun(arr0, rest)))

  (* xROTMG: (f64 * f64) --o (f64 * f64) --o (f64 * f64) * (f64 * Arr[0]) *)
  | Gen_GivensMod_Rotation ->
    let p2 = Ast.Pair (Float64, Float64) in
    return Ast.(Fun (p2, Fun(p2, Pair (p2, Pair(Float64, arr0)))))

  (* xSCAL: f64 --o Arr[0] --o Arr[0] *)
  | Scalar_Mult ->
    return Ast.(Fun (Float64, Fun(arr0, arr0)))

  (* IxAMAX: ∀x. Arr[x] --o int * Arr[x] *)
  | Index_of_Max_Abs ->
    let func_t x =
      Ast.(Fun (Array_t (Var x), Pair (Int, Array_t (Var x)))) in
    abstract_one "index_max_abs" func_t
;;

let error ~expected ~inferred_t =
  Check_monad.failf !"%s\nActual: %{sexp:Ast.linear_t}" expected inferred_t
;;

(* The actual checking algorithm *)
let rec (check : Ast.expression -> Check_monad.well_formed Check_monad.t) =
  let open Check_monad in
  let open Let_syntax in
  function

  (* Introduction rules are easy to type *)
  | Unit_Intro ->
    return wf_Unit

  | Int_Intro _ ->
    return wf_Int

  | Float64_Intro _ ->
    return wf_Float64

  | Pair_Intro (first, second) ->
    let%bind first_t = check first in
    let%bind second_t = check second in
    return (wf_Pair first_t second_t)

  | Lambda (var, var_t, body) ->
    let fmt, arg  = !"Type is not well-formed:\n%{sexp: Ast.linear_t}", var_t in
    let%bind var_t = well_formed_lt ~fmt ~arg var_t in
    let%bind expression_t = check body |> with_linear_t [(var, var_t)] in
    return (wf_Fun var_t expression_t)

  | ForAll_frac_cap (var, (expression : Ast.expression)) ->
    let%bind expression_t = (check expression) |> with_frac_cap [var] in
    return (wf_ForAll var expression_t)

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

  | Array_Intro expression ->
    begin match%bind check expression with
    | WF Int ->
      return wf_Array_t_Zero
    | WF inferred_t ->
      error "Array_Intro: expected Int" inferred_t
    end

  (* Elimination rules are more of a pain *)
  | Unit_Elim (expression, body) ->
    begin match%bind (check expression) with
    | WF Unit ->
      check body
    | WF inferred_t ->
      error "Unit_Elim: expected Unit" inferred_t
    end

  | Pair_Elim (first, second, expression, body) ->
    split_wf_Pair (check expression)
      ~if_pair:
        (fun first_t second_t ->
         check body |> with_linear_t [(first, first_t); (second, second_t)])
      ~not_pair:
        (fun inferred_t ->
           error "Pair_Elim: expected Pair(_,_)" inferred_t)

  | Specialise_frac_cap (expression, frac_cap) ->
    split_wf_ForAll (check expression)
      ~if_forall:
        (fun var well_formed_t ->
           if_well_formed frac_cap 
             ~then_:(wf_substitute_in well_formed_t var)
             ~else_:(failf !"Specialise_frac_cap: %{sexp: Ast.frac_cap} not found in environment."))

      ~not_forall:
        (fun inferred_t ->
           error "Specialise_frac_cap: expected ForAll_frac_cap(_,_)" inferred_t)

  | Array_Elim (var, expression, body) ->
    begin match%bind check expression with
    | WF (Array_t _) as arr_t ->
      check body |> with_linear_t [(var, arr_t)]
    | WF inferred_t ->
      error "Array_Elim: expected Array_t(_)" inferred_t
    end

  | App (func, arg) ->
    let if_fun (WF expected_arg_t) body_t =
      let%bind (WF actual_arg_t) = check arg in
      match Ast.same_linear_t expected_arg_t actual_arg_t with
      | Ok () ->
        return body_t
      | Error err ->
        failf "%a" (Fn.const Error.to_string_hum) err in
    let not_fun inferred_t = error "App: expected Fun(_,_)" inferred_t in
    split_wf_Fun (check func) ~if_fun ~not_fun

  | Primitive prim ->
    check_prim prim >>=
    well_formed_lt
      ~fmt:!"Internal Error: Primitive is not well-formed.\n%{sexp: Ast.primitive}"
      ~arg:prim
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
