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

  | Primitive prim ->
    check_prim prim

and (check_prim : Ast.primitive -> Ast.linear_t Check_monad.t) =
  let open Check_monad in
  let open Let_syntax in

  let abstract_one name f =
    let%bind x = create_fresh ~name () in
    return (Ast.ForAll_frac_cap(x, f x) : Ast.linear_t) in

  let abstract_two name_x name_y f =
    let%bind x = create_fresh ~name:name_x () in
    let%bind y = create_fresh ~name:name_y () in
    let arg_t, body_t = f x y in
    return (Ast.ForAll_frac_cap(x, Fun (arg_t, ForAll_frac_cap (y, body_t))) : Ast.linear_t) in

  let three_names ?(prefix = "") name1 name2 name3 =
    let%bind n1 = create_fresh ~name:(prefix ^ name1) () in
    let%bind n2 = create_fresh ~name:(prefix ^ name2) () in
    let%bind n3 = create_fresh ~name:(prefix ^ name3) () in
    return (n1, n2, n3) in

  let four_names ?(prefix = "") name1 name2 name3 name4 =
    let%bind n1, n2, n3 = three_names ~prefix name1 name2 name3 in
    let%bind n4 = create_fresh ~name:(prefix ^ name4) () in
    return (n1, n2, n3, n4) in

  function

  (* Operators *)

  (* Arr[x] -o Arr[x+1] * Arr[x+1] *)
  | Split_Permission ->
    let func_t x =
      let component = Ast.Array_t (Succ (Var x)) in
      Ast.Fun (Array_t (Var x), Pair (component, component)) in
    abstract_one "split_perm" func_t

  (* Arr[x+1] * Arr[x+1] -o Arr[x] *)
  | Merge_Permission ->
    let func_t x =
      let component = Ast.Array_t (Succ (Var x)) in
      Ast.Fun (Pair (component, component), Array_t (Var x)) in
    abstract_one "merge_perm" func_t

  (* Arr[0] -o I *)
  | Free ->
    return (Ast.Fun(Array_t Zero, Unit))

  (* xCOPY: ∀x. Arr[x] -o Arr[x] * Arr[0] *)
  (* NOTE: Is this correct/fair?  *)
  | Copy ->
    let func_t x = Ast.Fun(Array_t (Var x), Pair(Array_t (Var x), Array_t Zero)) in
    abstract_one "copy" func_t

  (* xSWAP: Arr[0] * Arr[0] -o Arr[0] * Arr[0] *)
  | Swap ->
    return (Ast.Fun(Pair(Array_t Zero, Array_t Zero), Pair(Array_t Zero, Array_t Zero)))

  (* Routines/Functions *)

  (* xASUM: ∀x. Arr[x] -> Arr[x] * Arr[0] *)
  | Sum_Mag ->
    let func_t x = Ast.Fun(Array_t (Var x), Pair(Array_t (Var x), Array_t Zero)) in
    abstract_one "sum_mag" func_t

  (* xAXPY: ∀ sc. Arr[sc] -o ∀ vec. Arr[vec] -o Arr[0] -> (Arr[sc] * Arr[vec]) * Arr[0] *)
  | Scalar_Mult_Then_Add ->
    let func_t scalar vector =
      let return_t = Ast.Pair (Pair (Array_t (Var scalar), Array_t (Var vector)), Array_t Zero) in
      let body_t = Ast.Fun ( Array_t (Var vector), Fun ( Array_t Zero, return_t )) in
      (Ast.Array_t (Var scalar), body_t) in
    abstract_two "sum_mag_scalar" "sum_mag_vec" func_t

  (* xDOT: ∀x. Arr[x] -o ∀y. Arr[y] -o (Arr[x] * Arr[y]) * Arr[0] *)
  | DotProd ->
    let func_t x y =
      let result_t = Ast.Pair (Pair (Array_t (Var x), Array_t (Var y)), Array_t Zero) in
      let body_t = Ast.Fun (Array_t (Var y), result_t) in
      (Ast.Array_t (Var x) , body_t) in
    abstract_two "dot_prod_x" "dot_prod_y" func_t

  (* xNRM2: ∀x. Arr[x] -> Arr[x] * Arr[0] *)
  | Norm2 ->
    let func_t x = Ast.Fun(Array_t (Var x), Pair(Array_t (Var x), Array_t Zero)) in
    abstract_one "norm2" func_t

  (* xROT: ∀x. Arr[0] * Arr[x] -o ∀y. Arr[0] * Arr[y] -o ∀s. Arr[s] -o ∀c. Arr[c] -o
           ((Arr[x]  * Arr[y]) * (Arr[s] * Arr[c])) * (Arr[0] * Arr[0]) *)
  | Plane_Rotation ->
    let%bind x, y, s, c = four_names ~prefix:"plane_rot_" "x" "y" "s" "c" in
    let return_t =
      let pair1 = Ast.Pair (Array_t (Var x), Array_t (Var y)) in
      let pair2 = Ast.Pair (Array_t (Var s), Array_t (Var c)) in
      let pair3 = Ast.Pair (Array_t Zero, Array_t Zero) in
      Ast.Pair (Pair (pair1, pair2), pair3) in
    let abstract x ?(arg_t = (Ast.Array_t (Var x))) body_t : Ast.linear_t =
      Ast.ForAll_frac_cap (x, Fun (arg_t, body_t)) in
    return (return_t
            |> abstract c
            |> abstract s
            |> abstract y ~arg_t:(Ast.Pair (Array_t Zero, Array_t (Var y)))
            |> abstract x ~arg_t:(Ast.Pair (Array_t Zero, Array_t (Var x))))

  (* xROTG: Arr[0] -o Arr[0] -o Arr[0] -o Arr[0] -o (Arr[0] * Arr[0] * Arr[0] * Arr[0]) *)
  | Givens_Rotation ->
    let zero = Ast.Array_t Zero in
    return (Ast.Fun (zero, Fun (zero, Fun (zero, Fun (zero, Pair (Pair(zero, zero), Pair(zero,zero)))))))

  (* xROTM: ∀x. Arr[0] * Arr[x] -o ∀y. Arr[0] * Arr[y] -o ∀p. Arr[p]
            ((Arr[x]  * Arr[y]) * (Arr[s] * Arr[c])) * Arr[0] *)
  | GivensMod_Rotation ->
    let%bind x, y, p = three_names ~prefix:"plane_rot_" "x" "y" "s" in
    let return_t =
      let pair1 = Ast.Pair (Array_t (Var x), Array_t (Var y)) in
      let pair2 = Ast.Array_t (Var p) in
      let pair3 = Ast.Pair (Array_t Zero, Array_t Zero) in
      Ast.Pair (Pair (pair1, pair2), pair3) in
    let abstract x ?(arg_t = (Ast.Array_t (Var x))) body_t : Ast.linear_t =
      Ast.ForAll_frac_cap (x, Fun (arg_t, body_t)) in
    return (return_t
            |> abstract p
            |> abstract y ~arg_t:(Ast.Pair (Array_t Zero, Array_t (Var y)))
            |> abstract x ~arg_t:(Ast.Pair (Array_t Zero, Array_t (Var x))))

  (* xROTMG: Arr[0] -o Arr[0] -o Arr[0] -o Arr[0] -o ∀y. Arr[y] -o
             (Arr[y] * (Arr[0] * Arr[0]) * (Arr[0] * Arr[0])) *)
  | Gen_GivensMod_Rotation ->
    let%bind y = create_fresh ~name:"mod_givens_y" () in
    let zero = Ast.Array_t Zero in
    return (Ast.Fun (zero, Fun (zero, Fun (zero, ForAll_frac_cap (y, Fun (Array_t (Var y), Fun (
      zero, Pair (Array_t (Var y), Pair (Pair(zero, zero), Pair(zero,zero))))))))))

  (* xSCAL: ∀sc. Arr[sc] -o ∀inc. Arr[inc] -o Arr[0] -> ((Arr[sc] * Arr[inc]) * Arr[0]) *)
  | Scalar_Mult ->
    abstract_two "scalar_mult_sc" "scalar_mult_inc" (fun sc inc ->
      (Ast.Array_t (Var sc), Ast.Fun(Array_t (Var inc), Fun (
         Array_t Zero, Pair (Pair (Array_t (Var sc), Array_t (Var inc)), Array_t Zero)))))

  (* IxAMAX: ∀inc. Arr[inc] -o ∀x. Arr[x] -o ((Arr[inc], Arr[x]) * Arr[0]) *)
  | Index_of_Max_Abs ->
    abstract_two "index_max_inc" "index_max_x" (fun inc x ->
      (Ast.Array_t (Var inc), Ast.Fun(Array_t (Var x), Pair (
         Pair (Array_t (Var inc), Array_t (Var x)), Array_t Zero))))

  (* IxAMIN -- Intel only: ∀inc. Arr[inc] -o ∀x. Arr[x] -o ((Arr[inc], Arr[x]) * Arr[0]) *)
  | Index_of_Min_Abs ->
    abstract_two "index_max_inc" "index_max_x" (fun inc x ->
      (Ast.Array_t (Var inc), Ast.Fun(Array_t (Var x), Pair (
         Pair (Array_t (Var inc), Array_t (Var x)), Array_t Zero))))

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
