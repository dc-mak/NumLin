(* Dhruv Makwana *)
(* LT4LA Abstract Syntax Tree *)
(* -------------------------- *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

(* Please read the .mli file for explanations. *)

open Base
;;

(** Variables are names (parsing/printing) and integers (uniqueness). *)
type variable =
  { id: int
  ; name: string (* [@compare.ignore] -- when available *)
  }
[@@deriving sexp_of]
;;

let compare_variable {id=x; _ } {id= y; _} =
  compare_int x y
;;

include
struct

  type frac_cap =
    | Zero
    | Succ of frac_cap
    | Var of variable
  [@@deriving sexp_of,compare]
  ;;

  let pp_frac_cap =
    let rec count acc = function 
      | Zero -> Caml.string_of_int acc
      | Succ frac_cap -> count (acc+1) frac_cap
      | Var var -> var.name ^ if acc = 0 then "" else "+" ^ Caml.string_of_int acc in
    count 0
  ;;

  let rec occurs_frac_cap var =
    function
    | Zero ->
      false
    | Succ frac_cap ->
      occurs_frac_cap var frac_cap
    | Var var' ->
      [%compare.equal : variable] var var'
  ;;

  let rec unify_frac_cap frac_cap1 frac_cap2 =
    let open Or_error.Let_syntax in
    match frac_cap1, frac_cap2 with
    | Zero, Zero ->
      return []
    | Succ frac_cap1, Succ frac_cap2 ->
      unify_frac_cap frac_cap1 frac_cap2
    | Var var1, Var var2 ->
      return (if [%compare.equal : variable] var1 var2 then [] else [(var1, Var var2)])
    | Var var, frac_cap
    | frac_cap, Var var ->
      if not (occurs_frac_cap var frac_cap) then
        return [(var, frac_cap)]
      else
        let pp () = pp_frac_cap in
        Or_error.errorf !"Variable: %{sexp: variable} occurs in %a" var pp frac_cap 
    | _, _ ->
      let pp () = pp_frac_cap in
      Or_error.errorf !"Could not unify %a with %a." pp frac_cap1 pp frac_cap2
  ;;

end

(** For Matrices, this would be int * int *)
(*
 * type size =
 *   | Int of int
 *   | Var of variable
 * ;;
 *)

include 
struct
  type linear_t =
    | Unit 
    | Pair of linear_t * linear_t 
    | Fun of linear_t * linear_t
    | ForAll_frac_cap of variable * linear_t
    | Array_t of frac_cap
  [@@deriving sexp_of,compare,traverse_map]
  ;;

  (* I should look in to https://mjambon.github.io/mjambon2016/easy-format.html *)
  let pp_linear_t ppf = 
    let open Caml.Format in
    let _wrap = ref 0 in
    let rec pp_linear_t ppf = function
      | Unit ->
        fprintf ppf "I"

      (* Technically, you don't need parentheses around ForAll_frac_cap when
         it's in the second position, but it makes reading it easier *)
      | Pair (first, second) ->
        let pair_parens = function
          | Unit | Array_t _ -> "", ""
          | Pair _ | Fun _ | ForAll_frac_cap _ -> "( ", " )" in
        let fl, fr = pair_parens first in
        let sl, sr = pair_parens second in
        fprintf ppf "@[%s%a%s@ @[* %s%a%s@]@]" fl pp_linear_t first fr sl pp_linear_t second sr

      (* Hackery to ensure (1) lollipops associate to the right and (2) when
         breaking, align with the "first" argument *)
      | Fun (arg_t, (Fun _ as body_t)) ->
        let al, ar = match arg_t with
          | Unit | Array_t _ | Pair _ -> "", ""
          | ForAll_frac_cap _ | Fun _ -> "( ", " )" in
        fprintf ppf "%s%a%s@;<1 2>--o %a" al pp_linear_t arg_t ar pp_linear_t body_t

      | Fun (arg_t, body_t) ->
        let al, ar = match arg_t with
          | Unit | Array_t _ | Pair _ -> "", ""
          | ForAll_frac_cap _ | Fun _ -> "( ", " )" in
        fprintf ppf "@[%s%a%s@ --o %a@]" al pp_linear_t arg_t ar pp_linear_t body_t

      (* Series of quantifiers in a line: ∀ i. ∀ j. ∀ k.\n  ... *)
      | ForAll_frac_cap (var, (ForAll_frac_cap _ as linear_t)) ->
        fprintf ppf "∀ %s. %a" var.name pp_linear_t linear_t

      | ForAll_frac_cap (var, linear_t) ->
        fprintf ppf "∀ %s.@;<1 2>@[%a@]" var.name pp_linear_t linear_t

      | Array_t frac_cap ->
        fprintf ppf "Arr[%s]" (pp_frac_cap frac_cap) in

    fprintf ppf "@[%a@]@." pp_linear_t
  ;;

  let string_of_linear_t linear_t =
    let buffer = Buffer.create 80 in
    pp_linear_t (Caml.Format.formatter_of_buffer buffer) linear_t;
    Buffer.contents buffer
  ;;

  (* NOTE I assume all binding variables (or at least their ids) are unique. *)
  let sub_frac_cap_exn linear_t ~var ~replacement =
    (object(self)
      inherit Ppx_traverse_builtins.map
      inherit map as super
      method variable var' =
        if [%compare.equal : variable] var var' then
          Printf.failwithf
            !"INTERNAL ERROR: Variable %{sexp: variable} not unique in linear_t.\n\
              Ensure you call unify_linear_t before sub_frac_cap_exn" var ()
        else
          var'
      method frac_cap = function
        | Zero -> Zero
        | Succ frac_cap -> Succ (self#frac_cap frac_cap)
        | Var var' as frac_cap ->
          if [%compare.equal : variable] var var' then replacement else frac_cap
    end)#linear_t linear_t
  ;;
  
  let rec occurs_in_linear_t var =
    function
    | Unit ->
      false
    | Pair (fst_t, snd_t) ->
      occurs_in_linear_t var fst_t || occurs_in_linear_t var snd_t
    | Fun (arg_t, body_t) ->
      occurs_in_linear_t var arg_t || occurs_in_linear_t var body_t
    | ForAll_frac_cap (var', t) ->
      [%compare.equal: variable] var var' || occurs_in_linear_t var t
    | Array_t frac_cap ->
      occurs_frac_cap var frac_cap
  ;;

  let apply subs linear_t =
    try 
      List.fold subs ~init:linear_t ~f:(fun linear_t (var, replacement) ->
        sub_frac_cap_exn linear_t ~var ~replacement)
    with 
    | Failure msg ->
      failwith (msg ^ "/apply")
  ;;

  (* TODO Precompose with an map_error to stack messages upon failure *)
  Or_error.tag
  let rec unify_linear_t linear_t1 linear_t2 =
    let open Or_error.Let_syntax in
    match linear_t1, linear_t2 with
    | Unit, Unit ->
      return []

    | Pair (fst1, snd1) , Pair(fst2,snd2) ->
      let%bind subs1 = unify_linear_t fst1 fst2 in
      let%bind subs2 = unify_linear_t (apply subs1 snd1) (apply subs1 snd2) in
      return (subs1 @ subs2)

    | Fun (arg1, body1) , Fun(arg2,body2) ->
      let%bind subs1 = unify_linear_t arg1 arg2 in
      let%bind subs2 = unify_linear_t (apply subs1 body1) (apply subs1 body2) in
      return (subs1 @ subs2)
      
    (* NOTE I assume all binding variables (or at least their ids) are unique. *)
    | ForAll_frac_cap (var1,linear_t1), ForAll_frac_cap (var2,linear_t2) ->
      if ([%compare.equal : variable] var1 var2) then
        Or_error.errorf
          !"INTERNAL ERROR: binding variables are not unique.\n\
            Body 1: %{sexp: linear_t}\nBody 2: %{sexp: linear_t}"
          linear_t1 linear_t2
      else if occurs_in_linear_t var1 linear_t2 then
        Or_error.errorf !"Variable: %{sexp: variable} occurs in %a" var1
          (Fn.const string_of_linear_t) linear_t2
      else
        let subs1 = [(var1, Var var2)] in
        let%bind subs2 = unify_linear_t linear_t1 (apply subs1 linear_t2) in
        return (subs1 @ subs2)

    | Array_t frac_cap1, Array_t frac_cap2 ->
      unify_frac_cap frac_cap1 frac_cap2

    | _, _ ->
      let pp () = string_of_linear_t in
      Or_error.errorf !"Couldn't unify\n    %awith\n    %a" pp linear_t1 pp linear_t2
  ;;

end

type array_type =
  Owl.Dense.Ndarray.S.arr
;;

let sexp_of_array_type _ =
  Sexplib.Sexp.Atom "<Array>"
;;

type expression =
  | Var of variable
  | Unit_Intro
  | Unit_Elim of expression * expression
  | Pair_Intro of expression * expression
  | Pair_Elim of variable * variable * expression * expression
  | Lambda of variable * linear_t * expression
  | App of expression * expression
  | ForAll_frac_cap of variable * expression
  | Specialise_frac_cap of expression * frac_cap 
  | Array_Intro of array_type
  | Array_Elim of variable * expression * expression
(*| ForAll_Size of variable * expression *)
(*| Primitive of primitive *)

(*Primitives/extensions *)
(*and primitive =  *)
(*| Split_Permission of expression *)
(*| Merge_Permission of expression *)
(*| InPlace_Multiply of expression *)
(*| Multiply of expression *)
(*| Free of expression *)
[@@deriving sexp_of]
;;

(* TODO Internal tests *)
let%test_module "Test" =
  (module struct
    let%test "true" = true
  end)
;;
