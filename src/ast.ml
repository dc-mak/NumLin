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

  let rec same_frac_cap equiv frac_cap1 frac_cap2 =
    let open Or_error.Let_syntax in
    match frac_cap1, frac_cap2 with
    | Zero, Zero ->
      return ()
    | Succ frac_cap1, Succ frac_cap2 ->
      same_frac_cap equiv frac_cap1 frac_cap2
    | Var var1, Var var2 ->
      let (=) = [%compare.equal : variable] in
      if List.exists ~f:(fun (x,y) -> x = var1 && y = var2 || y = var1 && x = var2) equiv then
        return ()
      else
        Or_error.errorf !"Could not show %s and %s and alpha-equivalent." var1.name var2.name

    | _, _ ->
      let pp () = pp_frac_cap in
      Or_error.errorf !"Could not show %a and %a are equal." pp frac_cap1 pp frac_cap2
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

  let substitute_in linear_t ~var ~replacement =
    try Or_error.return begin
      (object(self)
        inherit Ppx_traverse_builtins.map
        inherit map as super
        method variable var' =
          if [%compare.equal : variable] var' var then
            failwith "INTERNAL ERROR: binding variables are not unique."
          else
            var'
        method frac_cap = function
          | Zero -> Zero
          | Succ frac_cap -> Succ (self#frac_cap frac_cap)
          | Var var' as frac_cap ->
            if [%compare.equal : variable] var var' then replacement else frac_cap
      end)#linear_t linear_t end
    with Failure msg ->
      Or_error.error_string msg
  ;;

  (* Alpha-equivalence sucks *)
  let add (x,y) equiv =
    let (=) = [%compare.equal : variable] in
    try
      (x, y) :: List.fold_left equiv ~init:[] ~f:(fun rest (u,v) ->
        let rest = (u, v) :: rest in
        if u = x && v = y || u = y && v = x then
          failwith "don't add" (* ({u,v}={x,y}) + rest *)
        else if u = x then
          (y, v) :: rest       (* {x/u,v} + {x/u,y} + {y,v} + rest *)
        else if u = y then
          (x, v) :: rest       (* {y/u,v} + {x,y/u} + {x,v} + rest *)
        else if v = x then
          (u, y) :: rest       (* {u,x/v} + {x/v,y} + {y,u} + rest *)
        else if v = y then
          (u, x) :: rest       (* {u,v/y} + {x,v/y} + {u,x} + rest *)
        else
          rest)                (* {u,v} + {x,y} + rest *)
    with _ -> equiv
  ;;

  (* Same linear_t UP TO alpha-equivalence *)
  let rec same_linear_t equiv linear_t1 linear_t2 =
    let open Or_error.Let_syntax in

    match linear_t1, linear_t2 with
    | Unit, Unit ->
      return ()

    | Pair (fst1, snd1) , Pair(fst2,snd2) ->
      let%bind () = same_linear_t equiv fst1 fst2 in
      let%bind () = same_linear_t equiv snd1 snd2 in
      return ()

    | Fun (arg1, body1) , Fun(arg2,body2) ->
      let%bind () = same_linear_t equiv arg1 arg2 in
      let%bind () = same_linear_t equiv body1 body2 in
      return ()

    (* NOTE I assume all binding variables (or at least their ids) are unique. *)
    | ForAll_frac_cap (var1,linear_t1), ForAll_frac_cap (var2,linear_t2) ->
      if [%compare.equal : variable] var1 var2 then
        Or_error.errorf
          !"INTERNAL ERROR: binding variables are not unique.\n\
            Body 1: %{sexp: linear_t}\nBody 2: %{sexp: linear_t}"
          linear_t1 linear_t2
      else
        same_linear_t (add (var1, var2) equiv) linear_t1 linear_t2

    | Array_t frac_cap1, Array_t frac_cap2 ->
      same_frac_cap equiv frac_cap1 frac_cap2

    | _, _ ->
      let pp () = string_of_linear_t in
      Or_error.errorf !"Specifically, could not show this equality:\n    %awith\n    %a" pp linear_t1 pp linear_t2
  ;;

  let same_linear_t x y : unit Or_error.t =
    Result.map_error (same_linear_t [] x y) (fun err ->
      let pp () = string_of_linear_t in
      let tag = Printf.sprintf "Could not show equality:\n    %awith\n    %a" pp x pp y in
      Error.tag ~tag err)
  ;;

end

type array_type =
  Owl.Arr.arr
;;

let sexp_of_array_type _ =
  Sexplib.Sexp.Atom "<Array>"
;;

(* TODO: Use GADTs. Different type for values? E.g. type value = Unit | Array | Pair of value * value?  *)
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
  | Primitive of primitive

(* Primitives/extensions
   Intel Level 1: software.intel.com/en-us/mkl-developer-reference-c-blas-level-1-routines-and-functions
   BLAS Reference: www.netlib.org/blas/blasqr.pdf
   Not included: xxDOT (derivable), xDOTU, xDOTC (Complex Float32/64) *)
and primitive =
  (* Operators *)
  | Split_Permission
  | Merge_Permission
  | Free
  | Copy (* xCOPY *)
  | Swap (* xSWAP *)

  (* Routines/Functions *)
  | Sum_Mag (* xASUM *)
  | Scalar_Mult_Then_Add (* xAXPY *)
  | DotProd (* xDOT *)
  | Norm2 (* xNRM2 *)
  | Plane_Rotation (* xROT *)
  | Givens_Rotation (* xROTG *)
  | GivensMod_Rotation (* xROTM *)
  | Gen_GivensMod_Rotation (* xROTMG *)
  | Scalar_Mult (* xSCAL *)
  | Index_of_Max_Abs (* IxAMAX *)
  | Index_of_Min_Abs (* IxAMIN -- Intel only *)

[@@deriving sexp_of]
;;

(* TODO Internal tests *)
let%test_module "Test" =
  (module struct

    let printf =
      Core_kernel.printf
    ;;

    (* A stock of variables *)
    let one, two, three, four, five, six, seven, eight, nine, ten, eleven, sentinel =
      ( {id=1; name="one"}   ,   {id=2; name="two"}   ,   {id=3; name="three"}
      , {id=4; name="four"}  ,   {id=5; name="five"}  ,     {id=6; name="six"}
      , {id=7; name="seven"} ,  {id=8; name="eight"}  ,    {id=9; name="nine"}
      , {id=10; name="ten"}  , {id=11; name="eleven"} , {id=(-1); name="sentinel"} )
    ;;

    (* same_frac_cap (and pp_frac_cap) *)
    let%expect_test "same_frac_cap" =
      let open Ast in
      same_frac_cap [] Zero (Succ Zero)
      |> printf !"%{sexp: unit Or_error.t}";
      [%expect {| (Error "Could not show 0 and 1 are equal.") |}]
    ;;

    let%expect_test "same_frac_cap" =
      let open Ast in
      same_frac_cap [] (Succ (Succ (Var one))) (Var one)
      |> printf !"%{sexp: unit Or_error.t}";
      [%expect {| (Error "Could not show one+2 and one are equal.") |}]
    ;;

    let%expect_test "same_frac_cap" =
      let open Ast in
      same_frac_cap [] (Var one) (Succ (Succ (Var three)))
      |> printf !"%{sexp: unit Or_error.t}";
      [%expect {| (Error "Could not show one and three+2 are equal.") |}]
    ;;

    let%expect_test "add" =
      let ids x = List.map x (fun (x,y) -> (x.id, y.id)) in
      let show x = printf !"%{sexp: (int * int) list}\n" (ids x) in
      let it = add (one, two) []  in show it;
      let it = add (three, four) it in show it;
      let it = add (four, five) it in show it;
      let it = add (four, five) it in show it;
      [%expect {|
        ((1 2))
        ((3 4) (1 2))
        ((4 5) (1 2) (3 5) (3 4))
        ((4 5) (1 2) (3 5) (3 4)) |}]
    ;;

  end)
;;
