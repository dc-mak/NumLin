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
  ; name: string [@compare.ignore]
  }
[@@deriving sexp_of,compare]
;;

let (=~) {name=x;_} {name=y;_} =
  String.equal x y
;;

let string_of_variable {name;id} =
  name ^ "_" ^ (Int.to_string id)
;;

(* Fractional capabilities *)
include
struct

  type frac_cap =
    | Zero
    | Succ of frac_cap
    | Var of variable
  [@@deriving sexp_of,compare]
  ;;

  let string_of_frac_cap =
    let rec count acc = function
      | Zero -> Int.to_string acc
      | Succ frac_cap -> count (acc+1) frac_cap
      | Var var -> (string_of_variable var) ^ if acc = 0 then "" else "+" ^ Int.to_string acc in
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
        Or_error.errorf
          !"Could not show %s and %s and alpha-equivalent.\n"
          (string_of_variable var1)
          (string_of_variable var2)

    | _, _ ->
      let pp () = string_of_frac_cap in
      Or_error.errorf !"Could not show %a and %a are equal.\n" pp frac_cap1 pp frac_cap2
  ;;

  let rec bind_fc_fc var =
    function
    | Zero -> Zero
    | Succ fc -> Succ (bind_fc_fc var fc)
    | Var var' -> Var (if var =~ var' then var else var')
  ;;

end

(** For Matrices, this would be int * int *)
(*
 * type size =
 *   | Int of int
 *   | Var of variable
 * ;;
 *)

(* Linear types *)
include
struct

  type linear_t =
    | Unit
    | Int
    | Float64
    | Pair of linear_t * linear_t
    | Fun of linear_t * linear_t
    | ForAll_frac_cap of variable * linear_t
    | Array_t of frac_cap
  [@@deriving sexp_of,compare,traverse_map]
  ;;

  let pp_linear_t ppf =
    let open Caml.Format in
    let rec pp_linear_t ppf = function
      | Unit ->
        fprintf ppf "I"

      | Int ->
        fprintf ppf "int"

      | Float64 ->
        fprintf ppf "f64"

      (* Technically, you don't need parentheses around ForAll_frac_cap when
         it's in the second position, but it makes reading it easier *)
      | Pair (first, second) ->
        let pair_parens = function
          | Unit | Int | Float64 | Array_t _ -> "", ""
          | Pair _ | Fun _ | ForAll_frac_cap _ -> "( ", " )" in
        let fl, fr = pair_parens first in
        let sl, sr = pair_parens second in
        fprintf ppf "@[%s%a%s@ @[* %s%a%s@]@]" fl pp_linear_t first fr sl pp_linear_t second sr

      (* Hackery to ensure (1) lollipops associate to the right and (2) when
         breaking, align with the "first" argument *)
      | Fun (arg_t, (Fun _ as body_t)) ->
        let al, ar = match arg_t with
          | Unit | Int | Float64 | Array_t _ | Pair _ -> "", ""
          | ForAll_frac_cap _ | Fun _ -> "( ", " )" in
        fprintf ppf "@[%s%a%s --o@ %a@]" al pp_linear_t arg_t ar pp_linear_t body_t

      | Fun (arg_t, body_t) ->
        let al, ar = match arg_t with
          | Unit | Int | Float64 | Array_t _ | Pair _ -> "", ""
          | ForAll_frac_cap _ | Fun _ -> "( ", " )" in
        fprintf ppf "%s%a%s@ --o@ %a" al pp_linear_t arg_t ar pp_linear_t body_t

      (* Series of quantifiers in a line: ∀ i. ∀ j. ∀ k.\n  ... *)
      | ForAll_frac_cap (var, (ForAll_frac_cap _ as linear_t)) ->
        fprintf ppf "@,∀ %s. %a" (string_of_variable var) pp_linear_t linear_t

      | ForAll_frac_cap (var, linear_t) ->
        fprintf ppf "∀ %s.@;<1 2>@[%a@]" (string_of_variable var) pp_linear_t linear_t

      | Array_t frac_cap ->
        fprintf ppf "Arr[%s]" (string_of_frac_cap frac_cap) in

    fprintf ppf "@[%a@]@?" pp_linear_t
  ;;

  let string_of_linear_t linear_t =
    let buffer = Buffer.create 80 in
    pp_linear_t (Caml.Format.formatter_of_buffer buffer) linear_t;
    Buffer.contents buffer
  ;;

  let substitute_in =
    let var_ref = ref {name="INTERNAL_REF"; id=(-1)} in
    let rep_ref = ref Zero in
    let (=) = [%compare.equal : variable] in

    let sub_fun =
      (object(self)
        inherit Ppx_traverse_builtins.map
        inherit map as super
        method variable var =
          let msg = "INTERNAL ERROR: binding variables are not unique." in
          if var = !var_ref then failwith msg else var
        method frac_cap = function
          | Zero -> Zero
          | Succ frac_cap -> Succ (self#frac_cap frac_cap)
          | Var var as frac_cap -> if var = !var_ref then !rep_ref else frac_cap
      end)#linear_t in

    fun linear_t ~var ~replacement ->
      begin
        var_ref := var;
        rep_ref := replacement;
        try Or_error.return (sub_fun linear_t)
        with Failure msg ->
          Or_error.error_string msg
      end
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
    | Unit, Unit
    | Int, Int
    | Float64, Float64 ->
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
      let pp () x = string_of_linear_t x |> String.split ~on:'\n' |> String.concat ~sep:"\n    " in
      Or_error.errorf
        !"Specifically, could not show this equality:\n    %a\nwith\n    %a\n"
        pp linear_t1
        pp linear_t2
  ;;

  let same_linear_t x y : unit Or_error.t =
    Result.map_error (same_linear_t [] x y) (fun err ->
      let pp () x = string_of_linear_t x |> String.split ~on:'\n' |> String.concat ~sep:"\n    " in
      let tag = Printf.sprintf "Could not show equality:\n    %a\nwith\n    %a\n" pp x pp y in
      Error.tag ~tag err)
  ;;

  (* Bind fractional-capability variable in linear type *)
  let rec bind_fc_lt var =
    function
    | Unit | Int | Float64 as lt -> lt
    | Pair (fst, snd) -> Pair (bind_fc_lt var fst, bind_fc_lt var snd)
    | Fun (fst, snd) -> Fun (bind_fc_lt var fst, bind_fc_lt var snd)
    | Array_t fc -> Array_t (bind_fc_fc var fc)
    | ForAll_frac_cap (var', lt) as linear_t  ->
      if var =~ var' then linear_t else ForAll_frac_cap (var, bind_fc_lt var lt)
  ;;

end

include
struct
  (* Primitives/extensions
     Intel Level 1: software.intel.com/en-us/mkl-developer-reference-c-blas-level-1-routines-and-functions
     BLAS Reference: www.netlib.org/blas/blasqr.pdf
     Not included: xxDOT (derivable), xDOTU, xDOTC (Complex Float32/64) *)
  type primitive =
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

  [@@deriving sexp_of]
  ;;

  let string_of_primitive =
    function
    (* Operators *)
    | Split_Permission -> "split_perm"
    | Merge_Permission -> "merge_perm"
    | Free -> "free"
    | Copy (* xCOPY *) -> "copy"
    | Swap (* xSWAP *) -> "swap"

    (* Routines/Functions *)
    | Sum_Mag (* xASUM *) -> "asum"
    | Scalar_Mult_Then_Add (* xAXPY *) -> "axpy"
    | DotProd (* xDOT *) -> "dot"
    | Norm2 (* xNRM2 *) -> "nrm2"
    | Plane_Rotation (* xROT *) -> "rot"
    | Givens_Rotation (* xROTG *) -> "rotg"
    | GivensMod_Rotation (* xROTM *) -> "rotm"
    | Gen_GivensMod_Rotation (* xROTMG *) -> "rotmg"
    | Scalar_Mult (* xSCAL *) -> "scal"
    | Index_of_Max_Abs (* IxAMAX *) -> "amax"

  ;;

  (* NOTE: Is it worth having a GADT/typed-representation AFTER type-checking? *)
  (* Elimination rules for [Int]s and [Float64]s will come later, after an
     arithmetic expression language is fixed. *)
  type expression =
    | Var of variable
    | Int_Intro of int
    | Float64_Intro of float
    | Unit_Intro
    | Unit_Elim of expression * expression
    | Pair_Intro of expression * expression
    | Pair_Elim of variable * variable * expression * expression
    | Lambda of variable * linear_t * expression
    | App of expression * expression
    | ForAll_frac_cap of variable * expression
    | Specialise_frac_cap of expression * frac_cap
    | Array_Intro of expression
    | Array_Elim of variable * expression * expression
    (*| ForAll_Size of variable * expression *)
    | Primitive of primitive
  [@@deriving sexp_of]
  ;;

  let rec pp_expression ppf =
    let open Caml.Format in
    function

    | Var var  ->
      fprintf ppf "%s" (string_of_variable var)

    | Unit_Intro ->
      fprintf ppf "()"

    | Int_Intro i ->
      fprintf ppf "%d" i

    | Float64_Intro f ->
      fprintf ppf "%f" f

    | Unit_Elim (exp1, exp2) ->
      fprintf ppf "@[@[<2>let () =@ %a in@]@ %a@]" pp_expression exp1 pp_expression exp2

    | Pair_Intro (exp1, exp2) ->
      fprintf ppf "@[(%a@ @[, %a)@]@]" pp_expression exp1 pp_expression exp2

    | Pair_Elim (var1, var2, exp1, exp2) ->
      fprintf ppf "@[@[<2>let (%s, %s) =@ %a in@]@ %a@]"
        (string_of_variable var1) (string_of_variable var2) pp_expression exp1 pp_expression exp2

    | Lambda (var, linear_t, exp) ->
      fprintf ppf "@[<2>fun %s (* %s *) ->@ %a@]"
        (string_of_variable var) (string_of_linear_t linear_t) pp_expression exp

    | App (exp1, exp2) ->
      let parens ?(left=false) = function
        | App _ | Specialise_frac_cap _ ->
          if left then ("", "") else ("(", ")")
        | Var _| Unit_Intro | Int_Intro _ | Float64_Intro _ | Pair_Intro _ | Primitive _ ->
          "", ""
        | Unit_Elim _ | Pair_Elim _ | Lambda _ | Array_Intro _ | Array_Elim _ | ForAll_frac_cap _ ->
          "(", ")" in
      let (l1, r1), (l2, r2) = parens ~left:true exp1, parens exp2 in
      fprintf ppf "@[<2>%s%a%s@ %s%a%s@]" l1 pp_expression exp1 r1 l2 pp_expression exp2 r2

    | ForAll_frac_cap (var, exp ) ->
      fprintf ppf "@[<2>(* ∀ %s *)@ %a@]" (string_of_variable var) pp_expression exp

    | Specialise_frac_cap (exp, fc) ->
      let l,r = match exp with
        | Var _| Unit_Intro | Int_Intro _ | Float64_Intro _ | Pair_Intro _
        | Primitive _ | Specialise_frac_cap _ | App _ ->
          "", ""
        | Unit_Elim _ | Pair_Elim _ | Lambda _ | Array_Intro _ | Array_Elim _ | ForAll_frac_cap _ ->
          "(", ")" in
      fprintf ppf "@[<2>%s%a%s@ (* [%s] *)@]" l pp_expression exp r (string_of_frac_cap fc)

    | Array_Intro exp ->
      let l, r = match exp with
        | Var _| Unit_Intro | Int_Intro _ | Float64_Intro _ | Pair_Intro _ | Primitive _ ->
          "", ""
        | App _ | Unit_Elim _ | Pair_Elim _ | Lambda _ | Array_Intro _ | Array_Elim _
        | ForAll_frac_cap _ | Specialise_frac_cap _ ->
          "(", ")" in
      fprintf ppf "@[<2>array_intro@ %s%a%s@]" l pp_expression exp r

    | Array_Elim (var, exp1, exp2) ->
      fprintf ppf "@[@[<2>let %s =@ %a in@]@ %a@]"
        (string_of_variable var) pp_expression exp1 pp_expression exp2

    | Primitive primitive ->
      fprintf ppf "Prim.%s" (string_of_primitive primitive)
  ;;

  let pp_expression ppf exp =
    Caml.Format.fprintf ppf "@[%a@]@?" pp_expression exp
  ;;

  (* Bind fractional-capability variable in expression *)
  let rec bind_fc_exp var =
    function
    | Unit_Intro | Int_Intro _ | Float64_Intro _ | Primitive _ | Var _ as exp -> exp

    | Unit_Elim (exp1, exp2) -> Unit_Elim (bind_fc_exp var exp1, bind_fc_exp var exp2)
    | Pair_Intro (exp1, exp2) -> Pair_Intro (bind_fc_exp var exp1, bind_fc_exp var exp2)
    | App (exp1, exp2) -> App (bind_fc_exp var exp1, bind_fc_exp var exp2)
    | Array_Intro exp -> Array_Intro (bind_fc_exp var exp)
    | Array_Elim (var, exp1, exp2) -> Array_Elim (var, bind_fc_exp var exp1, bind_fc_exp var exp2)
    | Pair_Elim (var1, var2, exp1, exp2) -> Pair_Elim (var1, var2, bind_fc_exp var exp1, bind_fc_exp var exp2)
    | Specialise_frac_cap (exp, fc) -> Specialise_frac_cap (bind_fc_exp var exp, bind_fc_fc var fc)
    | Lambda (var', linear_t, exp) -> Lambda (var', bind_fc_lt var linear_t, bind_fc_exp var exp)

    | ForAll_frac_cap (var', exp) ->
      ForAll_frac_cap (var', if var =~ var' then exp else bind_fc_exp var exp)
  ;;

  (* Bind expression variable in expression *)
  let rec bind_exp var =
    function
    | Unit_Intro | Int_Intro _ | Float64_Intro _ | Primitive _ as exp -> exp

    | Unit_Elim (exp1, exp2) -> Unit_Elim (bind_exp var exp1, bind_exp var exp2)
    | Pair_Intro (exp1, exp2) -> Pair_Intro (bind_exp var exp1, bind_exp var exp2)
    | App (exp1, exp2) -> App (bind_exp var exp1, bind_exp var exp2)
    | ForAll_frac_cap (var, exp) -> ForAll_frac_cap (var, bind_exp var exp)
    | Specialise_frac_cap (exp, fc) -> Specialise_frac_cap (bind_exp var exp, fc)
    | Array_Intro exp -> Array_Intro (bind_exp var exp)

    | Var var' -> Var (if var =~ var' then var else var')

    | Array_Elim (var', exp1, exp2 ) ->
      Array_Elim (var', exp1, if var =~ var' then exp2 else bind_exp var exp2)

    | Pair_Elim (var1, var2, exp1, exp2) ->
      Pair_Elim (var1, var2, bind_exp var exp1,
        if var =~ var1 || var =~ var2 then exp2 else bind_exp var exp2)

    | Lambda (var', linear_t, exp ) ->
      Lambda (var', linear_t, if var =~ var' then exp else bind_exp var exp)
  ;;

end

(* TODO Internal tests *)
let%test_module "Test" =
  (module struct

    (* A stock of variables *)
    let one, two, three, four, five, six, seven, eight, nine, ten, eleven, sentinel =
      ( {id=1; name="one"}   ,   {id=2; name="two"}   ,   {id=3; name="three"}
      , {id=4; name="four"}  ,   {id=5; name="five"}  ,     {id=6; name="six"}
      , {id=7; name="seven"} ,  {id=8; name="eight"}  ,    {id=9; name="nine"}
      , {id=10; name="ten"}  , {id=11; name="eleven"} , {id=(-1); name="sentinel"} )
    ;;

    (* same_frac_cap (and pp_frac_cap) *)
    let%expect_test "same_frac_cap" =
      same_frac_cap [] Zero (Succ Zero)
      |> Stdio.printf !"%{sexp: unit Or_error.t}";
      [%expect {| (Error "Could not show 0 and 1 are equal.\n") |}]
    ;;

    let%expect_test "same_frac_cap" =
      same_frac_cap [] (Succ (Succ (Var one))) (Var one)
      |> Stdio.printf !"%{sexp: unit Or_error.t}";
      [%expect {| (Error "Could not show one+2 and one are equal.\n") |}]
    ;;

    let%expect_test "same_frac_cap" =
      same_frac_cap [] (Var one) (Succ (Succ (Var three)))
      |> Stdio.printf !"%{sexp: unit Or_error.t}";
      [%expect {| (Error "Could not show one and three+2 are equal.\n") |}]
    ;;

    let%expect_test "add" =
      let ids x = List.map x (fun (x,y) -> (x.id, y.id)) in
      let show x = Stdio.printf !"%{sexp: (int * int) list}\n" (ids x) in
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
