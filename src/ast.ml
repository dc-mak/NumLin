(* Dhruv Makwana *)
(* LT4LA Typed AST *)
(* --------------- *)

open Base
;;

(* Utilities *)
let string_of_pp ?(size=80) f x =
  let buffer = Buffer.create size in
  f (Caml.Format.formatter_of_buffer buffer) x;
  Buffer.contents buffer
;;

(* Variables *)
type var =
  string
[@@deriving sexp_of,compare]
;;

include Comparator.Make(
  struct
    type t = var
    let compare = [%compare:var]
    let sexp_of_t = [%sexp_of:var]
  end)
;;

(* Fractional capabilities *)
type fc =
  | Z
  | S of fc
  | V of var
  | U of var
[@@deriving sexp_of,compare]
;;

let (=) =
  [%compare.equal:var]
;;

let string_of_fc fc =
  let rec count acc = function
    | Z -> "z" :: acc
    | S fc -> count ("s" :: acc) fc
    | V var -> ("'" ^ var) :: acc
    | U var -> ("?" ^ var) :: acc in
  String.concat ~sep:" " @@ count [] fc
;;

let rec occurs_unify var = function
  | Z -> false
  | S fc -> occurs_unify var fc
  | V _ -> false
  | U var' -> String.(var = var')
;;

let rec occurs_var (=)  var = function
  | Z -> false
  | S fc -> occurs_var (=) var fc
  | U _ -> false
  | V var' -> var = var'
;;

(* [equiv] is the minimal description of alpha-equivalent variables, that is if
   x ~ y then either (x,y) in [equiv] or (y,x) in [equiv] (but not both) *)
let rec same_fc equiv fc1 fc2 =
  let open Result.Let_syntax in
  match fc1, fc2 with
  | U var, fc | fc, U var ->
    if occurs_unify var fc then
      Result.failf "Occurs check failed: ?%s found in %a.\n" var (Fn.const string_of_fc) fc
    else
      return [(var, fc)]

  | Z, Z ->
    return []
  | S fc1, S fc2 ->
    same_fc equiv fc1 fc2
  | V var1, V var2 ->
    if List.exists ~f:(fun (x,y) -> x = var1 && y = var2 || y = var1 && x = var2) equiv then
      return []
    else
      Result.failf "Could not show '%s and '%s and alpha-equivalent.\n" var1 var2

  | V var, fc | fc, V var ->
    let (=) var1 var2 =
      List.exists equiv
        ~f:(fun (x,y) -> x = var1 && y = var2 || y = var1 && x = var2) in
    if occurs_var (=) var fc then
      Result.failf !"Occurs check failed: '%s found in %a, under alpha-equivalences:\
                     \n%{sexp: (var*var) list}\n"
        var (Fn.const string_of_fc) fc  equiv
    else
      return []

  | _, _ ->
    let pp () = string_of_fc in
    Result.failf "Could not show %a and %a are equal.\n" pp fc1 pp fc2
;;

(* Linear types *)
type lin =
  | Unit
  | Bool
  | Int
  | Elt
  | Arr of fc
  | Mat of fc
  | Pair of lin * lin
  | Bang of lin
  | Fun of lin * lin
  | All of var * lin
[@@deriving sexp_of,compare]
;;

let pp_lin ppf =
  let open Caml.Format in
  let rec pp_lin ppf = function

    | Unit ->
      fprintf ppf "unit"

    | Bool ->
      fprintf ppf "bool"

    | Int ->
      fprintf ppf "int"

    | Elt ->
      fprintf ppf "float"

    | Bang lin ->
      let l, r = match lin with
        | Unit | Bool | Int | Elt | Bang _ -> "", ""
        | Pair _ | Arr _ | Mat _ | Fun _ | All _ -> "( ", " )" in
      fprintf ppf "!%s%a%s" l pp_lin lin r

    | Arr fc ->
      fprintf ppf "%s arr" @@ string_of_fc fc

    | Mat fc ->
      fprintf ppf "%s mat" @@ string_of_fc fc

    | Pair (fst, snd) ->
      let fl, fr = match fst with
        | Unit | Bool | Int | Elt | Bang _ | Arr _ | Mat _ -> "", ""
        | Pair _ | Fun _ | All _ -> "( ", " )"
      and sl, sr = match snd with
        | Unit | Bool | Int | Elt | Bang _ | Arr _ | Mat _ -> "", ""
        | Pair _ | Fun _ | All _ -> "( ", " )" in
      fprintf ppf "@[%s%a%s@ @[* %s%a%s@]@]" fl pp_lin fst fr sl pp_lin snd sr

    | Fun (arg, (Fun _ as res)) ->
      let al, ar = match arg with
        | Unit | Bool | Int | Elt | Bang _ | Arr _ | Mat _ | Pair _-> "",""
        | Fun _ | All _ -> "( ", " )" in
      fprintf ppf "@[%s%a%s --o@ %a@]" al pp_lin arg ar pp_lin res

    | Fun (arg, res) ->
      let al, ar = match arg with
        | Unit | Bool | Int | Elt | Bang _ | Arr _ | Mat _ | Pair _-> "",""
        | Fun _ | All _ -> "( ", " )" in
      fprintf ppf "%s%a%s@ --o@ %a" al pp_lin arg ar pp_lin res

    | All (var, (All _ as lin)) ->
      fprintf ppf "@ '%s. %a" var pp_lin lin

    | All (var, lin) ->
      fprintf ppf "'%s.@;<1 2>@[%a@]" var pp_lin lin in

  fprintf ppf "@[%a@]@?" pp_lin
;;

let rec substitute_in lin ~unify ~var ~replace =
  let rec loop = function
    | Z -> Z
    | S fc -> S (loop fc)
    | U var' | V var' as fc -> if var = var' then replace else fc in
  match lin with
  | Unit | Bool | Int | Elt as lin -> lin
  | Arr fc -> Arr (loop fc)
  | Mat fc -> Mat (loop fc)

  | Pair (fst, snd) ->
    Pair (substitute_in fst ~unify ~var ~replace,
          substitute_in snd ~unify ~var ~replace)

  | Bang lin ->
    Bang (substitute_in lin ~unify ~var ~replace)

  | Fun (arg, res) ->
    Fun (substitute_in arg ~unify ~var ~replace,
         substitute_in res ~unify ~var ~replace)

  | All (var', rest) as lin ->
    if not unify && var = var' then lin else All (var', substitute_in rest ~unify ~var ~replace)
;;

let substitute_unify lin ~var ~replace =
  substitute_in lin ~unify:true ~var ~replace
;;

let substitute_in lin ~var ~replace =
  substitute_in lin ~unify:false ~var ~replace
;;

(* Alpha-equivalence sucks *)
(* Think of this logic as joining two lines by the endpoints to form a trianlge.
   Case 1-2: if both endpoints are same (modulo flipping) then already in.
   Case 3-6: check if all pairs of endpoints match, and fill in missing edge.
   Case   7: neither endpoints match, so continue as before. *)
let add (x,y) equiv =
  let (=) = [%compare.equal : var] in
  let msg = "LT4LA: don't add" in
  try
    (x, y) :: List.fold_left equiv ~init:[] ~f:(fun rest (u,v) ->
      let rest = (u, v) :: rest in
      if u = x && v = y || u = y && v = x then
        failwith msg         (* ({u,v}={x,y}) + rest *)
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
  with Failure msg' as exn ->
    if phys_equal msg msg' then equiv else raise exn
;;

(* Same lin UP TO alpha-equivalence *)
let rec same_lin equiv lin1 lin2 =
  let open Result.Let_syntax in
  let apply subs lin =
    List.fold subs ~init:lin ~f:(fun lin (var, fc) -> substitute_unify lin ~var ~replace:fc) in
  let to_string = string_of_pp pp_lin in

  match lin1, lin2 with
  | Unit, Unit
  | Bool, Bool
  | Int, Int
  | Elt, Elt ->
    return []

  | Bang lin1, Bang lin2 ->
    same_lin equiv lin1 lin2

  | Arr fc1, Arr fc2
  | Mat fc1, Mat fc2 ->
    same_fc equiv fc1 fc2

  | Pair (fst1, snd1) , Pair(fst2,snd2) ->
    let%bind subs1 = same_lin equiv fst1 fst2 in
    let%bind subs2 = same_lin equiv (apply subs1 snd1) (apply subs1 snd2) in
    return @@ subs1 @ subs2

  | Fun (arg1, body1) , Fun(arg2,body2) ->
    let%bind subs1 = same_lin equiv arg1 arg2 in
    let%bind subs2 = same_lin equiv (apply subs1 body1) (apply subs1 body2) in
    return @@ subs1 @ subs2

  | All (var1,lin1), All (var2,lin2) ->
    same_lin (add (var1, var2) equiv) lin1 lin2

  | _, _ ->
    let pp () x =
      to_string x
      |> String.split ~on:'\n'
      |> String.concat ~sep:"\n    " in
    Result.failf
      !"Specifically, could not show this equality:\n    %a\nwith\n    %a\n"
      pp lin1 pp lin2
;;

let same_lin equiv x y : (var * fc) list Or_error.t =
  Result.map_error
    (same_lin equiv x y)
    ~f:(fun err ->
       let pp () x =
         string_of_pp pp_lin x
         |> String.split ~on:'\n'
         |> String.concat ~sep:"\n    " in
       Error.of_string @@
       Printf.sprintf "Could not show equality:\n    %a\nwith\n    %a\n\n%s" pp x pp y err)
;;

(* Primitives *)
type arith =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
[@@deriving sexp_of,compare]
;;

type prim =
  (* Boolean *)
  | Not_
  (* Arithmetic *)
  | IntOp of arith
  | EltOp of arith
  (* Arrays *)
  | Set
  | Get
  | Share
  | Unshare
  | Free
  (* Owl - no polymorphism so no Mapi :'( *)
  | Array
  | Copy
  | Sin
  | Hypot
  (* Level 1 BLAS *)
  | Asum
  | Axpy
  | Dot
  | Rotmg
  | Scal
  | Amax
  (* matrix *)
  | Get_mat
  | Set_mat
  | Share_mat
  | Unshare_mat
  | Free_mat
  | Matrix
  | Copy_mat
  (* Level 2/3 BLAS *)
  | Symv
  | Gemv
  | Trmv
  | Trsv
  | Ger
  | Gemm
  | Trmm
  | Trsm
[@@deriving sexp_of,compare]
;;

let string_of_prim x =
  match sexp_of_prim x with
  | Sexp.Atom str ->
    String.lowercase str
  | Sexp.(List [Atom "IntOp"; Atom arith]) ->
    String.lowercase arith ^ "I"
  | Sexp.(List [Atom "EltOp"; Atom arith]) ->
    String.lowercase arith ^ "E"
  | _ ->
    assert false
;;

(* Expressions *)
type exp =
  | Prim of prim
  | Var of var
  | Unit_I
  | True
  | False
  | Int_I of int
  | Elt_I of float
  | Pair_I of exp * exp
  | Bang_I of exp
  | Spc of exp * fc
  | App of exp * exp
  | Bang_E of var * exp * exp
  | Pair_E of var * var * exp * exp
  | Fix of var * var * lin * lin * exp
  | If of exp * exp * exp
  | Gen of var * exp
  | Lambda of var * lin * exp
[@@deriving sexp_of,compare]
;;

let prec = function
  | Prim _ | Var _ | Unit_I | Pair_I _ -> 0
  | True | False | Int_I _ | Elt_I _ | Spc _ | App _ | Bang_I _ -> -1
  | Bang_E _ | Pair_E _ | Fix _ | Gen _ | Lambda _ | If _ -> -2
;;

let rec is_value = function
  | Prim _ | Unit_I | True | False | Int_I _ | Elt_I _ | Var _ | Fix _ | Lambda _ -> true
  | App _ | Bang_E _ | Pair_E _ | If _ -> false
  | Gen (_, exp) | Spc (exp, _) | Bang_I exp -> is_value exp
  | Pair_I (fst, snd) -> is_value fst && is_value snd
;;

let pp_exp ppf =
  let open Caml.Format in
  let parens exp ref = if prec exp > prec ref then "","" else "(", ")" in
  let rec pp_exp ppf = function

    | Prim prim ->
      fprintf ppf "Prim.%s" (string_of_prim prim)

    | Var var ->
      fprintf ppf "%s" var

    | Unit_I ->
      fprintf ppf "()"

    | True ->
      fprintf ppf "Many true"

    | False ->
      fprintf ppf "Many false"

    | Int_I i ->
      fprintf ppf "Many %d" i

    | Elt_I e ->
      fprintf ppf "Many %f" e

    | Pair_I (exp1, exp2) ->
      fprintf ppf "@[(%a@ @[, %a)@]@]" pp_exp exp1 pp_exp exp2

    | Bang_I exp as bang ->
      let l,r = parens exp bang in
      fprintf ppf "Many %s%a%s" l pp_exp exp r

    | Spc (exp, fc) as spc ->
      let l, r = if prec exp >= prec spc then "","" else "(", ")" in
      fprintf ppf "@[<2>%s%a%s@ (* [%s] *)@]" l pp_exp exp r (string_of_fc fc)

    | App (Lambda(f2, Bang(Fun(tx, res)), Bang_E(f3, Var f2', body)),
           Fix (f1, x, tx', res', exp))
      when phys_equal tx tx' &&
           phys_equal res res' &&
           phys_equal f2 f2' &&
           phys_equal f1 f2 &&
           phys_equal f1 f3 ->
        fprintf ppf "@[@[<2>let rec %s (%s (* %s *)) (* %s *) =@ %a in@]@ %a@]"
          f1 x (string_of_pp pp_lin tx) (string_of_pp pp_lin res) pp_exp exp pp_exp body

    | App (Lambda(var, lin, body), exp) ->
      fprintf ppf "@[@[<2>let %s (* %s *) =@ %a in@]@ %a@]"
        var (string_of_pp pp_lin lin) pp_exp exp pp_exp body

    | App (fun_, arg) as app ->
      let fl, fr = if prec fun_ >= prec app then "","" else "(", ")"
      and al, ar = parens arg app in
      fprintf ppf "@[<2>%s%a%s@ %s%a%s@]" fl pp_exp fun_ fr al pp_exp arg ar

    | Bang_E (var1, Var var2, Bang_E (var3, Bang_I (Bang_I (Var var4)), body))
      when var1 = var2 &&
           var3 = var4 &&
           var1 = var4 ->
      fprintf ppf "@[%a@]" pp_exp body

    | Bang_E (var, exp, body) ->
      fprintf ppf "@[@[<2>let Many %s =@ %a in@]@ %a@]" var pp_exp exp pp_exp body

    | Pair_E (var1, var2, exp1, exp2) ->
      fprintf ppf "@[@[<2>let (%s, %s) =@ %a in@]@ %a@]"
        var1 var2 pp_exp exp1 pp_exp exp2

    | Fix (f, x, tx, res, body) ->
      fprintf ppf "@[@[<2>let rec %s (%s (* %s *)) (* %s *) =@ %a in@]@ Many %s@]"
        f x (string_of_pp pp_lin tx) (string_of_pp pp_lin res) pp_exp body f

    | If (cond, True, False) ->
      pp_exp ppf cond

    | If (cond, true_, False) ->
      fprintf ppf "@[<hv>(Many ( (Prim.extract %@%@ @[%a@]) && (Prim.extract %@%@@ @[%a@])))@]"
        pp_exp cond pp_exp true_

    | If (cond, True, false_) ->
      fprintf ppf "@[<hv>(Many ( (Prim.extract %@%@ @[%a@]) || (Prim.extract %@%@@ @[%a@])))@]"
        pp_exp cond pp_exp false_

    | If (cond, true_, false_) ->
      fprintf ppf "@[<hv>if Prim.extract %@%@@;<1 3> %a then@;<1 2>@[%a@]@;else@;<1 2>@[%a@]@]"
        pp_exp cond pp_exp true_ pp_exp false_

    | Gen (var, (Gen _ | Lambda _ as exp)) ->
      fprintf ppf "@[(* ∀ %s *)@ %a@]" var pp_exp exp

    | Gen (var, exp) ->
      fprintf ppf "@[<2>(* ∀ %s *)@ %a@]" var pp_exp exp

    | Lambda (var, lin, (Lambda _ | Gen _ as exp)) ->
      fprintf ppf "@[fun %s (* %s *) ->@ %a@]"
        var (string_of_pp pp_lin lin) pp_exp exp

    | Lambda (var, lin, exp) ->
      fprintf ppf "@[<2>fun %s (* %s *) ->@ %a@]"
        var (string_of_pp pp_lin lin) pp_exp exp in

  fprintf ppf "@[%a@]@?" pp_exp
;;

let%test_module "Test" =
  (module struct

    (* A stock of variables *)
    let one, two, three, four, five =
      ( "one", "two", "three", "four", "five" )
    ;;

    (* same_fc/string_of_fc *)
    let%expect_test "same_fc" =
      same_fc [] Z (S Z)
      |> Stdio.printf !"%{sexp:((var * fc) list,string)Result.t}";
      [%expect {| (Error "Could not show z and z s are equal.\n") |}]
    ;;

    let%expect_test "same_fc" =
      same_fc [] (S (S (V one))) (V one)
      |> Stdio.printf !"%{sexp:((var * fc) list,string)Result.t}";
      [%expect {| (Ok ()) |}]
    ;;

    let%expect_test "same_fc" =
      same_fc [(one,one)] (S (S (V one))) (V one)
      |> Stdio.printf !"%{sexp:((var * fc) list,string)Result.t}";
      [%expect {|
        (Error
          "Occurs check failed: 'one found in 'one s s, under alpha-equivalences:\
         \n((one one))\
         \n") |}]
    ;;


    let%expect_test "same_fc" =
      same_fc [] (V one) (S (S (V three)))
      |> Stdio.printf !"%{sexp:((var * fc) list,string)Result.t}";
      [%expect {| (Ok ()) |}]
    ;;

    let%expect_test "add" =
      let show x = Stdio.printf !"%{sexp: (var * var) list}\n" x in
      let it = add (one, two) []  in show it;
      let it = add (three, four) it in show it;
      let it = add (four, five) it in show it;
      let it = add (four, five) it in show it;
      [%expect {|
        ((one two))
        ((three four) (one two))
        ((four five) (one two) (three five) (three four))
        ((four five) (one two) (three five) (three four)) |}]
    ;;

  end)
;;
