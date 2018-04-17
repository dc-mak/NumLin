(* vim: set sw=2 sts=2 *)
%{
(* Dhruv Makwana *)
(* LT4LA Parser *)
(* ------------ *)
(* TODO:                                                         *)
(* - Make 'let (a,a) = (); Array a' error more accurate          *)
(*   (change return type to loc * Ast.exp and remove exceptions) *)
(* - arithmetic, boolean and matrix expressions                  *)
(* - short-circuiting boolean operators                          *)
(* - .messages error reporting                                   *)

(* --- Pure helper functions --- *)
let mk_app_like exp (hd :: tl) =
  let open Base.Either in
  let f exp hd = match hd with
    | First hd -> Ast.Spc (exp, hd)
    | Second hd -> Ast.App (exp, hd) in
  Base.List.fold_left tl ~init:(f exp hd) ~f
  [@@ocaml.warning "-8"]
;;

let mk_bang var body =
  Ast.Bang_E (var, Var var, Bang_E (var, Bang_I (Bang_I (Var var)), body))
;;

type bang_id =
  | Bang of Ast.var
  | NotB of Ast.var
;;

let unwrap = function
  | Bang x
  | NotB x -> x
;;

let mk_lambda var lin body =
  match var with
  | NotB var -> Ast.Lambda (var, lin, body)
  | Bang var -> Ast.Lambda (var, lin, mk_bang var body)
;;

type pat =
  | Many of Ast.var
  | Let of Ast.var * Ast.lin
  | LetBang of Ast.var * Ast.lin
  | Pair of Ast.var * Ast.var
  | PairBangL of Ast.var * Ast.var
  | PairBangR of Ast.var * Ast.var
  | PairBangLR of Ast.var * Ast.var
  | Fix of Ast.var * bang_id * Ast.lin * (bang_id * Ast.lin, Ast.var) Base.Either.t list * Ast.lin
;;

let bind_let (var, lin) =
  match var with
  | NotB var -> Let (var, lin)
  | Bang var -> LetBang (var, lin)
;;

let desugar_let exp body = function
  | Many var ->
    Ast.Bang_E (var, exp, body)

  | Let (var, lin) ->
    Ast.(App (Lambda (var, lin, body), exp))

  | LetBang (var, lin) ->
    Ast.(App (Lambda (var, lin, mk_bang var body), exp))

  | Pair (var_a, var_b) ->
    Ast.Pair_E (var_a, var_b, exp, body)

  | PairBangL (var_a, var_b) ->
    Ast.Pair_E (var_a, var_b, exp, mk_bang var_a body)

  | PairBangR (var_a, var_b) ->
    Ast.Pair_E (var_a, var_b, exp, mk_bang var_b body)

  | PairBangLR (var_a, var_b) ->
    Ast.Pair_E (var_a, var_b, exp, mk_bang var_a @@ mk_bang var_b body)

  | Fix (f, x, tx, xs, tres) ->
    let open Base.Either in
    let rec loop = function
      | [] -> (tres, exp)
      | x :: xs ->
        let (tres, exp) = loop xs in
        match x with
        | First (Bang x, tx) -> (Ast.Fun (tx, tres), Ast.Lambda (x, tx, mk_bang x exp))
        | First (NotB x, tx) -> (Ast.Fun (tx, tres), Ast.Lambda (x, tx, exp))
        | Second x -> (Ast.All (x, tres), Ast.Gen (x, exp)) in
    let (tres, exp) = loop xs in
    let fix = match x with
      | NotB x -> Ast.Fix (f, x, tx, tres, exp)
      | Bang x -> Ast.Fix (f, x, tx, tres, mk_bang x exp) in
    Ast.App (Lambda(f, Bang (Fun (tx, tres)), Bang_E (f, Var f, body)), fix)
;;

(* arrays *)
let desugar_assign str ~index exp =
  Ast.App(App(App(Prim Set, Var str), index), exp)
;;

(* boolean *)
let desugar_or fst snd =
  Ast.If (fst, True, snd)
;;

let desugar_and fst snd =
  Ast.If (fst, snd, False)
;;

(* integer *)
let desugar_eq fst snd =
  Ast.App(App(Prim (IntOp Eq), fst), snd)
;;

let desugar_lt fst snd =
  Ast.App(App(Prim (IntOp Lt), fst), snd)
;;

let desugar_add fst snd =
  Ast.App(App(Prim (IntOp Add), fst), snd)
;;

let desugar_sub fst snd =
  Ast.App(App(Prim (IntOp Sub), fst), snd)
;;

let desugar_mul fst snd =
  Ast.App(App(Prim (IntOp Mul), fst), snd)
;;

let desugar_div fst snd =
  Ast.App(App(Prim (IntOp Div), fst), snd)
;;

(* element *)
let desugar_eqE fst snd =
  Ast.App(App(Prim (EltOp Eq), fst), snd)
;;

let desugar_ltE fst snd =
  Ast.App(App(Prim (EltOp Lt), fst), snd)
;;

let desugar_addE fst snd =
  Ast.App(App(Prim (EltOp Add), fst), snd)
;;

let desugar_subE fst snd =
  Ast.App(App(Prim (EltOp Sub), fst), snd)
;;

let desugar_mulE fst snd =
  Ast.App(App(Prim (EltOp Mul), fst), snd)
;;

let desugar_divE fst snd =
  Ast.App(App(Prim (EltOp Div), fst), snd)
;;

(* --- Impure helper functions --- *)
let unify_var =
  let v = ref 0 and f = Format.sprintf "__unify%d" in
  fun () -> let x = !v in (v := x + 1; Ast.U (f x))
;;

let bind_pair a b : pat =
  let () = if Base.String.(unwrap a = unwrap b) then raise Error in
  match a,b with
  | NotB a, NotB b -> Pair (a,b)
  | Bang a, NotB b -> PairBangL (a,b)
  | NotB a, Bang b -> PairBangR (a,b)
  | Bang a, Bang b -> PairBangLR (a,b)
;;

let rec split (fst, snd) = function
  | [] -> (fst, snd)
  | Base.Either.First (x, _) :: xs -> split (unwrap x :: fst, snd) xs
  | Base.Either.Second x :: xs -> split (fst, x :: snd) xs
;;

let rec all_distinct f = function
  | [] -> true
  | x :: xs -> Base.String.(f <> x) && all_distinct f xs && all_distinct x xs
;;

let bind_fix f (x,tx) xs tres : pat =
  let () = let (fst, snd) = split ([], []) xs in
           if not @@ all_distinct f (unwrap x :: fst) && all_distinct f snd then raise Error in
  Fix (f, x, tx, xs, tres)
  [@@ocaml.warning "-8"]

let desugar_index str exp =
  Ast.App(App(Spc(Prim Get, unify_var ()), Var str), exp)
;;

[@@@ ocaml.warning "-9" (* labels not found in record pattern *) ]
%}

%token EOF
%token EOP

(* Fractional capabilities *)
%token UNDERSCORE
%token ZED
%token ES
%token <string> FC_VAR
%token <string> ID

(* Simple linear types *)
%token UNIT
%token INT_LT
%token ELT_LT
%token ARR_LT
%token L_PAREN
%token R_PAREN

(* Linear types *)
%token STAR
%token LOLLIPOP
%token DOT
%token BANG

(* Prims *)
%token NOT
%token SET
%token GET
%token SHARE
%token UNSHARE
%token FREE
%token ARRAY
%token COPY
%token SIN
%token HYPOT
%token ASUM
%token AXPY
%token DOTP
%token ROTMG
%token SCAL
%token AMAX

(* Simple expressions *)
%token TRUE
%token FALSE
%token <int> INT
%token <float> FLOAT
%token COMMA
%token COLON
%token FUN

(* Expressions *)
%token LET
%token REC
%token EQUAL
%token IN
%token MANY
%token ARROW
%token IF
%token THEN
%token ELSE
(* sugar *)
%token L_BRACKET
%token R_BRACKET
%token COLON_EQ
(* boolean *)
%token DOUBLE_BAR
%token DOUBLE_AND
(* integer *)
%token LESS_THAN
%token PLUS
%token MINUS
%token FWD_SLASH
(* element *)
%token EQUAL_DOT
%token LESS_THAN_DOT
%token PLUS_DOT
%token STAR_DOT
%token MINUS_DOT
%token FWD_SLASH_DOT

(* Associativity and precedence *)
%nonassoc IN ELSE ARROW COLON_EQ NON_LOW
%left PLUS PLUS_DOT MINUS MINUS_DOT
%left STAR STAR_DOT FWD_SLASH FWD_SLASH_DOT
%right DOUBLE_BAR
%right DOUBLE_AND LOLLIPOP
%nonassoc LESS_THAN LESS_THAN_DOT
%nonassoc EQUAL EQUAL_DOT

(* Grammar non-terminals *)
%start <Ast.exp> prog

%type <Ast.exp> simple_exp exp
%type <pat> pat
%type <(bang_id * Ast.lin, Ast.var) Base.Either.t> bind_arg_like
%type <bang_id * Ast.lin> bind_arg
%type <bang_id> bang_id
%type <(Ast.fc, Ast.exp) Base.Either.t> arg_like
%type <Ast.prim> prim
%type <Ast.lin> lin simple_lin
%type <Ast.fc> fc unit_fc simple_fc
%type <Ast.var> fc_var

%%

prog:
    | EOF         { raise Error }
    | exp=exp EOP { exp         }

exp:
    | simple_exp                                            { $1                            }
    | MANY exp=simple_exp                                   { Ast.Bang_I exp                }
    | FUN str=fc_var ARROW exp=exp                          { Ast.Gen (str, exp)            }
    | exp=simple_exp args=arg_like+                         { mk_app_like exp args          }
    | IF cond=exp THEN t=exp ELSE f=exp                     { Ast.If(cond, t, f)            }
    | FUN str=bang_id COLON lt=lin ARROW body=exp           { mk_lambda str lt body         }
    (* sugar *)
    | LET pat=pat EQUAL exp=exp IN body=exp                 { desugar_let exp body pat      }
    (* arrays *)
    | str=ID L_BRACKET exp=exp R_BRACKET                    { desugar_index str exp         }
    | str=ID L_BRACKET index=exp R_BRACKET COLON_EQ exp=exp { desugar_assign str ~index exp }
    (* boolean *)
    | fst=exp DOUBLE_BAR snd=exp                            { desugar_or fst snd            }
    | fst=exp DOUBLE_AND snd=exp                            { desugar_and fst snd           }
    (* integer *)
    | fst=exp EQUAL snd=exp                                 { desugar_eq fst snd            }
    | fst=exp LESS_THAN snd=exp                             { desugar_lt fst snd            }
    | fst=exp PLUS snd=exp                                  { desugar_add fst snd           }
    | fst=exp MINUS snd=exp                                 { desugar_sub fst snd           }
    | fst=exp STAR snd=exp                                  { desugar_mul fst snd           }
    | fst=exp FWD_SLASH snd=exp                             { desugar_div fst snd           }
    (* element *)
    | fst=exp EQUAL_DOT snd=exp                             { desugar_eqE fst snd           }
    | fst=exp LESS_THAN_DOT snd=exp                         { desugar_ltE fst snd           }
    | fst=exp PLUS_DOT snd=exp                              { desugar_addE fst snd          }
    | fst=exp MINUS_DOT snd=exp                             { desugar_subE fst snd          }
    | fst=exp STAR_DOT snd=exp                              { desugar_mulE fst snd          }
    | fst=exp FWD_SLASH_DOT snd=exp                         { desugar_divE fst snd          }

pat:
    | MANY str=ID                                                             { Many str                }
    | L_PAREN a=bang_id COMMA b=bang_id R_PAREN                               { bind_pair a b           }
    | res=bind_arg                                                            { bind_let res            }
    | REC f=ID L_PAREN x_tx=bind_arg R_PAREN xs=bind_arg_like* COLON tres=lin { bind_fix f x_tx xs tres }

bind_arg_like:
    | L_PAREN res=bind_arg R_PAREN { Base.Either.First res  }
    | L_PAREN str=fc_var R_PAREN   { Base.Either.Second str }

bind_arg:
    | x=bang_id COLON tx=lin { (x, tx) }

bang_id:
    | res=ID      { NotB res }
    | BANG res=ID { Bang res }

arg_like:
    | fc=simple_fc   { Base.Either.First fc             }
    | UNDERSCORE     { Base.Either.First (unify_var ()) }
    | exp=simple_exp { Base.Either.Second exp           }

simple_exp:
    | prim                                  { Ast.Prim $1           }
    | str=ID                                { Ast.Var str           }
    | L_PAREN R_PAREN                       { Ast.Unit_I            }
    | TRUE                                  { Ast.True              }
    | FALSE                                 { Ast.False             }
    | i=INT                                 { Ast.Int_I i           }
    | f=FLOAT                               { Ast.Elt_I f           }
    | L_PAREN fst=exp COMMA snd=exp R_PAREN { Ast.Pair_I (fst, snd) }
    | L_PAREN body=exp R_PAREN              { body                  }

prim:
    | NOT     { Ast.Not_        }
    | SET     { Ast.Set         }
    | GET     { Ast.Get         }
    | SHARE   { Ast.Share       }
    | UNSHARE { Ast.Unshare     }
    | FREE    { Ast.Free        }
    | ARRAY   { Ast.Array       }
    | COPY    { Ast.Copy        }
    | SIN     { Ast.Sin         }
    | HYPOT   { Ast.Hypot       }
    | ASUM    { Ast.Asum        }
    | AXPY    { Ast.Axpy        }
    | DOTP    { Ast.Dot         }
    | ROTMG   { Ast.Rotmg       }
    | SCAL    { Ast.Scal        }
    | AMAX    { Ast.Amax        }

lin:
    | simple_lin                         { $1                  }
    | str=fc_var DOT lt=lin              { Ast.All (str, lt)   } %prec NON_LOW
    | fst=simple_lin STAR snd=simple_lin { Ast.Pair (fst, snd) }
    | arg=lin LOLLIPOP res=lin           { Ast.Fun (arg, res)  }

simple_lin:
    | UNIT                   { Ast.Unit    }
    | INT_LT                 { Ast.Int     }
    | ELT_LT                 { Ast.Elt     }
    | fc=fc ARR_LT           { Ast.Arr fc  }
    | BANG lt=simple_lin     { Ast.Bang lt }
    | L_PAREN lt=lin R_PAREN { lt          }

simple_fc:
  | L_PAREN fc=fc R_PAREN { fc }
  | fc=unit_fc            { fc }

fc:
  | unit_fc  { $1       }
  | fc=fc ES { Ast.S fc }

unit_fc:
    | ZED         { Ast.Z     }
    | str=fc_var  { Ast.V str }

fc_var:
    | str=FC_VAR { Base.String.chop_prefix_exn ~prefix:"'" str}
