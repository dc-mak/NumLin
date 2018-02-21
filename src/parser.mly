(* vim: set sw=2 sts=2 *)
%{
(* Dhruv Makwana *)
(* LT4LA Parser *)
(* ------------ *)
(* TODO:                                                       *)
(* - Make 'let (a,a) = (); Array a' error more accurate        *)
(* (change return type to loc * Ast.exp and remove exceptions) *)
(* - More syntactic sugar (f (x : t) (y : t') : t'')           *)
(* - Auto-Many !t arguments                                    *)
(* - .messages error reporting                                 *)

(* --- Pure helper functions --- *)
let rec mk_fc ?str m =
  let zero = match str with None -> Ast.Z | Some str -> Ast.V str in
  let rec loop zero m = if m <= 0 then zero else Ast.S (loop zero (m-1)) in
  loop zero m
;;

let mk_app_like exp (hd :: tl) =
  let open Base.Either in
  let f exp hd = match hd with
    | First hd -> Ast.Spc (exp, hd)
    | Second hd -> Ast.App (exp, hd) in
  Base.List.fold_left tl ~init:(f exp hd) ~f
  [@@ocaml.warning "-8"]
;;

type pat =
  | Var of Ast.var
  | Many of Ast.var
  | Let of Ast.var * Ast.lin
  | Pair of Ast.var * Ast.var
  | Fix of Ast.var * Ast.var * Ast.lin * Ast.lin

let mk_let exp body = function
  | Var var ->
    Ast.Var var
  | Many var ->
    Ast.Bang_E(var, exp, body)
  | Let (var, lin) ->
    Ast.(App(Lambda(var, lin, body), exp))
  | Pair (var_a, var_b) ->
    Ast.Pair_E (var_a, var_b, exp, body)
  | Fix (f, x, tx, tres) ->
    Ast.(App(Lambda(f, Bang (Fun(tx, tres)), Bang_E(f, Var f, body)), Fix (f, x, tx, tres, exp)))
;;

(* --- Impure helper functions --- *)
let bind_pair a b : pat =
  let () = if Base.String.equal a b then raise Error in
  Pair (a,b)
;;

let bind_fix f x tx tres : pat =
  let () = if Base.String.equal f x then raise Error in
  Fix (f,x,tx,tres)
;;

%}

%token EOF
%token EOP

(* Fractional capabilities *)
%token <int> NAT
%token PLUS
%token <string> ID

(* Simple linear types *)
%token UNIT
%token INT_LT
%token ELT_LT
%token ARR_LT
%token L_BRACKET
%token R_BRACKET
%token L_PAREN
%token R_PAREN

(* Linear types *)
%token STAR
%token LOLLIPOP
%token ALL
%token DOT
%token BANG

(* Prims *)
%token ADD_INT
%token SUB_INT
%token MUL_INT
%token DIV_INT
%token EQ_INT
%token LT_INT
%token ADD_ELT
%token SUB_ELT
%token MUL_ELT
%token DIV_ELT
%token EQ_ELT
%token LT_ELT
%token AND
%token OR
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

(* Associativity and precedence *)
%nonassoc NON_LOW
%right LOLLIPOP

(* Grammar non-terminals *)
%start <Ast.exp> prog

%type <Ast.exp> simple_exp exp
%type <pat> pat
%type <(Ast.fc,Ast.exp) Base.Either.t> arg_like
%type <Ast.prim> prim
%type <Ast.lin> lin simple_lin
%type <Ast.fc> fc delimited_fc

%%

prog:
    | EOF         { raise Error }
    | exp=exp EOP { exp         }

exp:
    | simple_exp                              { $1                       }
    | MANY exp=simple_exp                     { Ast.Bang_I exp           }
    | ALL str=ID DOT exp=exp                  { Ast.Gen (str, exp)       }
    | exp=simple_exp args=arg_like+           { mk_app_like exp args     }
    | LET pat=pat EQUAL exp=exp IN body=exp   { mk_let exp body pat      }
    | IF cond=exp THEN t=exp ELSE f=exp       { Ast.If(cond, t, f)       }
    | FUN str=ID COLON lt=lin ARROW body=exp  { Ast.Lambda (str,lt,body) }

pat:
    | MANY str=ID                                               { Many str             }
    | L_PAREN a=ID COMMA b=ID R_PAREN                           { bind_pair a b        }
    | x=ID COLON tx=lin                                         { Let (x, tx)          }
    | REC f=ID L_PAREN x=ID COLON tx=lin R_PAREN COLON tres=lin { bind_fix f x tx tres }

arg_like:
    | fc=delimited_fc { Base.Either.First fc   }
    | exp=simple_exp  { Base.Either.Second exp }

simple_exp:
    | prim                                  { Ast.Prim $1           }
    | str=ID                                { Ast.Var str           }
    | L_PAREN R_PAREN                       { Ast.Unit_I            }
    | TRUE                                  { Ast.True              }
    | FALSE                                 { Ast.False             }
    | i=INT | i=NAT                         { Ast.Int_I i           }
    | f=FLOAT                               { Ast.Elt_I f           }
    | L_PAREN fst=exp COMMA snd=exp R_PAREN { Ast.Pair_I (fst, snd) }
    | L_PAREN body=exp R_PAREN              { body                  }

prim:
    | ADD_INT { Ast.(IntOp Add) }
    | SUB_INT { Ast.(IntOp Sub) }
    | MUL_INT { Ast.(IntOp Mul) }
    | DIV_INT { Ast.(IntOp Div) }
    | EQ_INT  { Ast.(IntOp Eq)  }
    | LT_INT  { Ast.(IntOp Lt)  }
    | ADD_ELT { Ast.(EltOp Add) }
    | SUB_ELT { Ast.(EltOp Sub) }
    | MUL_ELT { Ast.(EltOp Mul) }
    | DIV_ELT { Ast.(EltOp Div) }
    | EQ_ELT  { Ast.(EltOp Eq)  }
    | LT_ELT  { Ast.(EltOp Lt)  }
    | AND     { Ast.And_        }
    | OR      { Ast.Or_         }
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
    | ALL str=ID DOT lt=lin              { Ast.All (str, lt)   } %prec NON_LOW
    | fst=simple_lin STAR snd=simple_lin { Ast.Pair (fst, snd) }
    | arg=lin LOLLIPOP res=lin           { Ast.Fun (arg, res)  }

simple_lin:
    | UNIT                   { Ast.Unit    }
    | INT_LT                 { Ast.Int     }
    | ELT_LT                 { Ast.Elt     }
    | ARR_LT fc=delimited_fc { Ast.Arr fc  }
    | BANG lt=simple_lin     { Ast.Bang lt }
    | L_PAREN lt=lin R_PAREN { lt          }

delimited_fc:
    | L_BRACKET fc=fc R_BRACKET { fc }

fc:
    | str=ID PLUS n=NAT { mk_fc ~str n }
    | n=NAT             { mk_fc n      }
    | str=ID            { mk_fc ~str 0 }
