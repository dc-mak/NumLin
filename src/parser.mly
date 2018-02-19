(* vim: set sw=2 sts=2 *)
%{
(* Dhruv Makwana *)
(* LT4LA Parser *)
(* ------------ *)
(* TODO:                                                  *)
(*   - Check variables are bound while parsing            *)
(*   - Make 'let (a,a) = (); Array a' error more accurate *)
(*     (change return type to loc * Ast.expression)       *)
(*   - .messages error reporting                          *)

(* --- Pure helper functions --- *)
let err, ret =
  Base.Or_error.(error_string, return)
;;

let mk_id name =
  Ast.{name; id=(-1)}
;;

let rec mk_fc ?str m =
  let zero = match str with None -> Ast.Zero | Some str -> Ast.Var (mk_id str) in
  let rec loop zero m = if m <= 0 then zero else Ast.Succ (loop zero (m-1)) in
  loop zero m
;;

let mk_app_like exp (hd :: tl) =
  let open Base.Either in
  let f exp hd = match hd with
    | First hd -> Ast.Specialise_frac_cap (exp, hd)
    | Second hd -> Ast.App (exp, hd) in
  Base.List.fold_left tl ~init:(f exp hd) ~f
  [@@ocaml.warning "-8"]
;;

type pat =
  | Unit
  | Pair of Ast.variable * Ast.variable
  | Array of Ast.variable

let mk_let exp body = function
  | Unit ->
    Ast.Unit_Elim(exp, body)
  | Array var ->
    Ast.Array_Elim (var, exp, Ast.bind_exp var body)
  | Pair (var_a, var_b) ->
    Ast.Pair_Elim (var_a, var_b, exp, Ast.bind_exp var_b (Ast.bind_exp var_a body))
;;

(* --- Impure helper functions --- *)
let new_id =
  let counter = ref 0 in
  fun () ->
    let result = !counter in
    (counter := !counter + 1; result)
;;

let bind_array a =
  Array Ast.({name=a; id=new_id()})
;;

let bind_pair a b : pat =
  let () = if Base.String.equal a b then raise Error in
  let a,b = Ast.({name=a; id=new_id()}, {name=b; id=new_id()}) in
  Pair (a,b)
;;

let mk_forall name lt : Ast.linear_t =
  let var = Ast.{name; id=new_id ()} in
  Ast.ForAll_frac_cap (var, Ast.bind_fc_lt var lt)
;;

let mk_lambda name lt body =
  let var = Ast.{name; id=new_id()} in
  Ast.Lambda (var, lt, Ast.bind_exp var body)
;;


let mk_forall_exp name exp : Ast.expression =
  let var = Ast.{name; id=new_id() } in
  Ast.ForAll_frac_cap (var, Ast.bind_fc_exp var exp)
;;

%}

%token EOF

(* Fractional capabilities *)
%token <int> NAT
%token PLUS
%token <string> ID

(* Simple linear types *)
%token UNIT
%token INT_LT
%token F64_LT
%token ARR_LT
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN

(* Linear types *)
%token STAR
%token LOLLIPOP
%token ALL
%token DOT

(* Primitives *)
%token SPLIT_PERM
%token MERGE_PERM
%token FREE
%token COPY
%token SWAP
%token ASUM
%token AXPY
%token DOT_PROD
%token NRM2
%token ROT
%token ROTG
%token ROTM
%token ROTMG
%token SCAL
%token AMAX

(* Simple expressions *)
%token <int> INT
%token <float> FLOAT
%token COMMA
%token COLON
%token BACKSLASH

(* Expressions *)
%token LET
%token EQUAL
%token SEMICOLON
%token ARRAY

(* Associativity and precedence *)
%nonassoc NON_LOW
%right LOLLIPOP

(* Grammar non-terminals *)
%start <Ast.expression Base.Or_error.t> prog

%type <Ast.expression> simple_exp exp
%type <pat> pat
%type <(Ast.frac_cap,Ast.expression) Base.Either.t> arg_like
%type <Ast.primitive> primitive
%type <Ast.linear_t> linear_t simple_lt
%type <Ast.frac_cap> frac_cap delimited_fc

%%

prog:
    | EOF         { err "No input" }
    | exp=exp EOF { ret exp        }

exp:
    | simple_exp                                       { $1                         }
    | ALL str=ID DOT exp=exp                           { mk_forall_exp str exp      }
    | BACKSLASH str=ID  COLON lt=linear_t DOT body=exp { mk_lambda str lt body      }
    | ARRAY arg=simple_exp                             { Ast.Array_Intro arg        }
    | LET pat=pat EQUAL exp=exp SEMICOLON body=exp     { mk_let exp body pat        }
    | exp=simple_exp args=arg_like+                    { mk_app_like exp args       }

pat:
    | LEFT_PAREN RIGHT_PAREN                 { Unit           }
    | str=ID                                 { bind_array str }
    | LEFT_PAREN a=ID COMMA b=ID RIGHT_PAREN { bind_pair a b  }
    | LEFT_PAREN pat=pat RIGHT_PAREN         { pat            }

arg_like:
    | fc=delimited_fc { Base.Either.First fc   }
    | exp=simple_exp  { Base.Either.Second exp }

simple_exp:
    | str=ID                                       { Ast.Var (mk_id str)       }
    | i=INT | i=NAT                                { Ast.Int_Intro i           }
    | f=FLOAT                                      { Ast.Float64_Intro f       }
    | LEFT_PAREN RIGHT_PAREN                       { Ast.Unit_Intro            }
    | LEFT_PAREN fst=exp COMMA snd=exp RIGHT_PAREN { Ast.Pair_Intro (fst, snd) }
    | primitive                                    { Ast.Primitive $1          }
    | LEFT_PAREN body=exp RIGHT_PAREN              { body                      }

primitive:
    | SPLIT_PERM { Ast.Split_Permission       }
    | MERGE_PERM { Ast.Merge_Permission       }
    | FREE       { Ast.Free                   }
    | COPY       { Ast.Copy                   }
    | SWAP       { Ast.Swap                   }
    | ASUM       { Ast.Sum_Mag                }
    | AXPY       { Ast.Scalar_Mult_Then_Add   }
    | DOT_PROD   { Ast.DotProd                }
    | NRM2       { Ast.Norm2                  }
    | ROT        { Ast.Plane_Rotation         }
    | ROTG       { Ast.Givens_Rotation        }
    | ROTM       { Ast.GivensMod_Rotation     }
    | ROTMG      { Ast.Gen_GivensMod_Rotation }
    | SCAL       { Ast.Scalar_Mult            }
    | AMAX       { Ast.Index_of_Max_Abs       }

linear_t:
    | simple_lt                          { $1                  }
    | ALL str=ID DOT lt=linear_t         { mk_forall str lt    } %prec NON_LOW
    | fst=simple_lt STAR snd=simple_lt   { Ast.Pair (fst, snd) }
    | arg=linear_t LOLLIPOP res=linear_t { Ast.Fun (arg, res)  }

simple_lt:
    | UNIT                               { Ast.Unit       }
    | INT_LT                             { Ast.Int        }
    | F64_LT                             { Ast.Float64    }
    | ARR_LT fc=delimited_fc             { Ast.Array_t fc }
    | LEFT_PAREN lt=linear_t RIGHT_PAREN { lt             }

delimited_fc:
    | LEFT_BRACKET fc=frac_cap RIGHT_BRACKET { fc }

frac_cap:
    | str=ID PLUS n=NAT { mk_fc ~str n }
    | n=NAT             { mk_fc n      }
    | str=ID            { mk_fc ~str 0 }
