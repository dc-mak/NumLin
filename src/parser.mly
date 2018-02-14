(* vim: set sw=2 sts=2 *)
%{
(* Dhruv Makwana *)
(* LT4LA Parser *)
(* ------------ *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

(* TODO: Use Menhir features such as
 *   - Incremental parsing
 *   - .messages error reporting     *)

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

  let mk_app fn (hd :: tl) =
    Base.List.fold_left tl ~init:(Ast.App(fn, hd)) ~f:(fun fn arg -> Ast.App (fn, arg))
    [@@ocaml.warning "-8"]
  ;;

  let mk_specialise body (hd :: tl) =
    Base.List.fold_left tl ~init:(Ast.Specialise_frac_cap(body, hd))
    ~f:(fun fn arg -> Ast.Specialise_frac_cap (fn, arg))
    [@@ocaml.warning "-8"]
  ;;

  (* Binding fractional capability variables in different constructs uniformly. *)
  type _ construct =
    | Fc : Ast.frac_cap construct
    | Lt : Ast.linear_t construct
    | Exp : Ast.expression construct
  ;;

  let rec bind_fc : type a . a construct -> Ast.variable -> a -> a =
    fun cons var in_ ->
    let open Ast in
    let (=~) {name=x;_} {name=y;_} = String.equal x y in

    match cons with
    | Fc ->

      let rec bind_fc var = function
        | Zero -> Zero
        | Succ fc -> Succ (bind_fc var fc)
        | Var var' -> Var (if var =~ var' then var else var') in
      bind_fc var in_

    | Lt ->

      let rec bind_lt var = function
        | Unit | Int | Float64 as lt -> lt
        | Pair (fst, snd) -> Pair (bind_lt var fst, bind_lt var snd)
        | Fun (fst, snd) -> Fun (bind_lt var fst, bind_lt var snd)
        | Array_t fc -> Array_t (bind_fc Fc var fc)
        | ForAll_frac_cap (var', lt) as linear_t  ->
          if var =~ var' then linear_t else ForAll_frac_cap (var, bind_lt var lt) in
        bind_lt var in_

    | Exp ->

      let rec bind_exp var = function

        | Unit_Intro | Int_Intro _ | Float64_Intro _ | Primitive _ as exp -> exp

        | Unit_Elim (exp1, exp2) -> Unit_Elim (bind_exp var exp1, bind_exp var exp2)
        | Pair_Intro (exp1, exp2) -> Pair_Intro (bind_exp var exp1, bind_exp var exp2)
        | App (exp1, exp2) -> App (bind_exp var exp1, bind_exp var exp2)
        | ForAll_frac_cap (var, exp) -> ForAll_frac_cap (var, bind_exp var exp)
        | Specialise_frac_cap (exp, fc) -> Specialise_frac_cap (bind_exp var exp, bind_fc Fc var fc)
        | Array_Intro exp -> Array_Intro (bind_exp var exp)

        | Var var' -> Var (if var =~ var' then var else var')

        | Array_Elim (var', exp1, exp2 ) ->
          Array_Elim (var', exp1, if var =~ var' then exp2 else bind_exp var exp2)

        | Pair_Elim (var1, var2, exp1, exp2) ->
          Pair_Elim (var1, var2, bind_exp var exp1,
            if var =~ var1 || var =~ var2 then exp1 else bind_exp var exp1)

        | Lambda (var', linear_t, exp ) ->
          Lambda (var', bind_fc Lt var linear_t, if var =~ var' then exp else bind_exp var exp) in

      bind_exp var in_
  ;;

  (* Bind expression variable in expression *)
  let bind_exp var in_ =
    let open Ast in
    let (=~) {name=x;_} {name=y;_} = String.equal x y in

    let rec replace var = function

      | Unit_Intro | Int_Intro _ | Float64_Intro _ | Primitive _ as exp -> exp

      | Unit_Elim (exp1, exp2) -> Unit_Elim (replace var exp1, replace var exp2)
      | Pair_Intro (exp1, exp2) -> Pair_Intro (replace var exp1, replace var exp2)
      | App (exp1, exp2) -> App (replace var exp1, replace var exp2)
      | ForAll_frac_cap (var, exp) -> ForAll_frac_cap (var, replace var exp)
      | Specialise_frac_cap (exp, fc) -> Specialise_frac_cap (replace var exp, fc)
      | Array_Intro exp -> Array_Intro (replace var exp)

      | Var var' -> Var (if var =~ var' then var else var')

      | Array_Elim (var', exp1, exp2 ) ->
        Array_Elim (var', exp1, if var =~ var' then exp2 else replace var exp2)

      | Pair_Elim (var1, var2, exp1, exp2) ->
        Pair_Elim (var1, var2, replace var exp1,
          if var =~ var1 || var =~ var2 then exp1 else replace var exp1)

      | Lambda (var', linear_t, exp ) ->
        Lambda (var', linear_t, if var =~ var' then exp else replace var exp) in

    replace var in_
  ;;

  (* --- Impure helper functions --- *)
  let new_id =
    let counter = ref 0 in
    fun () ->
      let result = !counter in
      (counter := !counter + 1; result)
  ;;

  let mk_forall name lt : Ast.linear_t =
    let var = Ast.{name; id=new_id ()} in
    Ast.ForAll_frac_cap (var, bind_fc Lt var lt)
  ;;

  let mk_lambda name lt body =
    let var = Ast.{name; id=new_id()} in
    Ast.Lambda (var, lt, bind_exp var body)
  ;;

  let mk_pair_elim a b exp body =
    let var_a, var_b = Ast.({name=a; id=new_id()}, {name=b; id=new_id()}) in
    Ast.Pair_Elim (var_a, var_b, exp, bind_exp var_b (bind_exp var_a body))
  ;;

  let mk_array_elim name exp body =
    let var = Ast.{name; id=new_id()} in
    Ast.Array_Elim (var, exp, bind_exp var body)
  ;;

  let mk_forall_exp name exp : Ast.expression =
    let var = Ast.{name; id=new_id() } in
    Ast.ForAll_frac_cap (var, bind_fc Exp var exp)
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
%token IAMAX

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
    | LET LEFT_PAREN RIGHT_PAREN EQUAL
      exp=exp SEMICOLON body=exp                       { Ast.Unit_Elim (exp, body)  }
    | LET LEFT_PAREN a=ID COMMA b=ID RIGHT_PAREN EQUAL
      exp=exp SEMICOLON body=exp                       { mk_pair_elim a b exp body  }
    | LET str=ID EQUAL exp=exp SEMICOLON body=exp      { mk_array_elim str exp body }
    | exp=simple_exp args=arg_like+                    { mk_app_like exp args       }

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
    | IAMAX      { Ast.Index_of_Max_Abs       }

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

