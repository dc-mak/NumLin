(* vim: set sw=2 sts=2 *)
%{

  let err, ret =
    Base.Or_error.(error_string, return)
  ;;

  (* Helper functions *)
  let mk_id name =
    Ast.{name; id=(-1)}
  ;;

  let rec mk_fc ?(zero=Ast.Zero) m =
    if m <= 0 then
      zero
    else
      Ast.Succ (mk_fc ~zero (m-1))
  ;;

  (* impure *)
  let new_id =
    let counter = ref 0 in
    fun () ->
      let result = !counter in
      (counter := !counter + 1; result)
  ;;

  (* impure *)
  let rec bind ~str ~in_ =
    let (=) = String.equal in
    let open Ast in

    let rec bind_fc ~str ~in_ =
      match in_ with
      | Zero -> Zero
      | Succ fc -> Succ (bind_fc ~str ~in_:fc)
      | Var {name; _} as var ->
        if name = str then Var {name; id = new_id()} else var in

    match in_ with
    | Unit -> Unit
    | Int -> Int
    | Float64 -> Float64
    | Pair (fst, snd) -> Pair (bind ~str ~in_:fst, bind ~str ~in_:snd)
    | Fun (fst, snd) -> Fun (bind ~str ~in_:fst, bind ~str ~in_:snd)
    | ForAll_frac_cap ({name; _} as var, lt) as linear_t  ->
      if name = str then linear_t else ForAll_frac_cap (var, bind ~str ~in_:lt)
    | Array_t fc ->
      Array_t (bind_fc ~str ~in_:fc)
  ;;

%}

%token EOF

(* Fractional capabilities *)
%token <int> NAT
%token PLUS
%token <string> ID

(* Linear types *)
%token UNIT
%token INT_LT
%token F64_LT
%token ARR_LT
%token LEFT_BRACKET
%token RIGHT_BRACKET

(* Grammar non-terminals *)
%start <Ast.linear_t Base.Or_error.t> prog

%type <Ast.linear_t> simple_linear_t
%type <Ast.frac_cap> frac_cap

%%

prog:
    | EOF                       { err "No input" }
    | lt = simple_linear_t; EOF { ret lt         }
    ;

simple_linear_t:
    | UNIT                                               { Ast.Unit       }
    | INT_LT                                             { Ast.Int        }
    | F64_LT                                             { Ast.Float64    }
    | ARR_LT; LEFT_BRACKET; fc = frac_cap; RIGHT_BRACKET { Ast.Array_t fc }
    ;

frac_cap :
    | str = ID; PLUS; n = NAT { mk_fc ~zero:(Ast.Var (mk_id str)) n }
    | n = NAT                 { mk_fc n                             }
    | str = ID                { Ast.Var (mk_id str)                 }
    ;
