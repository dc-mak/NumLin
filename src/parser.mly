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

%}

%token EOF
(* Fractional capabilities *)
%token <int> NAT
%token PLUS
%token <string> ID

%start <Ast.frac_cap Base.Or_error.t> prog

%type <Ast.frac_cap> frac_cap

%%

prog:
    | EOF                 { err "No input" }
    | fc = frac_cap ; EOF { ret fc         }
    ;

frac_cap :
    | str = ID; PLUS; n = NAT { mk_fc ~zero:(Ast.Var (mk_id str)) n }
    | n = NAT                 { mk_fc n                             }
    | str = ID                { Ast.Var (mk_id str)                 }
    ;
