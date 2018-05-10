(* vim: set sw=2 sts=2 *)
%{
(* Dhruv Makwana *)
(* LT4LA Parser *)
(* ------------ *)
(* TODO:                                                         *)
(* - Make 'let (a,a) = (); Array a' error more accurate          *)
(*   (change return type to loc * Sugar.exp and remove exceptions) *)
(* - arithmetic, boolean and matrix expressions                  *)
(* - .messages error reporting                                   *)

let mk_app_like exp (first :: rest) =
  Sugar.(AppLike (loc exp, exp, {first; rest}))
  [@@ocaml.warning "-8"]
;;

let mk_lambda loc (first :: rest) body =
  Sugar.Lambda (loc, {first;rest}, body)
  [@@ocaml.warning "-8"]
;;

let mk_index loc str (fst, snd) =
  Sugar.Index (loc, str, fst, snd)
;;

let mk_assign loc str (fst, snd) exp =
  Sugar.Assign (loc, str, fst, snd, exp)
;;

type ('a, 'b) either = ('a, 'b) Base.Either.t =
  | First of 'a
  | Second of 'b
;;

let mk_let destruct loc exp body =
  match destruct with
  | First (bang_var, lin) ->
    Sugar.LetAnnot (loc, bang_var, lin, exp, body)
  | Second pat ->
    Sugar.LetPat (loc, pat, exp, body)
;;

let mk_rec str arg binds lin loc exp body =
  Sugar.LetRecFun (loc, str, arg, binds, lin, exp, body)
;;

let mk_fun str (first :: rest) lin loc exp body =
  Sugar.LetFun (loc, str, {first;rest}, lin, exp, body)
  [@@ocaml.warning "-8"]
;;

let mk_op loc op fst snd =
  Sugar.Infix (loc, fst, op, snd)
;;

type destruct =
  (Sugar.bang_var * Sugar.lin, Sugar.pat) either
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
%token MAT_LT
%token L_PAREN
%token R_PAREN

(* Linear types *)
%token STAR
%token LOLLIPOP
%token DOT
%token BANG

(* Prims *)
%token NOT
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

(* Matrix primitives *)
%token SHARE_M
%token UNSHARE_M
%token FREE_M
%token MATRIX
%token COPY_M
%token COPY_M_TO
%token SIZE_M
%token SYMM
%token GEMM
%token POSV
%token POTRS

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
%start <Sugar.exp> prog

%type < Sugar.exp                                            > simple_exp exp
%type < Sugar.loc -> Sugar.exp -> Sugar.exp -> Sugar.exp     > fun_args
%type < Sugar.exp -> Sugar.exp -> Sugar.exp                  > op
%type < Sugar.exp * Sugar.exp option                         > index
%type < (Sugar.annot_arg, Sugar.loc * Sugar.var) either list > binds
%type < (Sugar.annot_arg, Sugar.loc * Sugar.var) either      > bind
%type < destruct                                             > destruct
%type < Sugar.annot_arg                                      > annot_arg
%type < Sugar.pat                                            > pat
%type < Sugar.bang_var                                       > bang_var
%type < Sugar.arg_like                                       > arg_like
%type < Sugar.prim                                           > prim
%type < Sugar.lin                                            > lin simple_lin
%type < Sugar.fc                                             > fc unit_fc simple_fc
%type < Sugar.var                                            > fc_var

%%

prog:
    | EOF         { raise Error }
    | exp=exp EOP { exp         }

exp:
    | simple_exp                                      { $1                                        }
    | MINUS i=INT                                     { Sugar.Int_I ($symbolstartpos, ~- i)       }
    | MINUS f=FLOAT                                   { Sugar.Elt_I ($symbolstartpos, ~-. f)      }
    | MANY exp=simple_exp                             { Sugar.Bang_I ($symbolstartpos, exp)       }
    | exp=simple_exp args=arg_like+                   { mk_app_like exp args                      }
    | IF cond=exp THEN t=exp ELSE f=exp               { Sugar.If ($symbolstartpos, cond, t, f)    }
    | FUN binds=binds ARROW body=exp                  { mk_lambda ($symbolstartpos) binds body    }
    | LET fun_args=fun_args EQUAL exp=exp IN body=exp { fun_args ($symbolstartpos) exp body       }
    | str=ID index=index                              { mk_index ($symbolstartpos) str index      }
    | str=ID index=index COLON_EQ exp=exp             { mk_assign ($symbolstartpos) str index exp }
    | fst=exp op=op snd=exp                           { op fst snd                                }

(* syntactic sugar *)
%inline op:
    (* boolean *)
    | DOUBLE_BAR     { mk_op $symbolstartpos Or       }
    | DOUBLE_AND     { mk_op $symbolstartpos And      }
    (* integer *)
    | EQUAL          { mk_op $symbolstartpos Eq       }
    | LESS_THAN      { mk_op $symbolstartpos Lt       }
    | PLUS           { mk_op $symbolstartpos Plus     }
    | MINUS          { mk_op $symbolstartpos Minus    }
    | STAR           { mk_op $symbolstartpos Times    }
    | FWD_SLASH      { mk_op $symbolstartpos Div      }
    (* element *)
    | EQUAL_DOT      { mk_op $symbolstartpos EqDot    }
    | LESS_THAN_DOT  { mk_op $symbolstartpos LtDot    }
    | PLUS_DOT       { mk_op $symbolstartpos PlusDot  }
    | MINUS_DOT      { mk_op $symbolstartpos MinusDot }
    | STAR_DOT       { mk_op $symbolstartpos TimesDot }
    | FWD_SLASH_DOT  { mk_op $symbolstartpos DivDot   }

index:
    | L_BRACKET index=exp               R_BRACKET { (index, None)     }
    | L_BRACKET index=exp COMMA snd=exp R_BRACKET { (index, Some snd) }

(* formal parameters *)
fun_args:
    | destruct=destruct                                                  { mk_let destruct          }
    | REC str=ID L_PAREN arg=annot_arg R_PAREN binds=bind* COLON lin=lin { mk_rec str arg binds lin }
    | str=bang_var binds=bind+ COLON lin=lin                             { mk_fun str binds lin     }

(* pattern-matching/destructing and binding *)
binds:
    | res=annot_arg { [First res]           }
    | str=fc_var    { [Second ($symbolstartpos, str)] }
    | bind+         { $1                    }

bind:
    | L_PAREN res=annot_arg R_PAREN { First res           }
    | L_PAREN str=fc_var R_PAREN    { Second ($symbolstartpos, str) }

annot_arg:
    | pat=pat COLON lin=lin { Sugar.({pat;lin}) }

destruct:
    | str=bang_var COLON lin=lin { First (str, lin) }
    | pat=pat                    { Second pat       }

pat:
    | bang_var                          { Sugar.Base ($symbolstartpos, $1 ) }
    | L_PAREN R_PAREN                   { Sugar.Unit $symbolstartpos        }
    | MANY pat=pat                      { Sugar.Many ($symbolstartpos, pat) }
    | L_PAREN a=pat COMMA b=pat R_PAREN { Sugar.Pair ($symbolstartpos, a,b) }

bang_var:
    | res=ID      { Sugar.NotB res }
    | BANG res=ID { Sugar.Bang res }

(* simple expressions don't need parentheses around them *)
arg_like:
    | fc=simple_fc   { Sugar.Fc ($symbolstartpos, fc)   }
    | UNDERSCORE     { Sugar.Underscore $symbolstartpos }
    | exp=simple_exp { Sugar.Exp exp                    }

simple_exp:
    | prim                                  { Sugar.Prim ($symbolstartpos, $1)         }
    | str=ID                                { Sugar.Var ($symbolstartpos, str)         }
    | L_PAREN R_PAREN                       { Sugar.Unit_I ($symbolstartpos)           }
    | TRUE                                  { Sugar.True ($symbolstartpos)             }
    | FALSE                                 { Sugar.False ($symbolstartpos)            }
    | i=INT                                 { Sugar.Int_I ($symbolstartpos, i)         }
    | f=FLOAT                               { Sugar.Elt_I ($symbolstartpos, f)         }
    | L_PAREN fst=exp COMMA snd=exp R_PAREN { Sugar.Pair_I ($symbolstartpos, fst, snd) }
    | L_PAREN body=exp R_PAREN              { body                                     }

prim:
    | NOT       { Sugar.Not_        }
    | SHARE     { Sugar.Share       }
    | UNSHARE   { Sugar.Unshare     }
    | FREE      { Sugar.Free        }
    | ARRAY     { Sugar.Array       }
    | COPY      { Sugar.Copy        }
    | SIN       { Sugar.Sin         }
    | HYPOT     { Sugar.Hypot       }
    | ASUM      { Sugar.Asum        }
    | AXPY      { Sugar.Axpy        }
    | DOTP      { Sugar.Dot         }
    | ROTMG     { Sugar.Rotmg       }
    | SCAL      { Sugar.Scal        }
    | AMAX      { Sugar.Amax        }
    | SHARE_M   { Sugar.Share_mat   }
    | UNSHARE_M { Sugar.Unshare_mat }
    | FREE_M    { Sugar.Free_mat    }
    | MATRIX    { Sugar.Matrix      }
    | COPY_M    { Sugar.Copy_mat    }
    | COPY_M_TO { Sugar.Copy_mat_to }
    | SIZE_M    { Sugar.Size_mat    }
    | SYMM      { Sugar.Symm        }
    | GEMM      { Sugar.Gemm        }
    | POSV      { Sugar.Posv        }
    | POTRS     { Sugar.Potrs       }

(* types *)
lin:
    | simple_lin                         { $1                    }
    | str=fc_var DOT lt=lin              { Sugar.All (str, lt)   } %prec NON_LOW
    | fst=simple_lin STAR snd=simple_lin { Sugar.Pair (fst, snd) }
    | arg=lin LOLLIPOP res=lin           { Sugar.Fun (arg, res)  }

simple_lin:
    | UNIT                   { Sugar.Unit    }
    | INT_LT                 { Sugar.Int     }
    | ELT_LT                 { Sugar.Elt     }
    | fc=fc ARR_LT           { Sugar.Arr fc  }
    | fc=fc MAT_LT           { Sugar.Mat fc  }
    | BANG lt=simple_lin     { Sugar.Bang lt }
    | L_PAREN lt=lin R_PAREN { lt        }

simple_fc:
    | L_PAREN fc=fc R_PAREN { fc }
    | fc=unit_fc            { fc }

fc:
    | unit_fc  { $1         }
    | fc=fc ES { Sugar.S fc }

unit_fc:
    | ZED         { Sugar.Z     }
    | str=fc_var  { Sugar.V str }

fc_var:
    | str=FC_VAR { Base.String.chop_prefix_exn ~prefix:"'" str}
