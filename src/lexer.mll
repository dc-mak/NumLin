{
(* Dhruv Makwana *)
(* LT4LA Lexer *)
(* ----------- *)
(* TODO: Remove exception
 *       Unicode *)

open Lexing
;;

open Parser
;;

exception SyntaxError of position * string
;;

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with
      pos_bol = pos.pos_cnum;
      pos_lnum = pos.pos_lnum + 1;
    }
;;

let keywords =
  let open Base in
  let keywords =
    (* fractional capabilities *)
    [ ("z", ZED)
    ; ("s", ES)
    (* simple linear types *)
    ; ("unit", UNIT)
    ; ("int", INT_LT)
    ; ("elt", ELT_LT)
    ; ("arr", ARR_LT)
    ; ("mat", MAT_LT)
    (* primitives *)
    ; ("not", NOT)
    ; ("share", SHARE)
    ; ("unshare", UNSHARE)
    ; ("free", FREE)
    ; ("array", ARRAY)
    ; ("copy", COPY)
    ; ("sin", SIN)
    ; ("hypot", HYPOT)
    ; ("asum", ASUM)
    ; ("axpy", AXPY)
    ; ("dot", DOTP)
    ; ("rotmg", ROTMG)
    ; ("scal", SCAL)
    ; ("amax", AMAX)
    (* matrix primitives *)
    ; ("shareM", SHARE_M)
    ; ("unshareM", UNSHARE_M)
    ; ("freeM", FREE_M)
    ; ("matrix", MATRIX)
    ; ("eye", EYE)
    ; ("copyM", COPY_M)
    ; ("copyM_to", COPY_M_TO)
    ; ("sizeM", SIZE_M)
    ; ("transpose", TRANSPOSE)
    (* Level 2/3 BLAS *)
    ; ("symm", SYMM)
    ; ("gemm", GEMM)
    ; ("gesv", GESV)
    ; ("posv", POSV)
    ; ("posvFlip", POSV_FLIP)
    ; ("potrs", POTRS)
    ; ("syrk", SYRK)
    (* expressions *)
    ; ("true", TRUE)
    ; ("false", FALSE)
    ; ("if", IF)
    ; ("then", THEN)
    ; ("else", ELSE)
    ; ("let", LET)
    ; ("rec", REC)
    ; ("in", IN)
    ; ("fun", FUN)
    ; ("Many", MANY)
    (* inference *)
    ; ("_", UNDERSCORE)
    (* matrix expressions *)
    ; ("new", NEW)
    ] in
  let table = Hashtbl.of_alist_exn (module String) keywords in
  fun str -> match Hashtbl.find table str with
  | Some token -> token
  | None -> ID str
;;

}

let digit = ['0'-'9']
let int = digit+

let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let fc_var = ''' id

rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | eof     { EOF }
  | ";;"    { EOP }
  (* fractional capabilities *)
  | fc_var  { FC_VAR (Lexing.lexeme lexbuf) }
  | id      { keywords (Lexing.lexeme lexbuf) }
  (* simple linear types *)
  | '('     { L_PAREN }
  | ')'     { R_PAREN }
  (* linear types *)
  | '!'     { BANG }
  | '*'     { STAR }
  | "--o"   { LOLLIPOP }
  | '.'     { DOT }
  (* simple expressions *)
  | int     { INT (Base.Int.of_string (Lexing.lexeme lexbuf)) }
  | float   { FLOAT (Base.Float.of_string (Lexing.lexeme lexbuf)) }
  | ','     { COMMA }
  | ':'     { COLON }
  (* expressions *)
  | '='     { EQUAL }
  | "->"    { R_ARROW }
  (* sugar, arrays *)
  | '['     { L_BRACKET }
  | ']'     { R_BRACKET }
  | ":="    { COLON_EQ }
  | "||"    { DOUBLE_BAR }
  | "&&"    { DOUBLE_AND }
  (* integer arithmetic *)
  | '<'     { LESS_THAN }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '/'     { FWD_SLASH }
  (* element arithmetic *)
  | "=."    { EQUAL_DOT }
  | "<."    { LESS_THAN_DOT }
  | "+."    { PLUS_DOT }
  | "-."    { MINUS_DOT }
  | "*."    { STAR_DOT }
  | "/."    { FWD_SLASH_DOT }
  (* comments *)
  | "(*"    { comment 1 lexbuf }
  (* matrix expressions *)
  | "<-"    { L_ARROW }
  | "[|"    { L_SEMBRACK }
  | "|]"    { R_SEMBRACK }
  | '^'     { CARET }
  (* TODO: make more informative/friendly *)
  | _       { raise (SyntaxError (lexbuf.lex_curr_p, "unexpected char: " ^ Lexing.lexeme lexbuf)) }

(* [n] handles nested comments *)
and comment n = parse
  | "(*"    { comment (n+1) lexbuf}
  | "*)"    { if n=1 then read lexbuf else comment (n-1) lexbuf }
  | newline { next_line lexbuf; comment n lexbuf }
  | _       { comment n lexbuf }
