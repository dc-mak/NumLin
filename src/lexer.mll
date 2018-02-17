{
(* Dhruv Makwana *)
(* LT4LA Lexer *)
(* ----------- *)
(* TODO: Unicode *)

open Lexing
;;

open Parser
;;

exception SyntaxError of string
;;

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1;
    }
;;

let keywords =
  let open Base in
  let keywords =
    (* simple linear types *)
    [ ("I", UNIT)
    ; ("int", INT_LT)
    ; ("f64", F64_LT)
    ; ("Arr", ARR_LT)
    (* linear types *)
    ; ("all", ALL)
    (* primitives *)
    ; ("split_perm", SPLIT_PERM)
    ; ("merge_perm", MERGE_PERM)
    ; ("free", FREE)
    ; ("copy", COPY)
    ; ("swap", SWAP)
    ; ("asum", ASUM)
    ; ("axpy", AXPY)
    ; ("dot", DOT_PROD)
    ; ("nrm2", NRM2)
    ; ("rot", ROT)
    ; ("rotg", ROTG)
    ; ("rotm", ROTM)
    ; ("rotmg", ROTMG)
    ; ("scal", SCAL)
    ; ("iamax", IAMAX)
    (* expressions *)
    ; ("let", LET )
    ; ("Array", ARRAY )
    ] in
  let table = Hashtbl.of_alist_exn (module String) keywords in
  fun str -> match Hashtbl.find table str with
  | Some token -> token
  | None -> ID str
;;

}

let digit = ['0'-'9']

let nat = digit digit*
let int = '-'? nat

let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | eof      { EOF }
  (* fractional capabilities *)
  | nat      { NAT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'      { PLUS }
  | id       { keywords (Lexing.lexeme lexbuf) }
  (* simple linear types *)
  | '['      { LEFT_BRACKET }
  | ']'      { RIGHT_BRACKET }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  (* linear types *)
  | '*'      { STAR }
  | "--o"    { LOLLIPOP }
  | '.'      { DOT }
  (* simple expressions *)
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | ','      { COMMA }
  | ':'      { COLON }
  | '\\'     { BACKSLASH }
  (* expressions *)
  | '='      { EQUAL }
  | ';'      { SEMICOLON }
  (* TODO: make more informative/friendly *)
  | _        { raise (SyntaxError ("unexpected char: " ^ Lexing.lexeme lexbuf)) }
