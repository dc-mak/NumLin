{
(* Dhruv Makwana *)
(* LT4LA Lexer *)
(* ----------- *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

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
    [ ("I", UNIT)
    ; ("int", INT_LT)
    ; ("f64", F64_LT)
    ; ("Arr", ARR_LT)
    ; ("all", ALL)
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
  (* TODO: make more informative/friendly *)
  | _        { raise (SyntaxError ("unexpected char: " ^ Lexing.lexeme lexbuf)) }
