open Base
;;

open Lt4la
;;

let print_position () lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    Printf.sprintf "%a: %s (lexing)" print_position lexbuf msg
    |> Or_error.error_string
  | Parser.Error ->
    Printf.sprintf "%a: syntax error (parsing)" print_position lexbuf
    |> Or_error.error_string
;;

let string_of_linear_t linear_t =
  let buffer = Buffer.create 80 in
  Ast.pp_linear_t (Caml.Format.formatter_of_buffer buffer) linear_t;
  Buffer.contents buffer
;;

let parse_and_print lexbuf =
  let open Stdio.Out_channel in
  match parse_with_error lexbuf with
  | Ok value ->
    output_string stdout (Ast.pp_frac_cap value);
  | Error err ->
    output_string stdout (Error.to_string_hum err);
;;

(*
 * let loop_ filename =
 *   let inx = Stdio.In_channel.create filename in
 *   let lexbuf = Lexing.from_channel inx in
 *   lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
 *   parse_and_print lexbuf;
 *   Stdio.In_channel.close inx
 * ;;
 *)

let loop = function
  | ":q" ->
    Caml.exit 0
  | str ->
    let putstr = Stdio.Out_channel.(output_string stdout) in
    let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "repl" };
    putstr "- ";
    parse_and_print lexbuf;
    putstr "\n> ";
    Stdio.Out_channel.(flush stdout)
;;

let () =
  Stdio.Out_channel.(output_string stdout "> "; flush stdout);
  Stdio.In_channel.(iter_lines stdin) ~f:loop
;;
