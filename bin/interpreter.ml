(* Dhruv Makwana *)
(* Interpreter *)
(* ----------- *)

open Base
;;

type state =
  { n : int }
;;

let handler lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  let cpos = pos.pos_cnum - pos.pos_bol in
  let bytes = Bytes.create cpos in
  for i = 0 to cpos - 2 do
    Bytes.set bytes i ' ';
  done;
  Bytes.set bytes (cpos-1) '^';
  Bytes.to_string bytes
  |> Result.fail
;;

let accept v =
  match v with
  | Ok value ->
    Lt4la.Ast.(string_of_pp pp_expression value)
    |> String.split ~on:'\n'
    |> String.concat ~sep:"\n  "
    |> Result.return
  | Error err ->
    Error.to_string_hum err
    |> String.(split ~on:'\n')
    |> String.concat ~sep:"\n  "
    |> Result.fail
;;

let eval state str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "repl" };
  let out = Lt4la.(Parser_utils.(drive {handler;accept}) lexbuf
              (Parser.Incremental.prog lexbuf.lex_curr_p)) in
  ({ n = state.n + 1 }, out)
;;
