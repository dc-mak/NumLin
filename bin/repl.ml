(* Dhruv Makwana *)
(* LT4LA REPL *)
(* ---------- *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

(* TODO: split out useful functions for testing into a Parser_utils module in src. *)

open Base
;;

open Lt4la
;;

let string_of_exp exp =
  let buffer = Buffer.create 80 in
  Ast.pp_expression (Caml.Format.formatter_of_buffer buffer) exp;
  Buffer.contents buffer
;;

type colors =
  | B_Red
  | B_Green
  | B_Cyan
  | B_Yellow
;;

let color col str = 
  let code =
    let col = match col with
      | B_Red -> "1;31"
      | B_Green -> "1;32"
      | B_Cyan -> "1;36"
      | B_Yellow -> "1;33" in
    "\027[" ^ col ^ "m" in
  let reset = "\027[0m" in
  code ^ str ^ reset
;;

module Inc =
  Parser.MenhirInterpreter
;;

let rec loop lexbuf = function
  | Inc.InputNeeded _ as checkpoint ->
    let token = Lexer.read lexbuf in
    let startp, endp = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    let checkpoint = Inc.offer checkpoint (token, startp, endp) in
    loop lexbuf checkpoint

  | Inc.Shifting (_, _, _)
  | Inc.AboutToReduce (_, _) as checkpoint ->
    let checkpoint = Inc.resume checkpoint in
    loop lexbuf checkpoint

  | Inc.HandlingError env ->
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    let cpos = pos.pos_cnum - pos.pos_bol in
    let bytes = Bytes.create cpos in
    for i = 0 to cpos - 2 do
      Bytes.set bytes i ' ';
    done;
    Bytes.set bytes (cpos-1) '^';
    Bytes.to_string bytes;
    (* Printf.sprintf "%s:%d:%d" pos.pos_fname *)
    (* pos.pos_lnum (cpos)                     *)
    |> color B_Red
    |> Stdio.Out_channel.(output_string stdout)

  | Inc.Accepted v ->
    let open Stdio.Out_channel in
    begin match v with
    | Ok value ->
      string_of_exp value
      |> String.split ~on:'\n'
      |> String.concat ~sep:"\n  "
      |> color B_Green
      |> output_string stdout;
    | Error err ->
      Error.to_string_hum err
      |> String.split ~on:'\n'
      |> String.concat ~sep:"\n  "
      |> color B_Red
      |> output_string stdout;
    end

  | Inc.Rejected ->
    Stdio.Out_channel.(output_string stdout "Uh-oh")
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
    Stdio.Out_channel.(output_string stdout) (color B_Yellow "- Bye!\n");
    Caml.exit 0
  | str ->
    let putstr = Stdio.Out_channel.(output_string stdout) in
    let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "repl" };
    putstr (color B_Yellow "- ");
    loop lexbuf (Parser.Incremental.prog lexbuf.lex_curr_p);
    putstr (color B_Cyan ("\n> "));
    Stdio.Out_channel.(flush stdout)
;;

let () =
  let header = "LT4LA REPL (type ':q<Enter>' to quit)\n" in
  let banner = String.(make (length header - 1) '-' ^ "\n> ") in
  let msg = color B_Cyan (header ^ banner) in
  Stdio.Out_channel.(output_string stdout msg; flush stdout);
  Stdio.In_channel.(iter_lines stdin) ~f:loop
;;
