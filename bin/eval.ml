(* Dhruv Makwana *)
(* Eval for REPL *)
(* ------------- *)

open Base
;;

type out =
  | Done of int * (string, string) Result.t
  | More of int * (Lexing.lexbuf -> out)
;;

let handler n lexbuf ~msg =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  let cpos = pos.pos_cnum - pos.pos_bol in
  let bytes = Bytes.create cpos in
  for i = 0 to cpos - 2 do
    Bytes.set bytes i ' ';
  done;
  Bytes.set bytes (cpos-1) '^';
  Bytes.to_string bytes ^ "\n" ^ String.chop_suffix_exn ~suffix:"\n" msg
  |> Result.fail
  |> fun x -> Done (n, x)
;;

let accept n value =
  let sexp = Sexp.to_string_hum (Numlin.Ast.sexp_of_exp value) in
  let pp = Numlin.Ast.(string_of_pp pp_exp value)
           |> String.split ~on:'\n'
           |> String.concat ~sep:"\n         " in
  let check =
    match Numlin.Checker.check_expr value ~counter:0 with
    | Ok lin -> Numlin.Ast.(string_of_pp pp_lin lin) ^ "\n"
    | Error err -> Error.to_string_hum err in
  pp ^ "\n" ^ sexp ^ "\n" ^ String.chop_suffix_exn ~suffix:"\n" check
  |> Result.return
  |> fun x -> Done (n, x)
;;

let resume n cont =
  More (n, cont)
;;

let eval n input =
  let lexbuf = Lexing.from_string input in
  Lexing.(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "repl" });
  Numlin.Parse.(drive lexbuf {
    handler = handler (n+1); accept  = accept (n+1); resume  = Some (resume n); })
;;
