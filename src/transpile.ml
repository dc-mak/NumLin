open Base
;;

module Out =
  Stdio.Out_channel
;;

let handler lexbuf ~msg =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d\n%s"
    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg
  |> Result.fail
;;

let accept chan value =
  match Checker.check_expr value ~counter:0 with

  | Ok (_ : Ast.lin)  ->
    Out.output_lines chan [
      "open Numlin.Template"; ";;"; "";
      "open Ops"; "[@@ocaml.warning \"-33\"]"; ";;"; ""
    ];
    Caml.Format.(fprintf @@ formatter_of_out_channel chan)

(*
 | Automagically printing out correct OCaml type is a bit nuanced
 | ( 'x. 'x arr --o !int * !!int --o 'x arr * !int )
 | * ( !!int --o  'y. 'x. !int --o !int )
 *)

      "@[<2>let it =@;@[%a@]@]" (Ast.pp_exp ~comments:false) value;
    Out.output_lines chan [""; ";;"; ""];
    Result.return ()

  | Error err ->
    let string_of_exp = Ast.(string_of_pp pp_exp) in
    Printf.sprintf !"Pretty-print:\n%{string_of_exp}\n\
                     Error:\n%{Error.to_string_hum}" value err
    |> Result.fail
;;

let chans ~in_file from to_ =
    let lexbuf = Lexing.from_channel from in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = in_file };
    Parse.(drive lexbuf { handler; accept = accept to_; resume = None; });
;;

let files ~in_file ~out_file =
  Stdio.(In_channel.with_file in_file ~f:(fun from ->
    Out_channel.with_file out_file ~f:(fun to_ ->
      chans ~in_file from to_)))
;;

