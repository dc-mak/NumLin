(* Dhruv Makwana *)
(* Parser Utilities *)
(* ---------------- *)

type ('i, 'o) driver =
  { handler : Lexing.lexbuf -> msg:string -> 'o
  ; accept : 'i -> 'o
  ; resume : ((Lexing.lexbuf -> 'o) -> 'o) option
  }
;;

module Inc =
  Parser.Incremental
;;

(* TODO Catch Lexer.SyntaxError *)
let drive_no_resume accept handler lexbuf =
  let module Intp = Parser.MenhirInterpreter in
  let rec loop = function
  | Intp.InputNeeded _ as checkpoint ->
    let token = Lexer.read lexbuf in
    let startp, endp = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    let checkpoint = Intp.offer checkpoint (token, startp, endp) in
    loop checkpoint

  | Intp.Shifting (_, _, _)
  | Intp.AboutToReduce (_, _) as checkpoint ->
    let checkpoint = Intp.resume checkpoint in
    loop checkpoint

  | Intp.HandlingError env ->
    let state = Intp.current_state_number env in
    handler lexbuf ~msg:(
      try Error_msg.message state
      with Not_found ->
        Printf.sprintf "Error raised in state %d\n" state)

  | Intp.Accepted v ->
    begin match Sugar.ds_exp v with
    | Ok v -> accept v
    | Error msg -> handler lexbuf ~msg
    end

  | Intp.Rejected ->
    assert false

  in loop @@ Inc.prog lexbuf.lex_curr_p
;;

(* TODO Catch Lexer.SyntaxError *)
let drive accept handler resume lexbuf =
  let module Intp = Parser.MenhirInterpreter in
  let rec loop lexbuf = function
  | Intp.InputNeeded _ as checkpoint ->
    let token = Lexer.read lexbuf in
    if token = EOF then
      resume @@ fun lexbuf -> loop lexbuf checkpoint
    else
      let startp, endp = lexbuf.lex_start_p, lexbuf.lex_curr_p in
      let checkpoint = Intp.offer checkpoint (token, startp, endp) in
      loop lexbuf checkpoint

  | Intp.Shifting (_, _, _)
  | Intp.AboutToReduce (_, _) as checkpoint ->
    let checkpoint = Intp.resume checkpoint in
    loop lexbuf checkpoint

  | Intp.HandlingError env ->
    let state = Intp.current_state_number env in
    handler lexbuf ~msg:(
      try Error_msg.message state
      with Not_found ->
        Printf.sprintf "Error raised in state %d\n" state)

  | Intp.Accepted v ->
    begin match Sugar.ds_exp v with
    | Ok v -> accept v
    | Error msg -> handler lexbuf ~msg
    end

  | Intp.Rejected ->
    assert false

  in loop lexbuf @@ Inc.prog lexbuf.lex_curr_p

;;

let drive lexbuf driver =
  let {handler; accept; resume} = driver in
  match resume with
  | None -> drive_no_resume accept handler lexbuf
  | Some resume -> drive accept handler resume lexbuf
;;
