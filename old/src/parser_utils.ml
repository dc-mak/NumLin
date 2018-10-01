(* Dhruv Makwana *)
(* Parser Utilities *)
(* ---------------- *)

type ('i, 'o) driver =
  { handler : Lexing.lexbuf -> 'o
  ; accept : 'i -> 'o
  }
;;

let rec drive driver lexbuf =
  let module Inc = Parser.MenhirInterpreter in
  function
  | Inc.InputNeeded _ as checkpoint ->
    let token = Lexer.read lexbuf in
    let startp, endp = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    let checkpoint = Inc.offer checkpoint (token, startp, endp) in
    drive driver lexbuf checkpoint

  | Inc.Shifting (_, _, _)
  | Inc.AboutToReduce (_, _) as checkpoint ->
    let checkpoint = Inc.resume checkpoint in
    drive driver lexbuf checkpoint

  | Inc.HandlingError _ ->
    driver.handler lexbuf

  | Inc.Accepted v ->
    driver.accept v

  | Inc.Rejected ->
    assert false
;;

