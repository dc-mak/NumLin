type ('i, 'o) driver =
  { handler : Lexing.lexbuf -> msg:string -> 'o
  ; accept : 'i -> 'o
  ; resume : ((Lexing.lexbuf -> 'o) -> 'o) option
  }

val drive : Lexing.lexbuf -> (Ast.exp, 'a) driver -> 'a
