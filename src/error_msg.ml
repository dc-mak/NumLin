
(* This file was auto-generated based on "src/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 102 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 97 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 122 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 32 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 37 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 34 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 35 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 0 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 15 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 114 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 117 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 116 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 16 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 20 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 21 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 22 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 23 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 24 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 25 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 53 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 54 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 55 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 52 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 56 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 66 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 111 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 112 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 67 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 58 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 59 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 60 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 61 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 63 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 64 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 44 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 45 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 65 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 48 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 49 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 27 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 50 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 30 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 31 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 41 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 42 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 43 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 47 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 108 ->
        "I saw 'if <exp> then <exp>' and was expecting 'else <exp>' after it\n"
    | 109 ->
        "I saw 'if <exp> then <exp> else' and was expecting '<exp>' after it\n"
    | 107 ->
        "I saw 'if <exp> then' and was expecting '<exp> else <exp>' after it\n"
    | 106 ->
        "I saw 'if <exp>' and was expecting 'then <exp> else <exp>' after it\n"
    | 69 ->
        "I saw 'if' and was expecting an expression after it\n"
    | 73 ->
        "I saw 'fun' and was expecting '<id> : <type> -> <exp>' after it\n"
    | 74 ->
        "I saw 'fun <id>' and was expecting a type annotation and then '->'\nExample: 'fun x : unit -> <exp>'\n"
    | 75 ->
        "I saw 'fun <id> : ' and was expecting a <type> next\n"
    | 76 ->
        "I saw 'fun <id> : <type>' so far and was expecting a '->'\n"
    | 77 ->
        "I saw 'fun <id> : <type> ->' and was expecting an expression after it.\nExamples:\n    f x\n    let <pat> = <exp> in <exp>\n    if <cond> then <exp> else <exp>\n"
    | 92 ->
        "I saw 'all' and was expecting an identifier after it.\nExample: all x . <exp>\n"
    | 93 ->
        "I saw 'all <id>' and was expecting a '.' after it.\n"
    | 94 ->
        "I saw 'all <id> .' and was expecting an expression after it.\nExamples:\n    f x\n    let <pat> = <exp> in <exp>\n    if <cond> then <exp> else <exp>\n"
    | _ ->
        raise Not_found
