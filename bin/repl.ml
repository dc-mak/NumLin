(* Dhruv Makwana *)
(* REPL *)
(* ---- *)
(* Copyright : (c) 2015, Martin DeMello <mdemello@google.com>
   Licence   : BSD3
   This file was adapted from examples/repl.ml of Lambda-Term. *)

open React
;;

open Lwt
;;

open LTerm_style
;;

open LTerm_text
;;

(* Prompt based on current interpreter state *)
let make_prompt state =
  let prompt = Printf.sprintf "In  [%d]: " state in
  eval [B_fg cyan; S prompt; E_fg]
;;

(* Format the interpreter output for REPL display *)
let make_output state out =
  let pre = Printf.sprintf "Out [%d]: " (state - 1) in
  let out, col = match out with
    | Ok str -> str, green
    | Error str -> str, red in
  eval [B_fg col; S pre; E_fg; S out]
;;

(* Customization of the read-line engine *)
class read_line ~term ~history ~state =
  object(self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = false
    initializer self#set_prompt (S.const (make_prompt state))
  end
;;

let get_line term history state =

  try%lwt
    let history = LTerm_history.contents history in
    let rl = new read_line ~term ~history ~state in
    rl#run >|= Base.Option.return
  with
  | Sys.Break -> return None
  | exn -> Lwt.fail exn

;;

(* Main loop *)
let rec loop term history ?cont state =

  match%lwt get_line term history state with

  | Some command ->
    step term history command @@
    let command = Zed_string.to_utf8 command in
    begin match cont with
    | Some cont -> cont (Lexing.from_string command)
    | None -> Eval.eval state command
    end

  | None ->
    loop term history state

and step term history command = function

  | Eval.Done (state, out) ->
    let%lwt () = LTerm.fprintls term (make_output state out) in
    LTerm_history.add history command;
    loop term history state

  | Eval.More (state, cont) ->
    LTerm_history.add history command;
    loop term history ~cont state
;;

(* Entry point *)
let main () =
  let%lwt () =  LTerm_inputrc.load () in
  try%lwt
    let%lwt () = LTerm.printls (eval [S "NumLin REPL"]) in
    let%lwt term = Lazy.force LTerm.stdout in
    loop term (LTerm_history.create []) 0
  with
  | LTerm_read_line.Interrupt -> Lwt.return ()
  | exn -> Lwt.fail exn
;;

let () =
  Lwt_main.run (main ())
;;

