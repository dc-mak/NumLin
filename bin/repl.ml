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

module I =
  Interpreter
;;

(* Prompt based on current interpreter state *)
let make_prompt state =
  let prompt = Printf.sprintf "In  [%d]: " state.I.n in
  eval [B_fg cyan; S prompt; E_fg]
;;

(* Format the interpreter output for REPL display *)
let make_output state out =
  let pre = Printf.sprintf "Out [%d]: " (state.I.n - 1) in
  let out, col = match out with 
    | Ok str -> str, green
    | Error str -> str, red in
  eval [B_fg col; S pre; E_fg; S out]
;;

(* Customization of the read-line engine *)
class read_line ~term ~history ~state =
  object(self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_utf8.t] LTerm_read_line.term term
    method! show_box = false
    initializer self#set_prompt (S.const (make_prompt state))
  end

(* Main loop *)
let rec loop term history state =
  Lwt.catch

    (fun () ->
       let history = LTerm_history.contents history in
       let rl = new read_line ~term ~history ~state in
       rl#run >|= fun command -> Some command)

    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)

  >>= function

  | Some command ->
    let state, out = I.eval state command in
    LTerm.fprintls term (make_output state out) >>= fun () ->
    LTerm_history.add history command;
    loop term history state

  | None ->
    loop term history state
;;

(* Entry point *)
let main () =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch

    (fun () ->
       let state = { I.n = 1 } in
       LTerm.printls (eval [S "LT4LA REPL"]) >>= fun () ->
       Lazy.force LTerm.stdout >>= fun term ->
       loop term (LTerm_history.create []) state)

    (function
      | LTerm_read_line.Interrupt -> Lwt.return ()
      | exn -> Lwt.fail exn)
;;

let () =
  Lwt_main.run (main ())
;;

