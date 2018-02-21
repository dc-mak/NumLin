open Base
;;

let transpile chan file =
  match String.chop_suffix file ~suffix:".lt" with
  | None -> ()
  | Some file ->
    let from_name = ("examples/" ^ file ^ ".lt") in
    Stdio.In_channel.with_file from_name ~f:(fun from ->
      Stdio.Out_channel.output_string chan @@ Printf.sprintf "module %s =\nstruct\n" @@ String.capitalize file;
      begin match Lt4la.Transpile.chans ~in_file:from_name from chan with
      | Ok () ->
        Stdio.Out_channel.output_string chan @@ "end\n\n";
      | Error str ->
        Stdio.prerr_endline str;
        Caml.exit(1)
      end)
;;

let () =
  Stdio.Out_channel.with_file "examples.ml" ~f:(fun chan ->
    Array.iter ~f:(transpile chan) @@ Caml.Sys.(readdir @@ getcwd () ^ "/examples"))
;;

