open Base
;;

let speclist set_in set_out  =
  let open Caml.Arg in
  [ ("-i", Set_string set_in, "input file")
  ; ("-o", Set_string set_out, "output file")
  ]
;;

let () =
  let set_in, set_out = ref "", ref "" in
  Caml.Arg.parse (speclist set_in set_out) (Fn.const ()) "transpile -i <input> -o <output>";
  match Lt4la.Transpile.files ~in_file:!set_in ~out_file:!set_out with
  | Ok () -> ()
  | Error str -> Stdio.Out_channel.(output_string stderr) str; Caml.exit(1);
;;
