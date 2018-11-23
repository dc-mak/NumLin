Py.initialize ~version:3 ()
;;

let f ~source ~filename ?(dont_inherit = false)
      ?(optimize = `Default) mode =
  let compile =
    Py.Module.get_function_with_keywords (Py.Module.builtins ()) "compile" in
  let source = Py.String.of_string source in
  let filename = Py.String.of_string filename in
  let mode =
    Py.String.of_string @@ match mode with
    | `Exec -> "exec"
    | `Eval -> "eval"
    | `Single -> "single" in
  let optimize =
    Py.Int.of_int @@ match optimize with
    | `Default -> -1
    | `Debug -> 0
    | `Normal -> 1
    | `RemoveDocstrings -> 2 in
  let dont_inherit = Py.Bool.of_bool dont_inherit in
  compile [| source; filename; mode |]
    ["dont_inherit", dont_inherit; "optimize", optimize]
;;
