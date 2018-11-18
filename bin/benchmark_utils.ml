open Base
;;

module Time =
  Core_kernel.Time
;;

type 'a data = {
  ind_var: 'a;
  mean: Time.Span.t;
  plus_err: Time.Span.t;
  minus_err: Time.Span.t;
  r_sq: float option;
  sample: int;
}
[@@deriving sexp_of]
;;

(* Step 5: Process data *)
let transpose data =
  data
  |> List.concat_map ~f:(fun (n, data) ->
    List.map data ~f:(fun ({ind_var; _} as data) ->
      (ind_var, {data with ind_var = n;})))
  |> Hashtbl.of_alist_multi (module String)
  |> Hashtbl.to_alist
  |> List.map ~f:(fun (x,y) -> (x, List.rev y))
;;

let pretty_print ~title ~ind_var (index, data) =
  let headers = [ind_var; "Mean (us)"; "Sample"; "Err+"; "Err-"; " R^2"] in
  let init = List.map headers ~f:String.length in
  let maxf x y = max x (String.length @@ Printf.sprintf "%.0f" y) in
  match
    List.fold data ~init
      ~f:(fun [iv; m; s; pe; me; 4]
           {ind_var; mean; sample; plus_err; minus_err; r_sq=_} -> [
          max iv (String.length ind_var);
          maxf m (Time.Span.to_us mean);
          max s (String.length @@ Int.to_string sample);
          maxf pe (Time.Span.to_us plus_err);
          maxf me (Time.Span.to_us minus_err);
          4; (* d.dd *)
        ]
         ) [@ocaml.warning "-8"] with
  | [iv; m; s; pe; me; 4] as widths ->

    (* Table and Column Names *)
    Stdio.printf "%s = %s\n\n" title index;
    List.iter2_exn widths headers ~f:(Stdio.printf "%*s ");
    Stdio.print_endline "";

    (* Column underlines *)
    List.iter widths ~f:(fun i ->
      for _ = 1 to i do Stdio.Out_channel.(output_char stdout '-') done;
      Stdio.Out_channel.(output_char stdout ' '));
    Stdio.print_endline "";

    (* Data *)
    List.iter data ~f:(fun {ind_var; mean; sample; plus_err; minus_err; r_sq} ->
      Stdio.printf !"%*s %*.0f %*d %*.0f %*.0f %4s\n"
        iv ind_var
        m (Time.Span.to_us mean)
        s sample
        pe (Time.Span.to_us plus_err)
        me (Time.Span.to_us minus_err)
        Option.(value (map ~f:(Printf.sprintf "%0.2f") r_sq) ~default:"N/A");
    );

    (* End with # *)
    Stdio.print_endline "";
    List.iter widths ~f:(fun i ->
      for _ = 1 to i+1 do Stdio.Out_channel.(output_char stdout '#') done);
    Stdio.print_endline "";
    Stdio.print_endline "";

  | _ -> assert false
;;

let by_size (n, data) =
  pretty_print ~title:"Size N" ~ind_var:"Alg" (Int.to_string n, data)
;;

let by_alg (n, data) =
  pretty_print ~title:"Alg" ~ind_var:"Size N"
    (n, List.map data ~f:(fun x -> { x with ind_var = Int.to_string x.ind_var}))
;;
