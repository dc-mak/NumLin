open Owl
;;

open Base
;;

module Time =
  Core_kernel.Time
;;

module Command =
  Core.Command
;;

(* Lots of types *)
type mat_info = {
  name : string;
  dim : int -> int * int;
  make : scale:int -> Mat.mat;
  valid: Mat.mat -> bool;
}
;;

type kalman_input = {
  sigma : Mat.mat;
  h: Mat.mat;
  mu: Mat.mat;
  r : Mat.mat;
  data : Mat.mat;
}
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

module F =
  Functions
;;

(* Step 0: Platform and sanity checks. *)
module IO =
  Array_io.Make (struct let dir = "arrays" end)
;;

(* Step 1: Generate some data. Saved to disk for the sake of consistency. *)
let generate_exn files ~base ~start ~limit =
  assert (base >= 1 && start >= 1 && limit >= 1 && limit >= start);
  for i = start to limit do
    let scale = Int.pow base (i-1) in
    List.iter files ~f:(fun { name; dim; make; valid } ->
      let n, k = dim scale in
      let file = IO.filename name ~n ~k in
      if not @@ Caml.Sys.file_exists file then (
        let x = make ~scale in
        if not @@ valid x then failwith ("Matrix " ^ file ^ " not valid.");
        IO.output_exn file x;
      ))
  done
;;

(* Step 2: Small matrices *)
let get_micro ~n ~k { sigma; h; mu; r; data } (F.W fun_) =
  let f = F.get fun_ in
  let name = F.(name @@ W fun_) in
  let open Core_bench.Bench in
  match fun_ with
  | F.Chol ->
    Test.create ~name (fun () -> f ~sigma ~h ~mu ~r ~data)

  | F.Owl ->
    Test.create ~name (fun () -> f ~sigma ~h ~mu ~r ~data)

  | F.LT4LA ->
    (* [r] and [data] are overrwritten *)
    let r, data = Mat.copy r, Mat.copy data in
    Test.create ~name (fun () -> f.f ~sigma ~h ~mu ~r ~data)

  | F.TRANSP ->
    (* [r] and [data] are overrwritten *)
    let r, data = Mat.copy r, Mat.copy data in
    Test.create ~name (fun () -> f.f ~sigma ~h ~mu ~r ~data)

  | F.CBLAS ->
    (* Not super valid because of marshalling overhead *)
    (* [r] and [data] are overrwritten *)
    let r, data = Mat.copy r, Mat.copy data in
    Test.create ~name (fun () -> f ~n ~k ~sigma ~h ~mu ~r ~data)
;;

let micro_exn ~sec ~n ~k input tests =
  assert (n >= 1 && k >= 1 && sec >= 1);
  let open Core_bench.Bench in
  (* Trying to emulate options: -ci-absolute -quota 10 -clear-columns +time samples speedup *)
  let run_config =
    Run_config.create
      ~verbosity:(Core_bench.Verbosity.Quiet)
      ~time_quota:(Core_kernel.Time.Span.create ~sec ()) () in
  (* Ensures we have one (and only one) regression (Array.get _ 0) *)
  (* Ensures we have r_square AND a 95% CI (Option.value_exn)      *)
  let analysis_configs =
    Analysis_config.(List.map [nanos_vs_runs] ~f:(with_error_estimation)) in
  let data =
    tests
    |> List.map ~f:(get_micro ~n ~k input)
    |> measure ~run_config
    |> List.map ~f:(analyze ~analysis_configs)
    |> Or_error.combine_errors
    |> Or_error.ok_exn
    |> List.map ~f:(fun result ->
      let open Core_bench.Analysis_result in
      let regr = (regressions result).(0) in
      let coeff = (Regression.coefficients regr).(0) in
      let ci95 = Option.value_exn (Coefficient.ci95 coeff) in
      let mean_ns = Coefficient.estimate coeff in
      let (minus_err, plus_err) = Ci95.ci95_abs_err ci95 ~estimate:mean_ns in
      {
        ind_var = name result;
        mean = Time.Span.of_ns mean_ns;
        plus_err = Time.Span.of_ns plus_err;
        minus_err = Time.Span.of_ns minus_err;
        r_sq = Regression.r_square regr;
        sample = sample_count result;
      }
    )
  in
  (n, data)
;;

(* Step 3: big matrices *)
let macro ~f ~runs { sigma; h; mu; r; data } =
  assert (runs >= 1);
  Array.init runs ~f:(fun _ ->
    let () = Caml.Gc.full_major () in
    let {Unix.tms_utime=start;_} = Unix.times () in
    let _  = f ~sigma ~h ~mu ~r ~data in
    let {Unix.tms_utime=end_;_} = Unix.times () in
    Time.Span.(of_sec end_ - of_sec start)
  )
;;

let get_macro ~n ~k ~runs input (F.W fun_) =
  let f = F.get fun_ in
  match fun_ with
  | F.Chol ->
    macro ~f ~runs input

  | F.Owl ->
    macro ~f ~runs input

  | F.LT4LA ->
    (* [r] and [data] are overrwritten *)
    macro ~f:f.f ~runs
      { input with r = Mat.copy input.r; data = Mat.copy input.data }

  | F.TRANSP ->
    (* [r] and [data] are overrwritten *)
    macro ~f:f.f ~runs
      { input with r = Mat.copy input.r; data = Mat.copy input.data }

  | F.CBLAS ->
    (* Not super valid because of marshalling overhead *)
    (* [r] and [data] are overrwritten *)
    let { sigma; h; mu; r; data } =
      { input with r = Mat.copy input.r; data = Mat.copy input.data } in
    Array.init runs ~f:(fun _ ->
      Time.Span.of_us @@ f ~n ~k ~sigma ~h ~mu ~r ~data)
;;

let macro ~runs ~n ~k input tests =
  assert (runs >= 1 && n >= 1 && k >= 1);
  let f fun_ =
    let times = get_macro ~n ~k ~runs input fun_ in
    let mean, std =
      let times = Array.map times ~f:(Time.Span.to_us) in
      let mean = Stats.mean times in
      let std = Stats.std ~mean times in
      Time.Span.(of_us mean, of_us std)
    in
    {
      ind_var = F.name fun_;
      mean = mean;
      plus_err = std;
      minus_err = Time.Span.neg std;
      r_sq = None;
      sample = runs;
    }
  in
  (n, List.map ~f tests)
;;

let check_dims ~n ~k {sigma; h; mu; r; data} =
  assert (n >= 1 && k >= 1);
  let (=) = Caml.(=) and shape = Mat.shape in
  assert (( n, n ) = shape sigma);
  assert (( k, n ) = shape h);
  assert (( n, 1 ) = shape mu);
  assert (( k, k ) = shape r);
  assert (( k, 1 ) = shape data);
;;

(* Step 4: Select appropriate test and gather data. *)
let runtest_exn files ~macro_runs:runs ~micro_quota:sec ~base:n' ~cols:k' ~exp:i tests =
  assert (runs >= 1 && (Option.(is_none sec || value_exn sec >= 1)) && n' >= 1 && k' >= 1 && i >= 1);
  let scale = Int.pow n' (i-1) in
  match List.fold_right files ~init:[] ~f:(fun { name; dim; make=_; valid } args ->
    let n, k = dim scale in
    let file = IO.filename name ~n ~k in
    let y = IO.input_exn file ~n ~k in
    if valid y then
      y :: args
    else
      failwith ("File " ^ file ^ " failed validation")
  ) with
  | [sigma; h; mu; r; data] ->
    let input = { sigma; h; mu; r; data } in
    let n, k = scale * n', scale * k' in
    let () = check_dims ~n ~k input in
    begin match sec with
    | Some sec ->
      if i <= 3 (* micro-benchmark for small values only *) then
        micro_exn ~sec ~n ~k input tests
      else
        macro ~runs ~n ~k input tests
    | None ->
      macro ~runs ~n ~k input tests
    end
  | _ -> assert false
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

let files ~base:n' ~cols:k' =
  (* Mat.semidef doesn't produce exactly symmetric matrices for size 61 or greater ..?
     Thankfully, results not relevant to measurement, only consistency and computation. *)
  let pos_def_sym x = Linalg.D.(is_posdef x (*&& is_symmetric x*))
  and uniform = Mat.for_all (fun x -> Float.(0. <= x && x <= 1.))
  in
  [
    {
      name ="sigma";
      dim = (fun x -> n'*x , n'*x);
      make = (fun ~scale:x -> Mat.semidef (n'*x));
      valid = pos_def_sym;
    };

    {
      name = "h";
      dim = (fun x -> k'*x , n'*x);
      make = (fun ~scale:x -> Mat.uniform (k'*x) (n'*x));
      valid = uniform;
    };

    {
      name = "mu";
      dim = (fun x -> n'*x , 1);
      make = (fun ~scale:x -> Mat.uniform (n'*x) 1);
      valid = uniform;
    };

    {
      name = "r";
      dim = (fun x -> k'*x , k'*x);
      make = (fun ~scale:x -> Mat.semidef (k'*x));
      valid = pos_def_sym;
    };

    {
      name = "data";
      dim = (fun x -> k'*x , 1);
      make = (fun ~scale:x -> Mat.uniform (k'*x) 1);
      valid = uniform;
    };

  ]
;;

let run_with_params ?(analyse=true) ~start ~limit ~tests ~micro_quota ~macro_runs =
  let base, cols = 5, 3 in
  if base >= 1 && cols >= 1 && macro_runs >= 1 then
    let n = limit - start + 1 in
    let files = files ~base ~cols in
    let () = generate_exn files ~base ~start ~limit in
    let collected = List.init n ~f:(fun exp ->
      runtest_exn files ~micro_quota ~macro_runs ~base ~cols ~exp:(start+exp) tests)
    in
    if analyse then (
      List.iter collected ~f:by_size;
      List.iter (transpose collected) ~f:by_alg;
    )
;;

let run_with_params ~analyse ~start ~limit ~tests ~micro_quota ~macro_runs =
  let ok_if bool error = Result.ok_if_true bool ~error in
  match Result.combine_errors_unit @@ [
    ok_if (start >= 1) "Start must be at least 1";
    ok_if (limit >= 1) "Limit must be at least 1";
    ok_if (start <= limit) "Start must be less than or equal to limit";
    ok_if Option.(is_none micro_quota || value_exn micro_quota >= 1)
      "Micro-benchmark quota must be at least 1";
    ok_if (macro_runs >= 1) "Macro runs must be at least 1";
  ] with
  | Ok () ->
    run_with_params ~analyse ~start ~limit ~tests ~micro_quota ~macro_runs
  | Error err ->
    List.iter err ~f:(Stdio.eprintf "%s\n");
    Caml.exit 1
;;

let alg =
  Core.Command.Arg_type.create
    (function
      | "chol" -> [F.W Chol]
      | "owl" ->  [F.W Owl]
      | "lt4la" -> [F.W LT4LA]
      | "transp" -> [F.W TRANSP]
      | "cblas" -> [F.W CBLAS]
      | "all" -> F.all
      | "none" -> []
      | x ->
        Stdio.eprintf "'%s' not a supported implementation" x;
        Caml.exit 1)
;;

let command =
  let open Core in
  Command.basic
    ~summary:"Benchmark different implementations of a Kalman Filter"
    Command.Let_syntax.(
      let%map_open

        start =
        flag "--start" (required int)
          ~doc:"int Begin testing at this exponent"

      and limit =
        flag "--limit" (required int)
          ~doc:"int End testing at this exponent"

      and tests =
        flag "--alg" (required alg)
          ~doc:"alg Implementation to test\n(chol, owl, lt4la, cblas, transp, all, none)"

      and no_analyse =
        flag "--no-analyse" no_arg
          ~doc:" Don't analyse or print out data (for profiling)"

      and micro_quota =
        flag "--micro-quota" (optional int)
          ~doc:"int How many seconds to run micro-benchmarks (exp <= 3) for"

      and macro_runs =
        flag "--macro-runs" (required int)
          ~doc:"int How many seconds to run macro-benchmarks (exp >= 3) for"

      in
      fun () -> run_with_params ~analyse:(not no_analyse) ~start ~limit ~tests ~micro_quota ~macro_runs
    )
;;

let () =
  Core.Command.run command
;;
