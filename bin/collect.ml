open Owl
;;

open Base
;;

module Time =
  Core_kernel.Time
;;

type mat_info = {
  name : string;
  dim : int -> int * int;
  make : scale:int -> Mat.mat;
  valid: Mat.mat -> bool;
}
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

let read_in_exn ~scale files =
  List.fold_right files ~init:[] ~f:(fun { name; dim; make=_; valid } args ->
    let n, k = dim scale in
    let file = IO.filename name ~n ~k in
    let y = IO.input_exn file ~n ~k in
    if valid y then
      y :: args
    else
      failwith ("File " ^ file ^ " failed validation")
  )
;;

let micro_exn ~sec ~n ~k get_micro input tests =
  assert (n >= 1 && sec >= 1);
  let open Core_bench.Bench in
  (* Trying to emulate options: -ci-absolute -quota 10 -clear-columns +time samples speedup *)
  let run_config =
    Run_config.create
      ~verbosity:(Core_bench.Verbosity.Quiet)
      ~time_quota:(Time.Span.create ~sec ()) () in
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
      Data.{
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

let macro ~runs ~n ~k name get_macro input tests =
  assert (runs >= 1 && n >= 1 && k >= 1);
  let f fun_ =
    let times = get_macro ~n ~k ~runs input fun_ in
    let mean, std =
      let times = Array.map times ~f:(Time.Span.to_us) in
      let mean = Stats.mean times in
      let std = Stats.std ~mean times in
      Time.Span.(of_us mean, of_us std)
    in
    Data.{
      ind_var = name fun_;
      mean = mean;
      plus_err = std;
      minus_err = Time.Span.neg std;
      r_sq = None;
      sample = runs;
    }
  in
  (n, List.map ~f tests)
;;

