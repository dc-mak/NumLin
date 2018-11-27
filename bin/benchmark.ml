open Base
;;

let run_with_params ?(analyse=true) ~start ~limit ~tests ~micro_quota ~macro_runs =
  let base, cols = 5, 3 in
  if base >= 1 && cols >= 1 then
    let n = limit - start + 1 in
    let module F = (val tests : Utils.With_algs) in
    let files = F.files ~base ~cols in
    let () = Collect.generate_exn files ~base ~start ~limit in
    let collected = List.init n ~f:(fun exp ->
      F.runtest_exn files ~micro_quota ~macro_runs ~base ~cols ~exp:(start+exp) F.algs)
    in
    if analyse then (
      List.iter collected ~f:Data.by_size;
      List.iter (Data.transpose collected) ~f:Data.by_alg;
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
  let open Examples.Kalman in
  Core.Command.Arg_type.create @@
  let kalman algs =
    (module struct
      include Kalman_utils
      let algs = algs
    end : Utils.With_algs) in
  function
  | "none" -> kalman []
  | "owl" ->  kalman [W Owl]
  | "lt4la" -> kalman [W LT4LA]
  | "cblas" -> kalman [W CBLAS]
  | "numpy" -> kalman [W NumPy]
  | "kalman" -> kalman all
  | "l1_norm_min" ->
    (module struct
      include L1_norm_min_utils
      let algs = Examples.L1_norm_min.all
    end : Utils.With_algs)
  | "lin_reg" ->
    (module struct
      include Lin_reg_utils
      let algs = Examples.Lin_reg.all
    end : Utils.With_algs)
  | x ->
    Stdio.eprintf "'%s' not a supported implementation" x;
    Caml.exit 1
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
          ~doc:"alg Implementation to test\n(kalman, l1_norm_min, lin_reg, none)"

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
