open Base
;;

(* Lots of types *)
type mat_info = {
  name : string;
  dim : int -> int * int;
  make : scale:int -> Owl.Mat.mat;
  valid: Owl.Mat.mat -> bool;
}
;;

type kalman_input = {
  sigma : Owl.Mat.mat;
  h: Owl.Mat.mat;
  mu: Owl.Mat.mat;
  r : Owl.Mat.mat;
  data : Owl.Mat.mat;
}
;;

type 'a data = {
  name_or_size: 'a;
  mean_us: float;
  plus_err: float;
  minus_err: float;
  r_sq: float;
  sample: int;
}
[@@deriving sexp_of]
;;

type mat =
  Owl.Mat.mat
;;

type 'a lin_mat =
  'a Lt4la.Template.mat
;;

type z =
  Lt4la.Template.z
;;

type _ functions =
  | Chol
  | Owl
  | LT4LA : (sigma:mat -> h:mat -> mu:mat -> r:mat -> data:mat -> ('a lin_mat * ('b lin_mat * ('c lin_mat * (z lin_mat * z lin_mat)))) * (z lin_mat * z lin_mat)) functions
  | CBLAS
;;

(* Functions to benchmark *)
let lt4la = Test.lt4la_kalman
and owl = Test.owl_kalman
and chol = Test.chol_kalman
and cblas ~n ~k ~sigma ~h ~mu ~r ~data =
  let open Kalman_c_ffi in
  let f x = Bind.C.(bigarray_start Ctypes_static.Genarray x) in
  Bind.measure n k (f sigma) (f h) (f mu)
    (f @@ Owl.Mat.copy r) (f @@ Owl.Mat.copy data)
;;

(* Step 0: Platform and sanity checks. *)
module IO =
  Array_io.Make (struct let dir = "arrays" end)
;;

(* Step 1: Generate some data. Saved to disk for the sake of consistency. *)
let generate_exn files ~base ~start ~limit =
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
let micro_exn ~sec ~n ~k {sigma; h; mu; r; data} =
  let open Core_bench.Std.Bench in
  (* Trying to emulate options: -ci-absolute -quota 10 -clear-columns +time samples speedup *)
  let tests = [

    Test.create ~name:("Chol") (fun () -> chol ~sigma ~h ~mu ~r ~data);
    Test.create ~name:("Owl") (fun () -> owl ~sigma ~h ~mu ~r ~data);

    (* [r] and [data] are overrwritten *)
    Test.create ~name:("LT4LA") (
      let r = Owl.Mat.copy r and data = Owl.Mat.copy data in fun () ->
      lt4la ~sigma ~h ~mu ~r ~data);

    (* Not super valid because of marshalling overhead *)
    (* [r] and [data] are overrwritten *)
    Test.create ~name:("CBLAS") (
      let r = Owl.Mat.copy r and data = Owl.Mat.copy data in fun () ->
      cblas ~n ~k ~sigma ~h ~mu ~r ~data)

  ]
  in
  let run_config =
    Run_config.create ~time_quota:(Core.Time.Span.create ~sec ()) () in
  (* Ensures we have one (and only one) regression (Array.get _ 0) *)
  (* Ensures we have r_square AND a 95% CI (Option.value_exn)      *)
  let analysis_configs =
    Analysis_config.(List.map [nanos_vs_runs] ~f:(with_error_estimation)) in
  let analysis =
    measure ~run_config tests
    |> List.map ~f:(analyze ~analysis_configs)
    |> Or_error.combine_errors
    |> Or_error.ok_exn
  in
  let data =
    let f result =
      let open Core_bench.Analysis_result in
      let regr = Array.get (regressions result) 0 in
      let coeff = Array.get (Regression.coefficients regr) 0 in
      let ci95 = Option.value_exn (Coefficient.ci95 coeff) in
      let mean_ns = Coefficient.estimate coeff in
      let (minus_err, plus_err) = Ci95.ci95_abs_err ci95 ~estimate:mean_ns in
      { name_or_size = name result;
        mean_us = mean_ns /. 1000.;
        plus_err;
        minus_err;
        r_sq = Option.value_exn (Regression.r_square regr);
        sample = sample_count result;
      }
    in
    List.map analysis ~f
  in
  (n, data)
;;

(* Step 3: big matrices *)
let macro ~f ~runs { sigma; h; mu; r; data } =
  Array.init runs ~f:(fun _ ->
    let () = Caml.Gc.full_major () in
    let {Unix.tms_utime=start;_} = Unix.times () in
    let _  = f ~sigma ~h ~mu ~r ~data in
    let {Unix.tms_utime=end_;_} = Unix.times () in
    end_ -. start
  )
;;

let macro ~runs ~n ~k input =
  let stats times =
    let mean = Owl.Stats.mean times in
    let std = Owl.Stats.std ~mean times in
    mean, std
  in
  let chol = macro ~f:chol ~runs input in
  let owl = macro ~f:owl ~runs input in
  let input1 = { input with r = Owl.Mat.copy input.r; data = Owl.Mat.copy input.data } in
  let input2 = { input with r = Owl.Mat.copy input.r; data = Owl.Mat.copy input.data } in
  (* [r] and [data] are overrwritten *)
  let lt4la = macro ~f:lt4la ~runs input1 in
  (* Remember cblas times itself in C *)
  let { sigma; h; mu; r; data } = input2 in
  (* [r] and [data] are overrwritten *)
  let cblas = Array.init runs ~f:(fun _ ->
    cblas ~n ~k ~sigma ~h ~mu ~r ~data) in

  let f (name, (mean_us, std)) = {
    name_or_size = name;
    mean_us;
    plus_err = std;
    minus_err = ~-. std;
    r_sq = 1.;
    sample = runs;
  }
  in
  (n, List.map ~f [
    ("Chol", stats chol);
    ("Owl", stats owl);
    ("LT4LA", stats lt4la);
    ("CBLAS", stats cblas);
  ])
;;

let check_dims ~n ~k {sigma; h; mu; r; data} =
  let (=) = Caml.(=) and shape = Owl.Mat.shape in
  assert (( n, n ) = shape sigma);
  assert (( k, n ) = shape h);
  assert (( n, 1 ) = shape mu);
  assert (( k, k ) = shape r);
  assert (( k, 1 ) = shape data);
;;

(* Step 4: Select appropriate test and gather data. *)
let runtest_exn files ~macro_runs:runs ~micro_quota:sec  ~base:n' ~cols:k' ~exp:i =
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
    if i <= 3 (* micro-benchmark for small values only *) then
      micro_exn ~sec ~n ~k input
    else
      macro ~runs ~n ~k input
  | _ -> assert false
;;

(* Step 5: Process data *)
(* Want to transpose so it's by function *)

let files ~base:n' ~cols:k' =
  (* Mat.semidef doesn't produce exactly symmetric matrices for size 61 or greater ..?
     Thankfully, results not relevant to measurement, only consistency and computation. *)
  let pos_def_sym x = Owl.Linalg.D.(is_posdef x (*&& is_symmetric x*))
  and uniform = Owl.Mat.for_all (fun x -> Float.(0. <= x && x <= 1.))
  in
  [
    {
      name ="sigma";
      dim = (fun x -> n'*x , n'*x);
      make = (fun ~scale:x -> Owl.Mat.semidef (n'*x));
      valid = pos_def_sym;
    };

    {
      name = "h";
      dim = (fun x -> k'*x , n'*x);
      make = (fun ~scale:x -> Owl.Mat.uniform (k'*x) (n'*x));
      valid = uniform;
    };

    {
      name = "mu";
      dim = (fun x -> n'*x , 1);
      make = (fun ~scale:x -> Owl.Mat.uniform (n'*x) 1);
      valid = uniform;
    };

    {
      name = "r";
      dim = (fun x -> k'*x , k'*x);
      make = (fun ~scale:x -> Owl.Mat.semidef (k'*x));
      valid = pos_def_sym;
    };

    {
      name = "data";
      dim = (fun x -> k'*x , 1);
      make = (fun ~scale:x -> Owl.Mat.uniform (k'*x) 1);
      valid = uniform;
    };

  ]
;;

let () =
  let base, cols = 5, 3 in
  let micro_quota, macro_runs = 10, 10 in
  let files = files ~base ~cols in
  let () = generate_exn files ~base ~start:1 ~limit:3 in
  let (n, data) = runtest_exn files ~micro_quota ~macro_runs ~base ~cols ~exp:2 in
  Stdio.printf !"%d: %{sexp:string data list}\n" n data
;;
