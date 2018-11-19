open Base
;;

module Mat =
  Owl.Mat
;;

module Time =
  Core_kernel.Time
;;

type input = {
  sigma : Mat.mat;
  h: Mat.mat;
  mu: Mat.mat;
  r : Mat.mat;
  data : Mat.mat;
}
;;

module F =
  Examples.Kalman
;;

type wrap =
  F.wrap
;;

let make_microbench_tests ~n ~k { sigma; h; mu; r; data } (F.W fun_) =
  let f = F.get fun_ in
  let name = F.(name @@ W fun_) in
  let open Core_bench.Bench in
  match fun_ with

  | F.NumPy ->
    (* Not super valid because of marshalling overhead *)
    (* For consistency with others *)
    let f = snd f in
    Test.create ~name (fun () ->
      let mu, r, data = Mat.copy mu, Mat.copy r, Mat.copy data in
      f ~sigma ~h ~mu ~r ~data)

  | F.Owl ->
    (* For consistency with others *)
    Test.create ~name (fun () ->
      let mu, r, data = Mat.copy mu, Mat.copy r, Mat.copy data in
      f ~sigma ~h ~mu ~r ~data)

  | F.LT4LA ->
    (* [mu], [r] and [data] are overrwritten *)
    (* Adds overhead because of copying during test but oh well *)
    Test.create ~name (fun () ->
      let mu, r, data = Mat.copy mu, Mat.copy r, Mat.copy data in
      f.f ~sigma ~h ~mu ~r ~data)

  | F.CBLAS ->
    (* Not super valid because of marshalling overhead *)
    (* [mu], [r] and [data] are overrwritten *)
    (* Adds overhead because of copying during test but oh well *)
    let f = snd f in
    Test.create ~name (fun () ->
      let mu, r, data = Mat.copy mu, Mat.copy r, Mat.copy data in
      f ~n ~k ~sigma ~h ~mu ~r ~data)
;;

let macro ~f ?(clean=(fun () -> ())) ~runs { sigma; h; mu; r; data } =
  assert (runs >= 1);
  Array.init runs ~f:(fun _ ->
    let () = Caml.Gc.full_major () in
    let {Unix.tms_utime=start;_} = Unix.times () in
    let _  = f ~sigma ~h ~mu ~r ~data in
    let {Unix.tms_utime=end_;_} = Unix.times () in
    let () = clean () in
    Time.Span.(of_sec end_ - of_sec start)
  )
;;

let make_macro_timing_array ~n ~k ~runs input (F.W fun_) =
  let f = F.get fun_ in
  match fun_ with

  | F.NumPy ->
    let { sigma; h; mu; r; data } = input in
    let f = fst f in
    Array.init runs ~f:(fun _ ->
      Time.Span.of_us @@ f ~sigma ~h ~mu ~r ~data)

  | F.Owl ->
    macro ~f ~runs input

  | F.LT4LA ->
    let { sigma=_; h=_; mu; r; data } = input in
    let mu' = Mat.copy mu
    and r' = Mat.copy r
    and data' = Mat.copy data in
    (* [mu], [r] and [data] are overrwritten *)
    let clean () =
      Mat.copy_ mu' ~out:mu;
      Mat.copy_ r' ~out:r;
      Mat.copy_ data' ~out:data; in
    macro input ~runs ~f:f.f ~clean

  | F.CBLAS ->
    (* Not super valid because of marshalling overhead *)
    let { sigma; h; mu; r; data } = input in
    let mu' = Mat.copy mu
    and r' = Mat.copy r
    and data' = Mat.copy data in
    (* [mu], [r] and [data] are overrwritten *)
    let clean () =
      Mat.copy_ mu' ~out:mu;
      Mat.copy_ r' ~out:r;
      Mat.copy_ data' ~out:data; in
    let f = fst f in
    Array.init runs ~f:(fun _ ->
      let t = Time.Span.of_us @@ f ~n ~k ~sigma ~h ~mu ~r ~data in
      clean (); t)
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
  let scale = Int.pow n' (i-1) in
  match Collect.read_in_exn ~scale files with
  | [sigma; h; mu; r; data] ->
    let input = { sigma; h; mu; r; data } in
    let n, k = scale * n', scale * k' in
    let () = check_dims ~n ~k input in
    begin match sec with
    | Some sec ->
      if i <= 3 (* micro-benchmark for small values only *) then
        Collect.micro_exn ~sec ~n ~k make_microbench_tests input tests
      else
        Collect.macro ~runs ~n ~k F.name make_macro_timing_array input tests
    | None ->
      Collect.macro ~runs ~n ~k F.name make_macro_timing_array input tests
    end
  | _ -> assert false
;;

(* Mat.semidef isn't guaranteed to be perfectly symmetric
   Probably remove once cause of posv_flip errors has been identified *)
let semidef n =
  let tmp = Mat.semidef n in
  for i = 0 to n-1 do
    for j = 0 to i-1 do
      Mat.set tmp i j (Mat.get tmp j i)
    done
  done;
  tmp
;;

let files ~base:n' ~cols:k' =
  (* Mat.semidef doesn't produce exactly symmetric matrices for size 61 or greater ..?
     Thankfully, results not relevant to measurement, only consistency and computation. *)
  let pos_def_sym x = Owl.Linalg.D.(is_posdef x && is_symmetric x)
  and uniform = Mat.for_all (fun x -> Float.(0. <= x && x <= 1.))
  in
  Collect.[
    {
      name ="sigma";
      dim = (fun x -> n'*x , n'*x);
      make = (fun ~scale:x -> semidef (n' * x));
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
      make = (fun ~scale:x -> semidef (k'*x));
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
