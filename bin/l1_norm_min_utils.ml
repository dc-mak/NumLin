open Base
;;

module Mat =
  Owl.Mat
;;

type input = {
  q : Mat.mat;
  u : Mat.mat;
}
;;

module F =
  Examples.L1_norm_min
;;

type wrap =
  F.wrap
;;

let make_microbench_tests ~n:_ ~k:_ {q; u} (F.W fun_) =
  let f = F.get fun_ in
  let name = F.(name @@ W fun_) in
  let open Core_bench.Bench in
  match fun_ with

  | F.Owl ->
    (* For consistency with others *)
    Test.create ~name (fun () ->
      let q, u = Mat.(copy q, copy u) in f ~q ~u)

  | F.NumPy ->
    let f = snd f in
    Test.create ~name (fun () ->
      let q, u = Mat.(copy q, copy u) in f ~q ~u)

  | F.LT4LA ->
    (* [q] and [u] are overrwritten *)
    (* Adds overhead because of copying during test but oh well *)
    Test.create ~name (fun () ->
      let q, u = Mat.(copy q, copy u) in f ~q ~u)
;;

let macro ~f ?(clean=(fun () -> ())) ~runs {q; u} =
  assert (runs >= 1);
  Array.init runs ~f:(fun _ ->
    let () = Caml.Gc.full_major () in
    let {Unix.tms_utime=start;_} = Unix.times () in
    let _  = f ~q ~u in
    let {Unix.tms_utime=end_;_} = Unix.times () in
    let () = clean () in
    Core_kernel.Time.Span.(of_sec end_ - of_sec start)
  )
;;

let make_macro_timing_array ~n:_ ~k:_ ~runs input (F.W fun_) =
  let f = F.get fun_ in
  match fun_ with

  | F.Owl ->
    macro ~f ~runs input

  | F.NumPy ->
    let {q; u} = input in
    let f = fst f in
    Array.init runs ~f:(fun _ ->
      Core.Time.Span.of_us @@ f ~q ~u)

  | F.LT4LA ->
    let {q; u} = input in
    let q' = Mat.copy q and u' = Mat.copy u in
    let clean () =
      Mat.copy_ q' ~out:q;
      Mat.copy_ u' ~out:u; in
    macro input ~runs ~f ~clean
;;

let check_dims ~n ~k {q; u} =
  assert (n >= 1 && k >= 1);
  let (=) = Caml.(=) and shape = Mat.shape in
  assert (( n, n ) = shape q);
  assert (( n, k ) = shape u);
;;

(* Step 4: Select appropriate test and gather data. *)
let runtest_exn files ~macro_runs:runs ~micro_quota:sec ~base:n' ~cols:k' ~exp:i tests =
  let scale = Int.pow n' (i-1) in
  match Collect.read_in_exn ~scale files with
  | [q; u] ->
    let input = {q; u} in
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

let files ~base:n' ~cols:k' =
  let uniform = Mat.for_all (fun x -> Float.(0. <= x && x <= 1.)) in
  Collect.[

    {
      name ="q";
      dim = (fun x -> n'*x , n'*x);
      make = (fun ~scale:x -> Mat.uniform (n' *x) (n' * x));
      valid = uniform;
    };

    {
      name = "u";
      dim = (fun x -> n'*x , k'*x);
      make = (fun ~scale:x -> Mat.uniform (n'*x) (k'*x));
      valid = uniform;
    };

  ]
;;
