open Base
;;

module Mat =
  Owl.Mat
;;

type input = {
  x : Mat.mat;
  y : Mat.mat;
}
;;

module F =
  Examples.Lin_reg
;;

type wrap =
  F.wrap
;;

let make_microbench_tests ~n:_ ~k:_ {x; y} (F.W fun_) =
  let f = F.get fun_ in
  let name = F.(name @@ W fun_) in
  let open Core_bench.Bench in
  match fun_ with

  | F.Owl ->
    Test.create ~name (fun () -> f ~x ~y)

  | F.NumPy ->
    let f = snd f in
    Test.create ~name (fun () -> f ~x ~y)

  | F.NumLin ->
    let f = f.f in
    Test.create ~name (fun () -> f ~x ~y)
;;

let macro ~f ?(clean=(fun () -> ())) ~runs {x; y} =
  assert (runs >= 1);
  Array.init runs ~f:(fun _ ->
    let () = Caml.Gc.full_major () in
    let {Unix.tms_utime=start;_} = Unix.times () in
    let _  = f ~x ~y in
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
    let {x; y} = input in
    let f = fst f in
    Array.init runs ~f:(fun _ ->
      Core.Time.Span.of_us @@ f ~x ~y)

  | F.NumLin ->
    let f = f.f in
    macro ~f ~runs input

;;

let check_dims ~n ~k {x; y} =
  assert (n >= 1 && k >= 1);
  let (=) = Caml.(=) and shape = Mat.shape in
  assert (( n, k ) = shape x);
  assert (( n, 1 ) = shape y);
;;

(* Step 4: Select appropriate test and gather data. *)
let runtest_exn files ~macro_runs:runs ~micro_quota:sec ~base:n' ~cols:k' ~exp:i tests =
  let scale = Int.pow n' (i-1) in
  match Collect.read_in_exn ~scale files with
  | [x; y] ->
    let input = {x; y} in
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

  (* Memory leak! *)
  let make_x =
    let x_table = Hashtbl.create (module Int) in fun x ->
    Hashtbl.find_or_add x_table x ~default:(fun () -> Mat.uniform (n' * x) (k' * x)) in

  let valid = Mat.for_all (fun x -> Float.(0. <= x && x <= 1.)) in
  Collect.[

    {
      name ="x";
      dim = (fun x -> n'*x , k'*x);
      make = (fun ~scale:x -> make_x x);
      valid;
    };

    {
      name = "y";
      dim = (fun x -> n'*x , 1);
      make = (fun ~scale:x -> Mat.(make_x x *@ uniform ~a:1. Int.(k' * x) 1));
      valid = (fun _ -> true);
    };

  ]
;;
