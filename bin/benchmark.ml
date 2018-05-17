open Base
;;

(* Assumptions for rest of script *)
if Caml.Sys.big_endian || not Caml.Sys.unix then
  begin
    Stdio.eprintf "Need little-endian Unix platform to run benchmark.\n";
    Caml.exit 1
  end
;;

let dir =
  "arrays"
;;

if not @@ Caml.Sys.(file_exists dir && is_directory dir) then
  Unix.mkdir dir 0o773
;;

(* Step 0: Constants and Helpers. *)
let start, limit =
  5, 4
;;

let n', k' =
  5, 3
;;

(** Assumes little-endian encoding for output and 64-bit floats. *)
let output_float_le otch fv =
  let bits = ref (Int64.bits_of_float fv) in
  for _ = 1 to 8 do
    let byte = Int64.to_int_exn @@ Int64.(land) !bits 0xffL in
    bits := Int64.shift_right_logical !bits 8;
    Stdio.Out_channel.output_byte otch byte
  done
;;

let output ?(append=true) ?(fail_if_exists=true) file (arr : Owl.Dense.Ndarray.D.arr) =
  Stdio.Out_channel.with_file
    ~binary:true ~append ~fail_if_exists
    file ~f:(fun file ->
    Owl.Dense.Ndarray.D.iter (output_float_le file) arr
  )
;;

let input file ~n ~k =
  let fail ~st_size ~total =
    failwith @@
    Printf.sprintf "%s is of size: %dB and not of %dB = n:%d * k:%d * 8"
      file st_size total n k in
  Stdio.In_channel.with_file file ~f:(fun file ->
    let file = Unix.descr_of_in_channel file in
    let {Unix.st_size; _} = Unix.fstat file in
    let () = let total = n * k * 8 in if not (st_size = total) then fail ~st_size ~total in
    let shared = false in (* changes in memory are not reflected to file *)
    Unix.map_file file Bigarray.float64 Bigarray.c_layout shared [| n; k; |])
;;

let filename file ~n ~k =
  Printf.sprintf "./%s/%s_%d_%d.float64_c_layout_le" dir file n k
;;

(* Step 1: Sanity check *)

let sanity, x =
  let sanity = filename "sanity" ~n:n' ~k:k'
  and x = Owl.Mat.uniform n' k' in
  output ~append:false ~fail_if_exists:false sanity x;
  sanity, x
;;

let y =
  input sanity ~n:n' ~k:k'
;;

let () =
  assert ( Owl.Mat.( x = y ) )
;;

(* Step 2, generate files if they don't exist. *)
let files =
  (* Mat.semidef doesn't produce exactly symmetric matrices for size 61 or greater ..?
     Thankfully, results not relevant to measurement, only consistency and computation. *)
  let pos_def_sym x = Owl.Linalg.D.(is_posdef x (*&& is_symmetric x*))
  and uniform = Owl.Mat.for_all (fun x -> Float.(0. <= x && x <= 1.))
  in [
    ("sigma" , (fun x -> n'*x , n'*x) , (fun ~scale:x -> Owl.Mat.semidef (n'*x))        , pos_def_sym);
    ("r"     , (fun x -> k'*x , k'*x) , (fun ~scale:x -> Owl.Mat.semidef (k'*x))        , pos_def_sym);
    ("h"     , (fun x -> k'*x , n'*x) , (fun ~scale:x -> Owl.Mat.uniform (k'*x) (n'*x)) , uniform);
    ("mu"    , (fun x -> n'*x , 1   ) , (fun ~scale:x -> Owl.Mat.uniform (n'*x) 1)      , uniform);
    ("data"  , (fun x -> k'*x , 1   ) , (fun ~scale:x -> Owl.Mat.uniform (k'*x) 1)      , uniform);
  ]
;;

for i = start to limit do
  let scale = Int.pow n' (i-1) in
  List.iter files ~f:(fun (file, dim, create, valid) ->
    let n, k = dim scale in
    let file = filename file ~n ~k in
    if not @@ Caml.Sys.file_exists file then (
      let x = create ~scale in
      if not @@ valid x then failwith ("Matrix " ^ file ^ " not valid.");
      output file x;
    ))
done
;;

(* Step 3, read in files, validate size/properties and measure. *)
let lt4la = Test.Kalman.it
and owl = Test.owl_kalman
and lazy_ = Test.lazy_kalman
and chol = Test.chol_kalman
;;

let micro ~run_inf ~sigma ~r ~h ~mu ~data =
  let open Core_bench.Std in
  Core.Command.run @@ Bench.make_command [

    Bench.Test.create ~name:("LT4LA " ^ run_inf) (fun () ->
      ignore Lt4la.Template.(lt4la (M sigma) (M h) (M mu) (M r) (M data)));

    Bench.Test.create ~name:("Chol " ^ run_inf) (fun () ->
      ignore (chol sigma h mu r data));

    Bench.Test.create ~name:("Owl " ^ run_inf) (fun () ->
      ignore (owl sigma h mu r data));

    (* Bench.Test.create ~name:("Lazy " ^ n) (fun () -> *)
    (*   ignore (lazy_ sigma h mu r data));             *)
  ]
;;

let macro ~run_inf ~sigma ~r ~h ~mu ~data =
  failwith "Not implemented"
[@@ ocaml.warning "-27"]
;;

for i = start to limit do
  let scale = Int.pow n' (i-1) in
  match List.fold_right files ~init:[] ~f:(fun (file, dim, _, valid) args ->
    let n, k = dim scale in
    let file = filename file ~n ~k in
    let y = input file ~n ~k in
    if valid y then
      y :: args
    else
      failwith ("File " ^ file ^ " failed validation")
  ) with
  | [sigma; r; h; mu; data] ->
    let run_inf = Int.to_string @@ scale * n' in
    if i <= 3 (* micro-benchmark for small values only *) then
      micro ~run_inf ~sigma ~r ~h ~mu ~data
    else
      ()

  | _ -> assert false
done
;;

(* Step 4, measure/run C. *)
let measure_kalman =
  Measure_kalman_c_ffi.Bind.it
;;
let () = Stdio.printf "Called the C function: %f\n" @@ measure_kalman ();;


(* Step 5, print out CSV for each *)
