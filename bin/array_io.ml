module Make (M : sig val dir : string end)
  : sig
    val filename : string -> n:int -> k:int -> string
    val input_exn: string -> n:int -> k:int -> Owl.Arr.arr
    val output_exn : string -> Owl.Arr.arr -> unit
  end =
struct

  if Caml.Sys.big_endian || not Caml.Sys.unix then
    begin
      Stdio.eprintf "Need little-endian Unix platform to run benchmark.\n";
      Caml.exit 1
    end
  ;;

  if not @@ Caml.Sys.(file_exists M.dir && is_directory M.dir) then
    Unix.mkdir M.dir 0o773
  ;;

  let output_float_le otch fv =
    let bits = ref (Int64.bits_of_float fv) in
    for _ = 1 to 8 do
      let byte = Int64.to_int @@ Int64.logand !bits 0xffL in
      bits := Int64.shift_right_logical !bits 8;
      Stdio.Out_channel.output_byte otch byte
    done
  ;;

  let output_exn file arr =
    Stdio.Out_channel.with_file file
      ~binary:true
      ~append:true
      ~fail_if_exists:true
      ~f:(fun file -> Owl.Arr.iter (output_float_le file) arr)
  ;;

  let input_exn file ~n ~k =
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
    Printf.sprintf "./%s/%s_%d_%d.float64_c_layout_le" M.dir file n k
  ;;

  let () =
    let n, k = 5, 3 in
    let sanity = filename "sanity" ~n ~k
    and x = Owl.Mat.uniform n k in
    let () = Stdio.Out_channel.with_file sanity ~binary:true ~f:(fun file ->
      Owl.Arr.iter (output_float_le file) x) in
    let y = input_exn sanity ~n ~k in
    assert ( Owl.Mat.( x = y ) )
  ;;

end
