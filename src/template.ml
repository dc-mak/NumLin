(* Dhruv Makwana *)
(* LT4LA Compiler Output Header *)
(* ---------------------------- *)
(* This file is a header/wrapper around Owl's CBLAS bindings. *)

(* begin template *)
open Owl
;;

module Arr =
  Bigarray.Array1
;;

type f64 =
  Bigarray.float64_elt
;;

type zero =
    Zero
;;

type 'a succ =
    Succ
;;

type 'a arr = 
  (float, f64, Bigarray.c_layout) Arr.t
;;

type arr0 =
  zero arr
;;

let (=) (x: int) (y: int) =
  x = y
;;

module Prim =
struct

  let array_intro n : arr0 =
    Arr.(create Float64 C_layout n)
  ;;

  let split_perm : 'a . 'a arr -> ('a succ) arr * ('a succ) arr  = 
    fun arr ->
      (arr, arr)
  ;;

  let merg_perm : 'a. ('a succ) arr * ('a succ) arr -> 'a arr =
    fun (arr1, arr2) ->
      (* Is this correct? *)
      let () = assert (arr1 == arr2) in
      arr1
  ;;

  (* Can we "actually" free it? *)
  let free : arr0 -> unit =
    fun _ ->
      ()
  ;;

  let copy : 'a. 'a arr  -> 'a arr * arr0 =
    fun read ->
      let n = Arr.dim read in 
      let copied = array_intro n in
      let () = Cblas.copy n read 1 copied 1 in
      (read, copied)
  ;;

  let same_dim_exn : 'a 'b. 'a arr -> 'b arr -> int =
    fun read_a read_b ->
      let n_a, n_b = Arr.(dim read_a, dim read_b) in
      let () = assert (n_a = n_b) in
      n_a
  ;;

  let swap : arr0 * arr0 -> arr0 * arr0  =
    fun (write_x, write_y) ->
      let n = same_dim_exn write_x write_y in
      let () = Cblas.swap n write_x 1 write_y 1 in
      (write_x, write_y)
  ;;

  let asum : 'a. 'a arr -> 'a arr * float =
    fun read ->
      let result = Cblas.asum (Arr.dim read) read 1 in
      (read , result)
  ;;

  let axpy : 'a. float -> 'a arr -> arr0 -> 'a arr * arr0 =
    fun scalar read write ->
      let n = same_dim_exn read write in
      let () = Cblas.axpy n scalar read 1 write 1 in
      (read, write)
  ;;

  let dot : 'a 'b. 'a arr -> 'b arr -> (('a arr * 'b arr) * float) = 
    fun fst snd ->
      let n = same_dim_exn fst snd in
      let result = Cblas.dot n fst 1 snd 1 in
      ((fst, snd), result)
  ;;

  let nrm2 : 'a. 'a arr -> 'a arr * float =
    fun read ->
      let result = Cblas.nrm2 (Arr.dim read) read 1 in
      (read, result)
  ;;

  (* This one makes me questions the order of c/s before x/y. *)
  let rot : float -> float -> arr0 -> arr0 -> (arr0 * arr0) =
    fun c s write_x write_y ->
      let n = same_dim_exn write_x write_y in
      let () = Cblas.rot n write_x 1 write_y 1 c s in
      (write_x, write_y)
  ;;

  let rotg : float -> float -> (float * float) * (float * float) =
    fun a b ->
      let (a,b,c,s) = Cblas.rotg a b in
      ((a,b),(c,s))
  ;;

  let rotm : 'param. arr0 -> arr0 -> 'param arr -> (arr0 * arr0) * 'param arr =
    fun write_a write_b param ->
      let n = same_dim_exn write_a write_b in
      let () = Cblas.rotm n write_a 1 write_b 1 param in
      ((write_a, write_b), param)
  ;;

  let rotmg : float * float -> float * float -> (float * float) * (float * arr0) =
    fun (d1, d2) (b1, b2) ->
      let (d1, d2, b1, p) = Cblas.rotmg Bigarray.Float64 d1 d2 b1 b2 in
      ((d1, d2), (b1, p))
  ;;

  let scal : float -> arr0 -> arr0 =
    fun scal write ->
      let () = Cblas.scal (Arr.dim write) scal write 1 in
      write
  ;;

  let amax : 'a. 'a arr -> int * 'a arr =
    fun read ->
      let result = Cblas.amax (Arr.dim read) read 1 in
      (result, read)
  ;;
      
  end
;;

(* end template *)
