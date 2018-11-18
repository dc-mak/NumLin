module type Intf =
sig
  type mat_info
  type wrap
  val tests: wrap list
  val files: base:int -> cols:int -> mat_info list
  val generate_exn: mat_info list -> base:int -> start:int -> limit:int -> unit
  val runtest_exn:
    mat_info list ->
    macro_runs:int ->
    micro_quota:int option ->
    base:int ->
    cols:int ->
    exp:int ->
    wrap list ->
    int * string Benchmark_utils.data list
end
