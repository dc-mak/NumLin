module type Intf =
sig
  type wrap
  val files: base:int -> cols:int -> Collect.mat_info list
  val runtest_exn:
    Collect.mat_info list ->
    macro_runs:int ->
    micro_quota:int option ->
    base:int ->
    cols:int ->
    exp:int ->
    wrap list ->
    int * string Data.t list
end

module type With_algs =
sig
  include Intf
  val algs: wrap list
end
