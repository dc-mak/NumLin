(** Transpile across channels *)
val chans : in_file:string -> in_channel -> out_channel -> (unit, string) result

(** Transpile files (convenience function) *)
val files : in_file:string -> out_file:string -> (unit, string) result
