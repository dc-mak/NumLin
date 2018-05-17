let it =
  let open Ctypes in
  Foreign.foreign "measure_kalman" (void @-> returning double)
;;
