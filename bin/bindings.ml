module Make(F : Cstubs.FOREIGN) =
struct
  let foreign = F.foreign

  module C = struct
    include Ctypes
    let (@->) = F.(@->)
    let returning = F.returning
  end

  let measure = foreign "measure_kalman"
             C.(int @->          (* n *)
                int @->          (* k *)
                ptr double @->   (* sigma *)
                ptr double @->   (* h *)
                ptr double @->   (* mu *)
                ptr double @->   (* r *)
                ptr double @->   (* data *)
                returning double)

  let test = foreign "test" C.(int @-> returning double)

  let result : [`result] C.structure C.typ = C.structure "result"
  let new_sigma = C.(field result "new_sigma" (ptr double))
  let new_mu = C.(field result "new_mu" (ptr double))
  let () = C.seal result

  let results = foreign "results"
             C.(int @->          (* n *)
                int @->          (* k *)
                ptr double @->   (* sigma *)
                ptr double @->   (* h *)
                ptr double @->   (* mu *)
                ptr double @->   (* r *)
                ptr double @->   (* data *)
                returning result)

end
