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

  let result = foreign "result"
             C.(int @->          (* n *)
                int @->          (* k *)
                ptr double @->   (* sigma *)
                ptr double @->   (* h *)
                ptr double @->   (* mu *)
                ptr double @->   (* r *)
                ptr double @->   (* data *)
                returning (ptr double))

end
