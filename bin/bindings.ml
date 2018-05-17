module Make(F : Cstubs.FOREIGN) =
struct
  let foreign = F.foreign

  module C = struct
    include Ctypes
    let (@->) = F.(@->)
    let returning = F.returning
  end

  let it = foreign "measure_kalman" C.(int @-> returning double)

end
