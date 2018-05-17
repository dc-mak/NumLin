module M(F : Cstubs.FOREIGN) =
struct
  let foreign = F.foreign

  module C = struct
    include Ctypes
    let (@->) = F.(@->)
    let returning = F.returning
  end

  let measure_kalman =
    foreign "measure_kalman" C.(void @-> returning double)
end
