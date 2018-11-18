let naive ~x ~y =
  let open Owl.Mat in
  let ( * ) = dot in
  let x' = transpose x in
  inv (x' * x) * x' * y
;;

let owl ~x ~y =
  Owl.Regression.D.ols x y
;;

let lt4la ~x ~y =
  Gen.Lin_reg.it (M x) (M y)
;;

