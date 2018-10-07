(* Dhruv Makwana *)
(* All LT4LA External Tests *)

let%test_module "Ast_test" =
  (module Ast_test)
;;

let%test_module "Check_monad" =
  (module Check_monad_test)
;;

let%test_module "Checker" =
  (module Checker_test)
;;

(* TODO                            *)
(* let%test_module "Combinators" = *)
(*   (module Combinators_test)     *)
(* ;;                              *)

let%test_module "Examples" =
  (module Examples_test)
;;

include Kalman_test
;;
