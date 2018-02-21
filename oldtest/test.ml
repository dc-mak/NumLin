(* Dhruv Makwana *)
(* All Old External Tests *)

let%test_module "Ast_test" =
  (module Ast_test)
;;

let%test_module "Check_monad" =
  (module Check_monad_test)
;;

let%test_module "Checker" =
  (module Checker_test)
;;

let%test_module "Parser" =
  (module Parser_test)
;;

let%test_module "Combinators" =
  (module Combinators_test)
;;

let () =
  Ppx_inline_test_lib.Runtime.exit ()
;;
