(* Dhruv Makwana *)
(* LT4LA State_or_error *)
(* -------------------- *)
(* This is my first time doing anything like this so please feel free to give me feedback on:
   - OCaml features I should be using, like documentation comments and attributes
   - Structuring the project
   - Implementation tips and tricks *)

(* Please read the .mli file for explanations. *)

open Core_kernel

module Make (State : sig type t end) =
struct

  type 'a t =
    State.t -> ('a * State.t) Or_error.t

  let get : 'a t =
    fun state -> Or_error.return (state, state)

  let put state : unit t =
    fun _ -> Or_error.return ((), state)

  let fail ?strict str value conv : 'a t =
    fun _ -> Or_error.error ?strict str value conv

  let fail_string str : 'a t =
    fun _ -> Or_error.error_string str

  let failf fmt : 'a =
    Printf.ksprintf fail_string fmt

  let run value state = 
    value state

  include Monad.Make
      (struct

        type 'a tmp =
          'a t

        type 'a t =
          'a tmp

        let bind t ~f state =
          let open Or_error.Let_syntax in
          let%bind (result, state) = t state in
          run (f result) state

        let map =
          `Define_using_bind

        let return t state =
          Or_error.return (t, state)

      end)

end

(* TODO Internal tests *)
let%test_module "Test" =
  (module struct
    let%test "test" = true
  end)
;;
