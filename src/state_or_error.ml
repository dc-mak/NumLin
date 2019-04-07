(* Dhruv Makwana *)
(* LT4LA State_or_error *)
(* -------------------- *)

open Base

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

        type nonrec 'a t =
          'a t

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

