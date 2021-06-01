open Base

module T = struct
  type 'a t = (Error.t -> unit) -> ('a -> unit) -> unit

  let return x _on_err on_succ = on_succ x
  let map t ~f on_err on_succ = t on_err (fun v -> on_succ (f v))
  let map = `Custom map
  let bind t ~f on_err on_succ = t on_err (fun v -> (f v) on_err on_succ)
end

include T
include Monad.Make (T)

let run t on_err on_succ = t on_err on_succ
let make f on_err on_succ = f on_err on_succ
let fail err on_err _on_succ = on_err err
