type 'a t = ('a -> unit) -> unit

let return x on_succ = on_succ x
let map t ~f on_succ = t (fun v -> on_succ (f v))
let bind t ~f on_succ = t (fun v -> (f v) on_succ)

module Syntax = struct
  let ( let* ) t f = bind t ~f
  let ( let+ ) t f = map t ~f
end

let run t on_succ = t on_succ
let make f on_succ = f on_succ
