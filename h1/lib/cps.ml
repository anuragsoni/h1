type 'a t = { run : (Error.t -> unit) -> ('a -> unit) -> unit }

let return x = { run = (fun _on_err on_succ -> on_succ x) }
let fail err = { run = (fun on_err _on_succ -> on_err err) }

let map t ~f =
  { run = (fun on_err on_succ -> t.run on_err (fun v -> on_succ (f v))) }

let bind t ~f =
  {
    run =
      (fun on_err on_succ -> t.run on_err (fun v -> (f v).run on_err on_succ));
  }

module Infix = struct
  let ( >>= ) t f = bind t ~f
  let ( >>| ) t f = map t ~f
end

let run t on_err on_succ = t.run on_err on_succ
let make run = { run }
