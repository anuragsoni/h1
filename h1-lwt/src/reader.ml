type t = { buf : Bigbuffer.t; mutable closed : bool }

let create size =
  let buf = Bigbuffer.create size in
  { buf; closed = false }

let length t = Bigbuffer.length t.buf
let is_empty t = length t = 0

let fill ~f t =
  let%lwt count = Bigbuffer.fill ~f t.buf in
  Lwt.return (if count = 0 then `Eof else `Ok count)

let read ~f t = Bigbuffer.consume ~f t.buf
