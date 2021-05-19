type t = {
  buf : Bigstringaf.t;
  mutable pos : int;
  mutable max : int;
  mutable closed : bool;
}

let create size =
  let buf = Bigstringaf.create size in
  { buf; pos = 0; max = 0; closed = false }

let length t = t.max - t.pos
let is_empty t = length t = 0

let shift t =
  if t.pos > 0 then (
    let len = length t in
    if len > 0 then Bigstringaf.blit t.buf ~src_off:t.pos ~dst_off:0 ~len t.buf;
    t.pos <- 0;
    t.max <- len)

let fill ~f t =
  shift t;
  let%lwt count = f t.buf ~pos:t.max ~len:(Bigstringaf.length t.buf - t.max) in
  t.max <- t.max + count;
  Lwt.return (if count = 0 then `Eof else `Ok count)

let drop t n =
  (* TODO: Do bounds check here *)
  t.pos <- t.pos + n

let read ~f t =
  let res, count = f t.buf ~pos:t.pos ~len:(length t) in
  drop t count;
  res
