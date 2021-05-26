module Flush = struct
  type t = { bytes_written : int; wake_promise : unit Lwt.u }
end

type t = {
  buf : Bigbuffer.t;
  mutable closed : bool;
  mutable bytes_scheduled : int;
  mutable bytes_written : int;
  flushes : Flush.t Queue.t;
}

let create size =
  let buf = Bigbuffer.create size in
  {
    buf;
    closed = false;
    flushes = Queue.create ();
    bytes_scheduled = 0;
    bytes_written = 0;
  }

let pending t = Bigbuffer.length t.buf

let flushed t =
  if pending t = 0 then Lwt.return_unit
  else
    let p, w = Lwt.wait () in
    let flush = { Flush.bytes_written = t.bytes_scheduled; wake_promise = w } in
    Queue.push flush t.flushes;
    p

let write_string t msg =
  let len = String.length msg in
  t.bytes_scheduled <- t.bytes_scheduled + len;
  Bigbuffer.add_string t.buf msg

let writef t fmt = Format.kasprintf (write_string t) fmt

let write_char t msg =
  t.bytes_scheduled <- t.bytes_scheduled + 1;
  Bigbuffer.add_char t.buf msg

let write_bigstring t msg =
  let len = Bigstringaf.length msg in
  t.bytes_scheduled <- t.bytes_scheduled + len;
  Bigbuffer.add_bigstring t.buf msg

let write_iovec t iovec =
  let len = iovec.Iovec.len in
  t.bytes_scheduled <- t.bytes_scheduled + len;
  Bigbuffer.add_iovec t.buf iovec

let wakeup_flush_if_needed t =
  while
    (not (Queue.is_empty t.flushes))
    && (Queue.peek t.flushes).Flush.bytes_written <= t.bytes_written
  do
    Lwt.wakeup_later (Queue.pop t.flushes).Flush.wake_promise ()
  done

let write_all ~write t =
  let rec aux t =
    let pending = pending t in
    if pending = 0 then Lwt.return_unit
    else
      let view = Bigbuffer.consume t.buf in
      let%lwt count =
        write view.Bigbuffer.View.buffer ~pos:view.pos ~len:view.len
      in
      view.continue count;
      t.bytes_written <- t.bytes_written + count;
      wakeup_flush_if_needed t;
      if count = pending then Lwt.return_unit else aux t
  in
  aux t
