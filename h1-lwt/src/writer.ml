open Sexplib0.Sexp_conv

module Flush = struct
  type t = { bytes_written : int; wake_promise : (unit Lwt.u[@sexp.opaque]) }
  [@@deriving sexp_of]
end

type t = {
  buf : (Bigstringaf.t[@sexp.opaque]);
  mutable pos : int;
  mutable max : int;
  mutable closed : bool;
  mutable bytes_scheduled : int;
  mutable bytes_written : int;
  flushes : (Flush.t Queue.t[@sexp.opaque]);
  iovecs : (Iovec.t Lwt_dllist.t[@sexp.opaque]);
  writev : Iovec.t Lwt_dllist.t -> int Lwt.t;
}
[@@deriving sexp_of]

let create ~writev size =
  let buf = Bigstringaf.create size in
  {
    buf;
    pos = 0;
    max = 0;
    closed = false;
    flushes = Queue.create ();
    iovecs = Lwt_dllist.create ();
    writev;
    bytes_scheduled = 0;
    bytes_written = 0;
  }

let length t = t.max - t.pos
let pending t = Lwt_dllist.fold_l (fun x acc -> acc + x.Iovec.len) t.iovecs 0

let flushed t =
  if pending t = 0 then Lwt.return_unit
  else
    let p, w = Lwt.wait () in
    let flush = { Flush.bytes_written = t.bytes_scheduled; wake_promise = w } in
    Queue.push flush t.flushes;
    p

let schedule_iovec t iovec =
  t.bytes_scheduled <- t.bytes_scheduled + iovec.Iovec.len;
  ignore (Lwt_dllist.add_r iovec t.iovecs)

let schedule_bigstring ?pos ?len buf t =
  schedule_iovec t (Iovec.of_bigstring ?pos ?len buf)

(* TODO: Do bounds check and grow buffer to make more space if needed *)
let write_string t msg =
  let len = String.length msg in
  let pos = t.max in
  Bigstringaf.blit_from_string msg ~src_off:0 ~dst_off:t.max ~len t.buf;
  t.max <- t.max + len;
  schedule_iovec t (Iovec.of_bigstring t.buf ~pos ~len)

let write_char t msg =
  let pos = t.max in
  Bigstringaf.set t.buf t.max msg;
  t.max <- t.max + 1;
  schedule_iovec t (Iovec.of_bigstring t.buf ~pos ~len:1)

let write_bigstring t msg =
  let len = Bigstringaf.length msg in
  let pos = t.max in
  Bigstringaf.blit msg ~src_off:0 ~dst_off:t.max ~len t.buf;
  t.max <- t.max + len;
  schedule_iovec t (Iovec.of_bigstring t.buf ~pos ~len)

let wakeup_flush_if_needed t =
  while
    (not (Queue.is_empty t.flushes))
    && (Queue.peek t.flushes).Flush.bytes_written <= t.bytes_written
  do
    Lwt.wakeup_later (Queue.pop t.flushes).Flush.wake_promise ()
  done

let drop t count =
  let rec aux t count =
    if count = 0 then ();
    if Lwt_dllist.is_empty t.iovecs then
      invalid_arg
        "H1_lwt.Writer.drop: Trying to remove more bytes than we have in the \
         writer";
    let peek = Lwt_dllist.take_l t.iovecs in
    if peek.Iovec.len >= count then
      ignore
        (Lwt_dllist.add_l
           (Iovec.of_bigstring peek.Iovec.buf ~pos:(peek.pos + count)
              ~len:(peek.len - count))
           t.iovecs)
    else aux t (count - peek.len)
  in
  aux t count;
  t.bytes_written <- t.bytes_written + count;
  wakeup_flush_if_needed t

let write_all t =
  let rec aux t =
    let pending = pending t in
    if pending = 0 then Lwt.return_unit
    else
      let%lwt count = t.writev t.iovecs in
      drop t count;
      if count = pending then Lwt.return_unit else aux t
  in
  let%lwt () = aux t in
  t.max <- 0;
  Lwt.return_unit
