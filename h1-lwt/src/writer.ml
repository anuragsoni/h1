open Sexplib0.Sexp_conv

type t = {
  buf : (Faraday.t[@sexp.opaque]);
  writev : Faraday.bigstring Faraday.iovec list -> int Lwt.t;
}
[@@deriving sexp_of]

let create ~writev size =
  let buf = Faraday.create size in
  { buf; writev }

let pending t = Faraday.pending_bytes t.buf

let schedule_bigstring t ?pos ?len buf =
  Faraday.schedule_bigstring t.buf ?off:pos ?len buf

(* TODO: Do bounds check and grow buffer to make more space if needed *)
let write_string t msg = Faraday.write_string t.buf msg
let write_char t msg = Faraday.write_char t.buf msg
let write_bigstring t msg = Faraday.write_bigstring t.buf msg
let drop t count = Faraday.shift t.buf count

let rec write_all t =
  let pending = pending t in
  if pending = 0 then Lwt.return_unit
  else
    match Faraday.operation t.buf with
    | `Yield -> Lwt.return_unit
    | `Close -> assert false
    | `Writev iovecs ->
        let%lwt count = t.writev iovecs in
        drop t count;
        if count = pending then Lwt.return_unit else write_all t

let flushed t =
  let promise, wakeup = Lwt.wait () in
  Faraday.flush t.buf (fun () -> Lwt.wakeup_later wakeup ());
  promise
