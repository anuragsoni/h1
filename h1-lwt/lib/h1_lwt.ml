module Bigstring = Base_bigstring
open H1_types
module Logger = (val Logs.src_log (Logs.Src.create "H1.lwt"))

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Read = struct
  type t = {
    buf : Bigstring.t;
    mutable pos : int;
    mutable pos_unconsumed : int;
    decoder : H1.Decoder.decoder;
    refill : bigstring -> pos:int -> len:int -> int Lwt.t;
  }

  let create size refill =
    let buf = Bigstring.create size in
    {
      buf;
      pos = 0;
      pos_unconsumed = 0;
      decoder = H1.Decoder.decoder ();
      refill;
    }

  let shift t =
    if t.pos > 0 then (
      let len = t.pos_unconsumed - t.pos in
      if len > 0 then
        Bigstring.blit ~src:t.buf ~src_pos:t.pos ~dst_pos:0 ~len ~dst:t.buf;
      t.pos <- 0;
      t.pos_unconsumed <- len)

  let fill t =
    shift t;
    let%lwt count =
      t.refill t.buf ~pos:t.pos_unconsumed
        ~len:(Bigstring.length t.buf - t.pos_unconsumed)
    in
    if count = 0 then Lwt.return `Eof
    else (
      t.pos_unconsumed <- t.pos_unconsumed + count;
      Lwt.return `Ok)

  let next_event t =
    match H1.Decoder.decode t.decoder with
    | `Need_data as res ->
        let consumed = H1.Decoder.consumed t.decoder in
        t.pos <- t.pos + consumed;
        res
    | res -> res
end

module Write = struct
  type t = {
    buf : Bytebuffer.t;
    write : bigstring -> pos:int -> len:int -> unit Lwt.t;
  }

  let create size write =
    let buf = Bytebuffer.create size in
    { buf; write }

  let write_all_pending t =
    let consume = Bytebuffer.consume t.buf in
    let%lwt () =
      t.write consume.Bytebuffer.View.buffer ~pos:consume.pos ~len:consume.len
    in
    consume.continue consume.len;
    Lwt.return_unit
end

type conn = { read : Read.t; write : Write.t }

let create ~read ~read_buffer_size ~write ~write_buffer_size =
  let read = Read.create read_buffer_size read in
  let write = Write.create write_buffer_size write in
  { read; write }

let write_body t msg =
  match msg with
  | `Bigstring b -> Bytebuffer.add_bigstring t.write.Write.buf b
  | `String s -> Bytebuffer.add_string t.write.Write.buf s

type body_stream = unit -> string option Lwt.t

let make_body_stream t =
  let rec fn () =
    match H1.Decoder.decode t.read.decoder with
    | `Need_data -> (
        match%lwt Read.fill t.read with
        | `Eof -> Lwt.fail_with "EOF while reading body"
        | `Ok ->
            H1.Decoder.src t.read.decoder t.read.buf ~pos:t.read.pos
              ~len:(t.read.pos_unconsumed - t.read.pos);
            fn ())
    | `Data s -> Lwt.return (Some s)
    | `Error msg -> failwith msg
    | `Request _ -> failwith "Unexpected payload while parsing body"
    | `Request_complete -> Lwt.return None
  in
  fn

let rec iter_body t ~f =
  match%lwt t () with
  | None -> Lwt.return_unit
  | Some x ->
      let%lwt () = f x in
      iter_body t ~f

let rec iter_body' t ~f =
  match%lwt t () with
  | None -> Lwt.return_unit
  | Some x ->
      f x;
      iter_body' t ~f

let rec drain fn =
  match%lwt fn () with None -> Lwt.return_unit | Some _ -> drain fn

type service =
  Request.t * body_stream ->
  (Response.t * [ `Bigstring of bigstring | `String of string ]) Lwt.t

let run t service =
  let close, wakeup_close = Lwt.wait () in
  let rec aux () =
    match Read.next_event t.read with
    | `Need_data -> (
        match%lwt Read.fill t.read with
        | `Eof ->
            Lwt.wakeup_later wakeup_close ();
            Lwt.return_unit
        | `Ok ->
            H1.Decoder.src t.read.decoder t.read.buf ~pos:t.read.pos
              ~len:(t.read.pos_unconsumed - t.read.pos);
            aux ())
    | `Error msg -> failwith msg
    | `Request_complete ->
        H1.Decoder.next_cycle t.read.decoder;
        aux ()
    | `Request req ->
        let body_stream = make_body_stream t in
        let%lwt response, body = service (req, body_stream) in
        H1.serialize_response t.write.Write.buf response;
        write_body t body;
        let%lwt () = Write.write_all_pending t.write in
        let%lwt () = drain body_stream in
        H1.Decoder.next_cycle t.read.decoder;
        aux ()
    | `Data _ -> assert false
  in
  Lwt.dont_wait
    (fun () -> aux ())
    (fun exn ->
      Logger.err (fun m -> m "%s" (Printexc.to_string exn));
      Lwt.wakeup_later wakeup_close ());
  close
