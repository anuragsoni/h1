open H1_types

type t = {
  reader : Reader.t;
  writer : Writer.t;
  refill : Bigstringaf.t -> pos:int -> len:int -> int Lwt.t;
  write : Bigstringaf.t -> pos:int -> len:int -> int Lwt.t;
}

let create ~read_buf_size ~write_buf_size ~write ~refill =
  let reader = Reader.create read_buf_size in
  let writer = Writer.create write_buf_size in
  { reader; writer; write; refill }

let write_response conn resp =
  Writer.write_string conn.writer (Version.to_string @@ Response.version resp);
  Writer.write_char conn.writer ' ';
  Writer.write_string conn.writer (Status.to_string @@ Response.status resp);
  Writer.write_char conn.writer ' ';
  Writer.write_string conn.writer @@ Response.reason_phrase resp;
  Writer.write_string conn.writer "\r\n";
  Headers.iteri
    ~f:(fun ~key ~data ->
      Writer.write_string conn.writer key;
      Writer.write_string conn.writer ": ";
      Writer.write_string conn.writer data;
      Writer.write_string conn.writer "\r\n")
    (Response.headers resp);
  Writer.write_string conn.writer "\r\n"

let write conn msg =
  match msg with
  | `Response res -> write_response conn res
  | `Body b -> Body.write conn.writer b

(* let flushed conn = Writer.flushed conn.writer *)
let write_all conn = Writer.write_all ~write:conn.write conn.writer

type service = Request.t -> (Response.t * Body.t) Lwt.t

let run conn service =
  let stream = Http_stream.request_stream ~refill:conn.refill conn.reader in
  Lstream.iter
    ~f:(fun req ->
      let%lwt res, body = service req in
      write conn (`Response res);
      write conn (`Body body);
      write_all conn)
    stream
