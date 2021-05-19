open H1_types

module Server_state = struct
  type t = Idle | Request_received of Request.t
end

module Client_state = struct
  type t = Idle | Done
end

type action = Need_data | Req of Request.t | Paused | Close

type t = {
  reader : Reader.t;
  writer : Writer.t;
  mutable state : Server_state.t;
  mutable peer_state : Client_state.t;
}

let create ~read_buf_size ~write_buf_size writev =
  let reader = Reader.create read_buf_size in
  let writer = Writer.create ~writev write_buf_size in
  { reader; writer; state = Idle; peer_state = Idle }

let feed_data ~f conn = Reader.fill ~f conn.reader

let reset t =
  t.state <- Idle;
  t.peer_state <- Idle

let write_response conn resp =
  let buf = Buffer.create 512 in
  Buffer.add_string buf (Version.to_string @@ Response.version resp);
  Buffer.add_char buf ' ';
  Buffer.add_string buf (Status.to_string @@ Response.status resp);
  Buffer.add_char buf ' ';
  Buffer.add_string buf @@ Response.reason_phrase resp;
  Buffer.add_string buf "\r\n";
  Headers.iteri
    ~f:(fun ~key ~data ->
      Buffer.add_string buf key;
      Buffer.add_string buf ": ";
      Buffer.add_string buf data;
      Buffer.add_string buf "\r\n")
    (Response.headers resp);
  Buffer.add_string buf "\r\n";
  Writer.write_string conn.writer (Buffer.contents buf)

let write conn msg =
  match msg with
  | `Response res -> write_response conn res
  | `Data d -> Writer.schedule_bigstring conn.writer d

let flushed conn = Writer.flushed conn.writer
let write_all conn = Writer.write_all conn.writer

let next_action conn =
  match conn.peer_state with
  | Client_state.Idle when Reader.is_empty conn.reader -> Need_data
  | Idle ->
      Reader.read
        ~f:(fun buf ~pos ~len ->
          match H1_parser.parse_request buf ~off:pos ~len with
          | Ok (req, count) ->
              conn.state <- Server_state.Request_received req;
              conn.peer_state <- Client_state.Done;
              (Req req, count)
          | Error Partial -> (Need_data, 0)
          | Error (Msg msg) -> failwith msg)
        conn.reader
  | Done -> Paused
