open H1_types

module Server_state = struct
  type t = Idle | Request_received of Request.t [@@deriving sexp]
end

module Client_state = struct
  type t = Idle | Done [@@deriving sexp]
end

type action = Need_data | Req of Request.t | Paused | Close
[@@deriving sexp_of]

type t = {
  reader : Reader.t;
  writer : Writer.t;
  mutable state : Server_state.t;
  mutable peer_state : Client_state.t;
}
[@@deriving sexp_of]

let create ~read_buf_size ~write_buf_size writev =
  let reader = Reader.create read_buf_size in
  let writer = Writer.create ~writev write_buf_size in
  { reader; writer; state = Idle; peer_state = Idle }

let feed_data ~f conn = Reader.fill ~f conn.reader

let reset t =
  t.state <- Idle;
  t.peer_state <- Idle

let write_response conn resp =
  Writer.write_string conn.writer (Version.to_string resp.Response.version);
  Writer.write_char conn.writer ' ';
  Writer.write_string conn.writer (Status.to_string resp.status);
  Writer.write_char conn.writer ' ';
  Writer.write_string conn.writer resp.reason_phrase;
  Writer.write_string conn.writer "\r\n";
  Headers.iteri
    ~f:(fun ~key ~data ->
      Writer.write_string conn.writer key;
      Writer.write_string conn.writer ": ";
      Writer.write_string conn.writer data;
      Writer.write_string conn.writer "\r\n")
    resp.headers;
  Writer.write_string conn.writer "\r\n"

let write conn msg =
  match msg with
  | `Response res -> write_response conn res
  | `Data d -> Writer.schedule_bigstring d conn.writer

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