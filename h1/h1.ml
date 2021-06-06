open H1_types

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Bufview = struct
  type t = {
    buffer : bigstring;
    mutable pos : int;
    min_pos : int;
    upper_bound : int;
  }

  let of_bigstring buffer ~pos ~len =
    if pos < 0 || len < 0 || pos + len > Base_bigstring.length buffer then
      invalid_arg "H1.Bufview.of_bigstring: index out of bounds";
    { buffer; pos; min_pos = pos; upper_bound = pos + len }

  let empty () =
    let buffer = Base_bigstring.create 0 in
    of_bigstring buffer ~pos:0 ~len:0

  let advance t count =
    if count < 0 || t.pos + count > t.upper_bound then
      invalid_arg
        (Printf.sprintf
           "H1.Bufview.advance: Index out of bounds. Requested count: %d" count);
    t.pos <- t.pos + count

  let length t = t.upper_bound - t.pos
  let consumed t = t.pos - t.min_pos

  let consume t fn =
    let c, res = fn t.buffer ~pos:t.pos ~len:(length t) in
    advance t c;
    res

  let consume_string t len =
    if len > length t then invalid_arg "H1.Bufview: index out of bounds";
    let res = Base_bigstring.to_string t.buffer ~pos:t.pos ~len in
    advance t len;
    res
end

module Decoder = struct
  type event =
    [ `Request of Request.t
    | `Data of string
    | `Need_data
    | `Error of string
    | `Request_complete ]

  type decoder = { mutable source : Bufview.t; mutable cont : decoder -> event }

  let consumed t = Bufview.consumed t.source
  let unconsumed t = Bufview.length t.source
  let close t = t.source <- Bufview.empty ()

  let parse_body req =
    match Headers.get_transfer_encoding (Request.headers req) with
    | `Bad_request -> fun _ -> `Error "Could not determine transfer encoding"
    | `Chunked ->
        fun t ->
          Bufview.consume t.source (fun buffer ~pos ~len ->
              match H1_parser.parse_chunk buffer ~pos ~len with
              | Ok (None, count) ->
                  t.cont <- (fun _ -> `Request_complete);
                  (count, `Request_complete)
              | Ok (Some chunk, count) -> (count, `Data chunk)
              | Error Partial -> (0, `Need_data)
              | Error (Msg msg) -> (0, `Error msg))
    | `Fixed 0L ->
        fun t ->
          t.cont <- (fun _ -> `Request_complete);
          `Request_complete
    | `Fixed len ->
        let remaining = ref len in
        fun t ->
          if !remaining = 0L then `Request_complete
          else
            let len = unconsumed t in
            if len = 0 then `Need_data
            else
              let len = Int64.of_int len in
              let to_consume = if !remaining < len then !remaining else len in
              remaining := Int64.sub !remaining to_consume;
              `Data (Bufview.consume_string t.source (Int64.to_int to_consume))

  let parse_request t =
    Bufview.consume t.source (fun buffer ~pos ~len ->
        match H1_parser.parse_request buffer ~pos ~len with
        | Ok (req, count) ->
            t.cont <- parse_body req;
            (count, `Request req)
        | Error Partial -> (0, `Need_data)
        | Error (Msg msg) ->
            close t;
            t.cont <- (fun _ -> `Error msg);
            (0, `Error msg))

  let decoder () = { source = Bufview.empty (); cont = parse_request }
  let next_cycle t = t.cont <- parse_request
  let decode t = t.cont t
  let src t buf ~pos ~len = t.source <- Bufview.of_bigstring buf ~pos ~len
end

let serialize_response buf resp =
  Bytebuffer.add_string buf (Version.to_string @@ Response.version resp);
  Bytebuffer.add_char buf ' ';
  Bytebuffer.add_string buf (Status.to_string @@ Response.status resp);
  Bytebuffer.add_char buf ' ';
  Bytebuffer.add_string buf @@ Response.reason_phrase resp;
  Bytebuffer.add_string buf "\r\n";
  Headers.iteri
    ~f:(fun ~key ~data ->
      Bytebuffer.add_string buf key;
      Bytebuffer.add_string buf ": ";
      Bytebuffer.add_string buf data;
      Bytebuffer.add_string buf "\r\n")
    (Response.headers resp);
  Bytebuffer.add_string buf "\r\n"
