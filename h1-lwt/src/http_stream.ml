open H1_types
module Logger = (val Logs.src_log (Logs.Src.create "h1_lwt.http_stream"))

let fill_reader_if_needed ~refill reader =
  if Reader.is_empty reader then
    match%lwt Reader.fill ~f:refill reader with
    | `Eof -> Lwt.return `Eof
    | `Ok _ -> Lwt.return (`Ok ())
  else Lwt.return (`Ok ())

let make_body_stream ~refill reader req =
  match Headers.get_transfer_encoding (Request.headers req) with
  | `Fixed 0L -> Lstream.from_fn (fun () -> Lwt.return_none)
  | `Fixed len ->
      let to_consume = ref len in
      let fn () =
        if !to_consume <= 0L then Lwt.return_none
        else
          match%lwt fill_reader_if_needed ~refill reader with
          | `Eof -> Lwt.return_none
          | `Ok () ->
              let chunk =
                Reader.read
                  ~f:(fun buf ~pos ~len ->
                    let l = Int64.of_int len in
                    let c = if !to_consume > l then l else !to_consume in
                    to_consume := Int64.sub !to_consume c;
                    ( Bigstringaf.substring buf ~off:pos ~len:(Int64.to_int c),
                      Int64.to_int c ))
                  reader
              in
              Lwt.return_some chunk
      in
      Lstream.from_fn fn
  | `Chunked -> failwith "Not implemented yet"
  | `Bad_request -> failwith "Bad transfer encoding"

let request_stream ~refill reader =
  let rec fn () =
    match%lwt fill_reader_if_needed ~refill reader with
    | `Eof -> Lwt.return_none
    | `Ok () -> (
        match
          Reader.read
            ~f:(fun buf ~pos ~len ->
              match H1_parser.parse_request ~off:pos ~len buf with
              | Ok (req, c) -> (Ok req, c)
              | Error e -> (Error e, 0))
            reader
        with
        | Ok req ->
            let body_stream = make_body_stream ~refill reader req in
            Lwt.return_some (req, `Stream body_stream)
        | Error (Msg msg) -> failwith msg
        | Error Partial -> (
            match%lwt Reader.fill ~f:refill reader with
            | `Eof -> Lwt.return_none
            | `Ok _ -> fn ()))
  in
  Lstream.from_fn fn
