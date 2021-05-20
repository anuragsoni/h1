let request_stream ~refill reader =
  let rec fn () =
    match%lwt
      if Reader.is_empty reader then
        match%lwt Reader.fill ~f:refill reader with
        | `Eof -> Lwt.return `Eof
        | `Ok _ -> Lwt.return (`Ok ())
      else Lwt.return (`Ok ())
    with
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
        | Ok req -> Lwt.return_some req
        | Error (Msg msg) -> failwith msg
        | Error Partial -> (
            match%lwt Reader.fill ~f:refill reader with
            | `Eof -> Lwt.return_none
            | `Ok _ -> fn ()))
  in
  Lstream.from_fn fn
