let fill refill read_buf =
  let%lwt count = Bigbuffer.fill read_buf ~f:refill in
  if count <= 0 then Lwt.return `Eof else Lwt.return `Ok

let reader_stream read_buf_size refill =
  let read_buf = Bigbuffer.create read_buf_size in
  let fn () =
    if Bigbuffer.length read_buf = Bigbuffer.capacity read_buf then
      Bigbuffer.resize read_buf 0;
    if Bigbuffer.length read_buf > 0 then Lwt.return_some read_buf
    else
      match%lwt fill refill read_buf with
      | `Eof -> Lwt.return_none
      | `Ok -> Lwt.return_some read_buf
  in
  Lstream.from_fn fn
