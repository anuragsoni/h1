let fill refill read_buf =
  let view = Bigbuffer.fill read_buf in
  let%lwt count =
    refill view.Bigbuffer.View.buffer ~pos:view.pos ~len:view.len
  in
  view.continue count;
  if count <= 0 then Lwt.return `Eof else Lwt.return `Ok

let reader_stream read_buf_size refill =
  let read_buf = Bigbuffer.create read_buf_size in
  (* Use [prev_len] to keep track of whether any content was consumed from the
     buffer during a run. If a stream as a non-zero length, but some content was
     consumed in the previous run we return that right away. If no content was
     consumed in a run, but the downstream consumer pulls on this stream again
     we treat that as a scenario where the consumer needed more content than
     what's available in the buffer right now. In such scenarios we attempt to
     refill the buffer again and try again. *)
  let prev_len = ref @@ Bigbuffer.length read_buf in
  let fn () =
    if Bigbuffer.length read_buf = Bigbuffer.capacity read_buf then
      Bigbuffer.resize read_buf 0;
    if Bigbuffer.length read_buf > 0 && !prev_len <> Bigbuffer.length read_buf
    then (
      prev_len := Bigbuffer.length read_buf;
      Lwt.return_some (Bigbuffer.consume read_buf))
    else (
      prev_len := Bigbuffer.length read_buf;
      match%lwt fill refill read_buf with
      | `Eof -> Lwt.return_none
      | `Ok -> Lwt.return_some (Bigbuffer.consume read_buf))
  in
  Lstream.from_fn fn
