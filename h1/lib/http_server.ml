open Cps.Monad_infix
open H1_types

module Transport = struct
  let fill refill read_buf =
    let view = Bigbuffer.fill read_buf in
    refill view.Bigbuffer.View.buffer ~pos:view.pos ~len:view.len
    >>= fun count ->
    view.continue count;
    if count <= 0 then Cps.return `Eof else Cps.return `Ok

  let reader_stream read_buf_size refill =
    let read_buf = Bigbuffer.create read_buf_size in
    (* Use [prev_len] to keep track of whether any content was consumed from the
       buffer during a run. If a stream as a non-zero length, but some content
       was consumed in the previous run we return that right away. If no content
       was consumed in a run, but the downstream consumer pulls on this stream
       again we treat that as a scenario where the consumer needed more content
       than what's available in the buffer right now. In such scenarios we
       attempt to refill the buffer again and try again. *)
    let prev_len = ref @@ Bigbuffer.length read_buf in
    let fn () =
      if Bigbuffer.length read_buf = Bigbuffer.capacity read_buf then
        Bigbuffer.resize read_buf 0;
      if Bigbuffer.length read_buf > 0 && !prev_len <> Bigbuffer.length read_buf
      then (
        prev_len := Bigbuffer.length read_buf;
        Cps.return (Some (Bigbuffer.consume read_buf)))
      else (
        prev_len := Bigbuffer.length read_buf;
        fill refill read_buf >>= function
        | `Eof -> Cps.return None
        | `Ok -> Cps.return (Some (Bigbuffer.consume read_buf)))
    in
    Pull.from_fn fn
end

type service = Request.t * Body.t -> (Response.t * Body.t) Cps.t

let body_stream req bufstream =
  match Headers.get_transfer_encoding (Request.headers req) with
  | `Chunked ->
      let rec fn () =
        Pull.next bufstream >>= function
        | None -> Cps.return None
        | Some view -> (
            match
              H1_parser.parse_chunk ~off:view.Bigbuffer.View.pos ~len:view.len
                view.buffer
            with
            | Ok (chunk, c) ->
                view.continue c;
                Cps.return chunk
            | Error Partial -> fn ()
            | Error (Msg msg) ->
                Cps.fail (Base.Error.createf "Bad Request: %S" msg))
      in
      Pull.from_fn fn
  | `Bad_request ->
      Pull.from_fn (fun () -> Cps.fail (Base.Error.of_string "Bad Request"))
  | `Fixed 0L -> Pull.from_fn (fun () -> Cps.return None)
  | `Fixed len ->
      let to_consume = ref len in
      let fn () =
        if !to_consume <= 0L then Cps.return None
        else
          Pull.next bufstream >>= function
          | None -> Cps.return None
          | Some view ->
              let l = Int64.of_int view.Bigbuffer.View.len in
              let c = if !to_consume > l then l else !to_consume in
              to_consume := Int64.sub !to_consume c;
              let c' = Int64.to_int c in
              let chunk =
                Bigstringaf.substring view.buffer ~off:view.pos ~len:c'
              in
              view.continue c';
              Cps.return (Some chunk)
      in
      Pull.from_fn fn

let request_stream bufstream =
  (* TODO: Add some checks to ensure that we don't consume > UPPER_BOUND bytes
     to process a single request *)
  let rec fn () =
    Pull.next bufstream >>= function
    | None -> Cps.return None
    | Some view -> (
        match
          H1_parser.parse_request view.Bigbuffer.View.buffer
            ~off:view.Bigbuffer.View.pos ~len:view.len
        with
        | Ok (req, consumed) ->
            view.continue consumed;
            let body_stream = body_stream req bufstream in
            Cps.return (Some (req, `Stream body_stream))
        | Error (Msg msg) -> Cps.fail (Base.Error.createf "Bad Request: %S" msg)
        | Error Partial -> fn ())
  in
  Pull.from_fn fn

let write_response writer resp =
  Bigbuffer.add_string writer (Version.to_string @@ Response.version resp);
  Bigbuffer.add_char writer ' ';
  Bigbuffer.add_string writer (Status.to_string @@ Response.status resp);
  Bigbuffer.add_char writer ' ';
  Bigbuffer.add_string writer @@ Response.reason_phrase resp;
  Bigbuffer.add_string writer "\r\n";
  Headers.iteri
    ~f:(fun ~key ~data ->
      Bigbuffer.add_string writer key;
      Bigbuffer.add_string writer ": ";
      Bigbuffer.add_string writer data;
      Bigbuffer.add_string writer "\r\n")
    (Response.headers resp);
  Bigbuffer.add_string writer "\r\n"

let write_chunk writer = function
  | `String s ->
      let len = String.length s in
      Bigbuffer.addf writer "%x\r\n" len;
      Bigbuffer.add_string writer s;
      Bigbuffer.add_string writer "\r\n"
  | `Iovec iovec ->
      let len = iovec.Iovec.len in
      Bigbuffer.addf writer "%x\r\n" len;
      Bigbuffer.add_iovec writer iovec;
      Bigbuffer.add_string writer "\r\n"

let write_final_chunk writer =
  Bigbuffer.addf writer "%x\r\n" 0;
  Bigbuffer.add_string writer "\r\n"

let is_chunked_response resp =
  match Headers.get_transfer_encoding (Response.headers resp) with
  | `Chunked -> true
  | `Bad_request | `Fixed _ -> false

let write_all ~write buf =
  let rec aux () =
    let pending = Bigbuffer.length buf in
    if pending = 0 then Cps.return ()
    else
      let view = Bigbuffer.consume buf in
      write view.Bigbuffer.View.buffer ~pos:view.pos ~len:view.len
      >>= fun count ->
      view.continue count;
      if count = pending then Cps.return () else aux ()
  in
  aux ()

let run ~read_buf_size ~write_buf_size ~refill ~write service =
  let writer = Bigbuffer.create write_buf_size in
  let flush () = write_all ~write writer in
  let reader_stream = Transport.reader_stream read_buf_size refill in
  let request_stream = request_stream reader_stream in
  let rec aux () =
    Pull.next request_stream >>= function
    | Some (req, req_body) ->
        ( service (req, req_body) >>= fun (res, body) ->
          write_response writer res;
          let is_chunk = is_chunked_response res in
          match body with
          | `String s ->
              Bigbuffer.add_string writer s;
              flush ()
          | `Bigstring b ->
              Bigbuffer.add_bigstring writer b;
              flush ()
          | `Stream s ->
              (* TODO: This can potentially be factored out as a body encoder
                 written as a stream-conduit and we can pipe the body through
                 that body encoder. *)
              Pull.iter
                ~f:(fun str ->
                  if is_chunk then write_chunk writer (`String str)
                  else Bigbuffer.add_string writer str;
                  flush ())
                s
              >>= fun () ->
              if is_chunk then (
                write_final_chunk writer;
                flush ())
              else Cps.return ()
          | `Iovecs s ->
              Pull.iter
                ~f:(fun iovec ->
                  if is_chunk then write_chunk writer (`Iovec iovec)
                  else Bigbuffer.add_iovec writer iovec;
                  flush ())
                s
              >>= fun () ->
              if is_chunk then (
                write_final_chunk writer;
                flush ())
              else Cps.return () )
        >>= fun () ->
        Body.drain req_body >>= fun () ->
        if Headers.keep_alive (Request.headers req) then aux ()
        else Cps.return ()
    | None -> Cps.return ()
  in
  aux ()
