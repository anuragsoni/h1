open H1_types
module Logger = (val Logs.src_log (Logs.Src.create "http.server"))

type service = Request.t * Body.t -> (Response.t * Body.t) Lwt.t

exception Bad_request of string option

let body_stream req bufstream =
  match Headers.get_transfer_encoding (Request.headers req) with
  | `Chunked ->
      let rec fn () =
        match%lwt Lstream.next bufstream with
        | None -> Lwt.return_none
        | Some view -> (
            match
              H1_parser.parse_chunk ~off:view.Bigbuffer.View.pos ~len:view.len
                view.buffer
            with
            | Ok (chunk, c) ->
                view.continue c;
                Lwt.return chunk
            | Error Partial -> fn ()
            | Error (Msg msg) -> raise (Bad_request (Some msg)))
      in
      Lstream.from_fn fn
  | `Bad_request -> raise (Bad_request None)
  | `Fixed 0L -> Lstream.from_fn (fun () -> Lwt.return_none)
  | `Fixed len ->
      let to_consume = ref len in
      let fn () =
        if !to_consume <= 0L then Lwt.return_none
        else
          match%lwt Lstream.next bufstream with
          | None -> Lwt.return_none
          | Some view ->
              let l = Int64.of_int view.Bigbuffer.View.len in
              let c = if !to_consume > l then l else !to_consume in
              to_consume := Int64.sub !to_consume c;
              let c' = Int64.to_int c in
              let chunk =
                Bigstringaf.substring view.buffer ~off:view.pos ~len:c'
              in
              view.continue c';
              Lwt.return_some chunk
      in
      Lstream.from_fn fn

let request_stream bufstream =
  (* TODO: Add some checks to ensure that we don't consume > UPPER_BOUND bytes
     to process a single request *)
  let rec fn () =
    match%lwt Lstream.next bufstream with
    | None -> Lwt.return_none
    | Some view -> (
        match
          H1_parser.parse_request view.Bigbuffer.View.buffer
            ~off:view.Bigbuffer.View.pos ~len:view.len
        with
        | Ok (req, consumed) ->
            view.continue consumed;
            let body_stream = body_stream req bufstream in
            Lwt.return_some (req, `Stream body_stream)
        | Error (Msg msg) -> raise (Bad_request (Some msg))
        | Error Partial -> fn ())
  in
  Lstream.from_fn fn

let write_response writer resp =
  Writer.write_string writer (Version.to_string @@ Response.version resp);
  Writer.write_char writer ' ';
  Writer.write_string writer (Status.to_string @@ Response.status resp);
  Writer.write_char writer ' ';
  Writer.write_string writer @@ Response.reason_phrase resp;
  Writer.write_string writer "\r\n";
  Headers.iteri
    ~f:(fun ~key ~data ->
      Writer.write_string writer key;
      Writer.write_string writer ": ";
      Writer.write_string writer data;
      Writer.write_string writer "\r\n")
    (Response.headers resp);
  Writer.write_string writer "\r\n"

let write_chunk writer = function
  | `String s ->
      let len = String.length s in
      Writer.writef writer "%x\r\n" len;
      Writer.write_string writer s;
      Writer.write_string writer "\r\n"
  | `Iovec iovec ->
      let len = iovec.Iovec.len in
      Writer.writef writer "%x\r\n" len;
      Writer.write_iovec writer iovec;
      Writer.write_string writer "\r\n"

let write_final_chunk writer =
  Writer.writef writer "%x\r\n" 0;
  Writer.write_string writer "\r\n"

let is_chunked_response resp =
  match Headers.get_transfer_encoding (Response.headers resp) with
  | `Chunked -> true
  | `Bad_request | `Fixed _ -> false

let run ~read_buf_size ~write_buf_size ~refill ~write service =
  let writer = Writer.create write_buf_size in
  let flush () = Writer.write_all ~write writer in
  let reader_stream = Io.reader_stream read_buf_size refill in
  let request_stream = request_stream reader_stream in
  Lwt.catch
    (fun () ->
      let rec aux () =
        match%lwt Lstream.next request_stream with
        | Some (req, req_body) ->
            let%lwt res, body = service (req, req_body) in
            write_response writer res;
            let is_chunk = is_chunked_response res in
            let%lwt () =
              match body with
              | `String s ->
                  Writer.write_string writer s;
                  flush ()
              | `Bigstring b ->
                  Writer.write_bigstring writer b;
                  flush ()
              | `Stream s ->
                  (* TODO: This can potentially be factored out as a body
                     encoder written as a stream-conduit and we can pipe the
                     body through that body encoder. *)
                  let%lwt () =
                    Lstream.iter
                      ~f:(fun str ->
                        if is_chunk then write_chunk writer (`String str)
                        else Writer.write_string writer str;
                        flush ())
                      s
                  in
                  if is_chunk then (
                    write_final_chunk writer;
                    flush ())
                  else Lwt.return_unit
              | `Iovecs s ->
                  let%lwt () =
                    Lstream.iter
                      ~f:(fun iovec ->
                        if is_chunk then write_chunk writer (`Iovec iovec)
                        else Writer.write_iovec writer iovec;
                        flush ())
                      s
                  in
                  if is_chunk then (
                    write_final_chunk writer;
                    flush ())
                  else Lwt.return_unit
            in
            let%lwt () = Body.drain req_body in
            if Headers.keep_alive (Request.headers req) then aux ()
            else Lwt.return_unit
        | None -> Lwt.return_unit
      in
      aux ())
    (function
      | Bad_request msg ->
          let response =
            Response.create
              ~headers:
                (Headers.of_list
                   [ ("Content-Length", "0"); ("Connection", "close") ])
              `Bad_request
          in
          Option.iter
            (fun msg' -> Logger.err (fun m -> m "Invalid Request: %S" msg'))
            msg;
          write_response writer response;
          flush ()
      | exn -> Lwt.fail exn)
