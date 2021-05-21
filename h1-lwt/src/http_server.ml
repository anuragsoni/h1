open H1_types
module Logger = (val Logs.src_log (Logs.Src.create "http.server"))

type service = Request.t * Body.t -> (Response.t * Body.t) Lwt.t

let body_stream req bufstream =
  match Headers.get_transfer_encoding (Request.headers req) with
  | `Chunked -> failwith "Not implemented yet"
  | `Bad_request -> failwith "Bad request"
  | `Fixed 0L -> Lstream.from_fn (fun () -> Lwt.return_none)
  | `Fixed len ->
      let to_consume = ref len in
      let fn () =
        if !to_consume <= 0L then Lwt.return_none
        else
          match%lwt Lstream.next bufstream with
          | None -> Lwt.return_none
          | Some buf ->
              let chunk =
                Bigbuffer.consume buf ~f:(fun buf ~pos ~len ->
                    let l = Int64.of_int len in
                    let c = if !to_consume > l then l else !to_consume in
                    to_consume := Int64.sub !to_consume c;
                    ( Bigstringaf.substring buf ~off:pos ~len:(Int64.to_int c),
                      Int64.to_int c ))
              in
              Lwt.return_some chunk
      in
      Lstream.from_fn fn

let request_stream bufstream =
  (* TODO: Add some checks to ensure that we don't consume > UPPER_BOUND bytes
     to process a single request *)
  let rec fn () =
    match%lwt Lstream.next bufstream with
    | None -> Lwt.return_none
    | Some buf -> (
        match
          Bigbuffer.consume buf ~f:(fun buf ~pos ~len ->
              match H1_parser.parse_request buf ~off:pos ~len with
              | Ok (req, consumed) -> (Ok req, consumed)
              | Error e -> (Error e, 0))
        with
        | Ok req ->
            let body_stream = body_stream req bufstream in
            Lwt.return_some (req, `Stream body_stream)
        | Error (Msg msg) -> failwith msg
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

let run ~read_buf_size ~write_buf_size ~refill ~write service =
  let writer = Writer.create write_buf_size in
  let flush () = Writer.write_all ~write writer in
  Io.reader_stream read_buf_size refill
  |> Lstream.through request_stream
  |> Lstream.iter ~f:(fun (req, req_body) ->
         let%lwt res, body = service (req, req_body) in
         write_response writer res;
         let%lwt () =
           match body with
           | `String s ->
               Writer.write_string writer s;
               flush ()
           | `Bigstring b ->
               Writer.write_bigstring writer b;
               flush ()
           | `Stream s ->
               Lstream.iter
                 ~f:(fun str ->
                   Writer.write_string writer str;
                   flush ())
                 s
           | `Iovecs s ->
               Lstream.iter
                 ~f:(fun iovec ->
                   Writer.write_iovec writer iovec;
                   flush ())
                 s
         in
         Body.drain req_body)
