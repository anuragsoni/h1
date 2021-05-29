module Bigbuffer = Bigbuffer
module Iovec = Iovec

module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

module Make (IO : IO) = struct
  let ( let* ) t f = IO.bind ~f t

  module Pull = struct
    type 'a t = { next : unit -> 'a option IO.t; pushback : 'a -> unit }

    let from_fn fn =
      let pushback_bag = ref [] in
      let next () =
        match !pushback_bag with
        | x :: xs ->
            pushback_bag := xs;
            IO.return (Some x)
        | [] -> fn ()
      in
      let pushback x = pushback_bag := x :: !pushback_bag in
      { next; pushback }

    let next t = t.next ()
    let pushback t v = t.pushback v

    let concat ts =
      let current = ref (from_fn (fun () -> IO.return None)) in
      let rec aux () =
        let* n = next !current in
        match n with
        | None -> (
            let* n' = next ts in
            match n' with
            | None -> IO.return None
            | Some next_stream ->
                current := next_stream;
                aux ())
        | Some xs -> IO.return (Some xs)
      in
      from_fn aux

    let map ~f t =
      from_fn (fun () ->
          let* n = next t in
          match n with
          | None -> IO.return None
          | Some v -> IO.return (Some (f v)))

    let concat_map ~f t = concat (map ~f t)

    let rec fold t ~init ~f =
      let* n = next t in
      match n with
      | None -> IO.return init
      | Some x ->
          let* new_init = f init x in
          fold t ~init:new_init ~f

    let take t n =
      let rec aux n acc =
        if n = 0 then IO.return (List.rev acc)
        else
          let* next' = next t in
          match next' with
          | None -> IO.return (List.rev acc)
          | Some x -> aux (n - 1) (x :: acc)
      in
      aux n []

    let rec iter ~f t =
      let* n = next t in
      match n with
      | None -> IO.return ()
      | Some v ->
          let* () = f v in
          iter ~f t

    let drain t = iter ~f:(fun _ -> IO.return ()) t

    let of_list xs =
      let xs = ref xs in
      let fn () =
        match !xs with
        | [] -> IO.return None
        | x :: xs' ->
            xs := xs';
            IO.return (Some x)
      in
      from_fn fn
  end

  module Body = struct
    type t =
      [ `String of string
      | `Bigstring of Bigstringaf.t
      | `Stream of string Pull.t
      | `Iovecs of Iovec.t Pull.t ]

    let drain = function
      | `String _ | `Bigstring _ -> IO.return ()
      | `Stream xs -> Pull.drain xs
      | `Iovecs xs -> Pull.drain xs

    let to_string_stream = function
      | `String s -> Pull.of_list [ s ]
      | `Bigstring b -> Pull.of_list [ Bigstringaf.to_string b ]
      | `Stream xs -> xs
      | `Iovecs xs ->
          Pull.map
            ~f:(fun iovec ->
              Bigstringaf.substring iovec.Iovec.buf ~off:iovec.pos
                ~len:iovec.len)
            xs
  end

  module Transport = struct
    let fill refill read_buf =
      let view = Bigbuffer.fill read_buf in
      let* count =
        refill view.Bigbuffer.View.buffer ~pos:view.pos ~len:view.len
      in
      view.continue count;
      if count <= 0 then IO.return `Eof else IO.return `Ok

    let reader_stream read_buf_size refill =
      let read_buf = Bigbuffer.create read_buf_size in
      (* Use [prev_len] to keep track of whether any content was consumed from
         the buffer during a run. If a stream as a non-zero length, but some
         content was consumed in the previous run we return that right away. If
         no content was consumed in a run, but the downstream consumer pulls on
         this stream again we treat that as a scenario where the consumer needed
         more content than what's available in the buffer right now. In such
         scenarios we attempt to refill the buffer again and try again. *)
      let prev_len = ref @@ Bigbuffer.length read_buf in
      let fn () =
        if Bigbuffer.length read_buf = Bigbuffer.capacity read_buf then
          Bigbuffer.resize read_buf 0;
        if
          Bigbuffer.length read_buf > 0
          && !prev_len <> Bigbuffer.length read_buf
        then (
          prev_len := Bigbuffer.length read_buf;
          IO.return (Some (Bigbuffer.consume read_buf)))
        else (
          prev_len := Bigbuffer.length read_buf;
          let* res = fill refill read_buf in
          match res with
          | `Eof -> IO.return None
          | `Ok -> IO.return (Some (Bigbuffer.consume read_buf)))
      in
      Pull.from_fn fn
  end

  module Http_server = struct
    open H1_types
    module Logger = (val Logs.src_log (Logs.Src.create "http.server"))

    type service = Request.t * Body.t -> (Response.t * Body.t) IO.t

    exception Bad_request of string option

    let body_stream req bufstream =
      match Headers.get_transfer_encoding (Request.headers req) with
      | `Chunked ->
          let rec fn () =
            let* n = Pull.next bufstream in
            match n with
            | None -> IO.return None
            | Some view -> (
                match
                  H1_parser.parse_chunk ~off:view.Bigbuffer.View.pos
                    ~len:view.len view.buffer
                with
                | Ok (chunk, c) ->
                    view.continue c;
                    IO.return chunk
                | Error Partial -> fn ()
                | Error (Msg msg) -> raise (Bad_request (Some msg)))
          in
          Pull.from_fn fn
      | `Bad_request -> raise (Bad_request None)
      | `Fixed 0L -> Pull.from_fn (fun () -> IO.return None)
      | `Fixed len ->
          let to_consume = ref len in
          let fn () =
            if !to_consume <= 0L then IO.return None
            else
              let* n = Pull.next bufstream in
              match n with
              | None -> IO.return None
              | Some view ->
                  let l = Int64.of_int view.Bigbuffer.View.len in
                  let c = if !to_consume > l then l else !to_consume in
                  to_consume := Int64.sub !to_consume c;
                  let c' = Int64.to_int c in
                  let chunk =
                    Bigstringaf.substring view.buffer ~off:view.pos ~len:c'
                  in
                  view.continue c';
                  IO.return (Some chunk)
          in
          Pull.from_fn fn

    let request_stream bufstream =
      (* TODO: Add some checks to ensure that we don't consume > UPPER_BOUND
         bytes to process a single request *)
      let rec fn () =
        let* n = Pull.next bufstream in
        match n with
        | None -> IO.return None
        | Some view -> (
            match
              H1_parser.parse_request view.Bigbuffer.View.buffer
                ~off:view.Bigbuffer.View.pos ~len:view.len
            with
            | Ok (req, consumed) ->
                view.continue consumed;
                let body_stream = body_stream req bufstream in
                IO.return (Some (req, `Stream body_stream))
            | Error (Msg msg) -> raise (Bad_request (Some msg))
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
        if pending = 0 then IO.return ()
        else
          let view = Bigbuffer.consume buf in
          let* count =
            write view.Bigbuffer.View.buffer ~pos:view.pos ~len:view.len
          in
          view.continue count;
          if count = pending then IO.return () else aux ()
      in
      aux ()

    let run ~read_buf_size ~write_buf_size ~refill ~write service =
      let writer = Bigbuffer.create write_buf_size in
      let flush () = write_all ~write writer in
      let reader_stream = Transport.reader_stream read_buf_size refill in
      let request_stream = request_stream reader_stream in
      let rec aux () =
        let* next_req = Pull.next request_stream in
        match next_req with
        | Some (req, req_body) ->
            let* res, body = service (req, req_body) in
            write_response writer res;
            let is_chunk = is_chunked_response res in
            let* () =
              match body with
              | `String s ->
                  Bigbuffer.add_string writer s;
                  flush ()
              | `Bigstring b ->
                  Bigbuffer.add_bigstring writer b;
                  flush ()
              | `Stream s ->
                  (* TODO: This can potentially be factored out as a body
                     encoder written as a stream-conduit and we can pipe the
                     body through that body encoder. *)
                  let* () =
                    Pull.iter
                      ~f:(fun str ->
                        if is_chunk then write_chunk writer (`String str)
                        else Bigbuffer.add_string writer str;
                        flush ())
                      s
                  in
                  if is_chunk then (
                    write_final_chunk writer;
                    flush ())
                  else IO.return ()
              | `Iovecs s ->
                  let* () =
                    Pull.iter
                      ~f:(fun iovec ->
                        if is_chunk then write_chunk writer (`Iovec iovec)
                        else Bigbuffer.add_iovec writer iovec;
                        flush ())
                      s
                  in
                  if is_chunk then (
                    write_final_chunk writer;
                    flush ())
                  else IO.return ()
            in
            let* () = Body.drain req_body in
            if Headers.keep_alive (Request.headers req) then aux ()
            else IO.return ()
        | None -> IO.return ()
      in
      aux ()
  end
end
