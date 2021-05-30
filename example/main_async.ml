open Core
open Async
open H1_types

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of \
   sitting by her sister on the bank, and of having nothing to do: once or \
   twice she had peeped into the book her sister was reading, but it had no \
   pictures or conversations in it, <and what is the use of a book,> thought \
   Alice <without pictures or conversations?> So she was considering in her \
   own mind (as well as she could, for the hot day made her feel very sleepy \
   and stupid), whether the pleasure of making a daisy-chain would be worth \
   the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very \
   remarkable in that; nor did Alice think it so very much out of the way to \
   hear the Rabbit say to itself, <Oh dear! Oh dear! I shall be late!> (when \
   she thought it over afterwards, it occurred to her that she ought to have \
   wondered at this, but at the time it all seemed quite natural); but when \
   the Rabbit actually took a watch out of its waistcoat-pocket, and looked at \
   it, and then hurried on, Alice started to her feet, for it flashed across \
   her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, \
   she ran across the field after it, and fortunately was just in time to see \
   it pop down a large rabbit-hole under the hedge. In another moment down \
   went Alice after it, never once considering how in the world she was to get \
   out again. The rabbit-hole went straight on like a tunnel for some way, and \
   then dipped suddenly down, so suddenly that Alice had not a moment to think \
   about stopping herself before she found herself falling down a very deep \
   well. Either the well was very deep, or she fell very slowly, for she had \
   plenty of time as she went down to look about her and to wonder what was \
   going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the \
   sides of the well, and noticed that they were filled with cupboards......"

let text = Bigstringaf.of_string text ~off:0 ~len:(String.length text)

let rec read fd buf ~pos ~len =
  let open Unix.Error in
  match
    Fd.syscall fd ~nonblocking:true (fun file_descr ->
        Unix.Syscall_result.Int.ok_or_unix_error_exn ~syscall_name:"read"
          (Bigstring_unix.read_assume_fd_is_nonblocking file_descr buf ~pos ~len))
  with
  | `Already_closed | `Ok 0 -> return 0
  | `Ok n -> return n
  | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) -> (
      Fd.ready_to fd `Read >>= function
      | `Bad_fd -> failwith "Bad fd"
      | `Closed -> return 0
      | `Ready -> read fd buf ~pos ~len)
  | `Error (Unix.Unix_error (EBADF, _, _)) -> failwith "Bad fd"
  | `Error exn ->
      don't_wait_for (Fd.close fd);
      raise exn

let rec write fd buf ~pos ~len =
  let open Unix.Error in
  match
    Fd.syscall fd ~nonblocking:true (fun file_descr ->
        Bigstring_unix.write_assume_fd_is_nonblocking file_descr buf ~pos ~len)
  with
  | `Already_closed | `Ok 0 -> return 0
  | `Ok n -> return n
  | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) -> (
      Fd.ready_to fd `Write >>= function
      | `Bad_fd -> failwith "Bad fd"
      | `Closed -> return 0
      | `Ready -> write fd buf ~pos ~len)
  | `Error (Unix.Unix_error (EBADF, _, _)) -> failwith "Bad fd"
  | `Error exn ->
      don't_wait_for (Fd.close fd);
      raise exn

let run (sock : Fd.t) =
  let open H1_async in
  let service (_req, body) =
    let body = Body.to_string_stream body in
    let%bind () =
      iter
        ~f:(fun x ->
          Logs.info (fun m -> m "%s" x);
          return ())
        body
    in
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Bigstring.length text)) ])
        `Ok
    in
    return (resp, `Bigstring text)
  in
  Http_server.run ~read_buf_size:(10 * 1024) ~write_buf_size:(10 * 1024)
    ~write:(fun buf ~pos ~len -> write sock buf ~pos ~len)
    ~refill:(fun buf ~pos ~len -> read sock buf ~pos ~len)
    service

let run ~port =
  let (server : (Socket.Address.Inet.t, int) Tcp.Server.t) =
    Tcp.Server.create_sock_inet ~backlog:11_000 ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port port) (fun _addr sock ->
        let fd = Socket.fd sock in
        run fd)
  in
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5) >>| fun () ->
      Log.Global.printf "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  Command.async ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open port =
        flag "-port"
          (optional_with_default 8080 int)
          ~doc:" Port to listen on (default 8080)"
      in
      fun () -> run ~port)
  |> Command.run
