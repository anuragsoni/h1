open H1_types
open Lwt_result.Infix

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

[@@@part "simple_server"]

let run (sock : Lwt_unix.file_descr) =
  let service (_req, body) =
    let body = H1_lwt.Body.to_string_stream body in
    H1_lwt.iter
      ~f:(fun x ->
        Logs.info (fun m -> m "%s" x);
        Lwt_result.return ())
      body
    >>= fun () ->
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Bigstringaf.length text)) ])
        `Ok
    in
    Lwt_result.return (resp, `Bigstring text)
  in
  Lwt.catch
    (fun () ->
      H1_lwt.run_server ~read_buf_size:(10 * 1024) ~write_buf_size:(10 * 1024)
        ~write:(fun buf ~pos ~len ->
          let%lwt c = Lwt_bytes.write sock buf pos len in
          Lwt_result.return c)
        ~refill:(fun buf ~pos ~len ->
          let%lwt c = Lwt_bytes.read sock buf pos len in
          Lwt_result.return c)
        service)
    (fun exn ->
      Logs.err (fun m -> m "%s" (Printexc.to_string exn));
      Lwt_result.return ())

[@@@part "simple_server"]

let main port =
  let rec log_stats () =
    let stat = Gc.stat () in
    Logs.info (fun m ->
        m "Major collections: %d, Minor collections: %d" stat.major_collections
          stat.minor_collections);
    let%lwt () = Lwt_unix.sleep 5. in
    log_stats ()
  in
  Lwt.async log_stats;
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let open Lwt.Infix in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 listen_address
        (fun _ sock ->
          match%lwt run sock with
          | Ok () -> Lwt.return_unit
          | Error e ->
              Logs.err (fun m -> m "%s" (Base.Error.to_string_hum e));
              Lwt.return_unit)
      >>= fun _server -> Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  Lwt_main.run (main 8080)
