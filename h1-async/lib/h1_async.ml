open Core
open Async

let rec read_nonblock fd buf ~pos ~len =
  let open Unix.Error in
  match
    Fd.syscall fd ~nonblocking:true (fun file_descr ->
        Unix.Syscall_result.Int.ok_or_unix_error_exn ~syscall_name:"read"
          (Bigstring_unix.read_assume_fd_is_nonblocking file_descr buf ~pos ~len))
  with
  | `Already_closed | `Ok 0 -> return (Ok 0)
  | `Ok n -> return (Ok n)
  | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) -> (
      Fd.ready_to fd `Read >>= function
      | `Bad_fd ->
          return
            (Or_error.error_s
               [%message "H1_async.read_nonblock: Bad file descriptor"])
      | `Closed -> return (Ok 0)
      | `Ready -> read_nonblock fd buf ~pos ~len)
  | `Error (Unix.Unix_error (EBADF, _, _)) ->
      return
        (Or_error.error_s
           [%message "H1_async.read_nonblock: Bad file descriptor"])
  | `Error exn ->
      don't_wait_for (Fd.close fd);
      return (Or_error.of_exn exn)

let rec write_nonblock fd buf ~pos ~len =
  let open Unix.Error in
  match
    Fd.syscall fd ~nonblocking:true (fun file_descr ->
        Bigstring_unix.write_assume_fd_is_nonblocking file_descr buf ~pos ~len)
  with
  | `Already_closed | `Ok 0 -> return (Ok 0)
  | `Ok n -> return (Ok n)
  | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) -> (
      Fd.ready_to fd `Write >>= function
      | `Bad_fd ->
          return
            (Or_error.error_s
               [%message "H1_async.write_nonblock: Bad file descriptor"])
      | `Closed -> return (Ok 0)
      | `Ready -> write_nonblock fd buf ~pos ~len)
  | `Error (Unix.Unix_error (EBADF, _, _)) ->
      return
        (Or_error.error_s
           [%message "H1_async.write_nonblock: Bad file descriptor"])
  | `Error exn ->
      don't_wait_for (Fd.close fd);
      return (Or_error.of_exn exn)

module IO = struct
  open H1

  type 'a t = 'a Deferred.Or_error.t

  let of_cps cps =
    Deferred.create (fun i ->
        Cps.run cps
          (fun e -> Ivar.fill_if_empty i (Error e))
          (fun v -> Ivar.fill_if_empty i (Ok v)))

  let to_cps d =
    Cps.make @@ fun on_err on_finish ->
    let fill = function Ok v -> on_finish v | Error e -> on_err e in
    match Deferred.peek d with None -> upon d fill | Some v -> fill v
end

include H1.Async (IO)

(* let to_pipe stream = *)
(*   Pipe.create_reader ~close_on_exception:false (fun writer -> *)
(*       let rec aux () = *)
(*         match%bind next stream with *)
(*         | None -> return () *)
(*         | Some v -> *)
(*             let%bind () = Pipe.write writer v in *)
(*             aux () *)
(*       in *)
(*       aux ()) *)

(* let of_pipe pipe = *)
(* let fn () = match%map Pipe.read pipe with `Ok v -> Some v | `Eof -> None in *)
(* create_stream fn *)
