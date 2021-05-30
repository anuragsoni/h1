open Core
open Async

module IO = struct
  open H1

  type 'a t = 'a Deferred.t

  let of_cps cps =
    Deferred.create (fun i -> Cps.run cps (fun v -> Ivar.fill_if_empty i v))

  let to_cps d =
    Cps.make @@ fun on_finish ->
    match Deferred.peek d with
    | None -> upon d on_finish
    | Some v -> on_finish v
end

include H1.Async (IO)

let to_pipe stream =
  Pipe.create_reader ~close_on_exception:false (fun writer ->
      let rec aux () =
        match%bind next stream with
        | None -> return ()
        | Some v ->
            let%bind () = Pipe.write writer v in
            aux ()
      in
      aux ())

let of_pipe pipe =
  let fn () = match%map Pipe.read pipe with `Ok v -> Some v | `Eof -> None in
  create_stream fn
