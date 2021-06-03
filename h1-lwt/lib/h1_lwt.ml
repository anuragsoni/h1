module IO = struct
  open H1

  type 'a t = ('a, H1.Error.t) Lwt_result.t

  let of_cps cps =
    let p, w = Lwt.wait () in
    Cps.run cps
      (fun e -> Lwt.wakeup_later w (Error e))
      (fun v -> Lwt.wakeup_later w (Ok v));
    p

  let to_cps p =
    Cps.make @@ fun on_error finish ->
    let fill = function Ok v -> finish v | Error e -> on_error e in
    match Lwt.state p with
    | Lwt.Return x -> fill x
    | Lwt.Sleep ->
        Lwt.on_any p (fun v -> fill v) (fun exn -> on_error (`Exn exn))
    | Lwt.Fail exn -> on_error (`Exn exn)
end

include H1.Async (IO)
