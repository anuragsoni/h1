module IO = struct
  open H1

  type 'a t = 'a Lwt.t

  let of_cps cps =
    let p, w = Lwt.wait () in
    Cps.run cps (fun v -> Lwt.wakeup_later w v);
    p

  let to_cps p =
    Cps.make @@ fun finish ->
    match Lwt.state p with
    | Lwt.Return x -> finish x
    | Lwt.Sleep -> Lwt.on_any p (fun v -> finish v) (fun exn -> raise exn)
    | Lwt.Fail exn -> raise exn
end

include H1.Async (IO)
