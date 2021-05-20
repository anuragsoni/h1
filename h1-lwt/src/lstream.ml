type 'a t = { next : unit -> 'a option Lwt.t; pushback : 'a -> unit }

let from_fn fn =
  let pushback_bag = ref [] in
  let next () =
    match !pushback_bag with
    | x :: xs ->
        pushback_bag := xs;
        Lwt.return_some x
    | [] -> fn ()
  in
  let pushback x = pushback_bag := x :: !pushback_bag in
  { next; pushback }

let next t = t.next ()
let pushback t v = t.pushback v

let rec iter ~f t =
  match%lwt next t with
  | None -> Lwt.return_unit
  | Some v ->
      let%lwt () = f v in
      iter ~f t

let rec drain t =
  match%lwt next t with None -> Lwt.return_unit | Some _ -> drain t
