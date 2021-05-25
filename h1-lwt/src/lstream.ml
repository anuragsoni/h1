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

let concat ts =
  let current = ref (from_fn (fun () -> Lwt.return_none)) in
  let rec aux () =
    match%lwt next !current with
    | None -> (
        match%lwt next ts with
        | None -> Lwt.return_none
        | Some next_stream ->
            current := next_stream;
            aux ())
    | Some xs -> Lwt.return_some xs
  in
  from_fn aux

let map ~f t =
  from_fn (fun () ->
      match%lwt next t with
      | None -> Lwt.return_none
      | Some v -> Lwt.return_some (f v))

let concat_map ~f t = concat (map ~f t)

let rec fold t ~init ~f =
  match%lwt next t with
  | None -> Lwt.return init
  | Some x ->
      let%lwt new_init = f init x in
      fold t ~init:new_init ~f

let take t n =
  let rec aux n acc =
    if n = 0 then Lwt.return (List.rev acc)
    else
      match%lwt next t with
      | None -> Lwt.return (List.rev acc)
      | Some x -> aux (n - 1) (x :: acc)
  in
  aux n []

let rec iter ~f t =
  match%lwt next t with
  | None -> Lwt.return_unit
  | Some v ->
      let%lwt () = f v in
      iter ~f t

let rec drain t =
  match%lwt next t with None -> Lwt.return_unit | Some _ -> drain t

let of_list xs =
  let xs = ref xs in
  let fn () =
    match !xs with
    | [] -> Lwt.return_none
    | x :: xs' ->
        xs := xs';
        Lwt.return_some x
  in
  from_fn fn

type ('a, 'b) conduit = 'a t -> 'b t

let through conduit t = conduit t
