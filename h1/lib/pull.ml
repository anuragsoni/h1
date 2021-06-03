open Cps.Infix

type 'a t = { next : unit -> 'a option Cps.t; pushback : 'a -> unit }

let from_fn fn =
  let pushback_bag = ref [] in
  let next () =
    match !pushback_bag with
    | x :: xs ->
        pushback_bag := xs;
        Cps.return (Some x)
    | [] -> fn ()
  in
  let pushback x = pushback_bag := x :: !pushback_bag in
  { next; pushback }

let next t = t.next ()
let pushback t v = t.pushback v

let concat ts =
  let current = ref (from_fn (fun () -> Cps.return None)) in
  let rec aux () =
    next !current >>= function
    | None -> (
        next ts >>= function
        | None -> Cps.return None
        | Some next_stream ->
            current := next_stream;
            aux ())
    | Some xs -> Cps.return (Some xs)
  in
  from_fn aux

let map ~f t =
  from_fn (fun () ->
      next t >>= function
      | None -> Cps.return None
      | Some v -> Cps.return (Some (f v)))

let concat_map ~f t = concat (map ~f t)

let rec fold t ~init ~f =
  next t >>= function
  | None -> Cps.return init
  | Some x -> f init x >>= fun new_init -> fold t ~init:new_init ~f

let take t n =
  let rec aux n acc =
    if n = 0 then Cps.return (List.rev acc)
    else
      next t >>= function
      | None -> Cps.return (List.rev acc)
      | Some x -> aux (n - 1) (x :: acc)
  in
  aux n []

let rec iter ~f t =
  next t >>= function
  | None -> Cps.return ()
  | Some v -> f v >>= fun () -> iter ~f t

let drain t = iter ~f:(fun _ -> Cps.return ()) t

let of_list xs =
  let xs = ref xs in
  let fn () =
    match !xs with
    | [] -> Cps.return None
    | x :: xs' ->
        xs := xs';
        Cps.return (Some x)
  in
  from_fn fn
