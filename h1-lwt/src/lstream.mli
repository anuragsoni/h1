type 'a t

val from_fn : (unit -> 'a option Lwt.t) -> 'a t
val next : 'a t -> 'a option Lwt.t
val pushback : 'a t -> 'a -> unit
val iter : f:('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
val drain : 'a t -> unit Lwt.t
