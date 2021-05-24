type 'a t
type ('a, 'b) conduit = 'a t -> 'b t

val from_fn : (unit -> 'a option Lwt.t) -> 'a t
val next : 'a t -> 'a option Lwt.t
val take : 'a t -> int -> 'a list Lwt.t
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b Lwt.t) -> 'b Lwt.t
val map : f:('a -> 'b) -> 'a t -> 'b t
val pushback : 'a t -> 'a -> unit
val iter : f:('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
val drain : 'a t -> unit Lwt.t
val of_list : 'a list -> 'a t
val through : ('a, 'b) conduit -> 'a t -> 'b t
