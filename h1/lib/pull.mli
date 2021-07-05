type 'a t

val from_fn : (unit -> 'a option Cps.t) -> 'a t
val next : 'a t -> 'a option Cps.t
val iter : f:('a -> unit Cps.t) -> 'a t -> unit Cps.t
val drain : 'a t -> unit Cps.t
val of_list : 'a list -> 'a t
val map : f:('a -> 'b) -> 'a t -> 'b t
val pushback : 'a t -> 'a -> unit
val concat : 'a t t -> 'a t
val concat_map : f:('a -> 'b t) -> 'a t -> 'b t
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b Cps.t) -> 'b Cps.t
val take : 'a t -> int -> 'a list Cps.t
