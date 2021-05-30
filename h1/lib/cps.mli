type 'a t

val return : 'a -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val run : 'a t -> ('a -> unit) -> unit
val make : (('a -> unit) -> unit) -> 'a t
