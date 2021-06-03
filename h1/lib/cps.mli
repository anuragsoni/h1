type 'a t

val return : 'a -> 'a t
val fail : [ `Msg of string | `Exn of exn ] -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
end

val run :
  'a t -> ([ `Msg of string | `Exn of exn ] -> unit) -> ('a -> unit) -> unit

val make :
  (([ `Msg of string | `Exn of exn ] -> unit) -> ('a -> unit) -> unit) -> 'a t
