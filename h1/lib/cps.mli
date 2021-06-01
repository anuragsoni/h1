type 'a t

include Base.Monad.S with type 'a t := 'a t

val run : 'a t -> (Base.Error.t -> unit) -> ('a -> unit) -> unit
val make : ((Base.Error.t -> unit) -> ('a -> unit) -> unit) -> 'a t
val fail : Base.Error.t -> 'a t
