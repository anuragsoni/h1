open Async
include H1.ASYNC with type 'a promise := 'a Deferred.t

val to_pipe : ('a, [ `Async ]) H1.stream -> 'a Pipe.Reader.t
val of_pipe : 'a Pipe.Reader.t -> ('a, [ `Async ]) H1.stream
