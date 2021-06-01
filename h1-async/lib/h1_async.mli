open Async

val read_nonblock :
  Fd.t -> Bigstring_unix.t -> pos:int -> len:int -> int Deferred.Or_error.t

val write_nonblock :
  Fd.t -> Bigstring_unix.t -> pos:int -> len:int -> int Deferred.Or_error.t

include H1.ASYNC with type 'a promise := 'a Deferred.Or_error.t

(* val to_pipe : ('a, [ `Async ]) H1.stream -> 'a Pipe.Reader.t *)
(* val of_pipe : 'a Pipe.Reader.t -> ('a, [ `Async ]) H1.stream *)
