open H1_types

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type body_stream

val iter_body : body_stream -> f:(string -> unit Lwt.t) -> unit Lwt.t
val iter_body' : body_stream -> f:(string -> unit) -> unit Lwt.t

type service =
  Request.t * body_stream ->
  (Response.t * [ `Bigstring of bigstring | `String of string ]) Lwt.t

type conn

val create :
  read:(bigstring -> pos:int -> len:int -> int Lwt.t) ->
  read_buffer_size:int ->
  write:(bigstring -> pos:int -> len:int -> unit Lwt.t) ->
  write_buffer_size:int ->
  conn

val run : conn -> service -> unit Lwt.t
