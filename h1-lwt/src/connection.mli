type t
type action = Need_data | Req of H1_types.Request.t | Paused | Close

val create :
  read_buf_size:int ->
  write_buf_size:int ->
  (Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  t

val feed_data :
  f:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  t ->
  [ `Eof | `Ok of int ] Lwt.t

val reset : t -> unit

val write :
  t -> [< `Data of Bigstringaf.t | `Response of H1_types.Response.t ] -> unit

val flushed : t -> unit Lwt.t
val write_all : t -> unit Lwt.t
val next_action : t -> action
