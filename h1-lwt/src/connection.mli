open H1_types

type t

val create :
  read_buf_size:int ->
  write_buf_size:int ->
  write:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  refill:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  t

type service = Request.t -> (Response.t * Body.t) Lwt.t

val run : t -> service -> unit Lwt.t
