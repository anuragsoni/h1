open H1_types

type service = Request.t * Body.t -> (Response.t * Body.t) Lwt.t

val run :
  read_buf_size:int ->
  write_buf_size:int ->
  refill:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  write:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  service ->
  unit Lwt.t
