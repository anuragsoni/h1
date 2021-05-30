open H1_types

type service = Request.t * Body.t -> (Response.t * Body.t) Cps.t

val run :
  read_buf_size:int ->
  write_buf_size:int ->
  refill:(Bigstringaf.t -> pos:int -> len:int -> int Cps.t) ->
  write:(Bigstringaf.t -> pos:int -> len:int -> int Cps.t) ->
  service ->
  unit Cps.t
