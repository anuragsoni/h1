val reader_stream :
  int ->
  (Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  Bigbuffer.View.t Lstream.t
