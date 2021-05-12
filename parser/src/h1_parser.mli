type error = Msg of string | Partial [@@deriving sexp]

val parse_headers :
  ?off:int -> ?len:int -> Bigstringaf.t -> (H1.Headers.t * int, error) result

val parse_request :
  ?off:int -> ?len:int -> Bigstringaf.t -> (H1.Request.t * int, error) result
(** Attempts to parse a buffer into a HTTP request. If successful, it returns
    the parsed request and an offset value that indicates the starting point of
    unconsumed content left in the buffer. *)

val parse_chunk_length :
  ?off:int -> ?len:int -> Bigstringaf.t -> (int64 * int, error) result
