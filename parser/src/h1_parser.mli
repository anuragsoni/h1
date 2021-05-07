type err = Partial | Failure of string [@@deriving sexp]
type http_version = Http_1_0 | Http_1_1 [@@deriving sexp]

val parse_request :
  ?off:int ->
  ?len:int ->
  Bigstringaf.t ->
  ((string * string * http_version * (string * string) list) * int, err) result
(** Attempts to parse a buffer into a HTTP request. If successful, it returns
    the parsed request and an offset value that indicates the starting point of
    unconsumed content left in the buffer. *)
