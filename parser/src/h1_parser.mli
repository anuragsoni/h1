type error = Msg of string | Partial [@@deriving sexp]
type http_version = Http_1_0 | Http_1_1 [@@deriving sexp]

type request = {
  meth : Meth.t;
  path : string;
  version : http_version;
  headers : (string * string) list;
}
[@@deriving sexp]

val parse_headers :
  ?off:int ->
  ?len:int ->
  Bigstringaf.t ->
  ((string * string) list * int, error) result

val parse_request :
  ?off:int -> ?len:int -> Bigstringaf.t -> (request * int, error) result
(** Attempts to parse a buffer into a HTTP request. If successful, it returns
    the parsed request and an offset value that indicates the starting point of
    unconsumed content left in the buffer. *)

val parse_chunk_length :
  ?off:int -> ?len:int -> Bigstringaf.t -> (int64 * int, error) result
