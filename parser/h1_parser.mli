type err = Partial | Failure of string [@@deriving sexp]

type http_version = Http_1_0 | Http_1_1 [@@deriving sexp]

val parse_request :
  Bigstringaf.t ->
  ( (string * string * http_version * (string * string) list) * int,
    err )
  result
