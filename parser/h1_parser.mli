type err = Partial | Failure of string [@@deriving sexp]

val parse_request :
  Bigstringaf.t ->
  ((string * string * int * (string * string) list) * int, err) result
