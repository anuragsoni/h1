type t = Http_1_0 | Http_1_1 [@@deriving sexp]

let to_string = function Http_1_0 -> "HTTP/1.0" | Http_1_1 -> "HTTP/1.1"
