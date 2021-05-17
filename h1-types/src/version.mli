type t = Http_1_0 | Http_1_1 [@@deriving sexp]

val to_string : t -> string
