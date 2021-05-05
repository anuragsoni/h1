type err = Partial | Failure of string [@@deriving sexp]
type 'a parser

val request : (string * string * int * (string * string) list) parser
val parse : 'a parser -> Bigstringaf.t -> ('a * int, err) result
