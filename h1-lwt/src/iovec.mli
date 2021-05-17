type t = private { buf : (Bigstringaf.t[@sexp.opaque]); pos : int; len : int }
[@@deriving sexp_of]

val of_bigstring : ?pos:int -> ?len:int -> Bigstringaf.t -> t
