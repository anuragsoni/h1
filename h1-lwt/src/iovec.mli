type t = private { buf : Bigstringaf.t; pos : int; len : int }

val of_bigstring : ?pos:int -> ?len:int -> Bigstringaf.t -> t
