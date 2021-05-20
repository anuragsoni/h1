type t

val of_string : string -> t
val of_bigstring : Bigstringaf.t -> t
val write : Writer.t -> t -> unit
