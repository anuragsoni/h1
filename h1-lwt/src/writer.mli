type t

val create : write:(Iovec.t -> int Lwt.t) -> int -> t
val write_string : t -> string -> unit
val write_char : t -> char -> unit
val write_bigstring : t -> Bigstringaf.t -> unit
val flushed : t -> unit Lwt.t
val write_all : t -> unit Lwt.t
