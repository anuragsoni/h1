type t

val create : int -> t
val write_string : t -> string -> unit
val write_char : t -> char -> unit
val write_bigstring : t -> Bigstringaf.t -> unit
val write_iovec : t -> Iovec.t -> unit
val flushed : t -> unit Lwt.t

val write_all :
  write:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) -> t -> unit Lwt.t
