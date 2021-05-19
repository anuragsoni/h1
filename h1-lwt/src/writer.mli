type t

val create : writev:(Bigstringaf.t Faraday.iovec list -> int Lwt.t) -> int -> t
val write_string : t -> string -> unit
val write_char : t -> char -> unit
val schedule_bigstring : t -> ?pos:int -> ?len:int -> Bigstringaf.t -> unit
val write_bigstring : t -> Bigstringaf.t -> unit
val flushed : t -> unit Lwt.t
val write_all : t -> unit Lwt.t
