type t

val create : int -> t
val contents : t -> Bigstringaf.t
val length : t -> int
val clear : t -> unit
val reset : t -> unit
val resize : t -> int -> unit
val add_char : t -> char -> unit
val add_string : t -> string -> unit

val fill :
  f:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) -> t -> int Lwt.t

val add_bigstring : t -> Bigstringaf.t -> unit
val consume : f:(Bigstringaf.t -> pos:int -> len:int -> 'a * int) -> t -> 'a

val consume' :
  f:(Bigstringaf.t -> pos:int -> len:int -> ('a * int) Lwt.t) -> t -> 'a Lwt.t
