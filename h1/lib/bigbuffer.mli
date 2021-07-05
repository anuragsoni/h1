type t

module View : sig
  type t = private {
    buffer : Bigstringaf.t;
    pos : int;
    len : int;
    continue : int -> unit;
  }
end

val create : int -> t
val contents : t -> Bigstringaf.t
val length : t -> int
val capacity : t -> int
val clear : t -> unit
val reset : t -> unit
val resize : t -> int -> unit
val add_char : t -> char -> unit
val add_string : t -> string -> unit
val fill : t -> View.t
val consume : t -> View.t
val add_bigstring : t -> Bigstringaf.t -> unit
val add_iovec : t -> Iovec.t -> unit
val addf : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
