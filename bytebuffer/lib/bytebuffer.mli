type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Iovec : sig
  type t = private { buffer : bigstring; pos : int; len : int }

  val of_bigstring : ?pos:int -> ?len:int -> bigstring -> t
end

type t

module View : sig
  type t = private {
    buffer : bigstring;
    pos : int;
    len : int;
    continue : int -> unit;
  }
end

val create : int -> t
val of_bigstring : bigstring -> t
val contents : t -> bigstring
val contents_string : t -> string
val length : t -> int
val capacity : t -> int
val clear : t -> unit
val reset : t -> unit
val resize : t -> int -> unit
val add_char : t -> char -> unit
val add_string : t -> string -> unit
val fill : t -> View.t
val consume : t -> View.t
val add_bigstring : t -> bigstring -> unit
val add_iovec : t -> Iovec.t -> unit
val addf : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
val index : ?pos:int -> ?len:int -> char -> t -> int option
val unsafe_index : ?pos:int -> ?len:int -> char -> t -> int
val drop : t -> int -> unit
