type t =
  [ `String of string
  | `Bigstring of Bigstringaf.t
  | `Stream of string Pull.t
  | `Iovecs of Iovec.t Pull.t ]

val drain : t -> unit Cps.t
val to_string_stream : t -> string Pull.t
