type t =
  [ `String of string
  | `Bigstring of Bigstringaf.t
  | `Stream of string Lstream.t
  | `Iovecs of Iovec.t Lstream.t ]

val drain : t -> unit Lwt.t
val to_string_stream : t -> string Lstream.t
