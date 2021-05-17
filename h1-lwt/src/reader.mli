type t [@@deriving sexp_of]

val create : int -> t
val length : t -> int
val is_empty : t -> bool

val fill :
  f:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t) ->
  t ->
  [> `Eof | `Ok of int ] Lwt.t

val read : f:(Bigstringaf.t -> pos:int -> len:int -> 'a * int) -> t -> 'a
