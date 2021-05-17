type t [@@deriving sexp]

val create : ?version:Version.t -> ?headers:Headers.t -> Meth.t -> string -> t
