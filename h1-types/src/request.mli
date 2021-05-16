type t [@@deriving sexp]

val create :
  ?version:Version.t ->
  ?headers:Headers.t ->
  meth:Meth.t ->
  path:string ->
  unit ->
  t
