type t = {
  version : Version.t;
  status : Status.t;
  reason_phrase : string;
  headers : Headers.t;
}
[@@deriving sexp_of]

val create :
  ?version:Version.t ->
  ?reason_phrase:string ->
  ?headers:Headers.t ->
  Status.t ->
  t
