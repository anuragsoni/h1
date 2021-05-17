open Sexplib0.Sexp_conv

type t = {
  version : Version.t;
  status : Status.t;
  reason_phrase : string;
  headers : Headers.t;
}
[@@deriving sexp_of]

let create ?(version = Version.Http_1_1) ?reason_phrase
    ?(headers = Headers.empty) status =
  let reason_phrase =
    Option.value reason_phrase ~default:(Status.to_reason_phrase status)
  in
  { version; status; reason_phrase; headers }
