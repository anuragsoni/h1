open Base

type t = {
  meth : Meth.t;
  path : string;
  version : Version.t;
  headers : Headers.t;
}
[@@deriving sexp]

let create ?(version = Version.Http_1_1) ?(headers = Headers.empty) ~meth ~path
    () =
  { meth; path; version; headers }
