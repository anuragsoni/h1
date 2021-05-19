type t = {
  meth : Meth.t;
  path : string;
  version : Version.t;
  headers : Headers.t;
}

let create ?(version = Version.Http_1_1) ?(headers = Headers.empty) meth path =
  { meth; path; version; headers }

let meth t = t.meth
let path t = t.path
let version t = t.version
let headers t = t.headers

let pp =
  let meth = Fmt.Dump.field "meth" (fun t -> t.meth) Meth.pp in
  let path = Fmt.Dump.field "path" (fun t -> t.path) Fmt.Dump.string in
  let version = Fmt.Dump.field "version" (fun t -> t.version) Version.pp in
  let headers = Fmt.Dump.field "headers" (fun t -> t.headers) Headers.pp in
  Fmt.Dump.record [ meth; path; version; headers ]
