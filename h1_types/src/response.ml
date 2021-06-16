type t = {
  version : Version.t;
  status : Status.t;
  reason_phrase : string;
  headers : Headers.t;
}

let create ?(version = Version.Http_1_1) ?reason_phrase
    ?(headers = Headers.empty) status =
  let reason_phrase =
    Option.value reason_phrase ~default:(Status.to_reason_phrase status)
  in
  { version; status; reason_phrase; headers }

let version t = t.version
let status t = t.status
let reason_phrase t = t.reason_phrase
let headers t = t.headers

let pp =
  let version = Fmt.Dump.field "version" (fun t -> t.version) Version.pp in
  let status = Fmt.Dump.field "status" (fun t -> t.status) Status.pp in
  let reason_phrase =
    Fmt.Dump.field "reason_phrase" (fun t -> t.reason_phrase) Fmt.Dump.string
  in
  let headers = Fmt.Dump.field "headers" (fun t -> t.headers) Headers.pp in
  Fmt.Dump.record [ version; status; reason_phrase; headers ]
