open Sexplib0.Sexp_conv

(* TODO: Remove this if https://github.com/inhabitedtype/bigstringaf/pulls gets
   merged. *)
external unsafe_memchr : Bigstringaf.t -> int -> char -> int -> int
  = "bigstringaf_memchr"
  [@@noalloc]

module Source = struct
  type t = {
    buffer : (Bigstringaf.t[@sexp.opaque]);
    mutable off : int;
    min_off : int;
    upper_bound : int;
  }
  [@@deriving sexp_of]

  let of_bigstring ?off ?len buffer =
    let buf_len = Bigstringaf.length buffer in
    let off = Option.value off ~default:0 in
    if off < 0 || off > buf_len then
      invalid_arg
        (Printf.sprintf
           "H1_parser.Source.of_bigstring: Invalid offset %d. Buffer length: %d"
           off buf_len);
    let len = Option.value len ~default:(buf_len - off) in
    if len < 0 || off + len > buf_len then
      invalid_arg
        (Printf.sprintf
           "H1_parse.Source.of_bigstring: Invalid len %d. offset: %d, \
            buffer_length: %d, requested_length: %d"
           len off buf_len (off + len));
    { buffer; off; min_off = off; upper_bound = off + len }

  let get t idx =
    if idx < 0 || t.off + idx >= t.upper_bound then
      invalid_arg "H1_parser.Source.get: Index out of bounds";
    Bigstringaf.unsafe_get t.buffer (t.off + idx)

  let advance t count =
    if count < 0 || t.off + count > t.upper_bound then
      invalid_arg
        (Printf.sprintf
           "H1_parser.Source.advance: Index out of bounds. Requested count: %d"
           count);
    t.off <- t.off + count

  let length t = t.upper_bound - t.off
  (* let to_string t = Bigstringaf.substring t.buffer ~off:t.off ~len:(length t) *)

  let substring t ~off ~len =
    if
      off < 0
      || t.off + off >= t.upper_bound
      || len < 0
      || t.off + off + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "H1_parser.Source.substring: Index out of bounds. source: %a, \
            Requested off: %d, len: %d"
           Sexplib0.Sexp.pp_mach (sexp_of_t t) off len);
    Bigstringaf.substring t.buffer ~off:(t.off + off) ~len

  let consumed t = t.off - t.min_off

  let index t ch =
    let res = unsafe_memchr t.buffer t.off ch (length t) in
    if res = -1 then -1 else res - t.off
end

type error = Msg of string | Partial [@@deriving sexp]

type 'a parser = { run : 'r. Source.t -> (error -> 'r) -> ('a -> 'r) -> 'r }
[@@unboxed]

type http_version = Http_1_0 | Http_1_1 [@@deriving sexp]

let ( let+ ) t f =
  {
    run =
      (fun source on_err on_succ ->
        t.run source on_err (fun v -> on_succ (f v)));
  }

let ( and+ ) a b =
  {
    run =
      (fun source on_err on_succ ->
        a.run source on_err (fun res_a ->
            b.run source on_err (fun res_b -> on_succ (res_a, res_b))));
  }

let with_eof source on_err on_succ res =
  if Source.length source < 2 then on_err Partial
  else if
    Bigstringaf.unsafe_memcmp_string source.buffer source.off "\r\n" 0 2 = 0
  then (
    Source.advance source 2;
    on_succ res)
  else on_err (Msg "Expected eof")

let token =
  let run source on_err on_succ =
    let pos = Source.index source ' ' in
    if pos = -1 then on_err Partial
    else
      let res = Source.substring source ~off:0 ~len:pos in
      Source.advance source (pos + 1);
      on_succ res
  in
  { run }

let version =
  let run source on_err on_succ =
    if Source.length source < 8 then on_err Partial
    else if
      Bigstringaf.unsafe_memcmp_string source.buffer source.off "HTTP/1." 0 7
      = 0
    then (
      Source.advance source 7;
      match Source.get source 0 with
      | '0' ->
          Source.advance source 1;
          with_eof source on_err on_succ Http_1_0
      | '1' ->
          Source.advance source 1;
          with_eof source on_err on_succ Http_1_1
      | c -> on_err (Msg (Printf.sprintf "Invalid http version number 1.%c" c)))
    else on_err (Msg "Invalid http version header")
  in
  { run }

let header =
  let run source on_err on_succ =
    let len = Source.length source in
    if len > 0 && Source.get source 0 = '\r' then
      with_eof source on_err on_succ None
    else
      let pos = Source.index source ':' in
      if pos = -1 then on_err Partial
      else
        let key = Source.substring source ~off:0 ~len:pos in
        Source.advance source (pos + 1);
        while Source.length source > 0 && Source.get source 0 = ' ' do
          Source.advance source 1
        done;
        let pos = Source.index source '\r' in
        if pos = -1 then on_err Partial
        else
          let v = Source.substring source ~off:0 ~len:pos in
          Source.advance source pos;
          with_eof source on_err on_succ (Some (key, v))
  in
  { run }

let headers =
  let parse_header source =
    header.run source (fun e -> Error e) (fun v -> Ok v)
  in
  let run source on_err on_succ =
    let rec loop acc =
      match parse_header source with
      | Error e -> on_err e
      | Ok None -> on_succ acc
      | Ok (Some v) -> loop (v :: acc)
    in
    loop []
  in
  { run }

type request = {
  meth : string;
  path : string;
  version : http_version;
  headers : (string * string) list;
}
[@@deriving sexp]

let request =
  let+ meth = token and+ path = token and+ v = version and+ headers = headers in
  { meth; path; version = v; headers }

let parse_request ?off ?len buf =
  let source = Source.of_bigstring ?off ?len buf in
  request.run source
    (fun e -> Error e)
    (fun v -> Ok (v, Source.consumed source))
