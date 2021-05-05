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
  let to_string t = Bigstringaf.substring t.buffer ~off:t.off ~len:(length t)

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

type err = Partial | Failure of string [@@deriving sexp]

type 'a cps = { run : 'r. Source.t -> (err -> 'r) -> ('a -> 'r) -> 'r }
[@@unboxed]

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
