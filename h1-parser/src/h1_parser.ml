(* TODO: Remove this if https://github.com/inhabitedtype/bigstringaf/pulls gets
   merged. *)
external unsafe_memchr : Bigstringaf.t -> int -> char -> int -> int
  = "bigstringaf_memchr"
  [@@noalloc]

module Source = struct
  type t = {
    buffer : Bigstringaf.t;
    mutable off : int;
    min_off : int;
    upper_bound : int;
  }

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

  let to_string t ~off ~len =
    if
      off < 0
      || t.off + off >= t.upper_bound
      || len < 0
      || t.off + off + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "H1_parser.Source.substring: Index out of bounds., Requested off: \
            %d, len: %d"
           off len);
    Bigstringaf.substring t.buffer ~off:(t.off + off) ~len

  let consumed t = t.off - t.min_off

  let index t ch =
    let res = unsafe_memchr t.buffer t.off ch (length t) in
    if res = -1 then -1 else res - t.off

  let for_all t ~off ~len ~f =
    if
      off < 0
      || t.off + off >= t.upper_bound
      || len < 0
      || t.off + off + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "H1_parser.Source.substring: Index out of bounds. Requested off: \
            %d, len: %d"
           off len);
    let idx = ref off in
    while !idx < len && f (get t !idx) do
      incr idx
    done;
    if !idx = len then true else false
end

type error = Msg of string | Partial

type 'a parser = { run : 'r. Source.t -> (error -> 'r) -> ('a -> 'r) -> 'r }
[@@unboxed]

let ( let+ ) t f =
  {
    run =
      (fun source on_err on_succ ->
        t.run source on_err (fun v -> on_succ (f v)));
  }

let ( let* ) t f =
  {
    run =
      (fun source on_err on_succ ->
        t.run source on_err (fun v -> (f v).run source on_err on_succ));
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

(* token = 1*tchar

   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
   / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters *)

let is_tchar = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
      true
  | _ -> false

let token =
  let run source on_err on_succ =
    let pos = Source.index source ' ' in
    if pos = -1 then on_err Partial
    else
      let res = Source.to_string source ~off:0 ~len:pos in
      Source.advance source (pos + 1);
      on_succ res
  in
  { run }

let meth =
  let run source on_err on_succ =
    token.run source on_err (fun token ->
        match H1_types.Meth.of_string token with
        | None -> on_err (Msg "Invalid http verb")
        | Some m -> on_succ m)
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
          with_eof source on_err on_succ H1_types.Version.Http_1_0
      | '1' ->
          Source.advance source 1;
          with_eof source on_err on_succ H1_types.Version.Http_1_1
      | c -> on_err (Msg (Printf.sprintf "Invalid http version number 1.%c" c)))
    else on_err (Msg "Invalid http version header")
  in
  { run }

let parse_header source =
  let pos = Source.index source ':' in
  if pos = -1 then Error Partial
  else if pos = 0 then Error (Msg "Invalid header: Empty header key")
  else if Source.for_all source ~off:0 ~len:pos ~f:is_tchar then (
    let key = Source.to_string source ~off:0 ~len:pos in
    Source.advance source (pos + 1);
    while Source.length source > 0 && Source.get source 0 = ' ' do
      Source.advance source 1
    done;
    let pos = Source.index source '\r' in
    if pos = -1 then Error Partial
    else
      let v = Source.to_string source ~off:0 ~len:pos in
      Source.advance source pos;
      with_eof source (fun e -> Error e) (fun v -> Ok v) (key, String.trim v))
  else Error (Msg "Invalid Header Key")

let headers =
  let run source on_err on_succ =
    let rec loop acc =
      let len = Source.length source in
      if len > 0 && Source.get source 0 = '\r' then
        with_eof source on_err on_succ (H1_types.Headers.of_list @@ List.rev acc)
      else
        match parse_header source with
        | Error e -> on_err e
        | Ok v -> loop (v :: acc)
    in
    loop []
  in
  { run }

let chunk_length =
  let run source on_err on_succ =
    let ( lsl ) = Int64.shift_left in
    let ( lor ) = Int64.logor in

    let length = ref 0L in
    let stop = ref false in
    let state = ref `Ok in
    let count = ref 0 in

    let processing_chunk = ref true in

    let in_chunk_extension = ref false in

    while not !stop do
      if Source.length source = 0 then (
        stop := true;
        state := `Partial)
      else if !count = 16 && not !in_chunk_extension then (
        stop := true;
        state := `Chunk_too_big)
      else
        let ch = Source.get source 0 in
        Source.advance source 1;
        incr count;
        match ch with
        | '0' .. '9' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code '0') in
            length := (!length lsl 4) lor curr
        | 'a' .. 'f' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code 'a' + 10) in
            length := (!length lsl 4) lor curr
        | 'A' .. 'F' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code 'A' + 10) in
            length := (!length lsl 4) lor curr
        | ';' when not !in_chunk_extension ->
            in_chunk_extension := true;
            processing_chunk := false
        | ('\t' | ' ') when !processing_chunk -> processing_chunk := false
        | ('\t' | ' ') when (not !in_chunk_extension) && not !processing_chunk
          ->
            ()
        | '\r' ->
            if Source.length source = 0 then (
              stop := true;
              state := `Partial)
            else if Source.get source 0 = '\n' then (
              Source.advance source 1;
              stop := true)
            else (
              stop := true;
              state := `Expected_newline)
        | _ when !in_chunk_extension ->
            (* Chunk extensions aren't very common, see:
               https://tools.ietf.org/html/rfc7230#section-4.1.1

               Chunk extensions aren't pre-defined, and they are specific to
               invidividual connections. In the future we might surface these to
               the user somehow, but for now we will ignore any extensions.

               TODO: Should there be any limit on the size of chunk extensions
               we parse? We might want to error if a request contains really
               large chunk extensions. *)
            ()
        | ch ->
            stop := true;
            state := `Invalid_char ch
    done;
    match !state with
    | `Ok -> on_succ !length
    | `Partial -> on_err Partial
    | `Expected_newline -> on_err (Msg "Expected_newline")
    | `Chunk_too_big -> on_err (Msg "Chunk size is too large")
    | `Invalid_char ch ->
        on_err (Msg (Printf.sprintf "Invalid chunk_length character %C" ch))
  in

  { run }

let request =
  let+ meth = meth and+ path = token and+ v = version and+ headers = headers in
  H1_types.Request.create ~version:v ~headers meth path

let parse_chunk =
  let* chunk_length = chunk_length in
  let run source on_err on_succ =
    let chunk_length = Int64.to_int chunk_length in
    if chunk_length = 0 then with_eof source on_err on_succ None
    else if Source.length source < chunk_length + 2 then on_err Partial
    else
      let chunk = Source.to_string source ~off:0 ~len:chunk_length in
      Source.advance source chunk_length;
      with_eof source on_err on_succ (Some chunk)
  in
  { run }

let run_parser ?off ?len buf p =
  let source = Source.of_bigstring ?off ?len buf in
  p.run source (fun e -> Error e) (fun v -> Ok (v, Source.consumed source))

let parse_request ?off ?len buf = run_parser ?off ?len buf request
let parse_headers ?off ?len buf = run_parser ?off ?len buf headers
let parse_chunk_length ?off ?len buf = run_parser ?off ?len buf chunk_length
let parse_chunk ?off ?len buf = run_parser ?off ?len buf parse_chunk
