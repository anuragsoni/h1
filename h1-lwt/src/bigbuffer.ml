type t = {
  mutable buffer : Bigstringaf.t;
  mutable pos : int;
  mutable len : int;
  init : Bigstringaf.t;
}

module Logger = (val Logs.src_log (Logs.Src.create "h1_lwt.bigbuffer"))

let create size =
  let buffer = Bigstringaf.create size in
  { buffer; pos = 0; len = size; init = buffer }

let contents buf = Bigstringaf.sub buf.buffer ~off:0 ~len:buf.pos
let length buf = buf.pos
let clear buf = buf.pos <- 0

let reset buf =
  buf.pos <- 0;
  buf.buffer <- buf.init;
  buf.len <- Bigstringaf.length buf.buffer

let resize buf size =
  let new_len = (buf.len + size) * 2 in
  Logger.debug (fun m ->
      m "Resizing bigbuffer. Old_len: %d, new_len: %d" buf.len new_len);
  let new_buffer = Bigstringaf.create new_len in
  Bigstringaf.blit buf.buffer ~src_off:0 new_buffer ~dst_off:0 ~len:buf.len;
  buf.buffer <- new_buffer;
  buf.len <- new_len

let add_char buf c =
  let pos = buf.pos in
  if pos >= buf.len then resize buf 1;
  Bigstringaf.unsafe_set buf.buffer pos c;
  buf.pos <- pos + 1

let add_string buf s =
  let len = String.length s in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.unsafe_blit_from_string s ~src_off:0 buf.buffer ~dst_off:buf.pos
    ~len;
  buf.pos <- new_pos

let fill ~f t =
  if t.pos >= t.len then resize t 0;
  let%lwt count =
    f t.buffer ~pos:t.pos ~len:(Bigstringaf.length t.buffer - t.pos)
  in
  t.pos <- t.pos + count;
  Lwt.return count

let add_bigstring buf s =
  let len = Bigstringaf.length s in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.unsafe_blit s ~src_off:0 buf.buffer ~dst_off:buf.pos ~len;
  buf.pos <- new_pos

let consume ~f t =
  let res, count = f t.buffer ~pos:0 ~len:t.pos in
  if count < 0 || count > t.pos then
    invalid_arg "Bigbuffer.consume: Invalid response from f consuming buffer.";
  Bigstringaf.blit t.buffer ~src_off:count ~dst_off:0
    ~len:(length t - count)
    t.buffer;
  t.pos <- t.pos - count;
  res

let consume' ~f t =
  let%lwt res, count = f t.buffer ~pos:0 ~len:t.pos in
  if count < 0 || count > t.pos then
    invalid_arg "Bigbuffer.consume: Invalid response from f consuming buffer.";
  Bigstringaf.blit t.buffer ~src_off:count ~dst_off:0
    ~len:(length t - count)
    t.buffer;
  t.pos <- t.pos - count;
  Lwt.return res
