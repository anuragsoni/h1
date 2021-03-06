type t = { buf : Bigstringaf.t; pos : int; len : int }

let of_bigstring ?pos ?len buf =
  let buf_len = Bigstringaf.length buf in
  let pos = Option.value pos ~default:0 in
  if pos < 0 || pos > buf_len then
    invalid_arg
      (Printf.sprintf
         "H1_lwt.IOVec.of_bigstring: Invalid pos %d. Buffer length: %d" pos
         buf_len);
  let len = Option.value len ~default:(buf_len - pos) in
  if len < 0 || pos + len > buf_len then
    invalid_arg
      (Printf.sprintf
         "H1_lwt.IOVec.of_bigstring: Invalid len %d. offset: %d, \
          buffer_length: %d, requested_length: %d"
         len pos buf_len (pos + len));
  { buf; pos; len }
