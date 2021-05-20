type t = String of string | Bigstring of Bigstringaf.t

let of_string s = String s
let of_bigstring b = Bigstring b

let write writer b =
  match b with
  | String s -> Writer.write_string writer s
  | Bigstring b -> Writer.write_bigstring writer b
