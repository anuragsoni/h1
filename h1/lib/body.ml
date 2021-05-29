type t =
  [ `String of string
  | `Bigstring of Bigstringaf.t
  | `Stream of string Pull.t
  | `Iovecs of Iovec.t Pull.t ]

let drain = function
  | `String _ | `Bigstring _ -> Cps.return ()
  | `Stream xs -> Pull.drain xs
  | `Iovecs xs -> Pull.drain xs

let to_string_stream = function
  | `String s -> Pull.of_list [ s ]
  | `Bigstring b -> Pull.of_list [ Bigstringaf.to_string b ]
  | `Stream xs -> xs
  | `Iovecs xs ->
      Pull.map
        ~f:(fun iovec ->
          Bigstringaf.substring iovec.Iovec.buf ~off:iovec.pos ~len:iovec.len)
        xs
