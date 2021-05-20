type t =
  [ `String of string
  | `Bigstring of Bigstringaf.t
  | `Stream of string Lstream.t
  | `Iovecs of Iovec.t Lstream.t ]

let drain = function
  | `String _ | `Bigstring _ -> Lwt.return_unit
  | `Stream xs -> Lstream.drain xs
  | `Iovecs xs -> Lstream.drain xs

let to_string_stream = function
  | `String s -> Lstream.of_list [ s ]
  | `Bigstring b -> Lstream.of_list [ Bigstringaf.to_string b ]
  | `Stream xs -> xs
  | `Iovecs xs ->
      Lstream.map
        ~f:(fun iovec ->
          Bigstringaf.substring iovec.Iovec.buf ~off:iovec.pos ~len:iovec.len)
        xs
