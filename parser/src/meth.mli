type t =
  [ (* https://datatracker.ietf.org/doc/html/rfc7231#section-4.3 *)
    `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | (* https://datatracker.ietf.org/doc/html/rfc5789 *)
    `PATCH ]
[@@deriving sexp]

val of_string : string -> t option
