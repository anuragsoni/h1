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

let of_string = function
  | "GET" -> Some `GET
  | "HEAD" -> Some `HEAD
  | "POST" -> Some `POST
  | "PUT" -> Some `PUT
  | "DELETE" -> Some `DELETE
  | "CONNECT" -> Some `CONNECT
  | "OPTIONS" -> Some `OPTIONS
  | "TRACE" -> Some `TRACE
  | "PATCH" -> Some `PATCH
  | _ -> None
