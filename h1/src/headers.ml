open! Base

type t = (string, string) List.Assoc.t [@@deriving sexp]

let empty = []
let of_list xs = xs
let to_list headers = headers
let iteri ~f headers = List.iter headers ~f:(fun (key, data) -> f ~key ~data)
