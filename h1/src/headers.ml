open! Base

type t = (string, string) List.Assoc.t [@@deriving sexp]

let empty = []
let of_list xs = xs
let to_list headers = headers
let iteri ~f headers = List.iter headers ~f:(fun (key, data) -> f ~key ~data)
let find t f = List.Assoc.find ~equal:String.Caseless.equal t f
