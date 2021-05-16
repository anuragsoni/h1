open Sexplib0.Sexp_conv

type t = (string * string) list [@@deriving sexp]

let empty = []
let of_list xs = xs
let to_list headers = headers

let iteri ~f headers =
  ListLabels.iter headers ~f:(fun (key, data) -> f ~key ~data)

let caseless_equal a b =
  if a == b then true
  else
    let len = String.length a in
    len = String.length b
    &&
    let stop = ref false in
    let idx = ref 0 in
    while (not !stop) && !idx < len do
      let c1 = String.unsafe_get a !idx in
      let c2 = String.unsafe_get b !idx in
      if Char.lowercase_ascii c1 <> Char.lowercase_ascii c2 then stop := true;
      incr idx
    done;
    not !stop

let find t key =
  let rec aux = function
    | [] -> None
    | (k, v) :: _ when caseless_equal k key -> Some v
    | _ :: xs -> aux xs
  in
  aux t
