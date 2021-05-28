type t = (string * string) list

let pp_header = Fmt.Dump.pair Fmt.Dump.string Fmt.Dump.string
let pp = Fmt.Dump.list pp_header
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

let find_multi t key =
  let rec aux acc = function
    | [] -> List.rev acc
    | (k, v) :: xs when caseless_equal k key -> aux (v :: acc) xs
    | _ :: xs -> aux acc xs
  in
  aux [] t

let get_transfer_encoding headers =
  match List.rev @@ find_multi headers "Transfer-Encoding" with
  | x :: _ when caseless_equal x "chunked" -> `Chunked
  | _x :: _ -> `Bad_request
  | [] -> (
      match
        List.sort_uniq String.compare (find_multi headers "Content-Length")
      with
      | [] -> `Fixed 0L
      (* TODO: check for exceptions when converting to int *)
      | [ x ] -> `Fixed (Int64.of_string x)
      | _ -> `Bad_request)

let client_waiting_for_100_continue headers =
  match find headers "Expect" with
  | Some x when caseless_equal x "100-continue" -> true
  | Some _ -> false
  | None -> false

let keep_alive headers =
  match find headers "connection" with
  | Some x when caseless_equal x "close" -> false
  | _ -> true
