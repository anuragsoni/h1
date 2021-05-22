type t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
val empty : t
val of_list : (string * string) list -> t
val to_list : t -> (string * string) list
val iteri : f:(key:string -> data:string -> unit) -> t -> unit
val find : t -> string -> string option
val find_multi : t -> string -> string list
val get_transfer_encoding : t -> [ `Bad_request | `Chunked | `Fixed of int64 ]
val client_waiting_for_100_continue : t -> bool
