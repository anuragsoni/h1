type t

val pp : t Fmt.t
val empty : t
val of_list : (string * string) list -> t
val to_list : t -> (string * string) list
val iteri : f:(key:string -> data:string -> unit) -> t -> unit
val find : t -> string -> string option [@@ocaml.toplevel_printer]
