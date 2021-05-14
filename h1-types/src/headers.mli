open Base

type t [@@deriving sexp]

val empty : t
val of_list : (string, string) List.Assoc.t -> t
val to_list : t -> (string, string) List.Assoc.t
val iteri : f:(key:string -> data:string -> unit) -> t -> unit
val find : t -> string -> string option
