module Bigbuffer = Bigbuffer
module Iovec = Iovec

module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

module Make (IO : IO) : sig
  module Pull : sig
    type 'a t

    val from_fn : (unit -> 'a option IO.t) -> 'a t
    val iter : f:('a -> unit IO.t) -> 'a t -> unit IO.t
    val pushback : 'a t -> 'a -> unit
    val concat : 'a t t -> 'a t
    val concat_map : f:('a -> 'b t) -> 'a t -> 'b t
    val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b IO.t) -> 'b IO.t
    val take : 'a t -> int -> 'a list IO.t
  end

  module Body : sig
    type t =
      [ `String of string
      | `Bigstring of Bigstringaf.t
      | `Stream of string Pull.t
      | `Iovecs of Iovec.t Pull.t ]

    val drain : t -> unit IO.t
    val to_string_stream : t -> string Pull.t
  end

  module Http_server : sig
    open H1_types

    type service = Request.t * Body.t -> (Response.t * Body.t) IO.t

    val run :
      read_buf_size:int ->
      write_buf_size:int ->
      refill:(Bigstringaf.t -> pos:int -> len:int -> int IO.t) ->
      write:(Bigstringaf.t -> pos:int -> len:int -> int IO.t) ->
      service ->
      unit IO.t
  end
end
