module Bigbuffer = Bigbuffer
module Iovec = Iovec
module Cps = Cps

module type IO = sig
  type +'a t

  val of_cps : 'a Cps.t -> 'a t
  val to_cps : 'a t -> 'a Cps.t
end

type ('a, 'kind) stream

open H1_types

module type ASYNC = sig
  type 'a promise

  val create_stream : (unit -> 'a option promise) -> ('a, [ `Async ]) stream
  val next : ('a, [ `Async ]) stream -> 'a option promise
  val iter : ('a, [ `Async ]) stream -> f:('a -> unit promise) -> unit promise
  val stream_of_list : 'a list -> ('a, [ `Async ]) stream

  module Body : sig
    type t =
      [ `String of string
      | `Bigstring of Bigstringaf.t
      | `Stream of (string, [ `Async ]) stream
      | `Iovecs of (Iovec.t, [ `Async ]) stream ]

    val drain : t -> unit promise
    val to_string_stream : t -> (string, [ `Async ]) stream
  end

  module Http_server : sig
    type service = Request.t * Body.t -> (Response.t * Body.t) promise

    val run :
      read_buf_size:int ->
      write_buf_size:int ->
      refill:(Bigstringaf.t -> pos:int -> len:int -> int promise) ->
      write:(Bigstringaf.t -> pos:int -> len:int -> int promise) ->
      service ->
      unit promise
  end
end

module Async (IO : IO) : ASYNC with type 'a promise := 'a IO.t
