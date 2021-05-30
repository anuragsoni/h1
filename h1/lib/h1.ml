module Bigbuffer = Bigbuffer
module Iovec = Iovec
module Cps = Cps
open H1_types

module type IO = sig
  type +'a t

  val of_cps : 'a Cps.t -> 'a t
  val to_cps : 'a t -> 'a Cps.t
end

type ('a, 'kind) stream = 'a Pull.t

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

  type service = Request.t * Body.t -> (Response.t * Body.t) promise

  val run_server :
    read_buf_size:int ->
    write_buf_size:int ->
    refill:(Bigstringaf.t -> pos:int -> len:int -> int promise) ->
    write:(Bigstringaf.t -> pos:int -> len:int -> int promise) ->
    service ->
    unit promise
end

module Async (IO : IO) = struct
  let create_stream fn =
    let fn () = IO.to_cps (fn ()) in
    Pull.from_fn fn

  let next stream = IO.of_cps (Pull.next stream)

  let iter t ~f =
    let f x = IO.to_cps (f x) in
    IO.of_cps @@ Pull.iter ~f t

  let stream_of_list xs = Pull.of_list xs

  module Body = struct
    include Body

    let drain t = IO.of_cps (drain t)
  end

  type service = Request.t * Body.t -> (Response.t * Body.t) IO.t

  let run_server ~read_buf_size ~write_buf_size ~refill ~write service =
    let write buf ~pos ~len = IO.to_cps (write buf ~pos ~len) in
    let refill buf ~pos ~len = IO.to_cps (refill buf ~pos ~len) in
    let service req = IO.to_cps (service req) in
    IO.of_cps
    @@ Http_server.run ~read_buf_size ~write_buf_size ~write ~refill service
end
