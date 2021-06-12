# H1

h1 is a simple http 1.1 implementation. The core protocol layout doesn't enforce any concurrency strategry
and as a result it is possible to provide small translation modules that can provide an Lwt or Async aware interface.


### Warning
This is currently a proof-of-concept. There are many rough edges and probably many bugs. Use it at your own risk.
That said, the approach seems to work well and in my initial tests the performance seems pretty decent. With some more effort it should be possible to get to a respectable state with extensive tests.

## Features/Goals
* Asynchronous pull based design
* No I/O in the core library. Use it with any I/O paradigm (async, sync, threaded, etc)

## Usage

### HTTP Server

Example using lwt:

<!-- $MDX file=example/main.ml,part=simple_server -->
```ocaml
let rec write_all sock buf ~pos ~len =
  let%lwt count = Lwt_bytes.write sock buf pos len in
  if count = len then Lwt.return_unit
  else write_all sock buf ~pos:(pos + count) ~len:(len - count)

let run (sock : Lwt_unix.file_descr) =
  let service (_req, _body) =
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Base_bigstring.length text)) ])
        `Ok
    in
    Lwt.return (resp, `Bigstring text)
  in
  let conn =
    H1_lwt.create ~read_buffer_size:(10 * 1024) ~write_buffer_size:(10 * 1024)
      ~read:(fun buf ~pos ~len -> Lwt_bytes.read sock buf pos len)
      ~write:(write_all sock)
  in
  H1_lwt.run conn service
```

Example using async:

<!-- $MDX file=example/main_async.ml,part=simple_server -->
```ocaml
let run (sock : Fd.t) =
  let service (_req, _body) =
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Base_bigstring.length text)) ])
        `Ok
    in
    return (resp, `Bigstring text)
  in
  let conn =
    H1_async.create sock ~read_buffer_size:(10 * 1024)
      ~write_buffer_size:(10 * 1024)
  in
  H1_async.run conn service
```

## Todo
- [x] Chunked Encoding
- [ ] 0 copy streaming for bodies
- [ ] Better error handling
- [ ] Add docs
- [ ] Http client implementation (This can be pushed till after a full working/tested/documented server implementation)
