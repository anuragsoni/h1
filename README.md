# H1

h1 is a simple http 1.1 implementation. The core protocol layout doesn't enforce any concurrency strategry
and as a result it is possible to provide small translation modules that can provide an Lwt or Async aware interface.


### Warning
This is currently a proof-of-concept. There are many rough edges and probably many bugs. Use it at your own risk.
That said, the approach seems to work well and in my initial tests the performance seems pretty decent. With some more effort it should be possible to get to a respectable state with extensive tests.

## Features
* Asynchronous pull based design
* Execution environment agnostic.
    * The library should work on unix, windows and mirageOS, just provide the following functions when creating a connection:
        ```ocaml
        write:(Bigstringaf.t -> pos:int -> len:int -> int promise)
        refill:(Bigstringaf.t -> pos:int -> len:int -> int promise)
        ```

## Usage

### HTTP Server

Example using lwt:

<!-- $MDX file=example/main.ml,part=simple_server -->
```ocaml
let run (sock : Lwt_unix.file_descr) =
  let service (_req, body) =
    let body = H1_lwt.Body.to_string_stream body in
    H1_lwt.iter
      ~f:(fun x ->
        Logs.info (fun m -> m "%s" x);
        Lwt_result.return ())
      body
    >>= fun () ->
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Bigstringaf.length text)) ])
        `Ok
    in
    Lwt_result.return (resp, `Bigstring text)
  in
  Lwt.catch
    (fun () ->
      H1_lwt.run_server ~read_buf_size:(10 * 1024) ~write_buf_size:(10 * 1024)
        ~write:(fun buf ~pos ~len ->
          let%lwt c = Lwt_bytes.write sock buf pos len in
          Lwt_result.return c)
        ~refill:(fun buf ~pos ~len ->
          let%lwt c = Lwt_bytes.read sock buf pos len in
          Lwt_result.return c)
        service)
    (fun exn ->
      Logs.err (fun m -> m "%s" (Printexc.to_string exn));
      Lwt_result.return ())
```

Example using async:

<!-- $MDX file=example/main_async.ml,part=simple_server -->
```ocaml
let run (sock : Fd.t) =
  let service (_req, body) =
    let body = H1_async.Body.to_string_stream body in
    let%bind.Deferred.Or_error () =
      H1_async.iter
        ~f:(fun x ->
          Logs.info (fun m -> m "%s" x);
          Deferred.Or_error.return ())
        body
    in
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Bigstring.length text)) ])
        `Ok
    in
    return (Ok (resp, `Bigstring text))
  in
  H1_async.run_server ~read_buf_size:(10 * 1024) ~write_buf_size:(10 * 1024)
    ~write:(fun buf ~pos ~len -> H1_async.write_nonblock sock buf ~pos ~len)
    ~refill:(fun buf ~pos ~len -> H1_async.read_nonblock sock buf ~pos ~len)
    service
```

## Todo
- [x] Chunked Encoding
- [ ] 0 copy streaming for bodies
- [ ] Better error handling
- [ ] Add docs
- [ ] Http client implementation (This can be pushed till after a full working/tested/documented server implementation)
