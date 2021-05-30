# H1-lwt

h1-lwt is a simple http 1.1 implementation for [lwt](https://ocsigen.org/lwt/latest/manual/manual).


### Warning
This is currently a proof-of-concept. There are many rough edges and probably many bugs. Use it at your own risk.
That said, the approach seems to work well and in my initial tests the performance seems pretty decent. With some more effort it should be possible to get to a respectable state with extensive tests.

## Features
* Asynchronous pull based design
* Execution environment agnostic.
    * The library should work on unix, windows and mirageOS, just provide the following functions when creating a connection:
        ```ocaml
        write:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t)
        refill:(Bigstringaf.t -> pos:int -> len:int -> int Lwt.t)
        ```

## Usage

### HTTP Server

<!-- $MDX file=example/main.ml,part=simple_server -->
```ocaml
let run (sock : Lwt_unix.file_descr) =
  let open H1_lwt in
  let service (_req, body) =
    let body = Body.to_string_stream body in
    let%lwt () =
      iter
        ~f:(fun x ->
          Logs.info (fun m -> m "%s" x);
          Lwt.return_unit)
        body
    in
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Bigstringaf.length text)) ])
        `Ok
    in
    Lwt.return (resp, `Bigstring text)
  in
  Lwt.catch
    (fun () ->
      Http_server.run ~read_buf_size:(10 * 1024) ~write_buf_size:(10 * 1024)
        ~write:(fun buf ~pos ~len -> Lwt_bytes.write sock buf pos len)
        ~refill:(fun buf ~pos ~len -> Lwt_bytes.read sock buf pos len)
        service)
    (fun exn ->
      Logs.err (fun m -> m "%s" (Printexc.to_string exn));
      Lwt.return_unit)
```

## Todo
- [x] Chunked Encoding
- [ ] 0 copy streaming for bodies
- [ ] Better error handling
- [ ] Add docs
- [ ] Http client implementation (This can be pushed till after a full working/tested/documented server implementation)
