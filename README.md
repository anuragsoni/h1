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

```ocaml
let service (req, body) =
    let message = "Hello, World!" in
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (String.length message)) ])
        `Ok
    in
    Lwt.return (resp, `String message)

let run_server listen_address =
    Lwt_io.establish_server_with_client_socket listen_address
    (fun _ sock ->
        let conn =
            Connection.create ~read_buf_size:(10 * 1024)
                ~write_buf_size:(10 * 1024)
                ~write:(fun buf ~pos ~len -> Lwt_bytes.write sock buf pos len)
                ~refill:(fun buf ~pos ~len -> Lwt_bytes.read sock buf pos len)
        in
        Connection.run conn service)
```

## Todo
- [ ] Chunked Encoding
- [ ] Http client implementation
- [ ] 0 copy streaming for bodies
- [ ] Better error handling
- [ ] Add docs