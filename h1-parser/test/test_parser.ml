open Base
open Stdio
open H1_types

let req =
  "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com   \r\n\
   User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; \
   rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
   Accept-Encoding: gzip,deflate\r\n\
   Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
   Keep-Alive: 115\r\n\
   Connection: keep-alive\r\n\
   Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; \
   __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; \
   __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral\r\n\
   \r\n"

let pp_parse_result r =
  match r with
  | Error (H1_parser.Msg msg) -> "Error: " ^ msg
  | Error Partial -> "Error: Need more input"
  | Ok res ->
      let req = Fmt.Dump.field "request" (fun (req, _) -> req) Request.pp in
      let count = Fmt.Dump.field "bytes_consumed" (fun (_, d) -> d) Fmt.int in
      let pp = Fmt.Dump.record [ req; count ] in
      Fmt.str "%a" pp res

let%expect_test "Can parse single request" =
  let buf = Bigstringaf.of_string ~off:0 ~len:(String.length req) req in
  let res = H1_parser.parse_request buf in
  printf "%s" (pp_parse_result res);
  [%expect
    {|
    { "request" =
       { "meth" = GET;
         "path" = "/wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg";
         "version" = HTTP/1.1;
         "headers" =
          [("Host", "www.kittyhell.com");
           ("User-Agent",
            "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9");
           ("Accept",
            "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8");
           ("Accept-Language", "ja,en-us;q=0.7,en;q=0.3");
           ("Accept-Encoding", "gzip,deflate");
           ("Accept-Charset", "Shift_JIS,utf-8;q=0.7,*;q=0.7");
           ("Keep-Alive", "115"); ("Connection", "keep-alive");
           ("Cookie",
            "wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral")] };
      "bytes_consumed" = 706 } |}]

let more_requests =
  "GET / HTTP/1.1\r\n\
   Host: www.reddit.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) \r\n\
  \   Gecko/20100101 Firefox/15.0.1\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: en-us,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate\r\n\
   Connection: keep-alive\r\n\
   \r\n\
   GET /reddit.v_EZwRzV-Ns.css HTTP/1.1\r\n\
   Host: www.redditstatic.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) \
   Gecko/20100101 Firefox/15.0.1\r\n\
   Accept: text/css,*/*;q=0.1\r\n\
   Accept-Language: en-us,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate\r\n\
   Connection: keep-alive\r\n\
   Referer: http://www.reddit.com/\r\n\
   \r\n"

let%expect_test "Can parse starting at an offset within a buffer" =
  let buf =
    Bigstringaf.of_string ~off:0
      ~len:(String.length more_requests)
      more_requests
  in
  let res = H1_parser.parse_request ~off:304 buf in
  printf "%s" @@ pp_parse_result res;
  [%expect
    {|
    { "request" =
       { "meth" = GET;
         "path" = "/reddit.v_EZwRzV-Ns.css";
         "version" = HTTP/1.1;
         "headers" =
          [("Host", "www.redditstatic.com");
           ("User-Agent",
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) Gecko/20100101 Firefox/15.0.1");
           ("Accept", "text/css,*/*;q=0.1");
           ("Accept-Language", "en-us,en;q=0.5");
           ("Accept-Encoding", "gzip, deflate"); ("Connection", "keep-alive");
           ("Referer", "http://www.reddit.com/")] };
      "bytes_consumed" = 315 } |}]

let%expect_test "Informs the caller if the buffer contains partial request" =
  let buf = Bigstringaf.of_string ~off:0 ~len:(String.length req) req in
  let res = H1_parser.parse_request ~off:0 ~len:50 buf in
  printf "%s" @@ pp_parse_result res;
  [%expect {| Error: Need more input |}]

let%expect_test "Rejects any http version that isn't 1.0 or 1.1" =
  let req =
    "GET / HTTP/1.4\r\nHost: www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  let buf = Bigstringaf.of_string req ~off:0 ~len:(String.length req) in
  let res = H1_parser.parse_request ~off:0 ~len:50 buf in
  printf "%s" @@ pp_parse_result res;
  [%expect {| Error: Invalid http version |}]

let%expect_test "Parse request and report offset" =
  let buf =
    "POST / HTTP/1.1\r\n\
     Host: localhost:8080\r\n\
     User-Agent: curl/7.64.1\r\n\
     Accept: */*\r\n\
     Content-Length: 6\r\n\
     Content-Type: application/x-www-form-urlencoded\r\n\
     \r\n\
     foobar"
  in
  let v =
    H1_parser.parse_request
      (Bigstringaf.of_string buf ~off:0 ~len:(String.length buf))
    |> Result.ok
  in
  let req, count = Option.value_exn v in
  printf "%s" (Fmt.str "%a" Request.pp req);
  [%expect
    {|
    { "meth" = POST;
      "path" = "/";
      "version" = HTTP/1.1;
      "headers" =
       [("Host", "localhost:8080"); ("User-Agent", "curl/7.64.1");
        ("Accept", "*/*"); ("Content-Length", "6");
        ("Content-Type", "application/x-www-form-urlencoded")] } |}];
  printf "%d\n" count;
  [%expect {| 147 |}];
  print_endline (String.sub buf ~pos:count ~len:(String.length buf - count));
  [%expect {| foobar |}]

open Base_quickcheck

let%test_unit "parse_chunk_length" =
  Test.run_exn
    (module struct
      type t = int64 [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun num ->
      let payload =
        let s = Printf.sprintf "%Lx\r\n" num in
        Bigstringaf.of_string ~off:0 ~len:(String.length s) s
      in
      match H1_parser.parse_chunk_length payload with
      | Ok res ->
          [%test_eq: int64 * int] res
            (num, String.length (Printf.sprintf "%Lx" num) + 2)
      | Error Partial -> assert false
      | Error (Msg _) -> ())

let%test_unit "chunk length parser works with lowercase and uppercase hex \
               digits" =
  let run_test num str =
    let buf = Bigstringaf.of_string ~off:0 ~len:(String.length str) str in
    match H1_parser.parse_chunk_length buf with
    | Ok res ->
        [%test_eq: int64 * int] res
          (num, String.length (Printf.sprintf "%Lx" num) + 2)
    | Error Partial -> assert false
    | Error (Msg _) -> ()
  in
  Test.run_exn
    (module struct
      type t = int64 [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun num ->
      let payload = Printf.sprintf "%Lx\r\n" num in
      run_test num (String.uppercase payload);
      run_test num (String.lowercase payload))

let%expect_test "chunk length parser" =
  let run_test str =
    let buf = Bigstringaf.of_string ~off:0 ~len:(String.length str) str in
    match H1_parser.parse_chunk_length buf with
    | Ok (len, off) -> printf "Chunk length: %Ld, offset: %d\n" len off
    | Error Partial -> print_endline "Partial"
    | Error (Msg m) -> print_endline m
  in
  run_test "ab2\r\n";
  [%expect {| Chunk length: 2738, offset: 5 |}];
  run_test "4511ab\r\n";
  (* We will try to use the same chunk length, but this time with a chunk
     extension. This should not result in any change in our output. *)
  run_test "4511ab  ; a\r\n";
  run_test "4511ab; now in extension\r\n";
  run_test "4511ab a ; now in extension\r\n";
  [%expect
    {|
    Chunk length: 4526507, offset: 8
    Chunk length: 4526507, offset: 13
    Chunk length: 4526507, offset: 26
    Invalid chunk_length character 'a' |}];
  run_test "111111111111111\r\n";
  [%expect {| Chunk length: 76861433640456465, offset: 17 |}];
  run_test "1111111111111111\r\n";
  [%expect {| Chunk size is too large |}];
  run_test "abc\r12";
  [%expect {| Expected_newline |}];
  run_test "abc\n12";
  [%expect {| Invalid chunk_length character '\n' |}];
  run_test "121";
  [%expect {| Partial |}];
  run_test "121\r";
  [%expect {| Partial |}]

let%expect_test "Rejects headers with space before colon" =
  let run_test str =
    let buf = Bigstringaf.of_string ~off:0 ~len:(String.length str) str in
    let res = H1_parser.parse_request buf in
    print_endline @@ pp_parse_result res
  in
  let req =
    "GET / HTTP/1.1\r\nHost: www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  (* Can parse the request with proper headers *)
  run_test req;
  [%expect
    {|
    { "request" =
       { "meth" = GET;
         "path" = "/";
         "version" = HTTP/1.1;
         "headers" = [("Host", "www.kittyhell.com"); ("Keep-Alive", "115")] };
      "bytes_consumed" = 60 } |}];
  let req =
    "GET / HTTP/1.1\r\nHost : www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  run_test req;
  [%expect {| Error: Invalid Header Key |}];
  let req =
    "GET / HTTP/1.1\r\n: www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  run_test req;
  [%expect {| Error: Invalid header: Empty header key |}]

let%expect_test "can parse chunked encoded data" =
  let run_test str =
    let buf = Bigstringaf.of_string str ~off:0 ~len:(String.length str) in
    let res =
      match H1_parser.parse_chunk buf with
      | Ok (chunk, len) ->
          let pp = Fmt.Dump.option Fmt.Dump.string in
          Fmt.str "Chunk: %a , processed %d bytes" pp chunk len
      | Error Partial -> "Partial"
      | Error (Msg msg) -> Printf.sprintf "Error: %s" msg
    in
    print_endline res
  in
  run_test "4\r\nWiki\r\n";
  [%expect {| Chunk: Some "Wiki" , processed 9 bytes |}];
  run_test "6\r\npedia \r\n";
  [%expect {| Chunk: Some "pedia " , processed 11 bytes |}];
  run_test "4\r\nWi";
  [%expect {| Partial |}];
  run_test "0\r\n\r\n";
  [%expect {| Chunk: None , processed 5 bytes |}];
  run_test "E\r\nin \r\n\r\nchunks.\r\n0\r\n\r\n";
  [%expect {| Chunk: Some "in \r\n\r\nchunks." , processed 19 bytes |}]
