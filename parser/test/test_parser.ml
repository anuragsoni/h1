open Base
open Stdio

let req =
  "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com\r\n\
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

let%expect_test "Can parse single request" =
  let buf = Bigstringaf.of_string ~off:0 ~len:(String.length req) req in
  let res = H1_parser.parse_request buf in
  printf !"%{sexp: ((H1_parser.request * int)) option}" (Result.ok res);
  [%expect
    {|
    ((((meth GET)
       (path /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg)
       (version Http_1_1)
       (headers
        ((Cookie
          "wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral")
         (Connection keep-alive) (Keep-Alive 115)
         (Accept-Charset "Shift_JIS,utf-8;q=0.7,*;q=0.7")
         (Accept-Encoding gzip,deflate)
         (Accept-Language "ja,en-us;q=0.7,en;q=0.3")
         (Accept
          "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
         (User-Agent
          "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9")
         (Host www.kittyhell.com))))
      703)) |}]

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
  printf !"%{sexp: ((H1_parser.request * int)) option}" (Result.ok res);
  [%expect
    {|
    ((((meth GET) (path /reddit.v_EZwRzV-Ns.css) (version Http_1_1)
       (headers
        ((Referer http://www.reddit.com/) (Connection keep-alive)
         (Accept-Encoding "gzip, deflate") (Accept-Language "en-us,en;q=0.5")
         (Accept "text/css,*/*;q=0.1")
         (User-Agent
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) Gecko/20100101 Firefox/15.0.1")
         (Host www.redditstatic.com))))
      315)) |}]

let%expect_test "Informs the caller if the buffer contains partial request" =
  let buf = Bigstringaf.of_string ~off:0 ~len:(String.length req) req in
  let res = H1_parser.parse_request ~off:0 ~len:50 buf |> Result.error in
  printf !"%{sexp: H1_parser.error option}" res;
  [%expect {| (Partial) |}]

let%expect_test "Rejects any http version that isn't 1.0 or 1.1" =
  let req =
    "GET / HTTP/1.4\r\nHost: www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  let buf = Bigstringaf.of_string req ~off:0 ~len:(String.length req) in
  let res = H1_parser.parse_request ~off:0 ~len:50 buf |> Result.error in
  printf !"%{sexp: H1_parser.error option}" res;
  [%expect {| ((Msg "Invalid http version number 1.4")) |}]

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
  printf !"%{sexp: (H1_parser.request)}" req;
  [%expect
    {|
    ((meth POST) (path /) (version Http_1_1)
     (headers
      ((Content-Type application/x-www-form-urlencoded) (Content-Length 6)
       (Accept */*) (User-Agent curl/7.64.1) (Host localhost:8080)))) |}];
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
