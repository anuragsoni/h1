open H1_lwt

let stream_to_list stream =
  let%lwt l =
    Lstream.fold ~init:[] ~f:(fun acc x -> Lwt.return (x :: acc)) stream
  in
  Lwt.return (List.rev l)

let test_creation_from_list _switch () =
  let stream = Lstream.of_list [ 1; 2; 4 ] in
  let%lwt l = stream_to_list stream in
  Alcotest.(check (list int)) "Can create stream from list" l [ 1; 2; 4 ];
  Lwt.return_unit

let test_concat _switch () =
  let stream =
    Lstream.of_list
      [
        Lstream.of_list [ "a"; "b" ];
        Lstream.of_list [];
        Lstream.of_list [ "z" ];
        Lstream.of_list [ "abc" ];
      ]
  in
  let%lwt l = stream_to_list (Lstream.concat stream) in
  Alcotest.(check (list string))
    "Can concat a stream of streams" l [ "a"; "b"; "z"; "abc" ];
  Lwt.return_unit

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Lstream"
       [
         ( "List Operations",
           [
             Alcotest_lwt.test_case "create from list" `Quick
               test_creation_from_list;
             Alcotest_lwt.test_case "concat stream" `Quick test_concat;
           ] );
       ]
