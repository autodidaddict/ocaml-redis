open Resp

(* Behavioral tests: the parser is exercised only through its public surface
   (parse / feed), asserting on observable outcomes rather than internal state. *)

let test_rejects_unknown_type () =
  match Parser.parse "+OK\r\n" with
  | Parser.Failed _ -> ()
  | _ -> Alcotest.fail "expected an error for an unknown type byte"

let test_incomplete_without_type () =
  match Parser.parse "" with
  | Parser.Incomplete -> ()
  | _ -> Alcotest.fail "empty buffer should be incomplete"

let test_incomplete_partial_body () =
  (* '$5' promises 5 body bytes; only 2 are present -> waiting for more *)
  match Parser.parse "$5\r\nhe" with
  | Parser.Incomplete -> ()
  | _ -> Alcotest.fail "partial body should be incomplete"

let test_empty_bulk_string () =
  match Parser.parse "$0\r\n\r\n" with
  | Parser.Done (Value.Bulk_string (Some ""), "") -> ()
  | _ -> Alcotest.fail "expected an empty bulk string with no remainder"

let test_bulk_string () =
  match Parser.parse "$5\r\nhello\r\n" with
  | Parser.Done (Value.Bulk_string (Some "hello"), "") -> ()
  | _ -> Alcotest.fail "expected \"hello\" with no remainder"

let test_feed_resumes_across_chunks () =
  match Parser.feed (Parser.create ()) "$5\r\nhe" with
  | Ok ([], waiting) -> (
    match Parser.feed waiting "llo\r\n" with
    | Ok ([ Value.Bulk_string (Some "hello") ], _) -> ()
    | _ -> Alcotest.fail "expected \"hello\" after the second feed")
  | _ -> Alcotest.fail "expected to be waiting for input after the first feed"

let test_feed_drains_pipeline () =
  (* two complete values arriving in one chunk *)
  match Parser.feed (Parser.create ()) "$1\r\na\r\n$1\r\nb\r\n" with
  | Ok ([ Value.Bulk_string (Some "a"); Value.Bulk_string (Some "b") ], _) -> ()
  | _ -> Alcotest.fail "expected both pipelined values"

let () =
  Alcotest.run "resp"
    [ ( "parse"
      , [ Alcotest.test_case "rejects unknown type" `Quick test_rejects_unknown_type
        ; Alcotest.test_case "incomplete without type" `Quick test_incomplete_without_type
        ; Alcotest.test_case "incomplete partial body" `Quick test_incomplete_partial_body
        ; Alcotest.test_case "empty bulk string" `Quick test_empty_bulk_string
        ; Alcotest.test_case "bulk string" `Quick test_bulk_string
        ] )
    ; ( "feed"
      , [ Alcotest.test_case "resumes across chunks" `Quick test_feed_resumes_across_chunks
        ; Alcotest.test_case "drains a pipeline" `Quick test_feed_drains_pipeline
        ] )
    ]
