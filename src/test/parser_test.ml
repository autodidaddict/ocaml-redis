open Resp

(* Behavioral tests through the public surface (parse / feed), now asserting with
   testables so a mismatch prints a structured expected-vs-actual diff. *)

let check_outcome msg expected actual =
  Alcotest.check Testables.outcome msg expected actual

let check_values msg expected actual =
  Alcotest.(check (list Testables.value)) msg expected actual

let test_rejects_unknown_type () =
  check_outcome "unknown type byte fails" Testables.failed (Parser.parse "+OK\r\n")

let test_incomplete_without_type () =
  check_outcome "empty buffer is incomplete" Parser.Incomplete (Parser.parse "")

let test_incomplete_partial_body () =
  check_outcome "partial body is incomplete" Parser.Incomplete
    (Parser.parse "$5\r\nhe")

let test_empty_bulk_string () =
  check_outcome "empty bulk string"
    (Parser.Done (Value.Bulk_string (Some ""), ""))
    (Parser.parse "$0\r\n\r\n")

let test_bulk_string () =
  check_outcome "bulk string"
    (Parser.Done (Value.Bulk_string (Some "hello"), ""))
    (Parser.parse "$5\r\nhello\r\n")

let test_feed_resumes_across_chunks () =
  match Parser.feed (Parser.create ()) "$5\r\nhe" with
  | Error _ -> Alcotest.fail "first feed errored"
  | Ok (values, waiting) -> (
    check_values "nothing complete after a partial frame" [] values;
    match Parser.feed waiting "llo\r\n" with
    | Error _ -> Alcotest.fail "second feed errored"
    | Ok (values', _) ->
      check_values "hello completes once the rest arrives"
        [ Value.Bulk_string (Some "hello") ]
        values')

let test_feed_drains_pipeline () =
  match Parser.feed (Parser.create ()) "$1\r\na\r\n$1\r\nb\r\n" with
  | Error _ -> Alcotest.fail "feed errored"
  | Ok (values, _) ->
    check_values "both pipelined values"
      [ Value.Bulk_string (Some "a"); Value.Bulk_string (Some "b") ]
      values

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
