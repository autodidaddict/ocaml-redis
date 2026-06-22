open Resp
module R = Eio.Buf_read

(* Drive the parser over a Buf_read built from a string: no socket needed, and
   the same combinators the server uses. *)
let value = Testables.value
let parse s = Parser.value (R.of_string s)

let test_bulk_string () =
  Alcotest.check value "bulk string"
    (Value.Bulk_string (Some "hello"))
    (parse "$5\r\nhello\r\n")

let test_empty_bulk_string () =
  Alcotest.check value "empty bulk string"
    (Value.Bulk_string (Some ""))
    (parse "$0\r\n\r\n")

let test_ping_array () =
  (* how a real PING arrives on the wire *)
  Alcotest.check value "PING command array"
    (Value.Array [ Value.Bulk_string (Some "PING") ])
    (parse "*1\r\n$4\r\nPING\r\n")

let test_echo_array () =
  Alcotest.check value "ECHO command array"
    (Value.Array [ Value.Bulk_string (Some "ECHO"); Value.Bulk_string (Some "hey") ])
    (parse "*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n")

let test_unknown_type_fails () =
  match parse "+OK\r\n" with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail "expected a failure on an unknown type byte"

let test_incomplete_raises () =
  (* an array promising two elements but carrying one runs off the end *)
  match parse "*2\r\n$4\r\nECHO\r\n" with
  | exception End_of_file -> ()
  | _ -> Alcotest.fail "expected End_of_file on incomplete input"

let test_pipeline () =
  (* two values share one buffer; the second parse resumes where the first left off *)
  let r = R.of_string "$1\r\na\r\n$1\r\nb\r\n" in
  let first = Parser.value r in
  let second = Parser.value r in
  Alcotest.check value "first" (Value.Bulk_string (Some "a")) first;
  Alcotest.check value "second" (Value.Bulk_string (Some "b")) second

let () =
  Alcotest.run "resp"
    [ ( "value"
      , [ Alcotest.test_case "bulk string" `Quick test_bulk_string
        ; Alcotest.test_case "empty bulk string" `Quick test_empty_bulk_string
        ; Alcotest.test_case "ping array" `Quick test_ping_array
        ; Alcotest.test_case "echo array" `Quick test_echo_array
        ; Alcotest.test_case "unknown type fails" `Quick test_unknown_type_fails
        ; Alcotest.test_case "incomplete raises" `Quick test_incomplete_raises
        ; Alcotest.test_case "pipeline" `Quick test_pipeline
        ] )
    ]
