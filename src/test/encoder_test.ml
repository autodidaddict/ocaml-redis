open Resp

let check = Alcotest.(check string)

let test_simple_string () =
  check "PONG" "+PONG\r\n" (Encoder.encode (Value.Simple_string "PONG"))

let test_error () =
  check "error" "-ERR boom\r\n" (Encoder.encode (Value.Simple_error "ERR boom"))

let test_integer () =
  check "integer" ":42\r\n" (Encoder.encode (Value.Integer 42L))

let test_bulk_string () =
  check "bulk" "$5\r\nhello\r\n" (Encoder.encode (Value.Bulk_string (Some "hello")))

let test_empty_bulk_string () =
  check "empty bulk" "$0\r\n\r\n" (Encoder.encode (Value.Bulk_string (Some "")))

let test_null_bulk_string () =
  check "null bulk" "$-1\r\n" (Encoder.encode (Value.Bulk_string None))

let test_array () =
  check "array" "*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n"
    (Encoder.encode
       (Value.Array [ Value.Bulk_string (Some "ECHO"); Value.Bulk_string (Some "hey") ]))

(* Encoder and parser are inverses (for the types the parser understands). *)
let test_roundtrip_bulk () =
  let value = Value.Bulk_string (Some "hello") in
  match Parser.parse (Encoder.encode value) with
  | Parser.Done (Value.Bulk_string (Some "hello"), "") -> ()
  | _ -> Alcotest.fail "encode -> parse did not round-trip"

let () =
  Alcotest.run "resp-encoder"
    [ ( "encode"
      , [ Alcotest.test_case "simple string" `Quick test_simple_string
        ; Alcotest.test_case "error" `Quick test_error
        ; Alcotest.test_case "integer" `Quick test_integer
        ; Alcotest.test_case "bulk string" `Quick test_bulk_string
        ; Alcotest.test_case "empty bulk string" `Quick test_empty_bulk_string
        ; Alcotest.test_case "null bulk string" `Quick test_null_bulk_string
        ; Alcotest.test_case "array" `Quick test_array
        ; Alcotest.test_case "round-trips with parser" `Quick test_roundtrip_bulk
        ] )
    ]
