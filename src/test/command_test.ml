open Resp

let result = Alcotest.(result Testables.command string)

(* PING arrives as an array of one bulk string. *)
let ping_value = Value.Array [ Value.Bulk_string (Some "PING") ]

let test_detects_ping () =
  Alcotest.check result "PING" (Ok Command.Ping) (Command.of_value ping_value)

let test_ping_is_case_insensitive () =
  Alcotest.check result "ping (lowercase)" (Ok Command.Ping)
    (Command.of_value (Value.Array [ Value.Bulk_string (Some "ping") ]))

let test_detects_echo () =
  Alcotest.check result "ECHO hey" (Ok (Command.Echo "hey"))
    (Command.of_value
       (Value.Array [ Value.Bulk_string (Some "ECHO"); Value.Bulk_string (Some "hey") ]))

let test_echo_requires_an_argument () =
  match Command.of_value (Value.Array [ Value.Bulk_string (Some "ECHO") ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected ECHO with no argument to error"

let test_unknown_command_errors () =
  match Command.of_value (Value.Array [ Value.Bulk_string (Some "NOPE") ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected an unknown command to error"

let test_non_array_errors () =
  match Command.of_value (Value.Simple_string "PING") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected a non-array value to error"

let () =
  Alcotest.run "resp-command"
    [ ( "of_value"
      , [ Alcotest.test_case "detects ping" `Quick test_detects_ping
        ; Alcotest.test_case "ping is case-insensitive" `Quick test_ping_is_case_insensitive
        ; Alcotest.test_case "detects echo" `Quick test_detects_echo
        ; Alcotest.test_case "echo requires an argument" `Quick test_echo_requires_an_argument
        ; Alcotest.test_case "unknown command errors" `Quick test_unknown_command_errors
        ; Alcotest.test_case "non-array errors" `Quick test_non_array_errors
        ] )
    ]
