open Resp

let result = Alcotest.(result Testables.command string)

(* Build a command value: a RESP array of bulk strings from plain strings. *)
let cmd (parts : string list) : Value.t =
  Value.Array (List.map (fun s -> Value.Bulk_string (Some s)) parts)

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

let test_detects_get () =
  Alcotest.check result "GET foo" (Ok (Command.Get "foo"))
    (Command.of_value (cmd [ "GET"; "foo" ]))

let test_get_requires_an_argument () =
  match Command.of_value (cmd [ "GET" ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected GET with no argument to error"

let test_detects_bare_set () =
  Alcotest.check result "SET foo bar"
    (Ok
       (Command.Set
          { key = "foo"; value = "bar"; existence = Command.Always
          ; get = false; expiry = None }))
    (Command.of_value (cmd [ "SET"; "foo"; "bar" ]))

let test_set_with_px () =
  Alcotest.check result "SET foo bar PX 100"
    (Ok
       (Command.Set
          { key = "foo"; value = "bar"; existence = Command.Always
          ; get = false; expiry = Some (Command.Expire_millis 100) }))
    (Command.of_value (cmd [ "SET"; "foo"; "bar"; "PX"; "100" ]))

let test_set_options_order_and_case_insensitive () =
  Alcotest.check result "SET foo bar get nx"
    (Ok
       (Command.Set
          { key = "foo"; value = "bar"; existence = Command.If_not_exists
          ; get = true; expiry = None }))
    (Command.of_value (cmd [ "SET"; "foo"; "bar"; "get"; "nx" ]))

let test_set_nx_xx_conflict_errors () =
  match Command.of_value (cmd [ "SET"; "foo"; "bar"; "NX"; "XX" ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected NX and XX together to error"

let test_set_duplicate_expiry_errors () =
  match Command.of_value (cmd [ "SET"; "foo"; "bar"; "EX"; "1"; "PX"; "1" ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected two expiry options to error"

let test_set_invalid_expire_errors () =
  match Command.of_value (cmd [ "SET"; "foo"; "bar"; "EX"; "abc" ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected a non-integer expire time to error"

let test_set_requires_key_and_value () =
  match Command.of_value (cmd [ "SET"; "foo" ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected SET with no value to error"

let () =
  Alcotest.run "resp-command"
    [ ( "of_value"
      , [ Alcotest.test_case "detects ping" `Quick test_detects_ping
        ; Alcotest.test_case "ping is case-insensitive" `Quick test_ping_is_case_insensitive
        ; Alcotest.test_case "detects echo" `Quick test_detects_echo
        ; Alcotest.test_case "echo requires an argument" `Quick test_echo_requires_an_argument
        ; Alcotest.test_case "unknown command errors" `Quick test_unknown_command_errors
        ; Alcotest.test_case "non-array errors" `Quick test_non_array_errors
        ; Alcotest.test_case "detects get" `Quick test_detects_get
        ; Alcotest.test_case "get requires an argument" `Quick test_get_requires_an_argument
        ; Alcotest.test_case "detects bare set" `Quick test_detects_bare_set
        ; Alcotest.test_case "set with px expiry" `Quick test_set_with_px
        ; Alcotest.test_case "set options order/case-insensitive" `Quick test_set_options_order_and_case_insensitive
        ; Alcotest.test_case "set nx+xx conflict errors" `Quick test_set_nx_xx_conflict_errors
        ; Alcotest.test_case "set duplicate expiry errors" `Quick test_set_duplicate_expiry_errors
        ; Alcotest.test_case "set invalid expire errors" `Quick test_set_invalid_expire_errors
        ; Alcotest.test_case "set requires key and value" `Quick test_set_requires_key_and_value
        ] )
    ]
