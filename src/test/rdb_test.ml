(* Deterministic RDB-decoder tests: every input is an in-memory byte image built
   from the format's own primitives, so nothing here touches a file or a clock. *)

let entries = Alcotest.list Testables.rdb_entry

(* A length-prefixed string with a 6-bit size (only used for len < 64 here). *)
let s (str : string) : string =
  String.make 1 (Char.chr (String.length str)) ^ str

let header = "REDIS0011"

(* End-of-file opcode plus an 8-byte checksum the decoder should ignore. *)
let eof = "\xff\x00\x00\x00\x00\x00\x00\x00\x00"

let test_empty_database () =
  Alcotest.check entries "no entries" [] (Rdb.of_string (header ^ eof))

let test_single_key_no_expiry () =
  let db =
    header ^ "\xfe\x00" (* SELECTDB 0 *) ^ "\xfb\x01\x00"
    (* RESIZEDB: 1 key, 0 with expiry *) ^ "\x00" ^ s "foo" ^ s "bar" ^ eof
  in
  Alcotest.check entries "foo=bar"
    [ { Rdb.key = "foo"; value = "bar"; expires_at_millis = None } ]
    (Rdb.of_string db)

let test_key_with_ms_expiry () =
  (* Expiry bytes taken verbatim from the RDB spec: 1713824559637, little-endian. *)
  let db =
    header ^ "\xfe\x00" ^ "\xfc\x15\x72\xe7\x07\x8f\x01\x00\x00" ^ "\x00" ^ s "foo"
    ^ s "bar" ^ eof
  in
  Alcotest.check entries "foo=bar with ms expiry"
    [ { Rdb.key = "foo"; value = "bar"; expires_at_millis = Some 1713824559637 } ]
    (Rdb.of_string db)

let test_key_with_seconds_expiry () =
  (* Spec bytes for 1714089298 seconds, little-endian; stored as milliseconds. *)
  let db =
    header ^ "\xfe\x00" ^ "\xfd\x52\xed\x2a\x66" ^ "\x00" ^ s "baz" ^ s "qux" ^ eof
  in
  Alcotest.check entries "baz=qux with seconds expiry"
    [ { Rdb.key = "baz"; value = "qux"; expires_at_millis = Some (1714089298 * 1000) }
    ]
    (Rdb.of_string db)

let test_multiple_keys_in_order () =
  let db =
    header ^ "\xfe\x00" ^ "\xfb\x02\x00" ^ "\x00" ^ s "one" ^ s "1" ^ "\x00"
    ^ s "two" ^ s "2" ^ eof
  in
  Alcotest.check entries "two keys, encounter order preserved"
    [ { Rdb.key = "one"; value = "1"; expires_at_millis = None };
      { Rdb.key = "two"; value = "2"; expires_at_millis = None } ]
    (Rdb.of_string db)

let test_integer_encoded_values () =
  let db =
    header ^ "\xfe\x00"
    ^ "\x00" ^ s "i8" ^ "\xc0\x7b" (* C0: int8  -> "123" *)
    ^ "\x00" ^ s "i16" ^ "\xc1\x39\x30" (* C1: int16 -> "12345" *)
    ^ "\x00" ^ s "i32" ^ "\xc2\x87\xd6\x12\x00" (* C2: int32 -> "1234567" *)
    ^ eof
  in
  Alcotest.check entries "integer-encoded string values"
    [ { Rdb.key = "i8"; value = "123"; expires_at_millis = None };
      { Rdb.key = "i16"; value = "12345"; expires_at_millis = None };
      { Rdb.key = "i32"; value = "1234567"; expires_at_millis = None } ]
    (Rdb.of_string db)

let test_large_string_sizes () =
  (* Lengths of 64+ exercise the 14-bit (0b01) and 32-bit (0b10, big-endian)
     size encodings rather than the 6-bit form. *)
  let big14 = String.make 200 'a' in
  let big32 = String.make 70 'b' in
  let size14 n =
    let b0 = 0x40 lor ((n lsr 8) land 0x3f) and b1 = n land 0xff in
    String.make 1 (Char.chr b0) ^ String.make 1 (Char.chr b1)
  in
  let size32 n =
    "\x80" ^ String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 0xff))
  in
  let db =
    header ^ "\xfe\x00"
    ^ "\x00" ^ s "k14" ^ size14 (String.length big14) ^ big14
    ^ "\x00" ^ s "k32" ^ size32 (String.length big32) ^ big32
    ^ eof
  in
  Alcotest.check entries "14- and 32-bit string sizes"
    [ { Rdb.key = "k14"; value = big14; expires_at_millis = None };
      { Rdb.key = "k32"; value = big32; expires_at_millis = None } ]
    (Rdb.of_string db)

let test_rejects_bad_magic () =
  match Rdb.of_string "NOTRDB011\xff" with
  | _ -> Alcotest.fail "expected a bad magic header to raise"
  | exception _ -> ()

let () =
  Alcotest.run "rdb"
    [ ( "read",
        [ Alcotest.test_case "empty database" `Quick test_empty_database;
          Alcotest.test_case "single key, no expiry" `Quick
            test_single_key_no_expiry;
          Alcotest.test_case "key with ms expiry" `Quick test_key_with_ms_expiry;
          Alcotest.test_case "key with seconds expiry" `Quick
            test_key_with_seconds_expiry;
          Alcotest.test_case "multiple keys in order" `Quick
            test_multiple_keys_in_order;
          Alcotest.test_case "integer-encoded values" `Quick
            test_integer_encoded_values;
          Alcotest.test_case "large string sizes" `Quick test_large_string_sizes;
          Alcotest.test_case "rejects bad magic" `Quick test_rejects_bad_magic
        ] )
    ]
