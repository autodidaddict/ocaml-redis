(* Integration check: the decoder against a real RDB produced by `redis-server
   SAVE` (Redis 8 — version-14 header, with the aux metadata fields a real server
   emits). The fixture bytes are frozen in the repo, so the decoded entries —
   including the absolute expiry baked in when it was generated — are
   deterministic. Complements the hermetic, hand-built cases in rdb_test. *)

let entries = Alcotest.list Testables.rdb_entry

let test_decodes_real_redis8_file () =
  let image = In_channel.with_open_bin "fixtures/dump.rdb" In_channel.input_all in
  Alcotest.check entries "entries from a real Redis 8 SAVE"
    [ { Rdb.key = "temp"; value = "soon"; expires_at_millis = Some 1782678448647 };
      { Rdb.key = "foo"; value = "bar"; expires_at_millis = None };
      { Rdb.key = "baz"; value = "qux"; expires_at_millis = None } ]
    (Rdb.of_string image)

let () =
  Alcotest.run "rdb-fixture"
    [ ( "real file",
        [ Alcotest.test_case "decodes a real Redis 8 RDB" `Quick
            test_decodes_real_redis8_file
        ] )
    ]
