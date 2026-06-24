let result = Alcotest.(option (pair string string))

let test_get_known_params () =
  let c = Config.create ~dir:"/tmp/rdb" ~dbfilename:"dump.rdb" in
  Alcotest.check result "dir" (Some ("dir", "/tmp/rdb")) (Config.get c "dir");
  Alcotest.check result "dbfilename"
    (Some ("dbfilename", "dump.rdb"))
    (Config.get c "dbfilename")

let test_get_canonicalizes_name () =
  (* Any casing in, canonical lowercase name out — so CONFIG GET echoes what
     real Redis would. *)
  let c = Config.create ~dir:"/tmp/rdb" ~dbfilename:"dump.rdb" in
  Alcotest.check result "DIR -> dir" (Some ("dir", "/tmp/rdb")) (Config.get c "DIR")

let test_get_unknown_param () =
  let c = Config.create ~dir:"/tmp/rdb" ~dbfilename:"dump.rdb" in
  Alcotest.check result "unknown" None (Config.get c "maxmemory")

let () =
  Alcotest.run "config"
    [ ( "get"
      , [ Alcotest.test_case "known params" `Quick test_get_known_params
        ; Alcotest.test_case "canonicalizes name" `Quick test_get_canonicalizes_name
        ; Alcotest.test_case "unknown param" `Quick test_get_unknown_param
        ] )
    ]
