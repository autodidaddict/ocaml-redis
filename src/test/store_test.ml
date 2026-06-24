(* Because the store takes the current time as a plain [float] rather than a
   clock capability, these tests never touch Eio or a real clock — they just
   hand it whatever "now" the case needs. *)

let value = Alcotest.(option string)

let test_set_then_get () =
  let s = Store.create () in
  Store.set s "k" "v";
  Alcotest.check value "present" (Some "v") (Store.get s ~now:0. "k")

let test_missing_key () =
  let s = Store.create () in
  Alcotest.check value "absent" None (Store.get s ~now:0. "missing")

let test_overwrite_replaces () =
  let s = Store.create () in
  Store.set s "k" "v1";
  Store.set s "k" "v2";
  Alcotest.check value "latest wins" (Some "v2") (Store.get s ~now:0. "k")

let test_live_before_deadline () =
  let s = Store.create () in
  Store.set s ~expires_at:100. "k" "v";
  Alcotest.check value "still live" (Some "v") (Store.get s ~now:50. "k")

let test_gone_at_or_after_deadline () =
  let s = Store.create () in
  Store.set s ~expires_at:100. "k" "v";
  Alcotest.check value "expired" None (Store.get s ~now:150. "k")

let test_expiry_is_a_lazy_drop () =
  (* A read past the deadline doesn't just report absence, it removes the entry,
     so a later read — even with an earlier clock — still sees nothing. *)
  let s = Store.create () in
  Store.set s ~expires_at:100. "k" "v";
  ignore (Store.get s ~now:150. "k");
  Alcotest.check value "actually removed" None (Store.get s ~now:0. "k")

let () =
  Alcotest.run "store"
    [ ( "get/set"
      , [ Alcotest.test_case "set then get" `Quick test_set_then_get
        ; Alcotest.test_case "missing key" `Quick test_missing_key
        ; Alcotest.test_case "overwrite replaces" `Quick test_overwrite_replaces
        ; Alcotest.test_case "live before deadline" `Quick test_live_before_deadline
        ; Alcotest.test_case "gone at/after deadline" `Quick
            test_gone_at_or_after_deadline
        ; Alcotest.test_case "expiry is a lazy drop" `Quick
            test_expiry_is_a_lazy_drop
        ] )
    ]
