open Eio.Std

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module Read = Eio.Buf_read



(* Read one line from [client] and respond with "OK". *)
let rec handle_client flow addr =
  traceln "Reading line from %a" Eio.Net.Sockaddr.pp addr;
  (* We use a buffered reader because we may need to combine multiple reads
     to get a single line (or we may get multiple lines in a single read,
     although here we only use the first one). *)
  let from_client = Read.of_flow flow ~max_size:1024 in
  let line = Read.line from_client in
  traceln "Received: %S" line;
  Eio.Flow.copy_string "+PONG\r\n" flow;

  handle_client flow addr

(* Accept incoming client connections on [socket].
   We can handle multiple clients at the same time.
   Never returns (but can be cancelled). *)

let run socket =
  try
    traceln "Running server";
    Eio.Net.run_server socket handle_client
      ~on_error:(fun exn -> Eio.Net.close socket; traceln "Error handling connection: %a" Fmt.exn exn)
      ~max_connections:1000
  with _exn ->
    Eio.Net.close socket;
    traceln "foo"
