open Eio.Std

let traceln fmt = traceln ("server: " ^^ fmt)

module Read = Eio.Buf_read

let handle_client flow addr =
  traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr;
(*
  let from_client = Read.of_flow flow ~max_size:100 in
  traceln "Received: %S" (Read.line from_client);
*)
  Eio.Flow.copy_string "+PONG\r\n" flow

let run socket =
  Eio.Net.run_server socket handle_client
  ~on_error:(traceln "Error handling connection: %a" Fmt.exn)
  ~max_connections:1000
