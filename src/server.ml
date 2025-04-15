open Eio.Std

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module Read = Eio.Buf_read
module Write = Eio.Buf_write

(* Read one line from [client] and respond with "OK". *)
let handle_client flow addr =
  traceln "Reading line from %a" Eio.Net.Sockaddr.pp addr;
  (* We use a buffered reader because we may need to combine multiple reads
     to get a single line (or we may get multiple lines in a single read,
     although here we only use the first one). *)
  let from_client = Read.of_flow flow ~max_size:100 in
  let rec read_loop () =     
    try
      let line = Read.line from_client in    
      let linestr = Printf.sprintf "%S" line in
      
      traceln "Received: '%s'" linestr;
      if linestr = "\"PING\"" then
        Eio.Flow.copy_string "+PONG\r\n" flow
      else
        traceln "not a ping";
      
        
      read_loop ()
    with
    | End_of_file -> 
        traceln "client closed connection.";
        Eio.Flow.close flow
    | exn ->
        traceln "Error handling client: %a" Fmt.exn exn;
        Eio.Flow.close flow
    in
      read_loop()


(* Accept incoming client connections on [socket].
   We can handle multiple clients at the same time.
   Never returns (but can be cancelled). *)

let run socket =
    traceln "Running server";
    Eio.Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)
      ~max_connections:1000

