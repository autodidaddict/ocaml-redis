open Eio.Std
open Resp

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module R = Eio.Buf_read

(* Turn a parsed RESP value into a reply value by interpreting it as a typed
   command and dispatching on it. *)
let reply_to (value : Value.t) : Value.t =
  let open Value in
  match Command.of_value value with
  | Ok Ping -> Simple_string "PONG"
  | Ok (Echo _) -> Simple_error "ERR ECHO not implemented yet" (* next step *)
  | Error msg -> Simple_error ("ERR " ^ msg)

(* Parse RESP values straight from the connection's buffered reader and reply to
   each. Buf_read buffers bytes across socket reads, so a command split over
   several packets just makes [Parser.value] read again — no manual buffering. *)
let handle_client flow addr =
  traceln "client connected: %a" Eio.Net.Sockaddr.pp addr;
  let from_client = R.of_flow flow ~max_size:(1024 * 1024) in
  let rec loop () =
    if R.at_end_of_input from_client then traceln "client closed connection."
    else begin
      let reply = Encoder.encode (reply_to (Parser.value from_client)) in
      Eio.Flow.copy_string reply flow;
      loop ()
    end
  in
  try loop () with
  | End_of_file -> traceln "client closed connection."
  | Failure msg -> traceln "protocol error, closing: %s" msg
