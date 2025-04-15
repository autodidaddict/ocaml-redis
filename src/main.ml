(*
open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 6379)

(* Run a server and a test client, communicating using [net]. *)
let main ~net =
  Switch.run ~name:"main" @@ fun sw ->
  (* We create the listening socket first so that we can be sure it is ready
     as soon as the client wants to use it. *)
  let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  (* Start the server running in a new fiber.
     Using [fork_daemon] here means that it will be stopped once the client is done
     (we don't wait for it to finish because it will keep accepting new connections forever). *)
  Server.run listening_socket

let () =
  Eio_main.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env)
*)
  open Unix
  open Printf

let read_in_channel in_chan = input_line in_chan
let rec handle_client client_fd =
  let out_chan = out_channel_of_descr client_fd in
  try
    let _input = read_in_channel (in_channel_of_descr client_fd) in
    fprintf out_chan "+PONG\r\n";
    flush out_chan;
    handle_client client_fd
  with End_of_file -> ()

let () =
 let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 1;
  let client_socket, _ = accept server_socket in
  handle_client client_socket;
  close client_socket;
  close server_socket
