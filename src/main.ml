open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 6379)

(* Run a server and a test client, communicating using [net]. *)
let main ~net =
  Switch.run ~name:"main" @@ fun sw ->
  (* We create the listening socket first so that we can be sure it is ready
     as soon as the client wants to use it. *)
  let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
 Server.run listening_socket

let () =
  Eio_main.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env)

