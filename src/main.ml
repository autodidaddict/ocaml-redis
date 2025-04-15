open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 6379)

let main ~net =
  Switch.run ~name:"main" @@ fun sw ->
  let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
(*   Fiber.fork_daemon ~sw (fun () -> Server.run listening_socket) *)
  Server.run listening_socket

let () =
  Eio_main.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env)
