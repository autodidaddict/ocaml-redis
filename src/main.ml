open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 6379)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~backlog:128 addr
  in
  let clock = Eio.Stdenv.clock env in
  let store = Store.create () in
    (* See https://kevinhoffman.blog/posts/debug-io-uring-codecrafters-analysis/ for
       background on the eio setup and signal-safe shutdown.

       The signal handler stays signal-safe by only toggling an atomic and calling
       [Eio.Condition.broadcast] (unlike [Promise.resolve], broadcast is safe to
       call from a handler). A fiber waits on the condition for that broadcast.

       When it fires we cancel the server fiber via [Fiber.first] rather than use
       [run_server ~stop]: ~stop is graceful and waits for in-flight connections
       to finish, but our handlers block on clients that stay connected, so it
       would never return. Cancelling the server fiber also cancels the connection
       fibers, so the program exits promptly. *)
  let shutdown = Eio.Condition.create () in
  let signalled = Atomic.make false in
  let on_signal (_ : int) =
    Atomic.set signalled true;
    Eio.Condition.broadcast shutdown
  in
  Sys.set_signal Sys.sigterm (Signal_handle on_signal);
  Sys.set_signal Sys.sigint (Signal_handle on_signal);
  Fiber.first
    (fun () ->
      Eio.Condition.loop_no_mutex shutdown (fun () ->
          if Atomic.get signalled then Some () else None))
    (fun () ->
      Eio.Net.run_server socket (Server.handle_client ~clock ~store)
        ~on_error:(fun ex -> traceln "connection error: %a" Fmt.exn ex)
        ~max_connections:1000)
