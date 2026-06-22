open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 6379)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~backlog:128 addr
  in
  (* Graceful shutdown on SIGTERM/SIGINT.

     The signal handler only calls [Eio.Condition.broadcast], which Eio
     documents as lock-free and safe to run from a signal handler. A normal
     daemon fiber then resolves [stop], keeping the not-signal-safe
     [Promise.resolve] out of signal context. Resolving [stop] makes
     [run_server] stop accepting and lets the switch close the listening socket
     promptly - rather than leaving it to the io_uring backend's slower
     shutdown, during which the listener keeps completing handshakes for
     connections it will never serve (which breaks rapid restarts). *)
  let shutdown = Eio.Condition.create () in
  let signalled = Atomic.make false in
  let on_signal _ =
    Atomic.set signalled true;
    Eio.Condition.broadcast shutdown
  in
  Sys.set_signal Sys.sigterm (Signal_handle on_signal);
  Sys.set_signal Sys.sigint (Signal_handle on_signal);
  let stop, set_stop = Promise.create () in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Condition.loop_no_mutex shutdown (fun () ->
          if Atomic.get signalled then Some () else None);
      Promise.resolve set_stop ();
      `Stop_daemon);
  Eio.Net.run_server ~stop socket Server.handle_client
    ~on_error:(fun ex -> traceln "connection error: %a" Fmt.exn ex)
    ~max_connections:1000
