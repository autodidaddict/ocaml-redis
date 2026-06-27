open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 6379)

(* Populate [store] from the RDB file named by the configuration, if it exists.
   The decode is pure; this is the I/O edge — read the bytes through the
   filesystem capability and fold the entries in. A missing file is not an error:
   the database is simply empty. We use the [fs] capability rather than [cwd]
   because --dir is an absolute path the cwd sandbox would reject. Already-expired
   keys need no special handling — the store's lazy expiry drops them at read
   time. *)
let load_rdb ~fs ~(config : Config.t) (store : Store.t) : unit =
  let path =
    Eio.Path.(fs / Filename.concat (Config.dir config) (Config.dbfilename config))
  in
  match Eio.Path.kind ~follow:true path with
  | `Not_found -> ()
  | _ ->
    Eio.Path.load path |> Rdb.of_string
    |> List.iter (fun (e : Rdb.entry) ->
           Store.set store ?expires_at_millis:e.expires_at_millis e.key e.value)

(* Start the server and block until SIGTERM/SIGINT. [config] is the startup
   configuration capability; [env] is the Eio environment. *)
let serve ~config env =
  Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~backlog:128 addr
  in
  let clock = Eio.Stdenv.clock env in
  let store = Store.create () in
  load_rdb ~fs:(Eio.Stdenv.fs env) ~config store;
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
      Eio.Net.run_server socket (Server.handle_client ~clock ~store ~config)
        ~on_error:(fun ex -> traceln "connection error: %a" Fmt.exn ex)
        ~max_connections:1000)

(* Cmdliner term action: build the config from the parsed flags, then enter the
   Eio event loop. Argument parsing happens before any I/O. *)
let main dir dbfilename =
  let config = Config.create ~dir ~dbfilename in
  Eio_main.run @@ fun env -> serve ~config env

open Cmdliner

let dir_arg =
  let doc = "Directory where the RDB file is stored." in
  Arg.(value & opt string "." & info [ "dir" ] ~docv:"DIR" ~doc)

let dbfilename_arg =
  let doc = "Name of the RDB file." in
  Arg.(value & opt string "dump.rdb" & info [ "dbfilename" ] ~docv:"FILE" ~doc)

let cmd =
  let doc = "A toy Redis server (Codecrafters)" in
  Cmd.v
    (Cmd.info "redis-server" ~version:"0.1" ~doc)
    Term.(const main $ dir_arg $ dbfilename_arg)

let () = exit (Cmd.eval cmd)
