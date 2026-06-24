open Eio.Std
open Resp

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module R = Eio.Buf_read

(* RESP2 nil — a null bulk string ($-1). Used for "no such key" and for a SET
   whose NX/XX precondition blocks the write. *)
let nil = Value.Bulk_string None

(* Resolve a parsed (relative or absolute) expiry into an absolute POSIX
   deadline in seconds, given the instant [now] the command runs at. This is
   where SET's wire-level [PX 100] becomes a concrete "expires at now +. 0.1";
   the store only ever sees absolute deadlines. *)
let resolve_expiry ~(now : float) :
    Command.expiry option -> (float option, string) result = function
  | None -> Ok None
  | Some (Command.Expire_seconds s) -> Ok (Some (now +. float_of_int s))
  | Some (Command.Expire_millis ms) ->
    Ok (Some (now +. (float_of_int ms /. 1000.)))
  | Some (Command.Expire_at_seconds s) -> Ok (Some (float_of_int s))
  | Some (Command.Expire_at_millis ms) -> Ok (Some (float_of_int ms /. 1000.))
  | Some Command.Keep_ttl -> Error "KEEPTTL is not supported yet"

(* Apply a parsed SET against the store at instant [now]. The existence check
   and the optional old-value read both happen before the write; in Eio's
   single-domain model no fiber yields between them, so the read-decide-write
   sequence is atomic without a lock. *)
let apply_set ~(now : float) ~(store : Store.t)
    ({ key; value; existence; get; expiry } : Command.set_options) : Value.t =
  let open Value in
  let previous = Store.get store ~now key in
  let precondition_ok =
    match existence with
    | Command.Always -> true
    | Command.If_not_exists -> previous = None
    | Command.If_exists -> previous <> None
  in
  (* With GET, SET replies with the prior value (nil if none) instead of +OK. *)
  let reply_on_write =
    if get then match previous with Some v -> Bulk_string (Some v) | None -> nil
    else Simple_string "OK"
  in
  if precondition_ok then (
    match resolve_expiry ~now expiry with
    | Error msg -> Simple_error ("ERR " ^ msg)
    | Ok expires_at ->
      Store.set store ?expires_at key value;
      reply_on_write)
  else if get then reply_on_write (* blocked write, but GET still reports old *)
  else nil

(* Interpret a parsed RESP value as a command and apply it at instant [now],
   producing the reply value. Pure with respect to I/O: it takes the time as a
   plain value and the store as a capability, so tests drive it directly with no
   socket and no real clock. *)
let reply_to ~(now : float) ~(store : Store.t) (value : Value.t) : Value.t =
  let open Value in
  match Command.of_value value with
  | Ok Ping -> Simple_string "PONG"
  | Ok (Echo msg) -> Bulk_string (Some msg)
  | Ok (Get key) -> (
      match Store.get store ~now key with
      | Some v -> Bulk_string (Some v)
      | None -> nil)
  | Ok (Set opts) -> apply_set ~now ~store opts
  | Error msg -> Simple_error ("ERR " ^ msg)

(* Parse RESP values straight from the connection's buffered reader and reply to
   each. [clock] and [store] are the capabilities this handler needs: the clock
   to stamp each command's execution time, the store to read and write keys.
   Buf_read buffers across socket reads, so a command split over several packets
   just makes [Parser.value] read again. *)
let handle_client ~clock ~store flow addr =
  traceln "client connected: %a" Eio.Net.Sockaddr.pp addr;
  let from_client = R.of_flow flow ~max_size:(1024 * 1024) in
  let rec loop () =
    if R.at_end_of_input from_client then traceln "client closed connection."
    else begin
      (* Parse first (this blocks until a command arrives), then stamp the time
         it actually runs at. *)
      let command = Parser.value from_client in
      let now = Eio.Time.now clock in
      let reply = Encoder.encode (reply_to ~now ~store command) in
      Eio.Flow.copy_string reply flow;
      loop ()
    end
  in
  try loop () with
  | End_of_file -> traceln "client closed connection."
  | Failure msg -> traceln "protocol error, closing: %s" msg
