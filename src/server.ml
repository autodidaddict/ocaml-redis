open Eio.Std
open Resp

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module R = Eio.Buf_read

(* RESP2 nil — a null bulk string ($-1). Used for "no such key" and for a SET
   whose NX/XX precondition blocks the write. *)
let nil = Value.Bulk_string None

(* Resolve a parsed (relative or absolute) expiry into an absolute POSIX
   deadline in milliseconds, given the instant [now_millis] the command runs at.
   This is where SET's wire-level [PX 100] becomes a concrete "expires at
   now_millis + 100"; the store only ever sees absolute millisecond deadlines.
   All integer arithmetic — no float rounding. *)
let resolve_expiry ~(now_millis : int) :
    Command.expiry option -> (int option, string) result = function
  | None -> Ok None
  | Some (Command.Expire_seconds s) -> Ok (Some (now_millis + (s * 1000)))
  | Some (Command.Expire_millis ms) -> Ok (Some (now_millis + ms))
  | Some (Command.Expire_at_seconds s) -> Ok (Some (s * 1000))
  | Some (Command.Expire_at_millis ms) -> Ok (Some ms)
  | Some Command.Keep_ttl -> Error "KEEPTTL is not supported yet"

(* Apply a parsed SET against the store at instant [now_millis]. The existence
   check and the optional old-value read both happen before the write; in Eio's
   single-domain model no fiber yields between them, so the read-decide-write
   sequence is atomic without a lock. *)
let apply_set ~(now_millis : int) ~(store : Store.t)
    ({ key; value; existence; get; expiry } : Command.set_options) : Value.t =
  let open Value in
  let previous = Store.get store ~now_millis key in
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
    match resolve_expiry ~now_millis expiry with
    | Error msg -> Simple_error ("ERR " ^ msg)
    | Ok expires_at_millis ->
      Store.set store ?expires_at_millis key value;
      reply_on_write)
  else if get then reply_on_write (* blocked write, but GET still reports old *)
  else nil

(* Interpret a parsed RESP value as a command and apply it at instant
   [now_millis], producing the reply value. Pure with respect to I/O: it takes
   the time as a plain value and the store as a capability, so tests drive it
   directly with no socket and no real clock. *)
let reply_to ~(now_millis : int) ~(store : Store.t) ~(config : Config.t)
    (value : Value.t) : Value.t =
  let open Value in
  match Command.of_value value with
  | Ok Ping -> Simple_string "PONG"
  | Ok (Echo msg) -> Bulk_string (Some msg)
  | Ok (Get key) -> (
      match Store.get store ~now_millis key with
      | Some v -> Bulk_string (Some v)
      | None -> nil)
  | Ok (Set opts) -> apply_set ~now_millis ~store opts
  | Ok (Config_get param) -> (
      (* CONFIG GET replies with a flat [name; value] array, echoing the
         canonical parameter name (not the client's casing) exactly as real
         Redis does; an unknown parameter yields an empty array. *)
      match Config.get config param with
      | Some (name, v) -> Array [ Bulk_string (Some name); Bulk_string (Some v) ]
      | None -> Array [])
  | Error msg -> Simple_error ("ERR " ^ msg)

(* Parse RESP values straight from the connection's buffered reader and reply to
   each. [clock] and [store] are the capabilities this handler needs: the clock
   to stamp each command's execution time, the store to read and write keys.
   Buf_read buffers across socket reads, so a command split over several packets
   just makes [Parser.value] read again. *)
let handle_client ~clock ~store ~config flow addr =
  traceln "client connected: %a" Eio.Net.Sockaddr.pp addr;
  let from_client = R.of_flow flow ~max_size:(1024 * 1024) in
  let rec loop () =
    if R.at_end_of_input from_client then traceln "client closed connection."
    else begin
      (* Parse first (this blocks until a command arrives), then stamp the time
         it actually runs at. The Eio clock is the only place float seconds
         appear; convert to int milliseconds here at the edge so the rest of the
         server speaks integer time. *)
      let command = Parser.value from_client in
      let now_millis = int_of_float (Eio.Time.now clock *. 1000.) in
      let reply = Encoder.encode (reply_to ~now_millis ~store ~config command) in
      Eio.Flow.copy_string reply flow;
      loop ()
    end
  in
  try loop () with
  | End_of_file -> traceln "client closed connection."
  | Failure msg -> traceln "protocol error, closing: %s" msg
