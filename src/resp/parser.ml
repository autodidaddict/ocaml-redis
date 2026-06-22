(** Decoding of RESP values from an in-memory buffer.

    Pure and I/O-free: it works on plain [string] buffers, so it can be driven by
    a test, a file, or a socket reader alike. Internally it is a state machine,
    but that is an implementation detail — callers only ever see {!parse} (one
    shot) and {!feed} (resumable streaming). For now it understands only bulk
    strings (the ['$'] type). *)

(** Why a parse could not produce a value. *)
type error = Parse_error of string

(* Internal: where the machine is mid-value. The unconsumed bytes always live in
   the buffer, never half-read inside a state, which keeps each transition pure.
   [Reading_data] / [Awaiting_crlf] are the "waiting for more bytes" states. *)
type state =
  | Awaiting_type
  | Reading_length
  | Reading_data of int
  | Awaiting_crlf of string

(* Internal: the result of a single transition. *)
type transition =
  | Continue of state * string  (* next state, bytes still to consume *)
  | Produced of Value.t * string  (* a value, plus the bytes that followed it *)
  | Awaiting_more  (* buffer can't satisfy the current state yet *)
  | Invalid of error

(** Outcome of decoding a single value from the front of a buffer. *)
type outcome =
  | Done of Value.t * string  (** value, plus any bytes that followed it *)
  | Incomplete  (** buffer doesn't hold a full value yet; feed more *)
  | Failed of error

let initial = Awaiting_type

(* [split_crlf buf] returns the bytes before the first CRLF and the bytes after
   it, or [None] if no CRLF is present yet. *)
let split_crlf (buf : string) : (string * string) option =
  let len = String.length buf in
  let rec scan i =
    if i + 1 >= len then None
    else if buf.[i] = '\r' && buf.[i + 1] = '\n' then
      Some (String.sub buf 0 i, String.sub buf (i + 2) (len - i - 2))
    else scan (i + 1)
  in
  scan 0

(* One transition of the state machine. *)
let step (state : state) (buf : string) : transition =
  match state with
  | Awaiting_type -> (
    if String.length buf = 0 then Awaiting_more
    else
      match buf.[0] with
      | '$' -> Continue (Reading_length, String.sub buf 1 (String.length buf - 1))
      | c -> Invalid (Parse_error (Printf.sprintf "unexpected type byte %C" c)))
  | Reading_length -> (
    match split_crlf buf with
    | None -> Awaiting_more
    | Some (digits, rest) -> (
      match int_of_string_opt digits with
      | Some n when n >= 0 -> Continue (Reading_data n, rest)
      | _ -> Invalid (Parse_error (Printf.sprintf "invalid bulk length %S" digits))))
  | Reading_data n ->
    if String.length buf < n then Awaiting_more
    else
      let body = String.sub buf 0 n in
      let rest = String.sub buf n (String.length buf - n) in
      Continue (Awaiting_crlf body, rest)
  | Awaiting_crlf body ->
    if String.length buf < 2 then Awaiting_more
    else if buf.[0] = '\r' && buf.[1] = '\n' then
      Produced (Value.Bulk_string (Some body), String.sub buf 2 (String.length buf - 2))
    else Invalid (Parse_error "expected CRLF after bulk string body")

(** [parse buf] decodes a single value from the front of [buf]. *)
let parse (buf : string) : outcome =
  let rec drive state buf =
    match step state buf with
    | Continue (state', buf') -> drive state' buf'
    | Produced (value, rest) -> Done (value, rest)
    | Awaiting_more -> Incomplete
    | Invalid e -> Failed e
  in
  drive initial buf

(* A resumable parser handle: the current state plus the bytes seen so far that
   have not yet completed a value. Abstract to callers. *)
type partial = {
  state : state;
  pending : string;
}

let create () : partial = { state = initial; pending = "" }

(** [feed parser chunk] appends [chunk] to the buffered bytes and drains every
    complete value it can, returning them in order along with a parser carrying
    any leftover partial frame. *)
let feed (parser : partial) (chunk : string) :
    (Value.t list * partial, error) result =
  let rec loop state buf acc =
    match step state buf with
    | Continue (state', buf') -> loop state' buf' acc
    | Produced (value, rest) -> loop initial rest (value :: acc)
    | Awaiting_more -> Ok (List.rev acc, { state; pending = buf })
    | Invalid e -> Error e
  in
  loop parser.state (parser.pending ^ chunk) []
