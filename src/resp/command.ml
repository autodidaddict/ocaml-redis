(* SET key value [NX | XX] [GET] [EX s | PX ms | EXAT s | PXAT ms | KEEPTTL] *)
type existence =
  | Always  (** no NX/XX supplied *)
  | If_not_exists  (** NX *)
  | If_exists  (** XX *)

(** SET's expiry option. One field, so EX/PX/EXAT/PXAT/KEEPTTL are mutually
    exclusive by construction. EX/PX are relative durations; EXAT/PXAT are
    absolute Unix timestamps — keep them distinct here and resolve to an
    absolute deadline at apply time. *)
type expiry =
  | Expire_seconds of int  (** EX, seconds, relative *)
  | Expire_millis of int  (** PX, milliseconds, relative *)
  | Expire_at_seconds of int  (** EXAT, Unix time, seconds *)
  | Expire_at_millis of int  (** PXAT, Unix time, milliseconds *)
  | Keep_ttl  (** KEEPTTL - retain existing ttl*)

type set_options = {
  key : string;
  value : string;
  existence : existence;
  get : bool;
  expiry : expiry option;
}

(** A strongly typed client command, lifted from a raw {!Value.t}.

    Clients send commands as RESP arrays of bulk strings, e.g.
    [*1\r\n$4\r\nPING\r\n]. {!of_value} turns that wire shape into a typed
    command so dispatch can match on [Ping] / [Echo] rather than on array
    structure. *)
type t = Ping | Echo of string | Get of string | Set of set_options

(* A positive integer expiry argument, or None if it isn't one. Redis rejects
   zero and negatives, so we enforce that here at the wire boundary. *)
let positive_int (s : string) : int option =
  match int_of_string_opt s with Some n when n > 0 -> Some n | _ -> None

(* Recognise an expiry keyword, returning the constructor that still needs its
   numeric argument (or None for any non-expiry token). Folding recognition and
   construction together keeps the keyword set in exactly one place — no separate
   list to match against and then map over, and no unreachable [assert false]. *)
let expiry_kw : string -> (int -> expiry) option = function
  | "EX" -> Some (fun n -> Expire_seconds n)
  | "PX" -> Some (fun n -> Expire_millis n)
  | "EXAT" -> Some (fun n -> Expire_at_seconds n)
  | "PXAT" -> Some (fun n -> Expire_at_millis n)
  | _ -> None

(* Parse SET's trailing options (everything after key and value) into a
   [set_options]. Options may appear in any order and keywords are
   case-insensitive, so we recurse over the token list, refining an accumulator
   and rejecting anything that conflicts with what we have already accepted. *)
let parse_set (key : string) (value : string) (opts : Value.t list) :
    (t, string) result =
  let rec go (acc : set_options) (tokens : Value.t list) : (t, string) result =
    match tokens with
    | [] -> Ok (Set acc)
    | Bulk_string (Some tok) :: rest -> (
        match String.uppercase_ascii tok with
        | "NX" -> with_existence acc If_not_exists rest
        | "XX" -> with_existence acc If_exists rest
        | "GET" -> go { acc with get = true } rest
        | "KEEPTTL" -> with_expiry acc Keep_ttl rest
        | kw -> (
            (* Anything else must be EX/PX/EXAT/PXAT taking the next token as a
               positive integer; expiry_kw returns None for unknown keywords. *)
            match (expiry_kw kw, rest) with
            | Some make, Bulk_string (Some n) :: rest -> (
                match positive_int n with
                | Some n -> with_expiry acc (make n) rest
                | None ->
                    Error (Printf.sprintf "SET: invalid expire time in '%s'" kw))
            | Some _, _ ->
                Error (Printf.sprintf "SET: %s requires an integer argument" kw)
            | None, _ ->
                Error (Printf.sprintf "SET: unsupported option '%s'" kw)))
    | _ :: _ -> Error "SET: options must be bulk strings"
  (* The type already forbids two NX/XX or two expiries; these guards turn that
     same conflict, when it arrives on the wire, into a wire error. *)
  and with_existence acc cond rest =
    if acc.existence <> Always then Error "SET: NX and XX cannot be combined"
    else go { acc with existence = cond } rest
  and with_expiry acc e rest =
    if acc.expiry <> None then Error "SET: only one expiry option is allowed"
    else go { acc with expiry = Some e } rest
  in
  go { key; value; existence = Always; get = false; expiry = None } opts

(** [of_value v] interprets [v] as a client command, or returns a human-readable
    error if it is not a command this server understands. *)
let of_value (value : Value.t) : (t, string) result =
  match value with
  | Array (Bulk_string (Some name) :: args) -> (
      match (String.uppercase_ascii name, args) with
      | "PING", _ -> Ok Ping
      | "ECHO", [ Bulk_string (Some msg) ] -> Ok (Echo msg)
      | "ECHO", _ -> Error "ECHO expects one argument"
      | "GET", [ Bulk_string (Some key) ] -> Ok (Get key)
      | "GET", _ -> Error "GET expects one argument"
      | "SET", Bulk_string (Some key) :: Bulk_string (Some value) :: opts ->
          parse_set key value opts
      | "SET", _ -> Error "SET expects at least a key and a value"
      | other, _ -> Error (Printf.sprintf "unknown command '%s'" other))
  | _ -> Error "expected a non-empty array of bulk strings"
