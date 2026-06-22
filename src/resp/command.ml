(** A strongly typed client command, lifted from a raw {!Value.t}.

    Clients send commands as RESP arrays of bulk strings, e.g.
    [*1\r\n$4\r\nPING\r\n]. {!of_value} turns that wire shape into a typed command
    so dispatch can match on [Ping] / [Echo] rather than on array structure. *)
type t =
  | Ping
  | Echo of string

(** [of_value v] interprets [v] as a client command, or returns a human-readable
    error if it is not a command this server understands. *)
let of_value (value : Value.t) : (t, string) result =
  match value with
  | Array (Bulk_string (Some name) :: args) -> (
    match (String.uppercase_ascii name, args) with
    | "PING", _ -> Ok Ping
    | "ECHO", [ Bulk_string (Some msg) ] -> Ok (Echo msg)
    | "ECHO", _ -> Error "ECHO expects one argument"
    | other, _ -> Error (Printf.sprintf "unknown command '%s'" other))
  | _ -> Error "expected a non-empty array of bulk strings"
